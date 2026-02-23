# Time leakage happens when the model accidentally “sees the future” during training, even indirectly, because the train/test split breaks the time order. With a random split,  training points can come from after some test points, so the fitted model can use patterns that only exist later in time (trend, seasonality, autocorrelation), making test performance look better than it would be in real deployment. That is why random splits usually inflate performance for time-indexed data: they mix past and future and destroy the true forecasting problem. “Train on past, test on future” is the default rule because it matches how forecasting works in the real world—at prediction time, you only have history. Rolling-origin evaluatiom (backtesting) goes beyond a single split by repeating this past→future prediction many times, giving many out-of-sample errors and a more stable estimate of forecasting RMSE.

If you want the decomposition figure to be one custom “single panel” layout (Observed + Trend + Seasonal + Remainder stacked with the same x-axis), tell me and I’ll rewrite the plotting section to build it manually (still no functions).


############################################
# PART B: Synthetic DGP demo (autocorrelation + trend) + ACF/PACF diagnostics
############################################

# --- 6) Generate data from a known DGP: trend + AR(1) errors
set.seed(123)
n2 <- 300
t2 <- 1:n2

alpha <- 5
delta <- 0.03
phi2 <- 0.75
u <- rnorm(n2, mean = 0, sd = 1)

e <- rep(NA_real_, n2)
e[1] <- u[1]
for (i in 2:n2) {
  e[i] <- phi2 * e[i - 1] + u[i]
}

y2 <- alpha + delta * t2 + e

# --- 7) Plot the DGP series (SAVE)
png(filename = file.path(out_dir, "03_dgp_series.png"),
    width = 1600, height = 900, res = 150)
plot(t2, y2, type = "l",
     main = "Synthetic DGP: linear trend + AR(1) errors",
     xlab = "t", ylab = "y_t")
dev.off()

# --- 8) Diagnose dependence with ACF and PACF (SAVE)
png(filename = file.path(out_dir, "04_acf_y2.png"),
    width = 1600, height = 900, res = 150)
acf(y2, main = "ACF of y_t (trend + AR errors)")
dev.off()

png(filename = file.path(out_dir, "05_pacf_y2.png"),
    width = 1600, height = 900, res = 150)
pacf(y2, main = "PACF of y_t (trend + AR errors)")
dev.off()

# --- 9) Detrend and re-check ACF/PACF on residuals
fit_lm <- lm(y2 ~ t2)
resid2 <- residuals(fit_lm)

# Residual time plot (SAVE)
png(filename = file.path(out_dir, "06_residuals_detrended.png"),
    width = 1600, height = 900, res = 150)
plot(t2, resid2, type = "l",
     main = "Residuals after removing linear trend (should still show AR structure)",
     xlab = "t", ylab = "residual")
dev.off()

# ACF/PACF of residuals (SAVE)
png(filename = file.path(out_dir, "07_acf_resid2.png"),
    width = 1600, height = 900, res = 150)
acf(resid2, main = "ACF of residuals (trend removed)")
dev.off()

png(filename = file.path(out_dir, "08_pacf_resid2.png"),
    width = 1600, height = 900, res = 150)
pacf(resid2, main = "PACF of residuals (trend removed)")
dev.off()

# --- 10) Fit an AR(1) model to residuals and compare estimated phi to truth
fit_ar1 <- arima(resid2, order = c(1,0,0))
cat("\n==============================\n")
cat("DGP truth phi2 = ", phi2, "\n")
cat("Estimated AR(1) phi from residuals = ", fit_ar1$coef[1], "\n")
cat("==============================\n")

# --- 11) Narration-ready takeaway
cat("\nNarration-ready takeaway:\n")
cat("- In the DGP, we *know* the errors are AR(1), so observations are dependent over time.\n")
cat("- ACF/PACF make that dependence visible.\n")
cat("- Removing trend helps isolate autocorrelation in the error process.\n")
cat("- Separately: random splits leak time and look too good; past->future splits are the honest default.\n")

cat("\nSaved plots to: ", normalizePath(out_dir), "\n")





# --- 0) Setup
set.seed(123)

# Choose where to save (matches your folder idea)
# Option A (recommended): save inside your 06_timeseries/problem_set folder
out_dir <- file.path("06_timeseries", "problem_set", "output_images")

# Option B: current working directory (uncomment if you prefer)
# out_dir <- "output_images"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Helper for filenames (NO user-defined functions requested, so we do it manually)
# We'll open a PNG device before each plot, then dev.off() after each.

# Optional package check for auto.arima (nice-to-have)
use_forecast <- requireNamespace("forecast", quietly = TRUE)

# --- 1) Create a synthetic daily time series (trend + weekly seasonality + AR(1) noise)
n <- 600
dates <- seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = n)
t <- 1:n

trend <- 0.02 * t
weekly <- 1.2 * sin(2 * pi * t / 7)

phi <- 0.65
eps <- rnorm(n, mean = 0, sd = 1.0)
ar_noise <- rep(NA_real_, n)
ar_noise[1] <- eps[1]
for (i in 2:n) {
  ar_noise[i] <- phi * ar_noise[i - 1] + eps[i]
}

y <- 10 + trend + weekly + ar_noise

df <- data.frame(
  date = dates,
  t = t,
  y = y
)

# --- 2) Visualize the series (SAVE)
png(filename = file.path(out_dir, "01_synthetic_series.png"),
    width = 1600, height = 900, res = 150)
plot(df$date, df$y, type = "l",
     main = "Synthetic daily time series: trend + weekly seasonality + AR(1) noise",
     xlab = "Date", ylab = "y")
dev.off()

############################################
# PART A: Time leakage demo (random split vs time split)
############################################

# --- 3) WRONG evaluation: random train/test split (time leakage)
set.seed(123)
test_frac <- 0.20
test_n <- floor(n * test_frac)

test_idx_random <- sample(1:n, size = test_n, replace = FALSE)
train_idx_random <- setdiff(1:n, test_idx_random)

y_train_random <- df$y[train_idx_random]
y_test_random  <- df$y[test_idx_random]

if (use_forecast) {
  fit_random <- forecast::auto.arima(y_train_random)
  pred_random <- as.numeric(forecast::forecast(fit_random, h = length(y_test_random))$mean)
} else {
  fit_random <- arima(y_train_random, order = c(1,0,0))
  pred_random <- as.numeric(predict(fit_random, n.ahead = length(y_test_random))$pred)
}

rmse_random <- sqrt(mean((y_test_random - pred_random)^2))

cat("\n==============================\n")
cat("WRONG: Random split RMSE (time leakage): ", rmse_random, "\n")
cat("==============================\n")

# --- 4) RIGHT evaluation: train on past, test on future
cut <- n - test_n
train_idx_time <- 1:cut
test_idx_time  <- (cut + 1):n

y_train_time <- df$y[train_idx_time]
y_test_time  <- df$y[test_idx_time]

if (use_forecast) {
  fit_time <- forecast::auto.arima(y_train_time)
  pred_time <- as.numeric(forecast::forecast(fit_time, h = length(y_test_time))$mean)
} else {
  fit_time <- arima(y_train_time, order = c(1,0,0))
  pred_time <- as.numeric(predict(fit_time, n.ahead = length(y_test_time))$pred)
}

rmse_time <- sqrt(mean((y_test_time - pred_time)^2))

cat("\n==============================\n")
cat("RIGHT: Time split RMSE (train past, test future): ", rmse_time, "\n")
cat("==============================\n")

# --- 5) Plot the correct evaluation (SAVE)
png(filename = file.path(out_dir, "02_time_split_forecast.png"),
    width = 1600, height = 900, res = 150)
plot(df$date, df$y, type = "l",
     main = "Correct evaluation: train on past, test on future",
     xlab = "Date", ylab = "y")

abline(v = df$date[cut], lty = 2)
text(df$date[cut], max(df$y), labels = "cutoff", pos = 4)

lines(df$date[test_idx_time], pred_time, lty = 1)

legend("topleft",
       legend = c("Observed y", "Forecast (future)", "Train/Test cutoff"),
       lty = c(1, 1, 2),
       bty = "n")
dev.off()


out_dir <- file.path("..", "problem_set", "output_images")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)