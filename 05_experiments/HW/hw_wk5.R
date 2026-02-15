
### #CONCEPTUAL Qs: Large experiments rely on event logs, so changes in how outcomes are recorded can create fake treatment effects. If measurement definitions shift or events are inconsistently logged, differences between conditions may reflect instrumentation changes rather than real treatment impact. Randomization does not protect against measurement drift because it balances assignment to conditions, not the stability of the measurement system. If the logging process changes over time, all groups may be affected, making results unreliable.

###############################################
# SECTION 1: INITIAL SETUP AND CONFIGURATION
###############################################

# install.packages(c("devtools", "renv", "logger", "tidyverse", "data.table", "estimatr", "broom"))

library(logger)
library(tidyverse)
library(data.table)
library(estimatr)
library(broom)

# renv:
# renv::init()
# renv::restore()
# renv::snapshot()

###############################################
# SECTION 2: PROJECT DIRECTORY SETUP
###############################################

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

###############################################
# SECTION 3: LOGGING SETUP
###############################################

logger::log_threshold(INFO)
logger::log_appender(appender_file("analysis_log.txt"))

###############################################
# SECTION 4: BIG DATA EXPERIMENT PIPELINE (SEQUENTIAL)
###############################################

###############################################
# STEP 0: GLOBAL SETTINGS
###############################################

set.seed(123)

n_users <- 100000
n_days  <- 14

log_info("Starting big data experiment pipeline")
log_info(paste0("n_users = ", n_users, " | n_days = ", n_days))

###############################################
# STEP 1: GENERATE SYNTHETIC USERS (UNIT TABLE)
###############################################

log_info("Generating synthetic user table")

users <- tibble(
  user_id = 1:n_users,
  platform = sample(c("ios", "android", "web"), n_users, replace = TRUE, prob = c(0.35, 0.35, 0.30)),
  cluster_id = sample(1:500, n_users, replace = TRUE),
  baseline_activity = rgamma(n_users, shape = 2, scale = 2),
  signup_cohort = sample(c("cohort_A", "cohort_B", "cohort_C"), n_users, replace = TRUE, prob = c(0.40, 0.35, 0.25))
)

users <- users %>%
  mutate(
    pre_metric = baseline_activity + rnorm(n_users, 0, 0.5)
  )

write.csv(users, "data/raw/users.csv", row.names = FALSE)
log_info("Saved: data/raw/users.csv")

###############################################
# STEP 2: BLOCKING + RANDOM ASSIGNMENT (SAVE ASSIGNMENT!)
###############################################

log_info("Creating blocked assignment table (and saving it)")

users <- users %>%
  mutate(
    block = ntile(baseline_activity, 10)
  )

assignment <- users %>%
  group_by(block) %>%
  mutate(
    treat = rbinom(n(), size = 1, prob = 0.5)
  ) %>%
  ungroup() %>%
  select(user_id, treat, block, platform, cluster_id, signup_cohort, baseline_activity, pre_metric) %>%
  mutate(
    assignment_date = as.Date("2026-04-16")
  )

write.csv(assignment, "data/raw/assignment_table.csv", row.names = FALSE)
log_info("Saved: data/raw/assignment_table.csv")

###############################################
# STEP 3: GENERATE RAW EVENT LOGS (USER-DAY BIG TABLE)
###############################################

log_info("Generating synthetic event logs (user-day table)")

dt_assign <- as.data.table(assignment)

dt_days <- data.table(day = 1:n_days)
dt_days[, dummy := 1]
dt_assign[, dummy := 1]

dt_logs <- merge(dt_assign, dt_days, by = "dummy", allow.cartesian = TRUE)
dt_logs[, dummy := NULL]

dt_logs[, date := as.Date(assignment_date) + day - 1]
dt_logs[, dow := as.integer(format(date, "%u"))]

dt_logs[, logged_ok := rbinom(.N, 1, 0.98)]

# --------------------------------------------
# Q5: NONCOMPLIANCE (treatment receipt)
# --------------------------------------------
p_receive <- 0.80
log_info(paste0("Noncompliance enabled: p_receive = ", p_receive))

dt_logs[, received := 0L]
dt_logs[treat == 1, received := rbinom(.N, 1, p_receive)]

# Underlying click intensity
dt_logs[, base_rate :=
          exp(-1.2 +
                0.15 * log1p(baseline_activity) +
                0.05 * (platform == "ios") +
                0.03 * (platform == "android") +
                0.02 * (dow %in% c(6,7)) +
                0.01 * day
          )
]

# Treatment effect operates through RECEIVED (not assignment)
dt_logs[, click_rate := base_rate * exp(0.05 * received)]
dt_logs[, clicks := rpois(.N, lambda = click_rate)]

# Conversion probability (effect operates through RECEIVED)
dt_logs[, purchase_prob :=
          plogis(-5.0 +
                   0.08 * clicks +
                   0.10 * log1p(baseline_activity) +
                   0.15 * received +
                   0.02 * (dow %in% c(6,7))
          )
]

dt_logs[, purchase := rbinom(.N, 1, purchase_prob)]
dt_logs[, active := as.integer(clicks > 0 | purchase > 0)]

dt_logs[logged_ok == 0, clicks := NA_integer_]
dt_logs[logged_ok == 0, purchase := NA_integer_]
dt_logs[logged_ok == 0, active := NA_integer_]

fwrite(dt_logs, "data/raw/event_logs.csv")
log_info("Saved: data/raw/event_logs.csv")

###############################################
# STEP 4: BUILD AN ANALYSIS-READY DATASET (USER-LEVEL)
###############################################

log_info("Building analysis-ready dataset (user-level aggregation)")

dt_user <- dt_logs[, .(
  post_clicks = sum(clicks, na.rm = TRUE),
  post_purchases = sum(purchase, na.rm = TRUE),
  converted = as.integer(sum(purchase, na.rm = TRUE) > 0),
  days_observed = sum(!is.na(active)),
  missing_share = mean(is.na(active)),
  
  # Q4: retention-style outcomes (ignore missing days)
  days_active = sum(active == 1, na.rm = TRUE),
  retained_any = as.integer(sum(active == 1, na.rm = TRUE) >= 1),
  
  # Keep received at user-level (constant within user by construction)
  received = max(received, na.rm = TRUE)
), by = .(user_id, treat, block, platform, cluster_id, signup_cohort, baseline_activity, pre_metric)]

fwrite(dt_user, "data/processed/analysis_dataset.csv")
log_info("Saved: data/processed/analysis_dataset.csv")

###############################################
# STEP 5: RANDOMIZATION CHECKS / BALANCE CHECKS
###############################################

log_info("Running randomization checks / balance checks")

dt_temp <- as_tibble(dt_user)

balance_table <- dt_temp %>%
  group_by(treat) %>%
  summarize(
    n = n(),
    mean_baseline_activity = mean(baseline_activity, na.rm = TRUE),
    mean_pre_metric = mean(pre_metric, na.rm = TRUE),
    mean_missing_share = mean(missing_share, na.rm = TRUE),
    mean_days_active = mean(days_active, na.rm = TRUE),
    mean_retained_any = mean(retained_any, na.rm = TRUE),
    mean_received = mean(received, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(balance_table, "outputs/tables/balance_means.csv", row.names = FALSE)

mean_c <- dt_temp %>% filter(treat == 0) %>% summarize(m = mean(baseline_activity)) %>% pull(m)
mean_t <- dt_temp %>% filter(treat == 1) %>% summarize(m = mean(baseline_activity)) %>% pull(m)
sd_c   <- dt_temp %>% filter(treat == 0) %>% summarize(s = sd(baseline_activity)) %>% pull(s)
sd_t   <- dt_temp %>% filter(treat == 1) %>% summarize(s = sd(baseline_activity)) %>% pull(s)
sd_pool <- sqrt((sd_c^2 + sd_t^2) / 2)
smd_baseline_activity <- (mean_t - mean_c) / sd_pool

mean_c2 <- dt_temp %>% filter(treat == 0) %>% summarize(m = mean(pre_metric)) %>% pull(m)
mean_t2 <- dt_temp %>% filter(treat == 1) %>% summarize(m = mean(pre_metric)) %>% pull(m)
sd_c2   <- dt_temp %>% filter(treat == 0) %>% summarize(s = sd(pre_metric)) %>% pull(s)
sd_t2   <- dt_temp %>% filter(treat == 1) %>% summarize(s = sd(pre_metric)) %>% pull(s)
sd_pool2 <- sqrt((sd_c2^2 + sd_t2^2) / 2)
smd_pre_metric <- (mean_t2 - mean_c2) / sd_pool2

smd_table <- tibble(
  variable = c("baseline_activity", "pre_metric"),
  smd = c(smd_baseline_activity, smd_pre_metric)
)

write.csv(smd_table, "outputs/tables/balance_smd.csv", row.names = FALSE)

###############################################
# STEP 6: ESTIMATE EXPERIMENTAL EFFECTS (ATE)
###############################################

log_info("Estimating treatment effects (ATE)")

ate_converted <- with(dt_temp, mean(converted[treat == 1]) - mean(converted[treat == 0]))
ate_purchases <- with(dt_temp, mean(post_purchases[treat == 1]) - mean(post_purchases[treat == 0]))
ate_clicks    <- with(dt_temp, mean(post_clicks[treat == 1]) - mean(post_clicks[treat == 0]))

ate_simple <- tibble(
  outcome = c("converted", "post_purchases", "post_clicks"),
  ate_diff_in_means = c(ate_converted, ate_purchases, ate_clicks)
)

write.csv(ate_simple, "outputs/tables/ate_diff_in_means.csv", row.names = FALSE)

fit_conv <- lm_robust(converted ~ treat + baseline_activity + pre_metric + factor(block),
                      data = dt_temp,
                      clusters = cluster_id)

fit_pur  <- lm_robust(post_purchases ~ treat + baseline_activity + pre_metric + factor(block),
                      data = dt_temp,
                      clusters = cluster_id)

tidy_conv <- broom::tidy(fit_conv)
tidy_pur  <- broom::tidy(fit_pur)

write.csv(tidy_conv, "outputs/tables/regression_converted.csv", row.names = FALSE)
write.csv(tidy_pur,  "outputs/tables/regression_purchases.csv", row.names = FALSE)

###############################################
# STEP 6B: Q4 RETENTION OUTCOMES ATE
###############################################

log_info("Estimating treatment effects (ATE) for retention outcomes")

ate_days_active  <- with(dt_temp, mean(days_active[treat == 1]) - mean(days_active[treat == 0]))
ate_retained_any <- with(dt_temp, mean(retained_any[treat == 1]) - mean(retained_any[treat == 0]))

fit_days_active <- lm_robust(days_active ~ treat + baseline_activity + pre_metric + factor(block),
                             data = dt_temp,
                             clusters = cluster_id)

fit_retained_any <- lm_robust(retained_any ~ treat + baseline_activity + pre_metric + factor(block),
                              data = dt_temp,
                              clusters = cluster_id)

t_days_active <- broom::tidy(fit_days_active) %>% filter(term == "treat")
t_retained_any <- broom::tidy(fit_retained_any) %>% filter(term == "treat")

ate_retention <- tibble(
  outcome = c("days_active", "retained_any"),
  ate_diff_in_means = c(ate_days_active, ate_retained_any),
  ate_reg_adj = c(t_days_active$estimate, t_retained_any$estimate),
  se_cluster_robust = c(t_days_active$std.error, t_retained_any$std.error),
  p_value = c(t_days_active$p.value, t_retained_any$p.value)
)

write.csv(ate_retention, "outputs/tables/ate_retention.csv", row.names = FALSE)
log_info("Saved: outputs/tables/ate_retention.csv")

###############################################
# STEP 6C: Q5 ITT VS TOT (IV) UNDER NONCOMPLIANCE
###############################################

log_info("Estimating ITT vs TOT (IV) under noncompliance")

# ITT: assignment effect
fit_itt <- lm_robust(converted ~ treat + baseline_activity + pre_metric + factor(block),
                     data = dt_temp,
                     clusters = cluster_id)

# TOT/LATE: receipt effect (IV), instrument = treat
fit_tot <- iv_robust(converted ~ received + baseline_activity + pre_metric + factor(block) |
                       treat + baseline_activity + pre_metric + factor(block),
                     data = dt_temp,
                     clusters = cluster_id)

itt_row <- broom::tidy(fit_itt) %>% filter(term == "treat")
tot_row <- broom::tidy(fit_tot) %>% filter(term == "received")

itt_vs_tot <- tibble(
  p_receive = p_receive,
  outcome = "converted",
  estimand = c("ITT (assignment effect)", "TOT/LATE (receipt effect via IV)"),
  estimate = c(itt_row$estimate, tot_row$estimate),
  se_cluster_robust = c(itt_row$std.error, tot_row$std.error),
  p_value = c(itt_row$p.value, tot_row$p.value)
)

write.csv(itt_vs_tot, "outputs/tables/itt_vs_tot.csv", row.names = FALSE)
log_info("Saved: outputs/tables/itt_vs_tot.csv")

###############################################
# STEP 7: VISUALIZATIONS (BIG DATA EXPERIMENT DIAGNOSTICS)
###############################################

log_info("Creating figures")

p1 <- ggplot(dt_temp, aes(x = baseline_activity)) +
  geom_histogram(bins = 60) +
  facet_wrap(~ treat, ncol = 1, labeller = labeller(treat = c(`0` = "Control", `1` = "Treatment"))) +
  theme_bw() +
  labs(title = "Baseline Activity Distribution by Treatment Arm",
       x = "Baseline activity", y = "Count")

ggsave("outputs/figures/baseline_activity_by_treat.png", p1, width = 9, height = 6)

dt_day <- dt_logs[, .(
  conversion_rate = mean(purchase, na.rm = TRUE),
  mean_clicks = mean(clicks, na.rm = TRUE),
  missing_share = mean(is.na(purchase))
), by = .(date, day, treat)]

dt_day_tbl <- as_tibble(dt_day)

p2 <- ggplot(dt_day_tbl, aes(x = date, y = conversion_rate, group = factor(treat))) +
  geom_line() +
  theme_bw() +
  labs(title = "Daily Conversion Rate by Treatment Arm",
       x = "Date", y = "Conversion rate")

ggsave("outputs/figures/daily_conversion_rate.png", p2, width = 10, height = 4)

p3 <- ggplot(dt_day_tbl, aes(x = date, y = missing_share, group = factor(treat))) +
  geom_line() +
  theme_bw() +
  labs(title = "Daily Missingness in Logged Purchases (Instrumentation Check)",
       x = "Date", y = "Share missing")

ggsave("outputs/figures/daily_missingness.png", p3, width = 10, height = 4)

###############################################
# STEP 8: PLACEBO TESTS (A/A + PLACEBO OUTCOME)
###############################################

log_info("Running placebo tests (A/A and placebo outcome)")

placebo_pre <- lm_robust(pre_metric ~ treat + factor(block), data = dt_temp)
write.csv(broom::tidy(placebo_pre), "outputs/tables/placebo_pre_metric.csv", row.names = FALSE)

set.seed(123)

control_ids <- dt_temp %>% filter(treat == 0) %>% pull(user_id)
aa_results <- tibble()

for (b in 1:200) {
  
  a_group <- sample(control_ids, size = floor(length(control_ids) / 2), replace = FALSE)
  
  dt_sub <- dt_temp %>%
    filter(treat == 0) %>%
    mutate(aa = as.integer(user_id %in% a_group))
  
  aa_effect <- with(dt_sub, mean(converted[aa == 1]) - mean(converted[aa == 0]))
  
  aa_results <- aa_results %>%
    bind_rows(tibble(iter = b, aa_effect = aa_effect))
}

# Optional: save A/A results if you want
write.csv(aa_results, "outputs/tables/aa_results.csv", row.names = FALSE)

writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")
log_info("Pipeline complete")

```
