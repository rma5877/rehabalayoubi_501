###############################################################################
## Conceptual Q: Deterministic (exact) matching can fail because small differences in data (e.g., typos, nicknames, or missing values) prevent true matches, leading to missed matches (false negatives), while loosening rules risks false matches (false positives). Probabilistic methods like fastLink address this by estimating the likelihood that records match based on partial similarities, balancing the trade-off between missed and false matches.

library(fastLink)
library(dplyr)
library(ggplot2)
library(stringdist)

base_dir <- "soda501/soda_501/13_record_linkage/problem_set"
out_dir  <- file.path(base_dir, "outputs")

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set.seed(123)
n <- 10000

df_a <- data.frame(
  id = 1:n,
  firstname = sample(c("John","Jane","Michael","Emily","David","Sarah","William","Emma","James","Olivia"), n, replace = TRUE),
  lastname = sample(c("Smith","Johnson","Williams","Brown","Jones","Garcia","Miller","Davis","Rodriguez","Martinez"), n, replace = TRUE),
  birthyear = sample(1970:2000, n, replace = TRUE),
  zipcode = sample(10000:20000, n, replace = TRUE)
) %>% distinct()

df_b <- df_a

mod_firstname <- runif(nrow(df_b)) < 0.25
mod_lastname  <- runif(nrow(df_b)) < 0.25
mod_birthyear <- runif(nrow(df_b)) < 0.25

for (i in which(mod_firstname)) {
  chars <- strsplit(df_b$firstname[i], "")[[1]]
  num_replace <- sample(1:length(chars), 1)
  positions <- sample(1:length(chars), num_replace)
  for (pos in positions) {
    chars[pos] <- sample(letters, 1)
  }
  df_b$firstname[i] <- paste0(chars, collapse = "")
}

for (i in which(mod_lastname)) {
  chars <- strsplit(df_b$lastname[i], "")[[1]]
  num_replace <- sample(1:length(chars), 1)
  positions <- sample(1:length(chars), num_replace)
  for (pos in positions) {
    chars[pos] <- sample(letters, 1)
  }
  df_b$lastname[i] <- paste0(chars, collapse = "")
}

idx_birthyear <- which(mod_birthyear)
birthyear_shift <- sample(-2:2, length(idx_birthyear), replace = TRUE)
df_b$birthyear[idx_birthyear] <- df_b$birthyear[idx_birthyear] + birthyear_shift

write.csv(df_a, file.path(base_dir, "dataset_a.csv"), row.names = FALSE)
write.csv(df_b, file.path(base_dir, "dataset_b.csv"), row.names = FALSE)

df_a <- read.csv(file.path(base_dir, "dataset_a.csv"))
df_b <- read.csv(file.path(base_dir, "dataset_b.csv"))

det_matches <- merge(
  df_a, df_b,
  by = c("firstname", "lastname", "birthyear", "zipcode")
)

det_match_count <- nrow(det_matches)
det_match_rate  <- det_match_count / nrow(df_a)

det_results <- data.frame(
  match_count = det_match_count,
  match_rate  = det_match_rate
)

cat("Number of deterministic matches:", det_match_count, "\n")
cat("Deterministic match rate:", round(det_match_rate, 4), "\n")

write.csv(det_results, file.path(out_dir, "deterministic_results.csv"), row.names = FALSE)
write.csv(det_matches, file.path(out_dir, "deterministic_matches.csv"), row.names = FALSE)

fl_out <- fastLink(
  dfA = df_a,
  dfB = df_b,
  varnames = c("firstname", "lastname", "birthyear", "zipcode"),
  return.all = TRUE
)

threshold_grid <- seq(0, 1, 0.01)

match_counts <- sapply(threshold_grid, function(th) {
  temp_matches <- getMatches(
    dfA = df_a,
    dfB = df_b,
    fl.out = fl_out,
    threshold.match = th
  )
  nrow(temp_matches)
})

count_of_matches <- data.frame(
  threshold = threshold_grid,
  matches = match_counts
)

p1 <- ggplot(count_of_matches, aes(x = threshold, y = matches)) +
  geom_line() +
  geom_point(size = 0.8) +
  labs(
    title = "Number of Matches vs. Threshold",
    x = "Threshold",
    y = "Number of Matches"
  ) +
  theme_bw()

print(p1)

ggsave(file.path(out_dir, "threshold_curve.png"), plot = p1, width = 6, height = 4)
write.csv(count_of_matches, file.path(out_dir, "threshold_match_counts.csv"), row.names = FALSE)

matches_low <- getMatches(
  dfA = df_a,
  dfB = df_b,
  fl.out = fl_out,
  threshold.match = 0.000001
)

df_a_small <- df_a %>%
  rename(
    firstname_a = firstname,
    lastname_a  = lastname,
    birthyear_a = birthyear,
    zipcode_a   = zipcode
  )

df_b_small <- df_b %>%
  rename(
    firstname_b = firstname,
    lastname_b  = lastname,
    birthyear_b = birthyear,
    zipcode_b   = zipcode
  )

if ("id" %in% names(matches_low)) {
  compare_data <- matches_low %>%
    inner_join(df_a_small, by = "id") %>%
    inner_join(df_b_small, by = "id")
} else if (all(c("id.x", "id.y") %in% names(matches_low))) {
  compare_data <- matches_low %>%
    inner_join(df_a_small, by = c("id.x" = "id")) %>%
    inner_join(df_b_small, by = c("id.y" = "id"))
} else {
  stop("Could not find usable ID columns in matches_low. Run names(matches_low).")
}

compare_data <- compare_data %>%
  mutate(
    first_name_distance = stringdist(firstname_a, firstname_b, method = "lv"),
    last_name_distance  = stringdist(lastname_a, lastname_b, method = "lv"),
    birth_year_distance = abs(birthyear_a - birthyear_b),
    posterior_bin = cut(
      posterior,
      breaks = seq(0, 1, by = 0.1),
      include.lowest = TRUE,
      right = TRUE
    )
  )

avg_dist_by_bin <- compare_data %>%
  group_by(posterior_bin) %>%
  summarize(
    mean_first = mean(first_name_distance, na.rm = TRUE),
    mean_last  = mean(last_name_distance, na.rm = TRUE),
    mean_birth = mean(birth_year_distance, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(avg_dist_by_bin)

write.csv(avg_dist_by_bin, file.path(out_dir, "posterior_diagnostics.csv"), row.names = FALSE)
write.csv(compare_data, file.path(out_dir, "candidate_matches_with_distances.csv"), row.names = FALSE)

p2 <- ggplot(compare_data, aes(x = posterior_bin, y = first_name_distance)) +
  geom_boxplot() +
  labs(
    title = "First-Name Levenshtein Distance by Posterior Bin",
    x = "Posterior Bin",
    y = "First-Name Distance"
  ) +
  theme_bw()

p3 <- ggplot(compare_data, aes(x = posterior_bin, y = last_name_distance)) +
  geom_boxplot() +
  labs(
    title = "Last-Name Levenshtein Distance by Posterior Bin",
    x = "Posterior Bin",
    y = "Last-Name Distance"
  ) +
  theme_bw()

p4 <- ggplot(compare_data, aes(x = posterior_bin, y = birth_year_distance)) +
  geom_boxplot() +
  labs(
    title = "Birth-Year Difference by Posterior Bin",
    x = "Posterior Bin",
    y = "Absolute Birth-Year Difference"
  ) +
  theme_bw()

print(p2)
print(p3)
print(p4)

ggsave(file.path(out_dir, "first_name_distance.png"), plot = p2, width = 6, height = 4)
ggsave(file.path(out_dir, "last_name_distance.png"),  plot = p3, width = 6, height = 4)
ggsave(file.path(out_dir, "birthyear_distance.png"),  plot = p4, width = 6, height = 4)

example_thresholds <- c(0.1, 0.5, 0.7, 0.9, 0.95)

example_results <- data.frame(
  threshold = numeric(),
  matches = numeric()
)

for (th in example_thresholds) {
  temp_matches <- getMatches(
    dfA = df_a,
    dfB = df_b,
    fl.out = fl_out,
    threshold.match = th
  )
  
  cat("Threshold:", th, "- Matches:", nrow(temp_matches), "\n")
  
  example_results <- rbind(
    example_results,
    data.frame(threshold = th, matches = nrow(temp_matches))
  )
}

write.csv(example_results, file.path(out_dir, "example_threshold_results.csv"), row.names = FALSE)

cat("\nFiles saved in outputs folder:\n")
print(list.files(out_dir))

write.csv(det_results, file.path(out_dir, "deterministic_results.csv"), row.names = FALSE)
write.csv(avg_dist_by_bin, file.path(out_dir, "posterior_diagnostics.csv"), row.names = FALSE)

ggsave(file.path(out_dir, "threshold_curve.png"), plot = p1, width = 6, height = 4)
ggsave(file.path(out_dir, "first_name_distance.png"), plot = p2, width = 6, height = 4)
ggsave(file.path(out_dir, "last_name_distance.png"), plot = p3, width = 6, height = 4)
ggsave(file.path(out_dir, "birthyear_distance.png"), plot = p4, width = 6, height = 4)

setwd("~/Desktop/soda501/soda_501/13_record_linkage/problem_set")
getwd()
write.csv(data.frame(test = 1), file.path(out_dir, "test.csv"), row.names = FALSE)
list.files(out_dir)
