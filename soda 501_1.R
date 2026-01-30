# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
# Install (if needed) and load the necessary libraries.
#
# For teaching: keep installation lines commented out so students can run them
# manually if needed.

# install.packages(c("renv", "logger", "tidyverse", "broom"))

library(renv)       # Dependency management (renv.lock)
library(logger)     # Logging pipeline steps
library(tidyverse)  # Data manipulation + plotting
library(broom)      # Tidy regression outputs (for tables)


# renv::init() creates a project-local library and an renv.lock file.
# Teaching workflow:
# - Run renv::init() ONCE at the start of a project (not every time).
# - After that, use renv::snapshot() to record package versions.
# - On another machine, use renv::restore() to recreate the environment.
#
# NOTE: We leave renv::init() commented out to avoid re-initializing by accident.

#renv::init()
#renv::snapshot()

# Reproducible projects should separate:
# - raw data (unchanged inputs)
# - processed data (cleaned outputs)
# - figures and tazbles (final outputs)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# Logging creates an audit trail:
# - What ran
# - In what order
# - With what parameters
# - Where outputs were written

logger::log_threshold(DEBUG)
logger::log_appender(appender_file("analysis_log.txt"))

# Pipeline overview:
#   1) Load data
#   2) Save raw data (confirm location)
#   3) Clean data
#   4) Save processed data
#   5) Run three regressions (income as DV)
#   6) Create plot(s)
#   7) Save tables + session info

set.seed(123)  # Reproducible randomness for the full pipeline

log_info("Starting analysis pipeline")

# Expected location for this assignment:
# - data/raw/education_income.csv

log_info("Loading education/income dataset from data/raw/education_income.csv")

# read

log_info(paste("Rows loaded:", nrow(education_income_raw)))
log_info(paste("Columns loaded:", ncol(education_income_raw)))

# In many projects, "raw" is treated as read-only and comes from outside.
# Here we re-write it to confirm the exact file used in the run.

log_info("Saving raw data copy (unchanged)")
# readr::write_csv(education_income_raw, "data/raw/education_income.csv")

# Keep this simple and explicit:
# - Ensure education and income exist
# - Coerce to numeric (if needed)
# - Drop missing
#
# Note: No if/else. If columns are missing, the script will error (which is fine).

log_info("Cleaning education/income data")

education_income_clean <- education_income_raw |>
  dplyr::mutate(
    education = as.numeric(education),
    income    = as.numeric(income)
  ) |>
  dplyr::filter(!is.na(education), !is.na(income))

log_info(paste("Rows after cleaning:", nrow(education_income_clean)))

# Create log-income version for Model 3
# If income has zeros or negatives, log(income) is not finite.
education_income_clean <- education_income_clean |>
  dplyr::mutate(log_income = log(income))

education_income_log <- education_income_clean |>
  dplyr::filter(is.finite(log_income))

log_info(paste("Rows with finite log(income):", nrow(education_income_log)))

log_info("Saving processed data")
readr::write_csv(education_income_clean, "data/processed/cleaned_education_income.csv")


log_info("Fitting Model 1: income ~ education")
# TODO: model_1 <- ...

log_info("Fitting Model 2: income ~ education + I(education^2)")
# TODO: model_2 <- ...

log_info("Fitting Model 3: log(income) ~ education (finite log income rows only)")
# TODO: model_3 <- ...

# Save model summaries (plain text) for replication checks
log_info("Saving regression summaries to outputs/tables/")
# TODO: writeLines(capture.output(summary(model_1)), "outputs/tables/model_1_summary.txt")
# TODO: writeLines(capture.output(summary(model_2)), "outputs/tables/model_2_summary.txt")
# TODO: writeLines(capture.output(summary(model_3)), "outputs/tables/model_3_summary.txt")
# TODO: create and write a regression_coefficients.csv table

# TODO (students):
# - Write sessionInfo() output to outputs/session_info.txt

log_info("Saving session information")
# TODO: writeLines(capture.output(sessionInfo()), "outputs/session_info.txt")


# TODO (students):
# - After everything runs, snapshot dependencies.
# - Commit renv.lock to GitHub.

log_info("Snapshotting dependencies to renv.lock")
#renv::snapshot()

log_info("Analysis pipeline completed successfully")
