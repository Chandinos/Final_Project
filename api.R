# api.R
# API for diabetes prediction using final random forest model

# ------------------------------------------------------------
# Load required packages
# ------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(janitor)
library(plumber)
library(ggplot2)

set.seed(987)

# ------------------------------------------------------------
# Read cleaned data (from EDA)
# ------------------------------------------------------------
diabetes <- readr::read_rds("data/diabetes_clean.rds")

# ------------------------------------------------------------
# Specify and fit the final Random Forest model on FULL data
# ------------------------------------------------------------
rf_spec <- rand_forest(
  mtry  = 2,      # from tuning
  trees = 100,
  min_n = 20
) |>
  set_mode("classification") |>
  set_engine("ranger")

rf_wf <- workflow() |>
  add_model(rf_spec) |>
  add_formula(diabetes_binary ~ bmi + age + high_bp +
                high_chol + phys_activity + smoker)

rf_fit_full <- fit(rf_wf, data = diabetes)

# ------------------------------------------------------------
# Compute default values for `/pred` endpoint inputs
# ------------------------------------------------------------
# Means for numeric predictors
num_means <- diabetes |>
  summarise(
    bmi_mean = mean(bmi, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE)
  ) |>
  as.list()

# Mode function for factor predictors
mode_fun <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}

# Defaults for categorical predictors
cat_defaults <- list(
  high_bp_mode       = mode_fun(diabetes$high_bp),
  high_chol_mode     = mode_fun(diabetes$high_chol),
  phys_activity_mode = mode_fun(diabetes$phys_activity),
  smoker_mode        = mode_fun(diabetes$smoker)
)

# ------------------------------------------------------------
# End of Setup Section
# ------------------------------------------------------------
