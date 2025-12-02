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
# Load cleaned data (from EDA)
# ------------------------------------------------------------
diabetes <- readr::read_rds("data/diabetes_clean.rds")

# ------------------------------------------------------------
# Fit final RF model to full dataset
# ------------------------------------------------------------
rf_spec <- rand_forest(
  mtry  = 2,     
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
# Get default predictor values
# ------------------------------------------------------------
# Means for numeric predictors
num_means <- diabetes |>
  summarise(
    bmi_mean = mean(bmi, na.rm = TRUE),
  ) |>
  as.list()

# Mode function for factor predictors
mode_fun <- function(x) {
  tab <- table(x)
  names(tab)[which.max(tab)]
}

# Defaults for categorical predictors
cat_defaults <- list(
  age_mode = mode_fun(diabetes$age),
  high_bp_mode = mode_fun(diabetes$high_bp),
  high_chol_mode = mode_fun(diabetes$high_chol),
  phys_activity_mode = mode_fun(diabetes$phys_activity),
  smoker_mode = mode_fun(diabetes$smoker)
)

# ------------------------------------------------------------
# End of Setup Section
# ------------------------------------------------------------

# ------------------------------------------------------------
# API title 
# ------------------------------------------------------------
#* @apiTitle Diabetes Random Forest API

# ------------------------------------------------------------
# Prediction Endpoint
# ------------------------------------------------------------
#* Predict diabetes status
#* @description Valid inputs for `age` are:
#* "18–24", "25–29", "30–34", "35–39",
#* "40–44", "45–49", "50–54", "55–59",
#* "60–64", "65–69", "70–74", "75–79", "80+"
#* @param bmi Body mass index 
#* @param age Age in years 
#* @param high_bp High blood pressure 
#* @param high_chol High cholesterol 
#* @param phys_activity Physical activity 
#* @param smoker Smoker status 
#* @get /pred

function(
    bmi = num_means$bmi_mean,
    age = cat_defaults$age_mode,
    high_bp = cat_defaults$high_bp_mode,
    high_chol = cat_defaults$high_chol_mode,
    phys_activity = cat_defaults$phys_activity_mode,
    smoker = cat_defaults$smoker_mode
) {
  
  # Build 1-row input table
  api_data <- tibble(
    bmi = as.numeric(bmi),
    age = factor(age,
                 levels = levels(diabetes$age),
                 ordered = is.ordered(diabetes$age)),
    high_bp = factor(high_bp, levels = levels(diabetes$high_bp)),
    high_chol = factor(high_chol, levels = levels(diabetes$high_chol)),
    phys_activity = factor(phys_activity, levels = levels(diabetes$phys_activity)),
    smoker = factor(smoker, levels = levels(diabetes$smoker))
  )
  
  # Run predictions on that input row
  predict_class <- predict(rf_fit_full, new_data = api_data, type = "class")
  predict_prob  <- predict(rf_fit_full, new_data = api_data, type = "prob")
  
  # Return as a simple tibble (plumber will convert to JSON)
  tibble(
    pred = predict_class$.pred_class,
    prob_No  = predict_prob$.pred_No,
    prob_Yes = predict_prob$.pred_Yes
  )
}
# Example calls (run after starting Plumber locally)

## library(httr)

## 1) Call with all default values:
## GET("http://127.0.0.1:8000/pred")

## 2) Call while overriding BMI and age group only:
## GET("http://127.0.0.1:8000/pred", query = list(bmi = 28, age = "50–54"))

## 3) Call while specifying all predictor values:
## GET("http://127.0.0.1:8000/pred", query = list(
##     bmi = 26,
##     age = "40–44",
##     high_bp = "No",
##     high_chol = "Yes",
##     phys_activity = "Yes",
##     smoker = "No"
# ------------------------------------------------------------

