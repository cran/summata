## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  dpi = 150
)

# Use ragg for better font rendering if available
if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png")
}

old_opts <- options(width = 180)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# compfit(data, outcome, model_list, model_type, ...)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)

data(clintrial)
data(clintrial_labels)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example1 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Demographics" = c("age", "sex"),
    "Plus Stage" = c("age", "sex", "stage", "ecog"),
    "Full Model" = c("age", "sex", "stage", "ecog", "treatment", "smoking")
  ),
  model_type = "glm"
)

example1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example2 <- compfit(
  data = clintrial,
  outcome = "los_days",
  model_list = list(
    "Simple" = c("age", "sex"),
    "Disease" = c("age", "sex", "stage", "ecog"),
    "Treatment" = c("age", "sex", "stage", "ecog", "surgery", "treatment")
  ),
  model_type = "lm"
)

example2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example3 <- compfit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  model_list = list(
    "Unadjusted" = c("treatment"),
    "Demographics" = c("treatment", "age", "sex"),
    "Full" = c("treatment", "age", "sex", "stage", "ecog")
  ),
  model_type = "coxph"
)

example3

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- compfit(
  data = clintrial,
  outcome = "fu_count",
  model_list = list(
    "Minimal" = c("age", "treatment"),
    "Clinical" = c("age", "treatment", "stage", "ecog"),
    "Full" = c("age", "treatment", "stage", "ecog", "surgery", "diabetes")
  ),
  model_type = "glm",
  family = "poisson",
  labels = clintrial_labels
)

example4

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example5 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Main Effects" = c("age", "treatment", "sex"),
    "With Interaction" = c("age", "treatment", "sex")
  ),
  interactions_list = list(
    NULL,
    c("sex:treatment")
  ),
  model_type = "glm"
)

example5

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example6 <- compfit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  model_list = list(
    "Main Effects" = c("age", "treatment", "sex", "stage"),
    "Age × Treatment" = c("age", "treatment", "sex", "stage"),
    "Sex × Treatment" = c("age", "treatment", "sex", "stage"),
    "Both" = c("age", "treatment", "sex", "stage")
  ),
  interactions_list = list(
    NULL,
    c("age:treatment"),
    c("sex:treatment"),
    c("age:treatment", "sex:treatment")
  ),
  model_type = "coxph"
)

example6

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Model A" = c("age", "sex"),
    "Model B" = c("age", "sex", "stage"),
    "Model C" = c("age", "sex", "stage", "treatment")
  ),
  model_type = "glm",
  include_coefficients = TRUE,
  labels = clintrial_labels
)

# Main comparison
example7

# Coefficient table
coef_table <- attr(example7, "coefficients")
coef_table

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
models <- attr(example7, "models")
names(models)

# Examine a specific model
summary(models[["Model C"]])

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example9 <- compfit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  model_list = list(
    "Minimal" = c("treatment"),
    "Standard" = c("treatment", "age", "sex", "stage"),
    "Extended" = c("treatment", "age", "sex", "stage", "ecog", "grade")
  ),
  model_type = "coxph"
)

recommended <- attr(example9, "best_model")
cat("Recommended model:", recommended, "\n")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example10 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Simple" = c("age", "sex"),
    "Standard" = c("age", "sex", "stage"),
    "Full" = c("age", "sex", "stage", "treatment", "ecog")
  ),
  model_type = "glm",
  scoring_weights = list(
    convergence = 0.05,
    aic = 0.20,
    concordance = 0.60,
    pseudo_r2 = 0.10,
    brier = 0.05
  )
)

example10

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
scenario1 <- compfit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  model_list = list(
    "Crude" = c("treatment"),
    "Age-Sex Adjusted" = c("treatment", "age", "sex"),
    "Fully Adjusted" = c("treatment", "age", "sex", "stage", "ecog")
  ),
  model_type = "coxph",
  include_coefficients = TRUE,
  labels = clintrial_labels
)

scenario1

# Compare treatment effect across models
attr(scenario1, "coefficients")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Identify candidates via screening
screening <- uniscreen(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "bmi", "smoking", "diabetes",
                 "hypertension", "stage", "ecog", "treatment"),
  model_type = "glm",
  p_threshold = 0.10
)

# Extract significant predictors
sig_vars <- attr(screening, "significant")

scenario2 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Theory-Driven" = c("age", "sex", "stage", "treatment"),
    "Data-Driven" = sig_vars,
    "Combined" = unique(c("age", "sex", "stage", "treatment", sig_vars))
  ),
  model_type = "glm"
)

scenario2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
scenario3 <- compfit(
  data = clintrial,
  outcome = "los_days",
  model_list = list(
    "3 Predictors" = c("age", "surgery", "ecog"),
    "5 Predictors" = c("age", "surgery", "ecog", "stage", "treatment"),
    "8 Predictors" = c("age", "surgery", "ecog", "stage", "treatment",
                       "sex", "smoking", "diabetes")
  ),
  model_type = "lm",
  labels = clintrial_labels
)

scenario3

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Main comparison table
# table2docx(
#   table = example1,
#   file = file.path(tempdir(), "Model_Comparison.docx"),
#   caption = "Table 3. Model Comparison Results"
# )
# 
# # Coefficient table
# table2docx(
#   table = attr(example6, "coefficients"),
#   file = file.path(tempdir(), "Coefficient_Comparison.docx"),
#   caption = "Table S1. Coefficient Estimates Across Models"
# )
# 
# # PDF with landscape orientation for wide tables
# table2pdf(
#   table = example1,
#   file = file.path(tempdir(), "Model_Comparison.pdf"),
#   caption = "Model Comparison",
#   orientation = "landscape"
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Check convergence status
# comparison[, .(Model, Converged)]
# 
# # For non-converging models:
# # 1. Reduce complexity
# # 2. Check for separation (logistic)
# # 3. Examine predictor correlations

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Check sample sizes
# comparison[, .(Model, N, Events)]
# 
# # Use complete cases for fair comparison
# complete_data <- na.omit(data[, relevant_vars, with = FALSE])

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Examine individual metrics
# comparison[, .(Model, `Composite Model Score (CMS)`, AIC, Concordance)]
# 
# # Prefer parsimony when scores are close
# # Consider interpretability

## ----include = FALSE----------------------------------------------------------
options(old_opts)

