## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  fig.width = 12,
  fig.height = 6,
  dpi = 150
)

# Use ragg for better font rendering if available
if (requireNamespace("ragg", quietly = TRUE)) {
  knitr::opts_chunk$set(dev = "ragg_png")
}

old_opts <- options(width = 180)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# fullfit(data, outcome, predictors, model_type, ...)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)
library(survival)

data(clintrial)
data(clintrial_labels)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
screening_vars <- c("age", "sex", "race", "bmi", "smoking", 
                    "diabetes", "stage", "ecog", "treatment")

example1 <- uniscreen(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = screening_vars,
  model_type = "glm",
  labels = clintrial_labels
)

example1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example2 <- uniscreen(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = screening_vars,
  model_type = "glm",
  p_threshold = 0.01,
  labels = clintrial_labels
)

example2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- uniscreen(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "sex", "stage", "diabetes", "ecog"),
  model_type = "lm",
  labels = clintrial_labels
)

example4

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example3 <- uniscreen(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage", "ecog"),
  model_type = "coxph",
  labels = clintrial_labels
)

example3

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example5 <- uniscreen(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("age", "sex", "stage"),
  model_type = "glm",
  keep_models = TRUE
)

# Access individual models
models <- attr(example5, "models")
names(models)

# Examine a specific model
summary(models[["age"]])

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example6 <- fit(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("age", "sex", "treatment", "stage", "diabetes"),
  model_type = "glm",
  labels = clintrial_labels
)

example6

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example8 <- fit(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "sex", "stage", "ecog"),
  model_type = "lm",
  labels = clintrial_labels
)

example8

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7 <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage"),
  model_type = "coxph",
  labels = clintrial_labels
)

example7

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example9 <- fit(
  data = clintrial,
  outcome = "fu_count",
  predictors = c("age", "stage", "treatment", "surgery"),
  model_type = "glm",
  family = "poisson",
  labels = clintrial_labels
)

example9

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example10 <- fit(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("sex", "stage", "treatment"),
  model_type = "glm",
  reference_rows = FALSE,
  labels = clintrial_labels
)

example10

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example11 <- fit(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("age", "sex", "stage"),
  model_type = "glm",
  conf_level = 0.90
)

example11

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example12 <- fit(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("age", "sex", "stage"),
  model_type = "glm",
  exponentiate = FALSE
)

example12

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# fullfit(data, outcome, predictors, model_type, method, ...)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example13 <- fullfit(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("age", "sex", "bmi", "smoking", "diabetes",
                 "stage", "treatment"),
  model_type = "glm",
  method = "screen",
  p_threshold = 0.05,
  labels = clintrial_labels
)

example13

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example14 <- fullfit(
  data = clintrial,
  outcome = "any_complication",
  predictors = c("age", "sex", "treatment", "stage"),
  model_type = "glm",
  method = "all",
  labels = clintrial_labels
)

example14

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example15 <- fullfit(
  data = clintrial,
  outcome = "icu_admission",
  predictors = c("age", "sex", "bmi", "smoking", "stage", "treatment"),
  model_type = "glm",
  method = "custom",
  multi_predictors = c("age", "sex", "stage", "treatment"),
  labels = clintrial_labels
)

example15

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Univariable only
example16a <- fullfit(
  data = clintrial,
  outcome = "wound_infection",
  predictors = c("age", "sex", "stage"),
  model_type = "glm",
  columns = "uni"
)

example16a

# Multivariable only
example16b <- fullfit(
  data = clintrial,
  outcome = "wound_infection",
  predictors = c("age", "sex", "stage"),
  model_type = "glm",
  columns = "multi"
)

example16b

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example17 <- fullfit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage", "ecog"),
  model_type = "coxph",
  method = "screen",
  p_threshold = 0.10,
  labels = clintrial_labels
)

example17

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example18 <- fit(
  data = clintrial,
  outcome = "ae_count",
  predictors = c("age", "sex", "diabetes", "treatment"),
  model_type = "negbin",
  labels = clintrial_labels
)

example18

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example19 <- fit(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "ecog", "stage", "treatment"),
  model_type = "glm",
  family = Gamma(link = "log"),
  labels = clintrial_labels
)

example19

## ----eval = requireNamespace("lme4", quietly = TRUE)------------------------------------------------------------------------------------------------------------------------------
example20 <- fit(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "sex", "treatment", "stage", "(1|site)"),
  model_type = "lmer",
  labels = clintrial_labels
)

example20

## ----eval = requireNamespace("lme4", quietly = TRUE)------------------------------------------------------------------------------------------------------------------------------
example21 <- fit(
  data = clintrial,
  outcome = "readmission_30d",
  predictors = c("age", "sex", "diabetes", "treatment", "(1|site)"),
  model_type = "glmer",
  family = "binomial",
  labels = clintrial_labels
)

example21

## ----eval = requireNamespace("lme4", quietly = TRUE)------------------------------------------------------------------------------------------------------------------------------
example22 <- fit(
  data = clintrial,
  outcome = "fu_count",
  predictors = c("age", "stage", "treatment", "(1|site)"),
  model_type = "glmer",
  family = "poisson",
  labels = clintrial_labels
)

example22

## ----eval = requireNamespace("coxme", quietly = TRUE)-----------------------------------------------------------------------------------------------------------------------------
example23 <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage", "(1|site)"),
  model_type = "coxme",
  labels = clintrial_labels
)

example23

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example24 <- fit(
  data = clintrial,
  outcome = "any_complication",
  predictors = c("age", "sex", "diabetes", "stage"),
  model_type = "glm",
  family = "quasibinomial",
  labels = clintrial_labels
)

example24

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Matched case-control dataset (100 pairs)
set.seed(456)
n_pairs <- 100
matched_data <- do.call(rbind, lapply(1:n_pairs, function(i) {
  base_bmi <- rnorm(1, 27, 4)
  data.frame(
    match_id = i,
    case = c(1, 0),
    smoking = factor(c(rbinom(1, 1, 0.45), rbinom(1, 1, 0.30)), 
                     levels = c(0, 1), labels = c("No", "Yes")),
    diabetes = factor(c(rbinom(1, 1, 0.30), rbinom(1, 1, 0.20)), 
                      levels = c(0, 1), labels = c("No", "Yes")),
    bmi = c(base_bmi + rnorm(1, 1.5, 2), base_bmi + rnorm(1, -0.5, 2))
  )
}))

example25 <- fit(
  data = matched_data,
  outcome = "case",
  predictors = c("smoking", "diabetes", "bmi"),
  model_type = "clogit",
  strata = "match_id"
)

example25

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Microsoft Word
# table2docx(
#   table = example13,
#   file = file.path(tempdir(), "Table2_Regression.docx"),
#   caption = "Table 2. Univariable and Multivariable Analysis"
# )
# 
# # PDF
# table2pdf(
#   table = example13,
#   file = file.path(tempdir(), "Table2_Regression.pdf"),
#   caption = "Table 2. Regression Results"
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Check missingness
# sapply(clintrial[, c("age", "sex", "stage")], function(x) sum(is.na(x)))
# 
# # Create complete-case dataset explicitly
# complete_data <- na.omit(clintrial[, c("readmission_30d", "age", "sex", "stage")])

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Set specific reference level
# clintrial$stage <- relevel(factor(clintrial$stage), ref = "I")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Access model for diagnostics
# result <- fit(data, outcome, predictors, model_type = "glm")
# model <- attr(result, "model")
# 
# # Check convergence
# model$converged
# 
# # Large coefficients may indicate separation
# coef(model)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Fit Poisson model
# result <- fit(data, outcome, predictors, model_type = "glm", family = "poisson")
# model <- attr(result, "model")
# 
# # Dispersion estimate (should be ~1 for no overdispersion)
# sum(residuals(model, type = "pearson")^2) / model$df.residual

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Increase iterations
# result <- fit(
#   data = clintrial,
#   outcome = "readmission_30d",
#   predictors = c("age", "treatment", "(1|site)"),
#   model_type = "glmer",
#   family = "binomial",
#   control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
# )

## ----include = FALSE----------------------------------------------------------
options(old_opts)

