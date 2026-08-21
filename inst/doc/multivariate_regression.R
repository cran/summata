## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
## Use ragg for better font rendering if available
if (requireNamespace("ragg", quietly = TRUE)) {
  old_opts <- options(summata.use_ragg = TRUE, width = 180)
  knitr::opts_chunk$set(
    dev = "ragg_png",
    fig.retina = 1,
    collapse = TRUE,
    comment = "##>",
    message = FALSE,
    warning = FALSE,
    fig.width = 8,
    fig.height = 5,
    out.width = "80%"
  )
} else {
  old_opts <- options(width = 180)
  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "##>",
    message = FALSE,
    warning = FALSE,
    fig.width = 8,
    fig.height = 5,
    out.width = "80%"
  )
}

## Dynamic figure sizing. A chunk that renders a forest plot names the object it
## is about to print, by giving the chunk option rec_dims_from = "example1".
##
## Chunk options are resolved before knitr opens the graphics device, so this
## hook reads the plot's recommended dimensions from its "rec_dims" attribute
## and sets fig.width and fig.height from them. The object was created in an
## earlier chunk and so already exists in the knit environment. Plots render via
## ragg (dev = "ragg_png" set above) and knitr captures them natively; no files
## are written to disk.
##
## A plot without recommended dimensions, or a name that does not resolve,
## falls through to the default figure size rather than failing the build.
knitr::opts_hooks$set(rec_dims_from = function(options) {
  plot <- get0(options$rec_dims_from, envir = knitr::knit_global(),
               ifnotfound = NULL)
  dims <- attr(plot, "rec_dims")

  if (!is.null(dims)) {
    options$fig.width  <- dims$width
    options$fig.height <- dims$height
  }

  options
})

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# multifit(data, outcomes, predictor, covariates, model_type, ...)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)
library(survival)
library(ggplot2)

data(clintrial)
data(clintrial_labels)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, data = mydata)
# forestsave(p, "forest_plot.png")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example1 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection",
               "readmission_30d", "icu_admission"),
  predictor = "surgery",
  labels = clintrial_labels,
  parallel = FALSE
)

example1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example2 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection",
               "readmission_30d", "icu_admission"),
  predictor = "surgery",
  covariates = c("age", "sex", "smoking", "diabetes"),
  labels = clintrial_labels,
  parallel = FALSE
)

example2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example3 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection",
               "readmission_30d", "icu_admission"),
  predictor = "surgery",
  covariates = c("age", "sex", "diabetes", "surgery"),
  columns = "both",
  labels = clintrial_labels,
  parallel = FALSE
)

example3

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection", "icu_admission"),
  predictor = "age",
  covariates = c("sex", "treatment", "surgery"),
  labels = clintrial_labels,
  parallel = FALSE
)

example4

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example5 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection",
               "readmission_30d", "icu_admission"),
  predictor = "treatment",
  covariates = c("age", "sex", "surgery"),
  labels = clintrial_labels,
  parallel = FALSE
)

example5

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example6 <- multifit(
  data = clintrial,
  outcomes = c("Surv(pfs_months, pfs_status)",
               "Surv(os_months, os_status)"),
  predictor = "treatment",
  covariates = c("age", "sex", "stage"),
  model_type = "coxph",
  labels = clintrial_labels,
  parallel = FALSE
)

example6

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7 <- multifit(
  data = clintrial,
  outcomes = c("los_days", "pain_score", "recovery_days"),
  predictor = "treatment",
  covariates = c("age", "sex", "surgery"),
  model_type = "lm",
  labels = clintrial_labels,
  parallel = FALSE
)

example7

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example8 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection"),
  predictor = "treatment",
  covariates = c("age", "sex"),
  random = "(1|site)",
  model_type = "glmer",
  labels = clintrial_labels,
  parallel = FALSE
)

example8

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example9 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection"),
  predictor = "treatment",
  covariates = c("age", "sex"),
  interactions = c("treatment:sex"),
  labels = clintrial_labels,
  parallel = FALSE
)

example9

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example10 <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection",
               "readmission_30d", "icu_admission"),
  predictor = "treatment",
  covariates = c("age", "sex", "surgery"),
  p_threshold = 0.01,
  labels = clintrial_labels,
  parallel = FALSE
)

example10

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
result <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection"),
  predictor = "treatment",
  covariates = c("age", "sex"),
  labels = clintrial_labels,
  keep_models = TRUE,
  parallel = FALSE
)

# Access individual models
models <- attr(result, "models")
names(models)

# Examine a specific model
summary(models[["any_complication"]])

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
result <- multifit(
  data = clintrial,
  outcomes = c("any_complication", "wound_infection",
               "readmission_30d", "icu_admission"),
  predictor = "treatment",
  covariates = c("age", "sex", "diabetes", "surgery"),
  labels = clintrial_labels,
  parallel = FALSE
)

example12 <- multiforest(
  result,
  title = "Treatment Effects Across Outcomes",
  indent_predictor = TRUE,
  zebra_stripes = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example12"----------------------------------------------------------------------------------------------------------------
print(example12)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example13 <- multiforest(
  result,
  title = "Effect Estimates",
  column = "adjusted",
  show_predictor = FALSE,
  covariates_footer = TRUE,
  table_width = 0.65,
  color = "#4BA6B6"
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example13"----------------------------------------------------------------------------------------------------------------
print(example13)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
lm_result <- multifit(
  data = clintrial,
  outcomes = c("pain_score", "recovery_days", "los_days"),
  predictor = "treatment",
  covariates = c("age", "sex", "surgery"),
  model_type = "lm",
  parallel = FALSE
)

example14 <- multiforest(
  lm_result,
  title = "Treatment Effects on Recovery Metrics",
  show_predictor = FALSE,
  covariates_footer = TRUE,
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example14"----------------------------------------------------------------------------------------------------------------
print(example14)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cox_result <- multifit(
  data = clintrial,
  outcomes = c("Surv(pfs_months, pfs_status)",
               "Surv(os_months, os_status)"),
  predictor = "treatment",
  covariates = c("age", "sex", "stage"),
  model_type = "coxph",
  parallel = FALSE
)

example15 <- multiforest(
  cox_result,
  title = "Treatment Effects on Survival Outcomes",
  indent_predictor = TRUE,
  zebra_stripes = TRUE,
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example15"----------------------------------------------------------------------------------------------------------------
print(example15)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = result,
#   file = "multioutcome_analysis.docx",
#   caption = "Treatment Effects Across Outcomes"
# )
# 
# table2pdf(
#   table = result,
#   file = "multioutcome_analysis.pdf",
#   caption = "Treatment Effects Across Outcomes"
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- multiforest(result, title = "Effect Estimates")
# 
# forestsave(p, "multioutcome_forest.pdf")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Define outcomes by type
binary_outcomes <- c("any_complication", "wound_infection",
                     "readmission_30d", "icu_admission")
survival_outcomes <- c("Surv(pfs_months, pfs_status)",
                       "Surv(os_months, os_status)")

## Unadjusted screening
unadjusted <- multifit(
  data = clintrial,
  outcomes = binary_outcomes,
  predictor = "treatment",
  labels = clintrial_labels,
  parallel = FALSE
)

unadjusted

## Adjusted analysis with comparison
adjusted <- multifit(
  data = clintrial,
  outcomes = binary_outcomes,
  predictor = "treatment",
  covariates = c("age", "sex", "diabetes", "surgery"),
  columns = "both",
  labels = clintrial_labels,
  parallel = FALSE
)

adjusted

## Forest plot visualization
forest_plot <- multiforest(
  adjusted,
  title = "Treatment Effect Estimates",
  column = "adjusted",
  indent_predictor = TRUE,
  zebra_stripes = TRUE,
  table_width = 0.65,
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "forest_plot"--------------------------------------------------------------------------------------------------------------
print(forest_plot)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# ## Start with random intercepts only
# multifit(data, outcomes, predictor,
#          random = "(1|site)",
#          model_type = "glmer")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# complete_data <- na.omit(
#   clintrial[, c("los_days", "pain_score", "recovery_days",
#                 "treatment", "age", "sex", "surgery")]
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# clintrial$treatment_binary <- ifelse(clintrial$treatment == "Control",
#                                       "Control", "Active")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# ## Multinomial regression (unordered categories)
# library(nnet)
# model <- multinom(treatment ~ age + sex + stage, data = clintrial)
# 
# ## Ordinal regression (ordered categories)
# library(MASS)
# model <- polr(grade ~ age + sex + stage, data = clintrial, Hess = TRUE)

## ----include = FALSE----------------------------------------------------------
options(old_opts)

