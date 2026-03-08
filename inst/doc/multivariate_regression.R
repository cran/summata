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

## Dynamic figure sizing: queue_plot() stashes rec_dims from a plot object,
## and the opts_hook on the NEXT chunk (with use_rec_dims = TRUE) applies them
## before knitr opens the graphics device. Plots render via ragg (dev = "ragg_png"
## set above) and knitr captures them natively. No files written to disk.
.plot_dims <- new.env(parent = emptyenv())
.plot_dims$width <- NULL
.plot_dims$height <- NULL

knitr::opts_hooks$set(use_rec_dims = function(options) {
  if (isTRUE(options$use_rec_dims)) {
    if (!is.null(.plot_dims$width))  options$fig.width  <- .plot_dims$width
    if (!is.null(.plot_dims$height)) options$fig.height <- .plot_dims$height
    .plot_dims$width <- NULL
    .plot_dims$height <- NULL
  }
  options
})

## Call at the end of a plot-creation chunk to stash dimensions for the next chunk.
queue_plot <- function(plot) {
  dims <- attr(plot, "rec_dims")
  if (!is.null(dims)) {
    .plot_dims$width  <- dims$width
    .plot_dims$height <- dims$height
  }
  invisible(plot)
}

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
# dims <- attr(p, "rec_dims")
# ggplot2::ggsave("forest_plot.png", p,
#                 width = dims$width,
#                 height = dims$height)

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
queue_plot(example12)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example13)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example14)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example15)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example15)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = result,
#   file = file.path(tempdir(), "multioutcome_analysis.docx"),
#   caption = "Treatment Effects Across Outcomes"
# )
# 
# table2pdf(
#   table = result,
#   file = file.path(tempdir(), "multioutcome_analysis.pdf"),
#   caption = "Treatment Effects Across Outcomes"
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- multiforest(result, title = "Effect Estimates")
# dims <- attr(p, "rec_dims")
# 
# ggsave(file.path(tempdir(), "multioutcome_forest.pdf"), p,
#        width = attr(result, "rec_dims")$width,
#        height = attr(result, "rec_dims")$height,
#        units = "in")

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
queue_plot(forest_plot)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(forest_plot)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# ## Start with random intercepts only
# multifit(data, outcomes, predictor,
#          random = "(1|site)",
#          model_type = "glmer")

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

