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
    out.width = "100%"
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
    out.width = "100%"
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

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)
library(survival)
library(ggplot2)

data(clintrial)
data(clintrial_labels)

# Examine the clustering structure
table(clintrial$site)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, data = mydata)
# dims <- attr(p, "rec_dims")
# ggplot2::ggsave("forest_plot.png", p,
#                 width = dims$width,
#                 height = dims$height)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example1 <- fit(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "treatment", "stage"),
  interactions = c("sex:treatment"),
  model_type = "glm",
  labels = clintrial_labels
)

example1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example2 <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage"),
  interactions = c("sex:treatment", "stage:treatment"),
  model_type = "coxph",
  labels = clintrial_labels
)

example2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example3 <- fit(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "sex", "treatment", "stage", "surgery"),
  interactions = c("age:treatment"),
  model_type = "lm",
  labels = clintrial_labels
)

example3

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- fullfit(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "treatment", "stage", "sex:treatment"),
  model_type = "glm",
  method = "all",
  labels = clintrial_labels
)

example4

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example5 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Main Effects" = c("age", "sex", "treatment", "stage"),
    "Sex × Treatment" = c("age", "sex", "treatment", "stage"),
    "Stage × Treatment" = c("age", "sex", "treatment", "stage"),
    "Both Interactions" = c("age", "sex", "treatment", "stage")
  ),
  interactions_list = list(
    NULL,
    c("sex:treatment"),
    c("stage:treatment"),
    c("sex:treatment", "stage:treatment")
  ),
  model_type = "glm",
  labels = clintrial_labels
)

example5

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
interaction_model <- fit(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "treatment", "stage"),
  interactions = c("sex:treatment"),
  model_type = "glm",
  labels = clintrial_labels
)

example6 <- glmforest(
  x = attr(interaction_model, "model"),
  title = "Logistic Regression with Interaction",
  labels = clintrial_labels,
  indent_groups = TRUE,
  zebra_stripes = TRUE
)
queue_plot(example6)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example6)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7 <- fit(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "sex", "treatment", "stage", "(1|site)"),
  model_type = "lmer",
  labels = clintrial_labels
)

example7

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example8 <- fit(
    data = clintrial,
    outcome = "surgery",
    predictors = c("age", "sex", "treatment", "stage"),
    random = "(1|site)",
    model_type = "glmer",
    labels = clintrial_labels
)

example8

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example9 <- fit(
  data = clintrial,
  outcome = "los_days",
  predictors = c("age", "sex", "treatment", "stage", "(1 + treatment|site)"),
  model_type = "lmer",
  labels = clintrial_labels
)

example9

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example10 <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage", "(1|site)"),
  model_type = "coxme",
  labels = clintrial_labels
)

example10

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example11 <- glmforest(
  x = attr(example8, "model"),
  title = "Logistic Mixed Model (Fixed Effects)",
  labels = clintrial_labels,
  indent_groups = TRUE,
  zebra_stripes = TRUE
)
queue_plot(example11)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example11)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example12 <- compfit(
  data = clintrial,
  outcome = "los_days",
  model_list = list(
    "Random Intercepts" = c("age", "sex", "treatment", "stage", "(1|site)"),
    "Random Slopes" = c("age", "sex", "treatment", "stage", "(1 + treatment|site)")
  ),
  model_type = "lmer",
  labels = clintrial_labels
)

example12

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example13 <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment"),
  strata = "site",
  model_type = "coxph",
  labels = clintrial_labels
)

example13

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example14 <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage"),
  cluster = "site",
  model_type = "coxph",
  labels = clintrial_labels
)

example14

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create example weights
clintrial$analysis_weight <- runif(nrow(clintrial), 0.5, 2.0)

example15 <- fit(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "treatment", "stage"),
  weights = "analysis_weight",
  model_type = "glm",
  labels = clintrial_labels
)

example15

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example16 <- uniscreen(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "treatment", "stage"),
  model_type = "glmer",
  random = "(1|site)",
  labels = clintrial_labels
)

example16

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example17 <- uniscreen(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage", "ecog"),
  model_type = "coxme",
  random = "(1|site)",
  labels = clintrial_labels
)

example17

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
uni_results <- uniscreen(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "treatment", "stage", "ecog", "grade"),
  model_type = "coxph",
  labels = clintrial_labels
)

example18 <- uniforest(
  uni_results,
  title = "Univariable Screening Results",
  indent_groups = TRUE,
  zebra_stripes = TRUE
)
queue_plot(example18)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example18)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example19 <- multifit(
  data = clintrial,
  outcomes = c("surgery", "pfs_status", "os_status"),
  predictor = "treatment",
  covariates = c("age", "sex", "stage"),
  interactions = c("treatment:sex"),
  labels = clintrial_labels,
  parallel = FALSE
)

example19

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example20 <- multifit(
  data = clintrial,
  outcomes = c("surgery", "pfs_status"),
  predictor = "treatment",
  covariates = c("age", "sex"),
  random = "(1|site)",
  model_type = "glmer",
  labels = clintrial_labels,
  parallel = FALSE
)

example20

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example21 <- multifit(
  data = clintrial,
  outcomes = c("Surv(pfs_months, pfs_status)",
               "Surv(os_months, os_status)"),
  predictor = "treatment",
  covariates = c("age", "sex"),
  random = "(1|site)",
  model_type = "coxme",
  labels = clintrial_labels,
  parallel = FALSE
)

example21

## ----fig.width = 12, fig.height = 8-----------------------------------------------------------------------------------------------------------------------------------------------
# Step 1: Screen risk factors for primary outcome
risk_screening <- uniscreen(
  data = clintrial,
  outcome = "os_status",
  predictors = c("age", "sex", "bmi", "smoking", "diabetes",
                 "hypertension", "stage", "ecog", "treatment"),
  model_type = "glm",
  p_threshold = 0.20,
  labels = clintrial_labels
)

risk_screening

# Step 2: Test key exposure across multiple outcomes
effects <- multifit(
  data = clintrial,
  outcomes = c("surgery", "pfs_status", "os_status"),
  predictor = "treatment",
  covariates = c("age", "sex", "stage"),
  columns = "both",
  labels = clintrial_labels,
  parallel = FALSE
)

effects

# Step 3: Visualize effects
forest_plot <- multiforest(
  effects,
  title = "Effects Across Outcomes",
  indent_predictor = TRUE,
  zebra_stripes = TRUE
)
queue_plot(forest_plot)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(forest_plot)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Start with random intercepts only
# fit(data, outcome, c(predictors, "(1|site)"), model_type = "lmer")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Access model for detailed interpretation
# model <- attr(result, "model")
# summary(model)

## ----include = FALSE----------------------------------------------------------
options(old_opts)

