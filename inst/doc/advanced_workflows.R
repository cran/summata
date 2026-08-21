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
# forestsave(p, "forest_plot.png")

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

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example6"-----------------------------------------------------------------------------------------------------------------
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

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example11"----------------------------------------------------------------------------------------------------------------
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
  labels = clintrial_labels,
  indent_groups = TRUE,
  zebra_stripes = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example18"----------------------------------------------------------------------------------------------------------------
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
  labels = clintrial_labels,
  indent_predictor = TRUE,
  zebra_stripes = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "forest_plot"--------------------------------------------------------------------------------------------------------------
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

