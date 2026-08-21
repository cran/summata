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

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# forest_plot <- autoforest(x, data, ...)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# forestsave(forest_plot, "output.pdf")

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
logistic_model <- glm(
  surgery ~ age + sex + stage + treatment + ecog,
  data = clintrial,
  family = binomial
)

example1 <- glmforest(
  x = logistic_model,
  data = clintrial,
  title = "Logistic Regression: Predictors of Outcome",
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example1"-----------------------------------------------------------------------------------------------------------------
print(example1)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
linear_model <- lm(
  los_days ~ age + sex + stage + surgery + ecog,
  data = clintrial
)

example2 <- lmforest(
  x = linear_model,
  data = clintrial,
  title = "Linear Regression: Length of Stay",
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example2"-----------------------------------------------------------------------------------------------------------------
print(example2)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cox_model <- coxph(
  Surv(os_months, os_status) ~ age + sex + stage + treatment + ecog,
  data = clintrial
)

example3 <- coxforest(
  x = cox_model,
  data = clintrial,
  title = "Cox Regression: Survival Analysis",
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example3"-----------------------------------------------------------------------------------------------------------------
print(example3)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- autoforest(
  x = cox_model,
  data = clintrial,
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example4"-----------------------------------------------------------------------------------------------------------------
print(example4)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table_logistic <- fit(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "stage", "treatment", "ecog"),
  model_type = "glm",
  labels = clintrial_labels
)

example5 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Predictors of Surgical Intervention",
  labels = clintrial_labels,
  zebra_stripes = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example5"-----------------------------------------------------------------------------------------------------------------
print(example5)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table_cox <- fit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "stage", "treatment", "ecog"),
  model_type = "coxph",
  labels = clintrial_labels
)

example6 <- coxforest(
  x = attr(table_cox, "model"),
  title = "Predictors of Overall Survival",
  labels = clintrial_labels,
  zebra_stripes = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example6"-----------------------------------------------------------------------------------------------------------------
print(example6)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Indented Factor Levels",
  labels = clintrial_labels,
  indent_groups = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example7"-----------------------------------------------------------------------------------------------------------------
print(example7)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example8 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Condensed Display",
  labels = clintrial_labels,
  condense_table = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example8"-----------------------------------------------------------------------------------------------------------------
print(example8)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example9 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Without Zebra Striping",
  labels = clintrial_labels,
  indent_groups = TRUE,
  zebra_stripes = FALSE
)

## ----echo = FALSE, out.width = "90%", rec_dims_from = "example9"------------------------------------------------------------------------------------------------------------------
print(example9)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show both n and events
example10a <- coxforest(
  x = attr(table_cox, "model"),
  title = "With Sample Size and Events",
  labels = clintrial_labels,
  show_n = TRUE,
  show_events = TRUE,
  indent_groups = TRUE,
  zebra_stripes = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example10a"---------------------------------------------------------------------------------------------------------------
print(example10a)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Minimal display
example10b <- coxforest(
  x = attr(table_cox, "model"),
  title = "Minimal Display",
  labels = clintrial_labels,
  show_n = FALSE,
  show_events = FALSE,
  indent_groups = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example10b"---------------------------------------------------------------------------------------------------------------
print(example10b)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example11 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Custom Precision (3 decimal places)",
  labels = clintrial_labels,
  digits = 3,
  indent_groups = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example11"----------------------------------------------------------------------------------------------------------------
print(example11)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example12 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Custom Reference Label",
  labels = clintrial_labels,
  ref_label = "ref",
  indent_groups = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example12"----------------------------------------------------------------------------------------------------------------
print(example12)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example13 <- coxforest(
  x = attr(table_cox, "model"),
  title = "Custom Effect Label",
  labels = clintrial_labels,
  effect_label = "Effect (95% CI)",
  indent_groups = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example13"----------------------------------------------------------------------------------------------------------------
print(example13)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example14 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Custom Color",
  labels = clintrial_labels,
  color = "#E41A1C",
  indent_groups = TRUE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example14"----------------------------------------------------------------------------------------------------------------
print(example14)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example15 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Larger Font (1.5×)",
  labels = clintrial_labels,
  font_size = 1.5,
  indent_groups = TRUE,
  qc_footer = FALSE
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example15"----------------------------------------------------------------------------------------------------------------
print(example15)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Wide table (for long variable names)
example16a <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Wide Table (75%)",
  labels = clintrial_labels,
  table_width = 0.75
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example16a"---------------------------------------------------------------------------------------------------------------
print(example16a)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Narrow table (emphasizes forest plot)
example16b <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Narrow Table (50%)",
  labels = clintrial_labels,
  table_width = 0.50
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example16b"---------------------------------------------------------------------------------------------------------------
print(example16b)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Publication-Ready Plot",
  labels = clintrial_labels,
  indent_groups = TRUE,
  zebra_stripes = TRUE
)

# The recommended dimensions, in the units in which the plot was created
recdims(p)

# Journals commonly specify figure widths in millimeters
recdims(p, units = "mm")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(
#   x = attr(table_logistic, "model"),
#   title = "Forest Plot",
#   labels = clintrial_labels
# )
# 
# # The format follows the file extension, and the dimensions are applied
# # automatically in every case
# 
# # PDF (vector, best for publications)
# forestsave(p, "forest.pdf")
# 
# # PNG (raster, good for presentations)
# forestsave(p, "forest.png", dpi = 300)
# 
# # TIFF (high-quality raster, often required by journals)
# forestsave(p, "forest.tiff", dpi = 300)
# 
# # SVG (vector, good for web)
# forestsave(p, "forest.svg")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example19 <- coxforest(
  x = attr(table_cox, "model"),
  title = "Comprehensive Survival Analysis",
  labels = clintrial_labels,
  effect_label = "Hazard Ratio",
  digits = 2,
  show_n = TRUE,
  show_events = TRUE,
  indent_groups = TRUE,
  condense_table = FALSE,
  zebra_stripes = TRUE,
  ref_label = "reference",
  font_size = 1.0,
  table_width = 0.62,
  color = "#B5394C"
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example19"----------------------------------------------------------------------------------------------------------------
print(example19)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example20 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Extended with ggplot2",
  labels = clintrial_labels,
  indent_groups = TRUE
)

example20_modified <- example20 +
  theme(
    plot.title = element_text(face = "italic", color = "#A72727"),
    plot.background = element_rect(fill = "white", color = NA)
  )

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example20_modified"-------------------------------------------------------------------------------------------------------
print(example20_modified)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
poisson_model <- glm(
  fu_count ~ age + stage + treatment + surgery,
  data = clintrial,
  family = poisson
)

example21 <- glmforest(
  x = poisson_model,
  data = clintrial,
  title = "Poisson Regression: Follow-Up Visits",
  labels = clintrial_labels
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example21"----------------------------------------------------------------------------------------------------------------
print(example21)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nb_result <- fit(
  data = clintrial,
  outcome = "ae_count",
  predictors = c("age", "treatment", "diabetes", "surgery"),
  model_type = "negbin",
  labels = clintrial_labels
)

example22 <- glmforest(
  x = nb_result,
  title = "Negative Binomial: Adverse Events"
)

## ----echo = FALSE, out.width = "100%", rec_dims_from = "example22"----------------------------------------------------------------------------------------------------------------
print(example22)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, table_width = 0.75)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, font_size = 0.9)
# forestsave(p, "plot.pdf", width = 14, height = 8)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# labels <- c(
#   age = "Age (years)",
#   sex = "Sex",
#   stage = "Disease Stage"
# )
# p <- glmforest(model, labels = labels)

## ----include = FALSE----------------------------------------------------------
options(old_opts)

