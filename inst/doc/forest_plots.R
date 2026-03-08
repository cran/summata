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

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, data = mydata)
# dims <- attr(p, "rec_dims")
# ggplot2::ggsave("forest_plot.png", p,
#                 width = dims$width,
#                 height = dims$height)

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
queue_plot(example1)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example2)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example3)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example3)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- autoforest(
  x = cox_model,
  data = clintrial,
  labels = clintrial_labels
)
queue_plot(example4)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example5)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example6)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example6)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Indented Factor Levels",
  labels = clintrial_labels,
  indent_groups = TRUE
)
queue_plot(example7)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example7)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example8 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Condensed Display",
  labels = clintrial_labels,
  condense_table = TRUE
)
queue_plot(example8)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example8)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example9 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Without Zebra Striping",
  labels = clintrial_labels,
  indent_groups = TRUE,
  zebra_stripes = FALSE
)
queue_plot(example9)

## ----echo = FALSE, out.width = "90%", use_rec_dims = TRUE-------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example10a)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example10b)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example10b)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example11 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Custom Precision (3 decimal places)",
  labels = clintrial_labels,
  digits = 3,
  indent_groups = TRUE
)
queue_plot(example11)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example11)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example12 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Custom Reference Label",
  labels = clintrial_labels,
  ref_label = "ref",
  indent_groups = TRUE
)
queue_plot(example12)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example12)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example13 <- coxforest(
  x = attr(table_cox, "model"),
  title = "Custom Effect Label",
  labels = clintrial_labels,
  effect_label = "Effect (95% CI)",
  indent_groups = TRUE
)
queue_plot(example13)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example13)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example14 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Custom Color",
  labels = clintrial_labels,
  color = "#E41A1C",
  indent_groups = TRUE
)
queue_plot(example14)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example14)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example15 <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Larger Font (1.5×)",
  labels = clintrial_labels,
  font_size = 1.5,
  indent_groups = TRUE
)
queue_plot(example15)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example15)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Wide table (for long variable names)
example16a <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Wide Table (75%)",
  labels = clintrial_labels,
  table_width = 0.75
)
queue_plot(example16a)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example16a)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Narrow table (emphasizes forest plot)
example16b <- glmforest(
  x = attr(table_logistic, "model"),
  title = "Narrow Table (50%)",
  labels = clintrial_labels,
  table_width = 0.50
)
queue_plot(example16b)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example16b)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(
#   x = attr(table_logistic, "model"),
#   title = "Publication-Ready Plot",
#   labels = clintrial_labels,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )
# 
# # Get recommended dimensions
# dims <- attr(p, "rec_dims")
# cat("Width:", dims$width, "inches\n")
# cat("Height:", dims$height, "inches\n")
# 
# # Save with recommended dimensions
# ggsave(
#   filename = file.path(tempdir(), "forest_plot.pdf"),
#   plot = p,
#   width = dims$width,
#   height = dims$height,
#   units = "in"
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(
#   x = attr(table_logistic, "model"),
#   title = "Forest Plot",
#   labels = clintrial_labels
# )
# 
# dims <- attr(p, "rec_dims")
# 
# # PDF (vector, best for publications)
# ggsave("forest.pdf", p, width = dims$width, height = dims$height)
# 
# # PNG (raster, good for presentations)
# ggsave("forest.png", p, width = dims$width, height = dims$height, dpi = 300)
# 
# # TIFF (high-quality raster, often required by journals)
# ggsave("forest.tiff", p, width = dims$width, height = dims$height, dpi = 300)
# 
# # SVG (vector, good for web)
# ggsave("forest.svg", p, width = dims$width, height = dims$height)

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
  color = "#8A61D8"
)
queue_plot(example19)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example20_modified)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example21)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
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
queue_plot(example22)

## ----echo = FALSE, out.width = "100%", use_rec_dims = TRUE------------------------------------------------------------------------------------------------------------------------
print(example22)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, table_width = 0.75)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# p <- glmforest(model, font_size = 0.9)
# ggsave(file.path(tempdir(), "plot.pdf"), p, width = 14, height = 8)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# labels <- c(
#   age = "Age (years)",
#   sex = "Sex",
#   stage = "Disease Stage"
# )
# p <- glmforest(model, labels = labels)

## ----include = FALSE----------------------------------------------------------
options(old_opts)

