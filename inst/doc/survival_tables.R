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
# survtable(data, outcome, by, times, probs, ...)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)
library(survival)

data(clintrial)
data(clintrial_labels)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example1 <- survtable(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  by = "treatment",
  times = c(12, 24, 36),
  time_unit = "months"
)

example1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example2 <- survtable(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  by = "treatment",
  times = c(12, 24),
  probs = NULL,
  time_unit = "months",
  total = FALSE
)

example2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example3 <- survtable(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  by = "stage",
  times = NULL,
  probs = c(0.25, 0.5, 0.75),
  labels = clintrial_labels
)

example3

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- survtable(
  data = clintrial,
  outcome = c("Surv(pfs_months, pfs_status)", "Surv(os_months, os_status)"),
  by = "treatment",
  times = c(12, 24),
  probs = 0.5,
  time_unit = "months",
  total = FALSE,
  labels = c(
    "Surv(pfs_months, pfs_status)" = "Progression-Free Survival",
    "Surv(os_months, os_status)" = "Overall Survival"
  )
)

example4

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example5 <- survtable(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  by = "treatment",
  times = c(12, 24, 36),
  type = "risk",
  time_unit = "months"
)

example5

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example6 <- survtable(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  by = "treatment",
  times = c(12, 24),
  stats = c("survival", "ci", "n_risk"),
  time_unit = "months",
  total = FALSE
)

example6

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# library(ggsurvfit)
# library(survival)
# 
# # Fit the survival model
# km_fit <- survfit(Surv(os_months, os_status) ~ treatment, data = clintrial)
# 
# # Create Kaplan-Meier plot with risk table
# ggsurvfit(km_fit) +
#   add_confidence_interval() +
#   add_risktable() +
#   add_quantile(y_value = 0.5, linetype = "dashed") +
#   scale_ggsurvfit() +
#   labs(
#     title = "Overall Survival by Treatment",
#     x = "Time (months)",
#     y = "Survival Probability"
#   ) +
#   theme_minimal()
# 
# # Generate the companion table
# surv_table <- survtable(
#   data = clintrial,
#   outcome = "Surv(os_months, os_status)",
#   by = "treatment",
#   times = c(12, 24, 36),
#   probs = 0.5,
#   time_unit = "months"
# )
# 
# # Export both for publication
# ggsave(file.path(tempdir(), "km_curve.pdf"), width = 8, height = 6)
# table2pdf(surv_table, file.path(tempdir(), "survival_table.pdf"),
#           caption = "Table 2. Survival Estimates by Treatment Group")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Microsoft Word
# table2docx(
#   table = example1,
#   file = file.path(tempdir(), "SurvivalTable.docx"),
#   caption = "Table 2. Survival Estimates by Treatment Group"
# )
# 
# # PDF (requires LaTeX)
# table2pdf(
#   table = example1,
#   file = file.path(tempdir(), "SurvivalTable.pdf"),
#   caption = "Table 2. Survival Estimates by Treatment Group"
# )

## ----include = FALSE----------------------------------------------------------
options(old_opts)

