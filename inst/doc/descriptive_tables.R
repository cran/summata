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
# desctable(data, by, variables, ...)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)

data(clintrial)
data(clintrial_labels)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
desc_vars <- c("age", "sex", "race", "bmi", "stage", "ecog", 
               "Surv(os_months, os_status)")

example1 <- desctable(
  data = clintrial, 
  by = "treatment", 
  variables = desc_vars,
  labels = clintrial_labels
)

example1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example2 <- desctable(
  data = clintrial,
  variables = c("age", "bmi", "sex", "stage"),
  labels = clintrial_labels
)

example2

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example3 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "bmi", "los_days"),
  stats_continuous = c("mean_sd", "median_iqr", "median_range"),
  labels = clintrial_labels
)

example3

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example4 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("sex", "stage", "ecog"),
  stats_categorical = "percent",
  labels = clintrial_labels
)

example4

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example5 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "bmi", "sex"),
  digits = 2,
  p_digits = 4,
  test = TRUE,
  labels = clintrial_labels
)

example5

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example6 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "bmi", "sex", "stage"),
  test = FALSE,
  labels = clintrial_labels
)

example6

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7a <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "bmi", "los_days"),
  test = TRUE,
  test_continuous = "aov",  # ANOVA
  labels = clintrial_labels
)

example7a

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example7b <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("sex", "stage"),
  test = TRUE,
  test_categorical = "fisher",
  labels = clintrial_labels
)

example7b

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example8 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("smoking", "diabetes"),
  na_include = TRUE,
  labels = clintrial_labels
)

example8

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Percentages exclude missing (denominator = non-missing)
example9a <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("smoking"),
  na_include = TRUE,
  na_percent = FALSE,
  labels = clintrial_labels
)

example9a

# Percentages include missing (denominator = total)
example9b <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("smoking"),
  na_include = TRUE,
  na_percent = TRUE,
  labels = clintrial_labels
)

example9b

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
example10 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("smoking"),
  na_include = TRUE,
  na_label = "Not Reported",
  labels = clintrial_labels
)

example10

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Total column in last position
example11a <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "sex", "stage"),
  total = "last",
  labels = clintrial_labels
)

example11a

# No total column
example11b <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "sex", "stage"),
  total = FALSE,
  labels = clintrial_labels
)

example11b

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table1 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c(
    "age", "sex", "race", "ethnicity", "bmi",
    "smoking", "diabetes", "hypertension",
    "stage", "grade", "ecog",
    "Surv(os_months, os_status)"
  ),
  labels = clintrial_labels,
  stats_continuous = "mean_sd",
  stats_categorical = "n_percent",
  test = TRUE,
  total = TRUE,
  digits = 1,
  p_digits = 3
)

table1

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
raw_data <- attr(table1, "raw_data")
head(raw_data)

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Microsoft Word
# table2docx(
#   table = table1,
#   file = file.path(tempdir(), "Table1.docx"),
#   caption = "Table 1. Baseline Characteristics by Group"
# )
# 
# # PDF (requires LaTeX)
# table2pdf(
#   table = table1,
#   file = file.path(tempdir(), "Table1.pdf"),
#   caption = "Table 1. Baseline Characteristics by Group"
# )
# 
# # HTML
# table2html(
#   table = table1,
#   file = file.path(tempdir(), "Table1.html"),
#   caption = "Table 1. Baseline Characteristics by Group"
# )

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# data$stage <- factor(data$stage, levels = c("I", "II", "III", "IV"))

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# desctable(data, by, variables, stats_continuous = "median_iqr")

## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(table, file.path(tempdir(), "table1.pdf"), orientation = "landscape", font_size = 8)

## ----include = FALSE----------------------------------------------------------
options(old_opts)

