## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  eval = FALSE
)
old_opts <- options(width = 180)

## ----setup, eval = TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------
library(summata)

data(clintrial)
data(clintrial_labels)

# Create sample tables for export
table1 <- desctable(
  data = clintrial,
  by = "treatment",
  variables = c("age", "sex", "bmi", "smoking", "stage", "ecog"),
  labels = clintrial_labels
)

table2 <- fullfit(
  data = clintrial,
  outcome = "surgery",
  predictors = c("age", "sex", "stage", "treatment", "ecog"),
  model_type = "glm",
  labels = clintrial_labels
)

table3 <- fullfit(
  data = clintrial,
  outcome = "Surv(os_months, os_status)",
  predictors = c("age", "sex", "stage", "treatment", "ecog"),
  model_type = "coxph",
  labels = clintrial_labels
)

table4 <- compfit(
  data = clintrial,
  outcome = "surgery",
  model_list = list(
    "Demographics" = c("age", "sex"),
    "Clinical" = c("age", "sex", "stage", "ecog"),
    "Full" = c("age", "sex", "stage", "ecog", "treatment")
  ),
  model_type = "glm"
)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# data.table::fwrite(table1, file.path(tempdir(), "Table1.csv"))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# data.table::fwrite(table1, file.path(tempdir(), "Table1.tsv"), sep = "\t")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# data.table::fwrite(table1, file.path(tempdir(), "Table1.csv"), sep = ";")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table1,
#   file = file.path(tempdir(), "Table1.pdf")
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table1,
#   file = file.path(tempdir(), "Table1.pdf"),
#   caption = "Table 1. Baseline Characteristics by Group"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = file.path(tempdir(), "Table2.pdf"),
#   caption = "Table 2. Regression Analysis",
#   font_size = 8,
#   bold_significant = TRUE,
#   p_threshold = 0.05,
#   indent_groups = TRUE,
#   variable_padding = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = file.path(tempdir(), "Table2_Landscape.pdf"),
#   caption = "Table 2. Regression Results",
#   paper = "letter",
#   orientation = "landscape"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = file.path(tempdir(), "Table2_Margins.pdf"),
#   margins = c(0.5, 0.5, 0.5, 0.5)  # top, right, bottom, left
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = file.path(tempdir(), "Table2_Fitted.pdf"),
#   fit_to_page = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = file.path(tempdir(), "Table2_Auto.pdf"),
#   paper = "auto"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table1,
#   file = file.path(tempdir(), "Table1.docx")
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table1,
#   file = file.path(tempdir(), "Table1.docx"),
#   caption = "Table 1. Baseline Characteristics by Group"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table2,
#   file = file.path(tempdir(), "Table2.docx"),
#   caption = "Table 2. Regression Analysis",
#   font_size = 9,
#   font_family = "Times New Roman",
#   bold_significant = TRUE,
#   p_threshold = 0.05,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table2,
#   file = file.path(tempdir(), "Table2_Landscape.docx"),
#   caption = "Table 2. Regression Results",
#   paper = "letter",
#   orientation = "landscape"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table1,
#   file = file.path(tempdir(), "Table1_DarkHeader.docx"),
#   caption = "Table 1. Baseline Characteristics",
#   dark_header = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2html(
#   table = table1,
#   file = file.path(tempdir(), "Table1.html")
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2html(
#   table = table1,
#   file = file.path(tempdir(), "Table1.html"),
#   caption = "Table 1. Baseline Characteristics"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2html(
#   table = table2,
#   file = file.path(tempdir(), "Table2.html"),
#   caption = "Table 2. Regression Analysis",
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE,
#   stripe_color = "#f5f5f5"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table1,
#   file = file.path(tempdir(), "Table1.pptx")
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table1,
#   file = file.path(tempdir(), "Table1.pptx"),
#   slide_title = "Baseline Characteristics"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table2,
#   file = file.path(tempdir(), "Table2.pptx"),
#   slide_title = "Regression Analysis",
#   font_size = 10,
#   font_family = "Arial",
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table1,
#   file = file.path(tempdir(), "Table1_Custom.pptx"),
#   template = "my_template.pptx",
#   layout = "Title and Content",
#   master = "Office Theme"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2tex(
#   table = table1,
#   file = file.path(tempdir(), "Table1.tex")
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2tex(
#   table = table1,
#   file = file.path(tempdir(), "Table1.tex"),
#   caption = "Baseline Characteristics by Group",
#   label = "tab:demographics"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2tex(
#   table = table2,
#   file = file.path(tempdir(), "Table2.tex"),
#   caption = "Regression Analysis",
#   label = "tab:regression",
#   booktabs = TRUE,
#   bold_significant = TRUE,
#   indent_groups = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2rtf(
#   table = table1,
#   file = file.path(tempdir(), "Table1.rtf")
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2rtf(
#   table = table2,
#   file = file.path(tempdir(), "Table2.rtf"),
#   caption = "Table 2. Regression Analysis",
#   font_size = 9,
#   font_family = "Times New Roman",
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2rtf(
#   table = table2,
#   file = file.path(tempdir(), "Table2_Landscape.rtf"),
#   paper = "letter",
#   orientation = "landscape"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# autotable(table1, file.path(tempdir(), "Table1.csv"))   # CSV output
# autotable(table1, file.path(tempdir(), "Table1.tsv"))   # TSV output
# autotable(table1, file.path(tempdir(), "Table1.pdf"))   # PDF output
# autotable(table1, file.path(tempdir(), "Table1.docx"))  # DOCX output
# autotable(table1, file.path(tempdir(), "Table1.html"))  # HTML output
# autotable(table1, file.path(tempdir(), "Table1.pptx"))  # PPTX output
# autotable(table1, file.path(tempdir(), "Table1.tex"))   # TeX output
# autotable(table1, file.path(tempdir(), "Table1.rtf"))   # RTF output

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# autotable(
#   table = table2,
#   file = file.path(tempdir(), "Table2.pdf"),
#   caption = "Table 2. Regression Analysis",
#   orientation = "landscape",
#   font_size = 8,
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Full display
# table2docx(table1, file.path(tempdir(), "Table1_Full.docx"))
# 
# # Condense all variable types
# table2docx(
#   table = table1,
#   file = file.path(tempdir(), "Table1_Condensed.docx"),
#   condense_table = TRUE,
#   zebra_stripes = TRUE
# )
# 
# # Condense only continuous/survival (descriptive tables only)
# table2docx(
#   table = table1,
#   file = file.path(tempdir(), "Table1_CondenseQuant.docx"),
#   condense_quantitative = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# common_opts <- list(
#   caption = "Table 2. Regression Analysis",
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )
# 
# formats <- c("csv", "pdf", "docx", "html", "pptx", "rtf", "tex")
# 
# for (fmt in formats) {
#   autotable(
#     table = table2,
#     file = file.path(tempdir(), paste0("Table2.", fmt)),
#     caption = common_opts$caption,
#     bold_significant = common_opts$bold_significant,
#     indent_groups = common_opts$indent_groups,
#     zebra_stripes = common_opts$zebra_stripes
#   )
# }

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Check LaTeX installation
# Sys.which("pdflatex")
# 
# # Install TinyTeX if needed
# # tinytex::install_tinytex()
# 
# # Keep log files for debugging
# table2pdf(table, file.path(tempdir(), "debug.pdf"), show_logs = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Use landscape orientation
# table2pdf(table, file.path(tempdir(), "wide.pdf"), orientation = "landscape")
# 
# # Enable fit-to-page scaling
# table2pdf(table, file.path(tempdir(), "wide.pdf"), fit_to_page = TRUE)
# 
# # Reduce font size
# table2pdf(table, file.path(tempdir(), "wide.pdf"), font_size = 7)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ft <- table2docx(table1, file.path(tempdir(), "Table1.docx"), return_ft = TRUE)
# 
# library(flextable)
# ft <- ft %>%
#   bold(i = 1, part = "header") %>%
#   color(i = 1, color = "navy", part = "header")
# 
# save_as_docx(ft, path = file.path(tempdir(), "Table1_Custom.docx"))

## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
# options(old_opts)

