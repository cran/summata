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
# data.table::fwrite(table1, "table1.csv")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# data.table::fwrite(table1, "table1.tsv", sep = "\t")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# data.table::fwrite(table1, "table1_semicolon.csv", sep = ";")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table1,
#   file = "table1.pdf"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table1,
#   file = "table1_caption.pdf",
#   caption = "Table 1. Baseline Characteristics by Group"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = "table2.pdf",
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
#   file = "table2_landscape.pdf",
#   caption = "Table 2. Regression Results",
#   paper = "letter",
#   orientation = "landscape"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = "table2_margins.pdf",
#   margins = c(0.5, 0.5, 0.5, 0.5)  # top, right, bottom, left
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = "table2_fitted.pdf",
#   fit_to_page = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pdf(
#   table = table2,
#   file = "table2_auto.pdf",
#   paper = "auto"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table1,
#   file = "table1.docx"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table1,
#   file = "table1_caption.docx",
#   caption = "Table 1. Baseline Characteristics by Group"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table2,
#   file = "table2.docx",
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
#   file = "table2_landscape.docx",
#   caption = "Table 2. Regression Results",
#   paper = "letter",
#   orientation = "landscape"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2docx(
#   table = table1,
#   file = "table1_darkheader.docx",
#   caption = "Table 1. Baseline Characteristics",
#   dark_header = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2html(
#   table = table1,
#   file = "table1.html"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2html(
#   table = table1,
#   file = "table1_caption.html",
#   caption = "Table 1. Baseline Characteristics"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2html(
#   table = table2,
#   file = "table2.html",
#   caption = "Table 2. Regression Analysis",
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE,
#   stripe_color = "#f5f5f5"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table1,
#   file = "table1.pptx"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table1,
#   file = "table1_slidetitle.pptx",
#   slide_title = "Baseline Characteristics"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2pptx(
#   table = table2,
#   file = "table2.pptx",
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
#   file = "table1_custom.pptx",
#   template = "my_template.pptx",
#   layout = "Title and Content",
#   master = "Office Theme"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2tex(
#   table = table1,
#   file = "table1.tex"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2tex(
#   table = table1,
#   file = "table1_caption.tex",
#   caption = "Baseline Characteristics by Group",
#   label = "tab:demographics"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2tex(
#   table = table2,
#   file = "table2.tex",
#   caption = "Regression Analysis",
#   label = "tab:regression",
#   booktabs = TRUE,
#   bold_significant = TRUE,
#   indent_groups = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2rtf(
#   table = table1,
#   file = "table1.rtf"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# table2rtf(
#   table = table2,
#   file = "table2.rtf",
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
#   file = "table2_landscape.rtf",
#   paper = "letter",
#   orientation = "landscape"
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# tablesave(table1, "table1.csv")             # CSV output
# tablesave(table1, "table1.tsv")             # TSV output
# tablesave(table1, "table1.ssv", sep = " ")  # SSV output
# tablesave(table1, "table1.pdf")             # PDF output
# tablesave(table1, "table1.docx")            # DOCX output
# tablesave(table1, "table1.html")            # HTML output
# tablesave(table1, "table1.pptx")            # PPTX output
# tablesave(table1, "table1.tex")             # TeX output
# tablesave(table1, "table1.rtf")             # RTF output

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# tablesave(
#   table = table2,
#   file = "table2.pdf",
#   caption = "Table 2. Regression Analysis",
#   orientation = "landscape",
#   font_size = 8,
#   bold_significant = TRUE,
#   indent_groups = TRUE,
#   zebra_stripes = TRUE
# )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Full display
# table2docx(table1, "table1_full.docx")
# 
# # Condense all variable types
# table2docx(
#   table = table1,
#   file = "table1_condensed.docx",
#   condense_table = TRUE,
#   zebra_stripes = TRUE
# )
# 
# # Condense only continuous/survival (descriptive tables only)
# table2docx(
#   table = table1,
#   file = "table1_condensequant.docx",
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
#   tablesave(
#     table = table2,
#     file = paste0("Table2.", fmt),
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
# table2pdf(table, "debug.pdf", show_logs = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# # Use landscape orientation
# table2pdf(table, "wide.pdf", orientation = "landscape")
# 
# # Enable fit-to-page scaling
# table2pdf(table, "wide.pdf", fit_to_page = TRUE)
# 
# # Reduce font size
# table2pdf(table, "wide.pdf", font_size = 7)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ft <- table2docx(table1, "table1.docx", return_ft = TRUE)
# 
# library(flextable)
# ft <- ft %>%
#   bold(i = 1, part = "header") %>%
#   color(i = 1, color = "navy", part = "header")
# 
# save_as_docx(ft, path = "table1_custom.docx")

## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------
# options(old_opts)

