## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# install.packages("summata")

## -----------------------------------------------------------------------------
# install.packages("remotes")
# remotes::install_github("phmcc/summata")

## -----------------------------------------------------------------------------
# remotes::install_git("https://codeberg.org/phmcc/summata.git")

## -----------------------------------------------------------------------------
# library(summata)
# packageVersion("summata")

## -----------------------------------------------------------------------------
# # For mixed-effects models
# install.packages(c("lme4", "coxme"))
# 
# # For table export (all formats)
# install.packages(c("xtable", "flextable", "officer", "knitr", "tinytex"))
# 
# # For enhanced graphics
# install.packages(c("ragg", "systemfonts"))
# 
# # For model diagnostics and selection
# install.packages(c("MASS", "MuMIn", "pROC", "ResourceSelection"))

## -----------------------------------------------------------------------------
# install.packages("summata", dependencies = TRUE)

## -----------------------------------------------------------------------------
# install.packages("tinytex")
# tinytex::install_tinytex()

## -----------------------------------------------------------------------------
# tinytex::is_tinytex()
# tinytex::tlmgr_version()

## -----------------------------------------------------------------------------
# install.packages(c("flextable", "officer"))

## -----------------------------------------------------------------------------
# library(summata)
# 
# # Create test data
# test_data <- data.frame(
#   Variable = c("Sample Size", "Mean", "Standard Deviation"),
#   Value = c("150", "45.3", "12.1")
# )
# 
# # Test Word export (requires flextable, officer)
# table2docx(test_data, file = "verification_test.docx")
# 
# # Test PDF export (requires xtable, LaTeX)
# table2pdf(test_data, file = "verification_test.pdf")
# 
# # Clean up
# file.remove(
#   "verification_test.docx",
#   "verification_test.pdf"
# )

## -----------------------------------------------------------------------------
# Sys.which("pdflatex")

## -----------------------------------------------------------------------------
# tinytex::tlmgr_install("package_name")

## -----------------------------------------------------------------------------
# table2pdf(test_data, file = "debug.pdf", show_logs = TRUE)

## -----------------------------------------------------------------------------
# options(repos = c(CRAN = "https://cloud.r-project.org"))

## -----------------------------------------------------------------------------
# getwd()
# file.access(getwd(), mode = 2)  # Returns 0 if writable

