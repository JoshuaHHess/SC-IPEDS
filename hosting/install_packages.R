packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "ggplot2",
  "plotly",
  "readr",
  "stringr",
  "tidyr",
  "tibble",
  "scales",
  "htmltools",
  "jsonlite"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing)
} else {
  message("All required packages are already installed.")
}
