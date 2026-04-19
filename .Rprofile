if (interactive() &&
    identical(Sys.getenv("RSTUDIO"), "1") &&
    !exists("dashboard_data", envir = .GlobalEnv, inherits = FALSE)) {
  try(source("shiny/load_rstudio_environment.R"), silent = TRUE)
}
