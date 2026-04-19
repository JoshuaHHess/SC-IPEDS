script_file <- if (!is.null(sys.frame(1)$ofile)) {
  normalizePath(sys.frame(1)$ofile)
} else {
  normalizePath("shiny/load_rstudio_environment.R")
}

script_dir <- dirname(script_file)
project_root <- normalizePath(file.path(script_dir, ".."))

setwd(project_root)

source(file.path("shiny", "R", "load-data.R"))

load(file.path(project_root, "data", "processed", "dashboard.RData"), envir = .GlobalEnv)

dashboard_data <- load_dashboard_data()

origin <- dashboard_data[["origin"]]
stem <- dashboard_data[["stem"]]
charges <- dashboard_data[["charges"]]
schools <- dashboard_data[["schools"]]
ethnicity <- dashboard_data[["ethnicity"]]
workforce <- dashboard_data[["workforce"]]

message("Loaded raw IPEDS objects and derived dashboard data into the global environment.")
