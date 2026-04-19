script_file <- if (!is.null(sys.frame(1)$ofile)) {
  normalizePath(sys.frame(1)$ofile)
} else {
  normalizePath("hosting/run_app.R")
}

project_root <- normalizePath(file.path(dirname(script_file), ".."))

shiny::runApp(file.path(project_root, "shiny"))
