load_dashboard_data <- function(data_dir = "data/processed") {
  origin_path <- file.path(data_dir, "freshman_origin.rds")
  stem_path <- file.path(data_dir, "stem_completions.rds")
  charges_path <- file.path(data_dir, "charges.rds")

  required_files <- c(origin_path, stem_path, charges_path)
  if (all(file.exists(required_files))) {
    return(list(
      origin = readRDS(origin_path),
      stem = readRDS(stem_path),
      charges = readRDS(charges_path)
    ))
  }

  build_demo_dashboard_data()
}
