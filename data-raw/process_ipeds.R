suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

# Update these file names after you inspect the exact IPEDS exports in data/raw.
default_paths <- list(
  origin = "data/raw/freshman_origin.csv",
  completions = "data/raw/completions.csv",
  charges = "data/raw/charges.csv"
)

stem_cip_prefixes <- c(
  "01", "03", "11", "14", "15", "26", "27", "30.08", "40", "41"
)

read_required_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Missing file: ", path, call. = FALSE)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

normalize_sector <- function(x) {
  dplyr::case_when(
    str_detect(str_to_lower(x), "public.*4") ~ "Public 4-Year",
    str_detect(str_to_lower(x), "public.*2") ~ "Public 2-Year",
    str_detect(str_to_lower(x), "private") ~ "Private Nonprofit",
    TRUE ~ x
  )
}

build_origin_data <- function(df) {
  df |>
    transmute(
      unitid = .data$unitid,
      institution_name = .data$institution_name,
      sector = normalize_sector(.data$sector),
      year = as.integer(.data$year),
      origin_state = .data$origin_state,
      freshmen = as.numeric(.data$freshmen)
    ) |>
    filter(!is.na(year), !is.na(freshmen))
}

build_stem_data <- function(df) {
  df |>
    mutate(cip_code = as.character(.data$cip_code)) |>
    filter(str_detect(cip_code, paste0("^(", paste(stem_cip_prefixes, collapse = "|"), ")"))) |>
    transmute(
      unitid = .data$unitid,
      institution_name = .data$institution_name,
      sector = normalize_sector(.data$sector),
      year = as.integer(.data$year),
      completions = as.numeric(.data$completions)
    ) |>
    group_by(unitid, institution_name, sector, year) |>
    summarise(completions = sum(completions, na.rm = TRUE), .groups = "drop")
}

build_charges_data <- function(df) {
  df |>
    transmute(
      unitid = .data$unitid,
      institution_name = .data$institution_name,
      sector = normalize_sector(.data$sector),
      year = as.integer(.data$year),
      in_state_charge = as.numeric(.data$in_state_charge),
      out_state_charge = as.numeric(.data$out_state_charge)
    ) |>
    filter(!is.na(year))
}

process_ipeds <- function(paths = default_paths, output_dir = "data/processed") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  origin <- build_origin_data(read_required_csv(paths$origin))
  stem <- build_stem_data(read_required_csv(paths$completions))
  charges <- build_charges_data(read_required_csv(paths$charges))

  saveRDS(origin, file.path(output_dir, "freshman_origin.rds"))
  saveRDS(stem, file.path(output_dir, "stem_completions.rds"))
  saveRDS(charges, file.path(output_dir, "charges.rds"))

  invisible(list(origin = origin, stem = stem, charges = charges))
}

if (sys.nframe() == 0) {
  process_ipeds()
}
