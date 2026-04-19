script_file <- if (!is.null(sys.frame(1)$ofile)) {
  normalizePath(sys.frame(1)$ofile)
} else {
  normalizePath("hosting/build_static_dashboard.R")
}

project_root <- normalizePath(file.path(dirname(script_file), ".."))

setwd(project_root)

source(file.path("shiny", "R", "helpers.R"))
source(file.path("shiny", "R", "demo-data.R"))
source(file.path("shiny", "R", "load-data.R"))

library(dplyr)
library(jsonlite)

docs_dir <- file.path(project_root, "docs")
data_dir <- file.path(docs_dir, "data")
dir.create(docs_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

x <- load_dashboard_rdata(file.path(project_root, "data", "processed", "dashboard.RData"))

trim_num <- function(df, cols) {
  out <- df |>
    select(any_of(cols))

  for (nm in names(out)) {
    if (is.numeric(out[[nm]])) {
      out[[nm]] <- unname(out[[nm]])
    }
  }

  out
}

origin <- x[["origin"]] |>
  group_by(institution_name, sector, spring_year, origin_state) |>
  summarise(freshmen = sum(freshmen, na.rm = TRUE), .groups = "drop")

stem <- x[["stem"]] |>
  group_by(institution_name, sector, year, spring_year, award_level) |>
  summarise(completions = sum(completions, na.rm = TRUE), .groups = "drop")

charges <- x[["charges"]] |>
  group_by(institution_name, sector, year, spring_year) |>
  summarise(
    applications = mean(applications, na.rm = TRUE),
    in_state_charge = mean(in_state_charge, na.rm = TRUE),
    out_state_charge = mean(out_state_charge, na.rm = TRUE),
    .groups = "drop"
  )

schools <- x[["schools"]] |>
  group_by(institution_name, sector, year, spring_year, city, latitude, longitude) |>
  summarise(total_enrollment = sum(total_enrollment, na.rm = TRUE), .groups = "drop")

ethnicity <- x[["ethnicity"]] |>
  group_by(institution_name, sector, year, spring_year) |>
  summarise(
    male = sum(male, na.rm = TRUE),
    female = sum(female, na.rm = TRUE),
    american_indian = sum(american_indian, na.rm = TRUE),
    asian = sum(asian, na.rm = TRUE),
    black = sum(black, na.rm = TRUE),
    hispanic = sum(hispanic, na.rm = TRUE),
    pacific_islander = sum(pacific_islander, na.rm = TRUE),
    white = sum(white, na.rm = TRUE),
    two_or_more = sum(two_or_more, na.rm = TRUE),
    unknown = sum(unknown, na.rm = TRUE),
    nonresident = sum(nonresident, na.rm = TRUE),
    .groups = "drop"
  )

workforce <- x[["workforce"]] |>
  group_by(institution_name, sector, year, spring_year, workforce_group, program_label) |>
  summarise(majors = sum(majors, na.rm = TRUE), .groups = "drop")

payload <- list(
  meta = list(
    institutions = sort(unique(c(
      origin$institution_name,
      stem$institution_name,
      charges$institution_name,
      schools$institution_name,
      ethnicity$institution_name,
      workforce$institution_name
    ))),
    sectors = sort(unique(c(
      origin$sector,
      stem$sector,
      charges$sector,
      schools$sector,
      ethnicity$sector,
      workforce$sector
    )))
  ),
  origin = trim_num(origin, c("institution_name", "sector", "spring_year", "origin_state", "freshmen")),
  stem = trim_num(stem, c("institution_name", "sector", "year", "spring_year", "award_level", "completions")),
  charges = trim_num(charges, c("institution_name", "sector", "year", "spring_year", "applications", "in_state_charge", "out_state_charge")),
  schools = trim_num(schools, c("institution_name", "sector", "year", "spring_year", "city", "latitude", "longitude", "total_enrollment")),
  ethnicity = trim_num(ethnicity, c("institution_name", "sector", "year", "spring_year", "male", "female", "american_indian", "asian", "black", "hispanic", "pacific_islander", "white", "two_or_more", "unknown", "nonresident")),
  workforce = trim_num(workforce, c("institution_name", "sector", "year", "spring_year", "workforce_group", "program_label", "majors"))
)

write_json(
  payload,
  file.path(data_dir, "dashboard-client.json"),
  dataframe = "rows",
  auto_unbox = TRUE,
  pretty = FALSE,
  na = "null"
)

file.copy(
  file.path(project_root, "hosting", "static", "index.html"),
  file.path(docs_dir, "index.html"),
  overwrite = TRUE
)

writeLines("", file.path(docs_dir, ".nojekyll"))

cat("Wrote docs/data/dashboard-client.json and docs/index.html\n")
