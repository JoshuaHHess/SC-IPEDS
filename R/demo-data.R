build_demo_dashboard_data <- function() {
  years <- 2014:2024
  institutions <- tibble::tribble(
    ~unitid, ~institution_name, ~sector,
    217156, "University of South Carolina", "Public 4-Year",
    218663, "Clemson University", "Public 4-Year",
    218070, "College of Charleston", "Public 4-Year",
    217925, "Greenville Technical College", "Public 2-Year"
  )

  origin_states <- c("SC", "NC", "GA", "VA", "TN", "FL", "MD", "NJ")

  origin <- tidyr::crossing(institutions, year = years, origin_state = origin_states) |>
    dplyr::mutate(
      freshmen = dplyr::case_when(
        origin_state == "SC" ~ round(runif(dplyr::n(), 2200, 4800)),
        origin_state == "NC" ~ round(runif(dplyr::n(), 500, 1800)),
        origin_state == "GA" ~ round(runif(dplyr::n(), 350, 1200)),
        TRUE ~ round(runif(dplyr::n(), 75, 700))
      )
    )

  stem <- tidyr::crossing(institutions, year = years) |>
    dplyr::group_by(unitid) |>
    dplyr::mutate(
      completions = round(seq(950, 1550, length.out = dplyr::n()) + runif(dplyr::n(), -90, 120) + dplyr::cur_group_id() * 260)
    ) |>
    dplyr::ungroup()

  charges <- tidyr::crossing(institutions, year = years) |>
    dplyr::group_by(unitid) |>
    dplyr::mutate(
      in_state_charge = round(seq(9800, 14600, length.out = dplyr::n()) + dplyr::cur_group_id() * 680 + runif(dplyr::n(), -250, 250)),
      out_state_charge = round(in_state_charge + runif(dplyr::n(), 7800, 14500))
    ) |>
    dplyr::ungroup()

  list(origin = origin, stem = stem, charges = charges)
}
