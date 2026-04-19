state_fips_lookup <- c(
  `1` = 'AL', `2` = 'AK', `4` = 'AZ', `5` = 'AR', `6` = 'CA', `8` = 'CO',
  `9` = 'CT', `10` = 'DE', `11` = 'DC', `12` = 'FL', `13` = 'GA', `15` = 'HI',
  `16` = 'ID', `17` = 'IL', `18` = 'IN', `19` = 'IA', `20` = 'KS', `21` = 'KY',
  `22` = 'LA', `23` = 'ME', `24` = 'MD', `25` = 'MA', `26` = 'MI', `27` = 'MN',
  `28` = 'MS', `29` = 'MO', `30` = 'MT', `31` = 'NE', `32` = 'NV', `33` = 'NH',
  `34` = 'NJ', `35` = 'NM', `36` = 'NY', `37` = 'NC', `38` = 'ND', `39` = 'OH',
  `40` = 'OK', `41` = 'OR', `42` = 'PA', `44` = 'RI', `45` = 'SC', `46` = 'SD',
  `47` = 'TN', `48` = 'TX', `49` = 'UT', `50` = 'VT', `51` = 'VA', `53` = 'WA',
  `54` = 'WV', `55` = 'WI', `56` = 'WY'
)

recode_sector <- function(x) {
  dplyr::case_when(
    x == 1 ~ 'Public',
    x == 2 ~ 'Private',
    TRUE ~ as.character(x)
  )
}

load_dashboard_rdata <- function(path) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)

  df_control <- e[['df.control']]
  df_stem <- e[['df.stem']]
  efc <- e[['efc']]
  efa <- e[['efa']]
  efcp <- e[['efcp']]

  sc_control <- df_control |>
    dplyr::filter(.data[['STABBR']] == 'SC')

  control_year <- if ('year' %in% names(sc_control)) {
    as.integer(sc_control[['year']])
  } else {
    as.integer(sc_control[['ipedsyear']]) - 1L
  }

  sc_control <- sc_control |>
    dplyr::mutate(
      institution_name = .data[['INSTNM']],
      sector = recode_sector(.data[['SECTOR']]),
      year = control_year,
      spring_year = control_year + 1L,
      longitude = as.numeric(.data[['LONGITUD']]),
      latitude = as.numeric(.data[['LATITUDE']])
    )

  institution_lookup <- sc_control |>
    dplyr::distinct(UNITID, year, spring_year, institution_name, sector)

  origin <- efc |>
    dplyr::filter(.data[['UNITID']] %in% institution_lookup[['UNITID']]) |>
    dplyr::filter(!is.na(.data[['EFCSTATE']]), .data[['EFCSTATE']] %in% as.integer(names(state_fips_lookup))) |>
    dplyr::transmute(
      unitid = .data[['UNITID']],
      spring_year = as.integer(.data[['ipedsyear']]),
      year = as.integer(.data[['ipedsyear']]) - 1L,
      origin_state = unname(state_fips_lookup[as.character(.data[['EFCSTATE']])]),
      freshmen = as.numeric(.data[['EFRES01']])
    ) |>
    dplyr::filter(!is.na(.data[['origin_state']]), !is.na(.data[['freshmen']])) |>
    dplyr::left_join(institution_lookup, by = c('unitid' = 'UNITID', 'year' = 'year', 'spring_year' = 'spring_year')) |>
    dplyr::filter(!is.na(.data[['institution_name']]))

  stem <- df_stem |>
    dplyr::filter(.data[['stemdummy']] == 1) |>
    dplyr::transmute(
      unitid = .data[['UNITID']],
      year = as.integer(.data[['year']]),
      spring_year = as.integer(.data[['year']]) + 1L,
      award_level = dplyr::case_when(
        .data[['AWLEVEL']] %in% c('03', '3') ~ "Associate's degree",
        .data[['AWLEVEL']] %in% c('05', '5') ~ "Bachelor's degree",
        .data[['AWLEVEL']] %in% c('07', '7') ~ "Master's degree",
        TRUE ~ NA_character_
      ),
      completions = as.numeric(.data[['total']])
    ) |>
    dplyr::filter(!is.na(.data[['award_level']])) |>
    dplyr::group_by(.data[['unitid']], .data[['year']], .data[['spring_year']], .data[['award_level']]) |>
    dplyr::summarise(completions = sum(.data[['completions']], na.rm = TRUE), .groups = 'drop') |>
    dplyr::left_join(institution_lookup, by = c('unitid' = 'UNITID', 'year' = 'year', 'spring_year' = 'spring_year')) |>
    dplyr::filter(!is.na(.data[['institution_name']]))

  charges <- sc_control |>
    dplyr::transmute(
      unitid = .data[['UNITID']],
      institution_name = .data[['institution_name']],
      sector = .data[['sector']],
      year = .data[['year']],
      spring_year = .data[['spring_year']],
      applications = as.numeric(.data[['APPLCN']]),
      in_state_charge = as.numeric(.data[['under.in.charge']]),
      out_state_charge = as.numeric(.data[['under.out.charge']])
    ) |>
    dplyr::filter(!is.na(.data[['in_state_charge']]), !is.na(.data[['out_state_charge']]))

  schools <- efa |>
    dplyr::filter(
      .data[['UNITID']] %in% sc_control[['UNITID']],
      .data[['EFALEVEL']] == 24,
      .data[['LINE']] == 1,
      .data[['SECTION']] == 1,
      .data[['LSTUDY']] == 1
    ) |>
    dplyr::transmute(
      unitid = .data[['UNITID']],
      spring_year = as.integer(.data[['ipedsyear']]),
      year = as.integer(.data[['ipedsyear']]) - 1L,
      total_enrollment = as.numeric(.data[['EFTOTLT']])
    ) |>
    dplyr::left_join(
      sc_control |>
        dplyr::select(UNITID, year, spring_year, institution_name, sector, CITY, latitude, longitude),
      by = c('unitid' = 'UNITID', 'year' = 'year', 'spring_year' = 'spring_year')
    ) |>
    dplyr::rename(city = CITY) |>
    dplyr::filter(!is.na(.data[['institution_name']]), !is.na(.data[['latitude']]), !is.na(.data[['longitude']]))

  ethnicity <- efa |>
    dplyr::filter(
      .data[['UNITID']] %in% sc_control[['UNITID']],
      .data[['EFALEVEL']] == 24,
      .data[['LINE']] == 1,
      .data[['SECTION']] == 1,
      .data[['LSTUDY']] == 1
    ) |>
    dplyr::transmute(
      unitid = .data[['UNITID']],
      spring_year = as.integer(.data[['ipedsyear']]),
      year = as.integer(.data[['ipedsyear']]) - 1L,
      male = as.numeric(.data[['EFTOTLM']]),
      female = as.numeric(.data[['EFTOTLW']]),
      american_indian = as.numeric(.data[['EFAIANT']]),
      asian = as.numeric(.data[['EFASIAT']]),
      black = as.numeric(.data[['EFBKAAT']]),
      hispanic = as.numeric(.data[['EFHISPT']]),
      pacific_islander = as.numeric(.data[['EFNHPIT']]),
      white = as.numeric(.data[['EFWHITT']]),
      two_or_more = as.numeric(.data[['EF2MORT']]),
      unknown = as.numeric(.data[['EFUNKNT']]),
      nonresident = as.numeric(.data[['EFNRALT']])
    ) |>
    dplyr::left_join(institution_lookup, by = c('unitid' = 'UNITID', 'year' = 'year', 'spring_year' = 'spring_year')) |>
    dplyr::filter(!is.na(.data[['institution_name']]))

  workforce <- efcp |>
    dplyr::filter(.data[['UNITID']] %in% sc_control[['UNITID']]) |>
    dplyr::transmute(
      unitid = .data[['UNITID']],
      spring_year = as.integer(.data[['ipedsyear']]),
      year = as.integer(.data[['ipedsyear']]) - 1L,
      workforce_group = dplyr::case_when(
        .data[['LSTUDY']] == 24 & .data[['CIPCODE']] %in% c('13', '14', '26', '27', '40', '52') ~ 'Freshmen majors (24)',
        .data[['LSTUDY']] == 16 & .data[['CIPCODE']] %in% c('22.0101', '51.0401', '51.1201') ~ 'Professional degree majors',
        TRUE ~ NA_character_
      ),
      program_label = dplyr::case_when(
        .data[['LSTUDY']] == 24 & .data[['CIPCODE']] == '52' ~ 'Business',
        .data[['LSTUDY']] == 24 & .data[['CIPCODE']] %in% c('13', '14', '26', '27', '40') ~ trimws(as.character(.data[['CIPCODE.description']])),
        .data[['LSTUDY']] == 16 & .data[['CIPCODE']] %in% c('22.0101', '51.0401', '51.1201') ~ trimws(gsub("\\s*\\([^)]*\\)", "", as.character(.data[['CIPCODE.description']]))),
        TRUE ~ NA_character_
      ),
      majors = as.numeric(.data[['EFTOTLT']])
    ) |>
    dplyr::filter(!is.na(.data[['workforce_group']]), !is.na(.data[['program_label']])) |>
    dplyr::left_join(institution_lookup, by = c('unitid' = 'UNITID', 'year' = 'year', 'spring_year' = 'spring_year')) |>
    dplyr::filter(!is.na(.data[['institution_name']]))

  list(origin = origin, stem = stem, charges = charges, schools = schools, ethnicity = ethnicity, workforce = workforce)
}

load_dashboard_data <- function(data_dir = 'data/processed') {
  dashboard_rdata_path <- file.path(data_dir, 'dashboard.RData')
  origin_path <- file.path(data_dir, 'freshman_origin.rds')
  stem_path <- file.path(data_dir, 'stem_completions.rds')
  charges_path <- file.path(data_dir, 'charges.rds')

  if (file.exists(dashboard_rdata_path)) {
    return(load_dashboard_rdata(dashboard_rdata_path))
  }

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
