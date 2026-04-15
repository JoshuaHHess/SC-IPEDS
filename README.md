# SC IPEDS Dashboard

This repo contains an RStudio-friendly Shiny dashboard for South Carolina IPEDS analysis. It is set up around three views:

- where freshmen are coming from
- STEM degree completions over time
- average in-state and out-of-state charges over time

## Open In RStudio

Open [SC-IPEDS.Rproj](SC-IPEDS.Rproj), then run:

```r
install.packages(c("shiny", "bslib", "dplyr", "ggplot2", "plotly", "readr", "stringr", "tidyr", "tibble", "scales"))
shiny::runApp()
```

If `data/processed/*.rds` files do not exist yet, the app uses demo data so the dashboard still launches.

## Project Layout

```text
SC-IPEDS/
├── app.R
├── R/
├── data/
│   ├── raw/
│   └── processed/
└── data-raw/
```

## Use Your IPEDS Files

1. Put your raw IPEDS extracts in `data/raw/`.
2. Rename or remap them to match the placeholders in `data-raw/process_ipeds.R`.
3. Run:

```r
source("data-raw/process_ipeds.R")
```

4. Relaunch the app with:

```r
shiny::runApp()
```

## Expected Columns

The first pass assumes these columns are available after any export cleanup:

### Freshman origin

- `unitid`
- `institution_name`
- `sector`
- `year`
- `origin_state`
- `freshmen`

### Completions

- `unitid`
- `institution_name`
- `sector`
- `year`
- `cip_code`
- `completions`

### Charges

- `unitid`
- `institution_name`
- `sector`
- `year`
- `in_state_charge`
- `out_state_charge`

## Notes

- The STEM trend is currently defined by CIP prefixes in `data-raw/process_ipeds.R`.
- Once we inspect your exact IPEDS exports, we can tighten the mapping and use official variable names from your files.
- The current charge chart uses simple averages. If you want enrollment-weighted averages instead, we can switch that in the processing step.
