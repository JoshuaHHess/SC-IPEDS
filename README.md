# SC IPEDS Dashboard

This project is organized around two separate outputs:

- `shiny/` for the interactive app
- a static deployable dashboard built from `hosting/static/` into `docs/` for GitHub Pages

## Structure

- `shiny/` contains the interactive app and its helper files.
- `hosting/` contains run/install/inspection helpers plus the source files for the static deployment page.
- `docs/` is the generated GitHub Pages output.
- `data/` contains the processed IPEDS data used by the app.
- `data-raw/` contains the raw-data processing script.

## Run The App

From the project root in R:

```r
source("hosting/install_packages.R")
source("hosting/run_app.R")
```

Or directly:

```r
shiny::runApp("shiny")
```

## Build The Static Deployment Page

From the project root in R:

```r
source("hosting/install_packages.R")
source("hosting/build_static_dashboard.R")
```

That rebuilds:

- `docs/index.html`
- `docs/data/dashboard-client.json`

For GitHub Pages, publish the `docs/` folder from your `main` branch.

## RStudio Environment

When you open the project in RStudio, `.Rprofile` sources `shiny/load_rstudio_environment.R` so the key data frames are available in the Environment pane.
