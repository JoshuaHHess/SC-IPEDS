source("R/helpers.R")
source("R/demo-data.R")
source("R/load-data.R")

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)

theme_set(
  theme_minimal(base_size = 13, base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", color = "#17322F", size = 16),
      plot.subtitle = element_text(color = "#5C6F6D"),
      plot.caption = element_text(color = "#5C6F6D"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_blank()
    )
)

dashboard_data <- load_dashboard_data()

institutions <- sort(unique(c(
  dashboard_data[['origin']][['institution_name']],
  dashboard_data[['stem']][['institution_name']],
  dashboard_data[['charges']][['institution_name']],
  dashboard_data[['schools']][['institution_name']],
  dashboard_data[['workforce']][['institution_name']]
)))
sectors <- c("All", sort(unique(c(
  dashboard_data[['origin']][['sector']],
  dashboard_data[['stem']][['sector']],
  dashboard_data[['charges']][['sector']],
  dashboard_data[['schools']][['sector']],
  dashboard_data[['workforce']][['sector']]
))))

format_academic_year <- function(spring_year) {
  spring_year <- suppressWarnings(as.integer(spring_year)[1])

  if (length(spring_year) == 0 || is.na(spring_year)) {
    return("Academic Year")
  }

  paste0("Academic Year ", spring_year - 1, "-", substr(as.character(spring_year), 3, 4))
}

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#F4EFE5",
    fg = "#17322F",
    primary = "#D96C3F",
    secondary = "#2F7F79",
    base_font = font_google("Lora"),
    heading_font = font_google("Outfit"),
    code_font = font_google("IBM Plex Mono")
  ),
  tags$head(
    tags$style(HTML("
      .hero {
        padding: 1.75rem 1.75rem 1.25rem 1.75rem;
        border-radius: 24px;
        background: linear-gradient(135deg, rgba(255,250,242,0.96), rgba(243,228,203,0.92));
        box-shadow: 0 18px 60px rgba(80, 62, 35, 0.12);
        margin-bottom: 1rem;
      }
      .hero-title {
        font-family: 'Outfit', sans-serif;
        font-size: clamp(2rem, 5vw, 3.75rem);
        line-height: 0.95;
        margin: 0 0 0.5rem 0;
      }
      .hero-copy {
        color: #5C6F6D;
        max-width: 56rem;
        margin-bottom: 1rem;
      }
      .stat-card {
        background: rgba(255,250,242,0.96);
        border-radius: 20px;
        padding: 1rem 1.15rem;
        border: 1px solid rgba(23,50,47,0.08);
        min-height: 100%;
      }
      .stat-label {
        text-transform: uppercase;
        letter-spacing: 0.08em;
        font-size: 0.75rem;
        color: #5C6F6D;
        font-family: 'Outfit', sans-serif;
      }
      .stat-value {
        font-size: 2rem;
        line-height: 1;
        margin: 0.4rem 0;
      }
      .chart-card {
        background: rgba(255,250,242,0.96);
        border-radius: 24px;
        padding: 1rem 1rem 0.5rem 1rem;
        box-shadow: 0 18px 60px rgba(80, 62, 35, 0.12);
        margin-bottom: 1rem;
      }
      .hint-card {
        background: #17322F;
        color: #F8F3EA;
        border-radius: 24px;
        padding: 1.2rem 1.3rem;
      }
      .control-label {
        font-family: 'Outfit', sans-serif;
        text-transform: uppercase;
        font-size: 0.75rem;
        letter-spacing: 0.08em;
        color: #5C6F6D;
      }
      .section-title {
        font-family: 'Outfit', sans-serif;
        font-size: 1.4rem;
        letter-spacing: 0.02em;
        margin: 1rem 0 0.75rem 0.15rem;
      }
      .section-copy {
        color: #5C6F6D;
        margin: 0 0 1rem 0.15rem;
      }
    "))
  ),
  titlePanel(NULL),
  div(
    class = "hero",
    fluidRow(
      column(
        width = 8,
        div(class = "stat-label", "IPEDS Dashboard"),
        h1(class = "hero-title", "Higher Education in South Carolina"),
        p(
          class = "hero-copy",
          "Data from the National Center for Education Statistics"
        )
      ),
      column(
        width = 4,
        fluidRow(
          column(
            width = 12,
            selectInput("institution", label = div(class = "control-label", "Institution"), choices = c("All Institutions", institutions), selected = "All Institutions")
          ),
          column(
            width = 6,
            selectInput("sector", label = div(class = "control-label", "Sector"), choices = sectors, selected = "All")
          ),
        )
      )
    )
  ),
  fluidRow(
    column(4, uiOutput("kpi_origin")),
    column(4, uiOutput("kpi_stem")),
    column(4, uiOutput("kpi_gap"))
  ),
  uiOutput("snapshot_title"),
  p(class = "section-copy", "A quick view of the latest South Carolina institution map and out-of-state origin map for the freshmen snapshot."),
  fluidRow(
    column(
      width = 6,
      div(
        class = "chart-card",
        plotlyOutput("schools_plot", height = "480px")
      )
    ),
    column(
      width = 6,
      div(
        class = "chart-card",
        plotlyOutput("origin_plot", height = "480px")
      )
    )
  ),
  div(class = "section-title", "Freshmen Class Composition"),
  fluidRow(
    column(
      width = 6,
      div(
        class = "chart-card",
        plotlyOutput("ethnicity_plot", height = "480px")
      )
    ),
    column(
      width = 6,
      div(
        class = "chart-card",
        plotlyOutput("gender_plot", height = "480px")
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      div(
        class = "chart-card",
        plotlyOutput("workforce_plot", height = "480px")
      )
    )
  ),
  div(class = "section-title", "Time Trends"),
  p(class = "section-copy", "Multi-year trend lines for STEM degrees and average undergraduate charges."),
  fluidRow(
    column(
      width = 6,
      div(
        class = "chart-card",
        selectInput(
          "stem_award_level",
          label = div(class = "control-label", "STEM Degree Level"),
          choices = c(
            "Total" = "Total",
            "Associate's" = "Associate's degree",
            "Bachelor's" = "Bachelor's degree",
            "Master's" = "Master's degree"
          ),
          selected = "Total"
        ),
        plotlyOutput("stem_plot", height = "360px")
      )
    ),
    column(
      width = 6,
      div(
        class = "chart-card",
        plotlyOutput("charges_plot", height = "360px")
      )
    )
  )
)

server <- function(input, output, session) {
  filter_for_inputs <- function(df) {
    df |>
      filter(if (input$sector == "All") TRUE else sector == input$sector) |>
      filter(if (input$institution == "All Institutions") TRUE else institution_name == input$institution)
  }

  snapshot_spring_year <- reactive({
    spring_year_sets <- list(
      filter_for_inputs(dashboard_data$origin) |> pull(spring_year),
      filter_for_inputs(dashboard_data$schools) |> pull(spring_year),
      filter_for_inputs(dashboard_data$ethnicity) |> pull(spring_year),
      filter_for_inputs(dashboard_data$stem) |> pull(spring_year),
      filter_for_inputs(dashboard_data$charges) |> pull(spring_year)
    ) |>
      lapply(function(x) sort(unique(x[!is.na(x)])))

    if (any(lengths(spring_year_sets) == 0)) {
      return(NA_integer_)
    }

    common_years <- Reduce(intersect, spring_year_sets)

    if (length(common_years) == 0) {
      return(NA_integer_)
    }

    max(common_years, na.rm = TRUE)
  })

  snapshot_year_label <- reactive({
    format_academic_year(snapshot_spring_year())
  })

  output$snapshot_title <- renderUI({
    div(class = "section-title", paste("Freshmen Snapshot:", snapshot_year_label()))
  })

  snapshot_origin <- reactive({
    base <- filter_for_inputs(dashboard_data$origin)
    base |>
      filter(spring_year == snapshot_spring_year())
  })

  snapshot_schools <- reactive({
    base <- filter_for_inputs(dashboard_data$schools)
    base |>
      filter(spring_year == snapshot_spring_year())
  })

  snapshot_ethnicity <- reactive({
    base <- filter_for_inputs(dashboard_data$ethnicity)
    base |>
      filter(spring_year == snapshot_spring_year())
  })

  snapshot_stem <- reactive({
    base <- filter_for_inputs(dashboard_data$stem)
    base |>
      filter(spring_year == snapshot_spring_year())
  })

  snapshot_charges <- reactive({
    base <- filter_for_inputs(dashboard_data$charges)
    base |>
      filter(spring_year == snapshot_spring_year())
  })

  snapshot_workforce <- reactive({
    base <- dashboard_data$workforce |>
      filter(if (input$sector == "All") TRUE else sector == input$sector) |>
      filter(if (input$institution == "All Institutions") TRUE else institution_name == input$institution)

    latest_year <- max(base$year, na.rm = TRUE)
    base |>
      filter(year == latest_year)
  })

  filtered_origin <- reactive({
    dashboard_data$origin |>
      filter(if (input$sector == "All") TRUE else sector == input$sector) |>
      filter(if (input$institution == "All Institutions") TRUE else institution_name == input$institution)
  })

  filtered_stem <- reactive({
    dashboard_data$stem |>
      filter(if (input$sector == "All") TRUE else sector == input$sector) |>
      filter(if (input$institution == "All Institutions") TRUE else institution_name == input$institution)
  })

  filtered_charges <- reactive({
    dashboard_data$charges |>
      filter(if (input$sector == "All") TRUE else sector == input$sector) |>
      filter(if (input$institution == "All Institutions") TRUE else institution_name == input$institution)
  })

  output$kpi_origin <- renderUI({
    origin <- snapshot_origin()
    top_state <- origin |>
      filter(origin_state != "SC") |>
      group_by(origin_state) |>
      summarise(freshmen = sum(freshmen, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(freshmen)) |>
      slice_head(n = 1)

    div(
      class = "stat-card",
      div(class = "stat-label", "Largest Out-of-State Origin"),
      div(class = "stat-value", top_state$origin_state %||% "NA"),
      p(comma(top_state$freshmen %||% 0), paste0(" out-of-state freshmen in ", snapshot_year_label()))
    )
  })

  output$kpi_stem <- renderUI({
    stem <- snapshot_stem()

    selected_stem <- if (input$stem_award_level == "Total") {
      stem
    } else {
      stem |>
        filter(award_level == input$stem_award_level)
    }

    selected_label <- if (input$stem_award_level == "Total") "Total" else input$stem_award_level
    total_degrees <- sum(selected_stem$completions, na.rm = TRUE)

    div(
      class = "stat-card",
      div(class = "stat-label", "STEM Degrees"),
      div(class = "stat-value", comma(total_degrees)),
      p(paste0("Total ", selected_label, " in ", snapshot_year_label()))
    )
  })

  output$kpi_gap <- renderUI({
    applications_base <- filtered_charges() |>
      filter(!is.na(applications))

    latest_app_spring_year <- max(applications_base$spring_year, na.rm = TRUE)

    applications_total <- applications_base |>
      filter(spring_year == latest_app_spring_year) |>
      summarise(applications = sum(applications, na.rm = TRUE))

    div(
      class = "stat-card",
      div(class = "stat-label", "Applications"),
      div(class = "stat-value", comma((applications_total$applications %||% 0)[1])),
      p(paste0("Total applications in ", format_academic_year(latest_app_spring_year)))
    )
  })

  output$schools_plot <- renderPlotly({
    school_summary <- snapshot_schools() |>
      mutate(
        hover_text = paste0(
          institution_name,
          "<br>",
          city,
          ", SC",
          "<br>Total enrollment: ",
          comma(total_enrollment)
        ),
        marker_size = scales::rescale(sqrt(total_enrollment), to = c(10, 42))
      )

    public_schools <- school_summary |>
      filter(sector == "Public")
    private_schools <- school_summary |>
      filter(sector == "Private")

    plot_ly() |>
      add_trace(
        type = "choropleth",
        locationmode = "USA-states",
        locations = "SC",
        z = 1,
        colorscale = list(c(0, "#F3E4CB"), c(1, "#F3E4CB")),
        showscale = FALSE,
        hoverinfo = "skip",
        marker = list(line = list(color = "rgba(23,50,47,0.9)", width = 2.2))
      ) |>
      add_trace(
        data = public_schools,
        type = "scattergeo",
        mode = "markers",
        name = "Public",
        lat = ~latitude,
        lon = ~longitude,
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        marker = list(
          color = "#2F7F79",
          size = public_schools$marker_size,
          line = list(color = "rgba(23,50,47,0.65)", width = 1),
          opacity = 0.88
        )
      ) |>
      add_trace(
        data = private_schools,
        type = "scattergeo",
        mode = "markers",
        name = "Private",
        lat = ~latitude,
        lon = ~longitude,
        text = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        marker = list(
          color = "#D96C3F",
          size = private_schools$marker_size,
          line = list(color = "rgba(23,50,47,0.65)", width = 1),
          opacity = 0.88
        )
      ) |>
      layout(
        title = list(
          text = paste0("South Carolina Institutions<br><sup>", snapshot_year_label(), " sized by total enrollment</sup>"),
          x = 0
        ),
        geo = list(
          scope = "north america",
          fitbounds = FALSE,
          projection = list(type = "mercator"),
          center = list(lat = 33.9, lon = -80.9),
          lonaxis = list(range = c(-83.6, -78.2), showgrid = FALSE),
          lataxis = list(range = c(31.8, 35.3), showgrid = FALSE),
          showframe = FALSE,
          showland = TRUE,
          landcolor = "#F3E4CB",
          showsubunits = TRUE,
          subunitcolor = "rgba(23,50,47,0.55)",
          showcountries = FALSE,
          showcoastlines = FALSE,
          showlakes = FALSE,
          showrivers = FALSE,
          bgcolor = "rgba(0,0,0,0)"
        ),
        legend = list(orientation = "h", x = 0.3, y = 1.08),
        dragmode = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 10, r = 10, t = 80, b = 10)
      ) |>
      config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
  })

  output$origin_plot <- renderPlotly({
    origin_summary <- snapshot_origin() |>
      filter(origin_state != "SC") |>
      group_by(origin_state) |>
      summarise(freshmen = sum(freshmen, na.rm = TRUE), .groups = "drop") |>
      mutate(
        origin_state = toupper(origin_state),
        hover_text = paste0(origin_state, ": ", comma(freshmen), " out-of-state freshmen")
      ) |>
      filter(nchar(origin_state) == 2)

    plot_ly(
      data = origin_summary,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~origin_state,
      z = ~freshmen,
      text = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      colorscale = list(
        c(0, "#FFF8EF"),
        c(0.18, "#F6D58A"),
        c(0.42, "#E89A47"),
        c(0.7, "#D2552F"),
        c(1, "#6E1F1B")
      ),
      marker = list(line = list(color = "rgba(255,250,242,0.85)", width = 1)),
      colorbar = list(title = list(text = "Freshmen"))
    ) |>
      layout(
        title = list(
          text = paste0("Out-of-State Origins<br><sup>", snapshot_year_label(), " excluding South Carolina residents</sup>"),
          x = 0
        ),
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = FALSE,
          bgcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 10, r = 10, t = 80, b = 10)
      ) |>
      config(displayModeBar = FALSE, scrollZoom = FALSE, doubleClick = FALSE)
  })

  output$ethnicity_plot <- renderPlotly({
    ethnicity_summary <- snapshot_ethnicity() |>
      summarise(
        `American Indian` = sum(american_indian, na.rm = TRUE),
        Asian = sum(asian, na.rm = TRUE),
        Black = sum(black, na.rm = TRUE),
        Hispanic = sum(hispanic, na.rm = TRUE),
        `Pacific Islander` = sum(pacific_islander, na.rm = TRUE),
        White = sum(white, na.rm = TRUE),
        `Two or More` = sum(two_or_more, na.rm = TRUE),
        Unknown = sum(unknown, na.rm = TRUE),
        Nonresident = sum(nonresident, na.rm = TRUE)
      ) |>
      tidyr::pivot_longer(cols = everything(), names_to = "group", values_to = "count") |>
      filter(count > 0)

    plot_ly(
      data = ethnicity_summary,
      labels = ~group,
      values = ~count,
      type = "pie",
      textinfo = "label+percent",
      textposition = "inside",
      sort = FALSE,
      marker = list(colors = c("#2F7F79", "#C79A36", "#D96C3F", "#6D8E89", "#B65D3A", "#204B47", "#E8B66A", "#A7B8B5", "#8A9F4D"), line = list(color = "#FFF8EF", width = 1))
    ) |>
      layout(
        title = list(text = paste0("Race and Ethnicity<br><sup>", snapshot_year_label(), "</sup>"), x = 0),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 10, r = 10, t = 80, b = 10)
      )
  })


  output$gender_plot <- renderPlotly({
    gender_summary <- snapshot_ethnicity() |>
      summarise(
        Men = sum(male, na.rm = TRUE),
        Women = sum(female, na.rm = TRUE)
      ) |>
      tidyr::pivot_longer(cols = everything(), names_to = "group", values_to = "count") |>
      filter(count > 0)

    plot_ly(
      data = gender_summary,
      labels = ~group,
      values = ~count,
      type = "pie",
      textinfo = "label+percent",
      textposition = "inside",
      sort = FALSE,
      marker = list(colors = c("#2F7F79", "#D96C3F"), line = list(color = "#FFF8EF", width = 1))
    ) |>
      layout(
        title = list(text = paste0("Gender<br><sup>", snapshot_year_label(), "</sup>"), x = 0),
        showlegend = FALSE,
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 10, r = 10, t = 80, b = 10)
      )
  })

  output$workforce_plot <- renderPlotly({
    workforce_summary <- snapshot_workforce() |>
      group_by(workforce_group, program_label) |>
      summarise(majors = sum(majors, na.rm = TRUE), .groups = "drop") |>
      mutate(
        workforce_group = factor(workforce_group, levels = c("Freshmen majors (24)", "Professional degree majors")),
        program_label = factor(
          program_label,
          levels = c(
            "Education",
            "Engineering",
            "Biological Sciences/Life Sciences",
            "Mathematics",
            "Physical Sciences",
            "Business Management & Admin. Services",
            "Law",
            "Dentistry",
            "Medicine"
          )
        ),
        hover_text = paste0(program_label, "<br>", workforce_group, ": ", comma(majors), " majors")
      ) |>
      arrange(program_label)

    plot_ly(
      data = workforce_summary,
      x = ~program_label,
      y = ~majors,
      color = ~workforce_group,
      colors = c("Freshmen majors (24)" = "#2F7F79", "Professional degree majors" = "#D96C3F"),
      type = "bar",
      text = ~comma(majors),
      textposition = "outside",
      hovertemplate = "%{text}<extra></extra>"
    ) |>
      layout(
        title = list(text = paste0("Workforce Readiness Majors<br><sup>", snapshot_year_label(), "</sup>"), x = 0),
        xaxis = list(title = "", tickangle = -25, categoryorder = "array"),
        yaxis = list(title = "Majors", tickformat = ",d", autorange = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 60, r = 20, t = 80, b = 120),
        legend = list(orientation = "h", x = 0.05, y = 1.12, title = list(text = "")),
        barmode = "group"
      )
  })

  output$stem_plot <- renderPlotly({
    stem_base <- if (input$stem_award_level == "Total") {
      filtered_stem()
    } else {
      filtered_stem() |>
        filter(award_level == input$stem_award_level)
    }

    stem_summary <- stem_base |>
      group_by(year) |>
      summarise(completions = sum(completions, na.rm = TRUE), .groups = "drop") |>
      arrange(year)

    stem_breaks <- seq(min(stem_summary$year, na.rm = TRUE), max(stem_summary$year, na.rm = TRUE), by = 1)
    selected_label <- if (input$stem_award_level == "Total") "Total" else input$stem_award_level
    selected_color <- dplyr::case_when(
      input$stem_award_level == "Associate's degree" ~ "#2F7F79",
      input$stem_award_level == "Bachelor's degree" ~ "#C79A36",
      input$stem_award_level == "Master's degree" ~ "#D96C3F",
      TRUE ~ "#2F7F79"
    )

    plot_ly(
      data = stem_summary,
      x = ~year,
      y = ~completions,
      type = "scatter",
      mode = "lines+markers",
      line = list(color = selected_color, width = 3),
      marker = list(color = selected_color, size = 9),
      text = ~paste0(selected_label, "<br>", year, ": ", comma(completions), " degrees"),
      hovertemplate = "%{text}<extra></extra>"
    ) |>
      layout(
        title = list(text = paste0("STEM Degrees<br><sup>", selected_label, "</sup>"), x = 0),
        xaxis = list(title = "", tickmode = "array", tickvals = stem_breaks, ticktext = stem_breaks, autorange = TRUE),
        yaxis = list(title = "Degrees", tickformat = ",d", autorange = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 60, r = 20, t = 70, b = 40),
        showlegend = FALSE
      )
  })

  output$charges_plot <- renderPlotly({
    charges_summary <- filtered_charges() |>
      group_by(year) |>
      summarise(
        in_state_charge = mean(in_state_charge, na.rm = TRUE),
        out_state_charge = mean(out_state_charge, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(year)

    charge_breaks <- seq(min(charges_summary$year, na.rm = TRUE), max(charges_summary$year, na.rm = TRUE), by = 1)

    plot_ly() |>
      add_trace(
        data = charges_summary,
        x = ~year,
        y = ~in_state_charge,
        type = "scatter",
        mode = "lines+markers",
        name = "In-State",
        line = list(color = "#C79A36", width = 3),
        marker = list(color = "#C79A36", size = 9),
        text = ~paste0(year, ": ", dollar(in_state_charge)),
        hovertemplate = "In-State<br>%{text}<extra></extra>"
      ) |>
      add_trace(
        data = charges_summary,
        x = ~year,
        y = ~out_state_charge,
        type = "scatter",
        mode = "lines+markers",
        name = "Out-of-State",
        line = list(color = "#D96C3F", width = 3),
        marker = list(color = "#D96C3F", size = 9),
        text = ~paste0(year, ": ", dollar(out_state_charge)),
        hovertemplate = "Out-of-State<br>%{text}<extra></extra>"
      ) |>
      layout(
        title = list(text = "Average Charges Over Time<br><sup>Mean charges for the current selection</sup>", x = 0),
        xaxis = list(title = "", tickmode = "array", tickvals = charge_breaks, ticktext = charge_breaks, autorange = TRUE),
        yaxis = list(title = "Average charge", tickprefix = "$", separatethousands = TRUE, autorange = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 60, r = 20, t = 70, b = 40),
        legend = list(orientation = "h", x = 0.25, y = 1.1)
      )
  })
}

shinyApp(ui, server)
