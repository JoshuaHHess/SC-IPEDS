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

institutions <- sort(unique(dashboard_data$origin$institution_name))
sectors <- c("All", sort(unique(dashboard_data$origin$sector)))
years <- range(dashboard_data$stem$year, na.rm = TRUE)

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
    "))
  ),
  titlePanel(NULL),
  div(
    class = "hero",
    fluidRow(
      column(
        width = 8,
        div(class = "stat-label", "IPEDS Dashboard"),
        h1(class = "hero-title", "South Carolina Student Flow and Cost Trends"),
        p(
          class = "hero-copy",
          "A prototype Shiny dashboard for South Carolina institutions showing where freshmen come from, how STEM completions change over time, and how resident and nonresident charges compare."
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
          column(
            width = 6,
            sliderInput("year_range", label = div(class = "control-label", "Year Range"), min = years[1], max = years[2], value = years, sep = "")
          )
        )
      )
    )
  ),
  fluidRow(
    column(4, uiOutput("kpi_origin")),
    column(4, uiOutput("kpi_stem")),
    column(4, uiOutput("kpi_gap"))
  ),
  fluidRow(
    column(
      width = 8,
      div(
        class = "chart-card",
        plotlyOutput("origin_plot", height = "520px")
      )
    ),
    column(
      width = 4,
      div(
        class = "hint-card",
        h3("How To Wire Real IPEDS Data"),
        p("Drop your raw files into data/raw, run source('data-raw/process_ipeds.R'), and reload the app. The dashboard will prefer processed .rds files when they exist."),
        h3("Expected Inputs"),
        p("Freshman origin, completions by CIP, and tuition or price data with year, institution, and sector fields. The processing script is set up so we can tailor the column mapping once we inspect your files.")
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      div(
        class = "chart-card",
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
  filtered_origin <- reactive({
    dashboard_data$origin |>
      filter(year >= input$year_range[1], year <= input$year_range[2]) |>
      filter(input$sector == "All" || sector == input$sector) |>
      filter(input$institution == "All Institutions" || institution_name == input$institution)
  })

  filtered_stem <- reactive({
    dashboard_data$stem |>
      filter(year >= input$year_range[1], year <= input$year_range[2]) |>
      filter(input$sector == "All" || sector == input$sector) |>
      filter(input$institution == "All Institutions" || institution_name == input$institution)
  })

  filtered_charges <- reactive({
    dashboard_data$charges |>
      filter(year >= input$year_range[1], year <= input$year_range[2]) |>
      filter(input$sector == "All" || sector == input$sector) |>
      filter(input$institution == "All Institutions" || institution_name == input$institution)
  })

  output$kpi_origin <- renderUI({
    origin <- filtered_origin()
    top_state <- origin |>
      group_by(origin_state) |>
      summarise(freshmen = sum(freshmen, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(freshmen)) |>
      slice_head(n = 1)

    div(
      class = "stat-card",
      div(class = "stat-label", "Largest Origin"),
      div(class = "stat-value", top_state$origin_state %||% "NA"),
      p(comma(top_state$freshmen %||% 0), " freshmen in the selected view")
    )
  })

  output$kpi_stem <- renderUI({
    stem <- filtered_stem() |>
      group_by(year) |>
      summarise(completions = sum(completions, na.rm = TRUE), .groups = "drop") |>
      arrange(year)

    growth <- if (nrow(stem) >= 2 && stem$completions[1] > 0) {
      percent((last(stem$completions) / stem$completions[1]) - 1, accuracy = 0.1)
    } else {
      "n/a"
    }

    div(
      class = "stat-card",
      div(class = "stat-label", "STEM Completion Growth"),
      div(class = "stat-value", growth),
      p("Across the current filter selection")
    )
  })

  output$kpi_gap <- renderUI({
    charges <- filtered_charges() |>
      summarise(gap = mean(out_state_charge - in_state_charge, na.rm = TRUE))

    div(
      class = "stat-card",
      div(class = "stat-label", "Average Charge Gap"),
      div(class = "stat-value", dollar(charges$gap %||% 0)),
      p("Out-of-state charges above in-state charges")
    )
  })

  output$origin_plot <- renderPlotly({
    origin_summary <- filtered_origin() |>
      group_by(origin_state) |>
      summarise(freshmen = sum(freshmen, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(freshmen)) |>
      slice_head(n = 12)

    p <- ggplot(origin_summary, aes(x = reorder(origin_state, freshmen), y = freshmen, fill = origin_state, text = paste0(origin_state, ": ", comma(freshmen), " freshmen"))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = rep_len(c("#2F7F79", "#D96C3F", "#C79A36", "#6D8E89"), nrow(origin_summary))) +
      labs(
        title = "Where Freshmen Are Coming From",
        subtitle = "Top origin states for the current South Carolina selection",
        y = "Freshmen",
        x = NULL
      )

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 80, r = 20, t = 70, b = 40))
  })

  output$stem_plot <- renderPlotly({
    stem_summary <- filtered_stem() |>
      group_by(year) |>
      summarise(completions = sum(completions, na.rm = TRUE), .groups = "drop")

    p <- ggplot(stem_summary, aes(x = year, y = completions, text = paste0(year, ": ", comma(completions), " completions"))) +
      geom_line(color = "#2F7F79", linewidth = 1.4) +
      geom_point(color = "#2F7F79", size = 2.8) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "STEM Degree Completions",
        subtitle = "Illustrated as a single statewide trend",
        y = "Completions"
      )

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 60, r = 20, t = 70, b = 40))
  })

  output$charges_plot <- renderPlotly({
    charges_summary <- filtered_charges() |>
      group_by(year) |>
      summarise(
        in_state_charge = mean(in_state_charge, na.rm = TRUE),
        out_state_charge = mean(out_state_charge, na.rm = TRUE),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(cols = c(in_state_charge, out_state_charge), names_to = "charge_type", values_to = "amount")

    p <- ggplot(charges_summary, aes(x = year, y = amount, color = charge_type, text = paste0(year, ": ", dollar(amount)))) +
      geom_line(linewidth = 1.4) +
      geom_point(size = 2.8) +
      scale_x_continuous(breaks = pretty_breaks()) +
      scale_y_continuous(labels = dollar) +
      scale_color_manual(
        values = c(in_state_charge = "#C79A36", out_state_charge = "#D96C3F"),
        labels = c(in_state_charge = "In-State", out_state_charge = "Out-of-State")
      ) +
      labs(
        title = "Average Charges Over Time",
        subtitle = "Mean charges for the current selection",
        y = "Average charge",
        color = NULL
      )

    ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 60, r = 20, t = 70, b = 40), legend = list(orientation = "h", x = 0.25, y = 1.1))
  })
}

shinyApp(ui, server)
