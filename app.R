# River Hydrology Teaching Module
#
# Interactive app for exploring hydrology metrics (RBI, recession slope)
# across North American LTER and USGS sites. Built for CUAHSI workshops.
#
# Update data_path below to point at your local data directory.

suppressPackageStartupMessages({
  if (!require("librarian")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    install.packages("librarian")
  }
  librarian::shelf(shiny, bslib, dplyr, ggplot2, leaflet, leaflet.extras, plotly, viridis)
})

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Hydrology_Lab/CUAHSI-teaching-modules-shiny/data"

# shared palette — keeps colors consistent between the map, plots, and UI
module_colors <- c(
  "primary" = "#6b9bd1",
  "secondary" = "#5a7fa8",
  "success" = "#7fb069",
  "danger" = "#d67e7e",
  "warning" = "#e6c79c"
)

snow_colors <- c(
  "Low (0-40 days)" = "#d67e7e",
  "Medium (40-80 days)" = "#e6c79c",
  "High (80+ days)" = "#6b9bd1"
)

# these get reused in multiple ggplots, so pulling them out here
base_plot_theme <- theme_minimal(base_family = "Work Sans") +
  theme(
    plot.background = element_rect(fill = "#fefcfb", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
    panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
    text = element_text(color = "#2d2926"),
    axis.text = element_text(color = "#2d2926")
  )

plotly_bg <- list(paper_bgcolor = "#fefcfb", plot_bgcolor = "#ffffff")

# load discharge once at startup — 269 MB so we use fread for speed
discharge_global <- data.table::fread(
  file.path(data_path, "discharge_north_america.csv")
) %>%
  as.data.frame() %>%
  mutate(Date = as.Date(Date))


# --- UI -------------------------------------------------------------------

ui <- page_navbar(
  title = "River Hydrology Teaching Module",
  theme = bs_theme(
    base_font = font_google("Work Sans"),
    bg = "#fefcfb",
    fg = "#2d2926",
    navbar_bg = "#ffffff",
    navbar_fg = "#2d2926",
    primary = "#6b9bd1",
    secondary = "#5a7fa8",
    success = "#7fb069",
    danger = "#d67e7e",
    "card-bg" = "#ffffff",
    "card-border-color" = "#d4e3f0"
  ),

  header = tags$head(
    tags$style(HTML(
      "
      body {
        background: #fefcfb !important;
        font-family: 'Work Sans', sans-serif !important;
      }

      #map, .leaflet-container {
        background: #ffffff !important;
      }

      .card {
        border: 1px solid #d4e3f0 !important;
        box-shadow: 0 4px 20px rgba(107,155,209,0.08) !important;
        border-radius: 12px !important;
        background: #ffffff !important;
      }

      .card-header {
        background: #f5f9fc !important;
        border-bottom: 1px solid #d4e3f0 !important;
        color: #2d2926 !important;
        font-weight: 600 !important;
        border-radius: 12px 12px 0 0 !important;
      }

      .bslib-value-box {
        border: 1px solid #d4e3f0 !important;
        box-shadow: 0 4px 20px rgba(107,155,209,0.08) !important;
        border-radius: 12px !important;
        background: #ffffff !important;
      }

      .sidebar {
        background: #ffffff !important;
        border: 1px solid #d4e3f0 !important;
        box-shadow: 0 4px 20px rgba(107,155,209,0.08) !important;
        border-radius: 12px !important;
      }

      .navbar {
        box-shadow: 0 2px 8px rgba(107,155,209,0.1) !important;
        background-color: #ffffff !important;
        border-bottom: 1px solid #d4e3f0 !important;
      }

      .nav-link.active {
        color: #6b9bd1 !important;
        border-bottom: 2px solid #6b9bd1 !important;
      }

      .btn-primary {
        background-color: #6b9bd1 !important;
        border-color: #6b9bd1 !important;
        border-radius: 8px !important;
      }

      .card, .btn, .bslib-value-box {
        transition: all 0.3s ease !important;
      }
    "
    ))
  ),

  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Dataset Overview"),
        tableOutput("overview_table"),
        hr(),
        h4("Map Controls"),
        selectInput(
          "map_color_by",
          "Color sites by:",
          choices = c(
            "Climate Zone" = "ClimateZ",
            "Snow Fraction" = "snow_fraction",
            "RBI (Flashiness)" = "RBI",
            "RCS (Recession Curve Slope)" = "recession_slope",
            "Major Land Use" = "major_land",
            "LTER Network" = "LTER"
          ),
          selected = "ClimateZ"
        ),
        checkboxGroupInput(
          "map_lter",
          "Filter by LTER:",
          choices = NULL,
          selected = NULL
        ),
        checkboxInput(
          "map_show_complete",
          "Show only complete data sites",
          value = TRUE
        )
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Study Sites Across North America"),
          leafletOutput("site_map", height = 600)
        ),
        card(
          card_header("About This Module"),
          p(
            "This interactive module explores river hydrology metrics and their relationship
            to watershed characteristics across North American rivers."
          ),
          br(),
          h4("Key Metrics:"),
          tags$ul(
            tags$li(
              strong("RBI (Richards-Baker Flashiness Index):"),
              "Measures how rapidly streamflow changes over time"
            ),
            tags$li(
              strong("Recession Curve Slope (RCS):"),
              "Describes how quickly discharge decreases after peak flow"
            ),
            tags$li(
              strong("Climate & Land Use:"),
              "Koppen-Geiger classification, precipitation, snow fraction, land cover"
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 1: Snow Influence on Flashiness",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Compare Snow Influence"),
        p(
          "Explore how snow affects river flashiness (RBI) and recession behavior (RCS)",
          style = "font-size: 0.9em; color: #666;"
        ),
        hr(),

        # scatter plot controls — only visible when that tab is active
        conditionalPanel(
          condition = "input.activity1_tab == 'RCS vs RBI by Snow'",
          h4("Filter Scatter Plot"),
          p(
            "Show/hide snow categories on the plot:",
            style = "font-size: 0.85em; color: #666;"
          ),
          checkboxGroupInput(
            "show_snow_categories",
            "Display:",
            choices = c(
              "Low (0-40 days)" = "Low (0-40 days)",
              "Medium (40-80 days)" = "Medium (40-80 days)",
              "High (80+ days)" = "High (80+ days)"
            ),
            selected = c(
              "Low (0-40 days)",
              "Medium (40-80 days)",
              "High (80+ days)"
            )
          ),
          hr(),
          h4("Highlight Specific Sites"),
          p(
            "Select sites to highlight on the plot:",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "highlight_sites",
            "Select sites:",
            choices = NULL,
            multiple = TRUE
          )
        ),

        # hydrograph controls
        conditionalPanel(
          condition = "input.activity1_tab == 'Hydrographs'",
          h4("Select Sites to Compare"),
          p(
            "Choose sites from different snow categories to compare their discharge patterns.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "low_snow_sites",
            "Low snow sites (0-40 days):",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "medium_snow_sites",
            "Medium snow sites (40-80 days):",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "high_snow_sites",
            "High snow sites (80+ days):",
            choices = NULL,
            multiple = TRUE
          ),
          actionButton(
            "plot_hydrographs",
            "Plot Hydrographs",
            class = "btn-primary mt-2 w-100"
          ),
          p(
            "Select at least 2 sites total, then click the button above.",
            style = "font-size: 0.85em; color: #666; margin-top: 8px;"
          )
        )
      ),

      navset_card_tab(
        id = "activity1_tab",
        nav_panel(
          "RCS vs RBI by Snow",
          layout_columns(
            col_widths = c(12, 12),
            card(
              full_screen = TRUE,
              card_header(
                "How does snow influence flashiness and recession patterns?"
              ),
              plotlyOutput("rcs_rbi_plot", height = 600)
            ),
            card(
              full_screen = TRUE,
              card_header("Selected Sites Detail"),
              plotlyOutput("rcs_rbi_highlight_plot", height = 500)
            )
          )
        ),
        nav_panel(
          "Hydrographs",
          card(
            full_screen = TRUE,
            card_header("Compare Discharge Patterns"),
            plotlyOutput("hydrograph_plot", height = 600)
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 2: Stream Salinity",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Stream Chloride Patterns"),
        p(
          "Explore spatial patterns of stream chloride across North American sites
          and how they relate to land use and precipitation.",
          style = "font-size: 0.9em; color: #666;"
        ),
        hr(),

        # chloride map controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Chloride Map'",
          h4("Map Controls"),
          selectInput(
            "cl_map_color",
            "Color markers by:",
            choices = c(
              "Mean Chloride (uM)" = "mean_Cl_uM",
              "% Cropland" = "land_cropland",
              "% Urban" = "land_urban_and_built_up_land",
              "Mean Annual Precip (mm)" = "mean_annual_precip"
            ),
            selected = "mean_Cl_uM"
          ),
          p(
            "Zoom in to see individual site markers. The heatmap shows
            Cl concentration hotspots at continental scale.",
            style = "font-size: 0.85em; color: #666;"
          )
        ),

        # seasonal plot controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Seasonal Cl & Discharge'",
          h4("Site Selection"),
          selectInput(
            "cl_site_select",
            "Choose a site:",
            choices = NULL
          ),
          checkboxInput(
            "cl_show_discharge",
            "Overlay monthly discharge",
            value = FALSE
          ),
          p(
            "Toggle discharge to see how Cl concentration relates to
            streamflow seasonality.",
            style = "font-size: 0.85em; color: #666;"
          )
        )
      ),

      navset_card_tab(
        id = "activity2_tab",
        nav_panel(
          "Chloride Map",
          card(
            full_screen = TRUE,
            card_header("Stream Chloride Across North America"),
            leafletOutput("cl_map", height = 600)
          )
        ),
        nav_panel(
          "Seasonal Cl & Discharge",
          card(
            full_screen = TRUE,
            card_header("Monthly Chloride & Discharge Patterns"),
            plotlyOutput("cl_seasonal_plot", height = 600)
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 3: C-Q Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Concentration-Discharge Analysis"),
        p(
          "The C-Q framework follows the power-law model from",
          tags$a("Godsey et al. (2009)",
                 href = "https://doi.org/10.1002/hyp.7315",
                 target = "_blank"),
          "where concentration scales with discharge as:",
          style = "font-size: 0.9em; color: #666;"
        ),
        p(
          tags$em("C = a Q"),
          tags$sup(tags$em("b")),
          style = "font-size: 0.95em; text-align: center; margin: 4px 0;"
        ),
        p(
          "The exponent ", tags$em("b"), " is the C-Q slope, estimated here
          via linear regression in log-log space:
          log(C) = log(a) + b \u00b7 log(Q).",
          style = "font-size: 0.9em; color: #666;"
        ),
        hr(),

        # C-Q scatter controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Relationships'",
          h4("Select Sites & Solutes"),
          selectInput(
            "cq_sites",
            "Sites (max 3):",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "cq_solutes",
            "Solutes (max 2):",
            choices = NULL,
            multiple = TRUE
          ),
          checkboxInput(
            "cq_show_trendline",
            "Show trendlines",
            value = TRUE
          ),
          hr(),
          p(
            tags$b("Interpreting C-Q slopes:"), br(),
            "Positive slope = enrichment (concentration rises with flow)", br(),
            "Slope near 0 = chemostatic (concentration stable)", br(),
            "Negative slope = dilution (concentration falls with flow)",
            style = "font-size: 0.85em; color: #666;"
          )
        ),

        # histogram controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Slope Distribution'",
          h4("Slope Distribution"),
          selectInput(
            "cq_hist_solute",
            "Select solute:",
            choices = NULL
          ),
          hr(),
          p(
            tags$b("Reading the histogram:"), br(),
            "Each bar represents a group of sites. Sites with slopes below
            -0.1 show dilution (concentration decreases with flow). Sites
            above +0.1 show enrichment. Sites between -0.1 and +0.1 are
            approximately chemostatic.",
            style = "font-size: 0.85em; color: #666;"
          )
        )
      ),

      navset_card_tab(
        id = "activity3_tab",
        nav_panel(
          "C-Q Relationships",
          card(
            full_screen = TRUE,
            card_header("log₁₀(Concentration) vs log₁₀(Discharge)"),
            plotlyOutput("cq_scatter_plot", height = 600)
          )
        ),
        nav_panel(
          "C-Q Slope Distribution",
          card(
            full_screen = TRUE,
            card_header("Distribution of C-Q Slopes Across All Sites"),
            plotlyOutput("cq_histogram", height = 600)
          )
        )
      )
    )
  )
)


# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  harmonized_complete <- reactive({
    read.csv(
      file.path(data_path, "harmonized_north_america_complete.csv"),
      stringsAsFactors = FALSE
    )
  })

  harmonized_partial <- reactive({
    read.csv(
      file.path(data_path, "harmonized_north_america_partial.csv"),
      stringsAsFactors = FALSE
    )
  })

  discharge_data <- reactive({ discharge_global })

  # bin sites by annual snow days for the scatter plots
  harmonized_with_categories <- reactive({
    harmonized_complete() %>%
      filter(!is.na(RBI), !is.na(recession_slope), !is.na(mean_snow_days)) %>%
      mutate(
        snow_cat = case_when(
          mean_snow_days < 40 ~ "Low (0-40 days)",
          mean_snow_days < 80 ~ "Medium (40-80 days)",
          TRUE ~ "High (80+ days)"
        ),
        snow_cat = factor(
          snow_cat,
          levels = c(
            "Low (0-40 days)",
            "Medium (40-80 days)",
            "High (80+ days)"
          )
        )
      )
  })

  output$overview_table <- renderTable({
    data.frame(
      Metric = c(
        "Total North American Sites",
        "Sites with Discharge Data",
        "Sites with Complete Data",
        "LTER Networks Represented"
      ),
      Count = c(
        nrow(harmonized_partial()),
        sum(!is.na(harmonized_partial()$RBI)),
        nrow(harmonized_complete()),
        length(unique(harmonized_complete()$LTER))
      )
    )
  })

  # populate the LTER filter checkboxes once data loads
  observe({
    lter_choices <- sort(unique(harmonized_complete()$LTER))
    updateCheckboxGroupInput(
      session,
      "map_lter",
      choices = lter_choices,
      selected = lter_choices
    )
  })

  # populate the per-category site dropdowns in Activity 1
  observe({
    site_data <- harmonized_with_categories()

    # helper to build named choice vectors for selectInput
    make_choices <- function(df) {
      df <- df %>% arrange(Stream_Name)
      setNames(
        df$Stream_ID,
        paste0(
          df$Stream_Name,
          " [",
          df$LTER,
          ", ",
          round(df$mean_snow_days, 0),
          " days]"
        )
      )
    }

    low_choices <- make_choices(filter(
      site_data,
      snow_cat == "Low (0-40 days)"
    ))
    medium_choices <- make_choices(filter(
      site_data,
      snow_cat == "Medium (40-80 days)"
    ))
    high_choices <- make_choices(filter(
      site_data,
      snow_cat == "High (80+ days)"
    ))
    all_choices <- make_choices(arrange(site_data, snow_cat, Stream_Name))

    updateSelectInput(session, "low_snow_sites", choices = low_choices)
    updateSelectInput(session, "medium_snow_sites", choices = medium_choices)
    updateSelectInput(session, "high_snow_sites", choices = high_choices)
    updateSelectInput(session, "highlight_sites", choices = all_choices)
  })

  # --- Map -----------------------------------------------------------------

  output$site_map <- renderLeaflet({
    req(input$map_lter, input$map_color_by)

    map_data <- harmonized_complete() %>%
      filter(LTER %in% input$map_lter, !is.na(Latitude), !is.na(Longitude))

    # 20 high-contrast colors for categorical variables with many levels
    distinct_colors <- c(
      "#e41a1c",
      "#377eb8",
      "#4daf4a",
      "#984ea3",
      "#ff7f00",
      "#ffff33",
      "#a65628",
      "#f781bf",
      "#66c2a5",
      "#fc8d62",
      "#8da0cb",
      "#e78ac3",
      "#a6d854",
      "#ffd92f",
      "#e5c494",
      "#b3b3b3",
      "#1b9e77",
      "#d95f02",
      "#7570b3",
      "#e7298a"
    )

    color_var <- map_data[[input$map_color_by]]
    legend_title <- gsub("_", " ", input$map_color_by)

    # numeric columns get a gradient, categorical get distinct colors
    if (input$map_color_by %in% c("RBI", "recession_slope", "snow_fraction")) {
      pal <- colorNumeric(
        palette = c("#d67e7e", "#e6c79c", "#7fb069", "#6b9bd1", "#5a7fa8"),
        domain = color_var,
        na.color = "#cccccc"
      )
    } else {
      pal <- colorFactor(
        palette = distinct_colors,
        domain = color_var,
        na.color = "#cccccc"
      )
    }

    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        fillColor = ~ pal(color_var),
        color = "#2d2926",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.7,
        popup = ~ paste0(
          "<b>",
          Stream_Name,
          "</b><br>",
          "LTER: ",
          LTER,
          "<br>",
          "RBI: ",
          round(RBI, 3),
          "<br>",
          "RCS: ",
          round(recession_slope, 3),
          "<br>",
          "Climate: ",
          ClimateZ,
          "<br>",
          "Snow Fraction: ",
          round(snow_fraction, 3),
          "<br>",
          "Mean Annual Precip: ",
          round(mean_annual_precip, 1),
          " mm"
        ),
        label = ~Stream_Name
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = color_var,
        title = legend_title,
        opacity = 0.7
      )
  })

  # --- Hydrograph ----------------------------------------------------------

  hydrograph_data <- eventReactive(input$plot_hydrographs, {
    all_selected <- c(
      input$low_snow_sites,
      input$medium_snow_sites,
      input$high_snow_sites
    )

    if (length(all_selected) < 2) return(NULL)

    selected_sites <- harmonized_with_categories() %>%
      filter(Stream_ID %in% all_selected) %>%
      select(Stream_ID, Stream_Name, LTER, snow_cat, mean_snow_days)

    discharge_data() %>%
      filter(Stream_ID %in% all_selected) %>%
      left_join(selected_sites, by = c("Stream_ID", "Stream_Name", "LTER")) %>%
      mutate(
        site_label = paste0(
          Stream_Name,
          " (",
          gsub(" \\(.*", "", snow_cat),
          " snow)"
        )
      )
  })

  output$hydrograph_plot <- renderPlotly({
    plot_data <- hydrograph_data()

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select at least 2 sites and click 'Plot Hydrographs'",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    p <- ggplot(
      plot_data,
      aes(x = Date, y = Qcms, color = site_label, group = site_label)
    ) +
      geom_line(linewidth = 0.7, alpha = 0.8) +
      labs(x = "Date", y = "Discharge (cms)", color = "Site") +
      base_plot_theme +
      theme(legend.position = "bottom")

    ggplotly(p) %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # --- RCS vs RBI scatter (all sites, filterable by snow category) ---------

  output$rcs_rbi_plot <- renderPlotly({
    req(input$show_snow_categories)

    plot_data <- harmonized_with_categories() %>%
      filter(snow_cat %in% input$show_snow_categories) %>%
      mutate(is_highlighted = Stream_ID %in% input$highlight_sites)

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select at least one snow category to display",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    p <- ggplot(
      plot_data,
      aes(
        x = RBI,
        y = recession_slope,
        color = snow_cat,
        alpha = is_highlighted,
        size = is_highlighted,
        text = paste0(
          "<b>",
          Stream_Name,
          "</b><br>",
          "LTER: ",
          LTER,
          "<br>",
          "Snow Category: ",
          snow_cat,
          "<br>",
          "Snow Days/Year: ",
          round(mean_snow_days, 0),
          "<br>",
          "RBI: ",
          round(RBI, 3),
          "<br>",
          "RCS: ",
          round(recession_slope, 3)
        )
      )
    ) +
      geom_point() +
      labs(
        x = "Richards-Baker Flashiness Index (RBI)\n(higher = more flashy)",
        y = "Recession Curve Slope (RCS)\n(higher = faster drainage)",
        color = "Snow Category",
        title = "Do low snow sites have different flashiness than high snow sites?"
      ) +
      base_plot_theme +
      theme(
        plot.title = element_text(size = 11, color = "#666", face = "italic")
      ) +
      scale_color_manual(values = snow_colors) +
      scale_alpha_manual(
        values = c("FALSE" = 0.4, "TRUE" = 1),
        guide = "none"
      ) +
      scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none")

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        title = list(
          text = "Do low snow sites have different flashiness than high snow sites?",
          font = list(size = 12, color = "#666")
        )
      )
  })

  # --- RCS vs RBI scatter (just the highlighted sites, with labels) --------

  output$rcs_rbi_highlight_plot <- renderPlotly({
    if (length(input$highlight_sites) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select sites from the dropdown to see detailed comparison",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    plot_data <- harmonized_with_categories() %>%
      filter(Stream_ID %in% input$highlight_sites)

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No data for selected sites",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    p <- ggplot(
      plot_data,
      aes(
        x = RBI,
        y = recession_slope,
        color = snow_cat,
        text = paste0(
          "<b>",
          Stream_Name,
          "</b><br>",
          "LTER: ",
          LTER,
          "<br>",
          "Snow Category: ",
          snow_cat,
          "<br>",
          "Snow Days/Year: ",
          round(mean_snow_days, 0),
          "<br>",
          "RBI: ",
          round(RBI, 3),
          "<br>",
          "RCS: ",
          round(recession_slope, 3)
        )
      )
    ) +
      geom_point(size = 5, alpha = 0.8) +
      geom_text(
        aes(label = Stream_Name),
        hjust = -0.1,
        vjust = 0,
        size = 3,
        show.legend = FALSE
      ) +
      labs(
        x = "Richards-Baker Flashiness Index (RBI)\n(higher = more flashy)",
        y = "Recession Curve Slope (RCS)\n(higher = faster drainage)",
        color = "Snow Category"
      ) +
      base_plot_theme +
      scale_color_manual(values = snow_colors)

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor
      )
  })

  # --- Activity 2: Stream Salinity ------------------------------------------

  # sites from harmonized partial that have Cl data
  cl_sites <- reactive({
    harmonized_partial() %>%
      filter(!is.na(mean_Cl_uM), !is.na(Latitude), !is.na(Longitude))
  })

  cl_monthly <- reactive({
    read.csv(
      file.path(data_path, "cl_monthly_summary.csv"),
      stringsAsFactors = FALSE
    )
  })

  # monthly mean discharge per site (reuses the already-loaded discharge data)
  discharge_monthly <- reactive({
    discharge_data() %>%
      mutate(month = as.integer(format(Date, "%m"))) %>%
      group_by(Stream_ID, month) %>%
      summarise(mean_Q_cms = mean(Qcms, na.rm = TRUE), .groups = "drop")
  })

  # populate the site dropdown for the seasonal plot
  observe({
    sites <- cl_sites() %>%
      arrange(LTER, Stream_Name)
    choices <- setNames(
      sites$Stream_ID,
      paste0(sites$Stream_Name, " [", sites$LTER, "]")
    )
    updateSelectInput(session, "cl_site_select", choices = choices)
  })

  # --- Chloride Map ---------------------------------------------------------

  output$cl_map <- renderLeaflet({
    map_data <- cl_sites()
    req(nrow(map_data) > 0)

    color_var_name <- input$cl_map_color
    color_vals <- map_data[[color_var_name]]

    # label for legend
    legend_labels <- c(
      "mean_Cl_uM" = "Mean Cl (uM)",
      "land_cropland" = "% Cropland",
      "land_urban_and_built_up_land" = "% Urban",
      "mean_annual_precip" = "Precip (mm/yr)"
    )
    legend_title <- legend_labels[[color_var_name]]

    # continuous palette
    pal <- colorNumeric(
      palette = viridis(256),
      domain = color_vals,
      na.color = "#cccccc"
    )

    leaflet(map_data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~Longitude,
        lat = ~Latitude,
        intensity = ~mean_Cl_uM,
        blur = 20,
        max = quantile(map_data$mean_Cl_uM, 0.95, na.rm = TRUE),
        radius = 15,
        group = "Heatmap"
      ) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        fillColor = ~ pal(color_vals),
        color = "#2d2926",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.7,
        group = "Sites",
        popup = ~ paste0(
          "<b>", Stream_Name, "</b><br>",
          "LTER: ", LTER, "<br>",
          "Mean Cl: ", round(mean_Cl_uM, 1), " uM<br>",
          "% Cropland: ", round(land_cropland, 1), "<br>",
          "% Urban: ", round(land_urban_and_built_up_land, 1), "<br>",
          "Precip: ", round(mean_annual_precip, 0), " mm/yr"
        ),
        label = ~Stream_Name
      ) %>%
      addLayersControl(
        overlayGroups = c("Heatmap", "Sites"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = color_vals,
        title = legend_title,
        opacity = 0.7
      )
  })

  # --- Seasonal Cl & Discharge plot -----------------------------------------

  output$cl_seasonal_plot <- renderPlotly({
    req(input$cl_site_select)

    site_id <- input$cl_site_select

    # monthly Cl for this site
    cl_data <- cl_monthly() %>%
      filter(Stream_ID == site_id) %>%
      arrange(month)

    if (nrow(cl_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No chloride data for this site",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    site_name <- cl_data$Stream_Name[1]
    site_lter <- cl_data$LTER[1]
    month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

    p <- plot_ly() %>%
      add_trace(
        data = cl_data,
        x = ~month,
        y = ~mean_Cl_uM,
        type = "scatter",
        mode = "lines+markers",
        name = "Mean Cl (uM)",
        line = list(color = "#6b9bd1", width = 3),
        marker = list(color = "#6b9bd1", size = 8),
        hovertemplate = paste0(
          "Month: %{x}<br>",
          "Mean Cl: %{y:.1f} uM<br>",
          "<extra></extra>"
        )
      )

    # dual y-axis with discharge if toggled on
    if (isTRUE(input$cl_show_discharge)) {
      q_data <- discharge_monthly() %>%
        filter(Stream_ID == site_id) %>%
        arrange(month)

      if (nrow(q_data) > 0) {
        p <- p %>%
          add_trace(
            data = q_data,
            x = ~month,
            y = ~mean_Q_cms,
            type = "scatter",
            mode = "lines+markers",
            name = "Mean Q (cms)",
            yaxis = "y2",
            line = list(color = "#7fb069", width = 2, dash = "dash"),
            marker = list(color = "#7fb069", size = 6),
            hovertemplate = paste0(
              "Month: %{x}<br>",
              "Mean Q: %{y:.3f} cms<br>",
              "<extra></extra>"
            )
          )
      }
    }

    y2_config <- if (isTRUE(input$cl_show_discharge)) {
      list(
        title = list(text = "Mean Discharge (cms)", font = list(color = "#7fb069")),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        tickfont = list(color = "#7fb069")
      )
    } else {
      list(overlaying = "y", side = "right", visible = FALSE)
    }

    p %>%
      layout(
        title = list(
          text = paste0(site_name, " (", site_lter, ")"),
          font = list(size = 14, color = "#2d2926")
        ),
        xaxis = list(
          title = "Month",
          tickmode = "array",
          tickvals = 1:12,
          ticktext = month_labels,
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = list(text = "Mean Chloride (uM)", font = list(color = "#6b9bd1")),
          gridcolor = "#d4e3f0",
          tickfont = list(color = "#6b9bd1")
        ),
        yaxis2 = y2_config,
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        margin = list(r = if (isTRUE(input$cl_show_discharge)) 80 else 20),
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )
  })

  # --- Activity 3: C-Q Analysis -----------------------------------------------

  cq_paired_data <- reactive({
    read.csv(
      file.path(data_path, "cq_paired_obs.csv"),
      stringsAsFactors = FALSE
    ) %>%
      mutate(date = as.Date(date))
  })

  cq_slopes_data <- reactive({
    read.csv(
      file.path(data_path, "cq_slopes.csv"),
      stringsAsFactors = FALSE
    )
  })

  # friendly labels for the 10 target solutes
  cq_solute_choices <- c(
    "Chloride (Cl)" = "Cl",
    "Nitrate (NO3)" = "NO3",
    "Sulfate (SO4)" = "SO4",
    "Calcium (Ca)" = "Ca",
    "Magnesium (Mg)" = "Mg",
    "Sodium (Na)" = "Na",
    "Potassium (K)" = "K",
    "Dissolved Silica (DSi)" = "DSi",
    "Phosphate (PO4)" = "PO4",
    "Dissolved Org C (DOC)" = "DOC"
  )

  # populate site dropdown — only sites that have C-Q slopes
  observe({
    slopes <- cq_slopes_data()
    sites <- slopes %>%
      select(Stream_ID, LTER, Stream_Name) %>%
      distinct() %>%
      arrange(LTER, Stream_Name)
    choices <- setNames(
      sites$Stream_ID,
      paste0(sites$Stream_Name, " [", sites$LTER, "]")
    )
    updateSelectInput(session, "cq_sites", choices = choices)
  })

  # populate solute dropdowns with only solutes present in the data
  observe({
    available <- unique(cq_slopes_data()$variable)
    scatter_choices <- cq_solute_choices[cq_solute_choices %in% available]
    hist_choices <- scatter_choices
    updateSelectInput(session, "cq_solutes", choices = scatter_choices)
    updateSelectInput(session, "cq_hist_solute", choices = hist_choices,
                      selected = hist_choices[1])
  })

  # enforce max 3 sites

  observe({
    if (length(input$cq_sites) > 3) {
      updateSelectInput(session, "cq_sites", selected = input$cq_sites[1:3])
    }
  })

  # enforce max 2 solutes
  observe({
    if (length(input$cq_solutes) > 2) {
      updateSelectInput(session, "cq_solutes", selected = input$cq_solutes[1:2])
    }
  })

  # light/dark pairs per site — solute 1 gets light, solute 2 gets dark
  cq_site_palettes <- list(
    c("#a3c4e9", "#3b6fa0"),
    c("#a8d89a", "#3d7a2e"),
    c("#e8a5a5", "#b03a3a")
  )

  # --- C-Q Scatter Plot -------------------------------------------------------

  output$cq_scatter_plot <- renderPlotly({
    req(input$cq_sites, input$cq_solutes)

    paired <- cq_paired_data() %>%
      filter(Stream_ID %in% input$cq_sites,
             variable %in% input$cq_solutes)

    if (nrow(paired) == 0) {
      return(
        plotly_empty() %>%
          layout(title = list(
            text = "No paired C-Q data for selected sites/solutes",
            font = list(color = "#666", size = 14)
          ))
      )
    }

    # build one trace per site×solute combo
    combos <- paired %>%
      select(Stream_ID, Stream_Name, LTER, variable) %>%
      distinct()

    # map each site to a palette, each solute to light (1) or dark (2)
    unique_sites <- unique(combos$Stream_ID)
    unique_solutes <- unique(combos$variable)

    p <- plot_ly()
    cq_annotations <- list()

    for (i in seq_len(nrow(combos))) {
      row <- combos[i, ]
      d <- paired %>%
        filter(Stream_ID == row$Stream_ID, variable == row$variable)

      solute_label <- names(cq_solute_choices)[cq_solute_choices == row$variable]
      trace_name <- paste0(row$Stream_Name, " — ", solute_label)
      site_idx <- match(row$Stream_ID, unique_sites)
      solute_idx <- match(row$variable, unique_solutes)
      clr <- cq_site_palettes[[site_idx]][solute_idx]

      p <- p %>%
        add_trace(
          data = d,
          x = ~log10(Q),
          y = ~log10(value),
          type = "scatter",
          mode = "markers",
          name = trace_name,
          marker = list(color = clr, size = 5, opacity = 0.6),
          hovertemplate = paste0(
            row$Stream_Name, "<br>",
            solute_label, "<br>",
            "Q: %{customdata:.4f} cms<br>",
            "C: %{meta:.2f}<br>",
            "<extra></extra>"
          ),
          customdata = d$Q,
          meta = d$value
        )

      # optional trendline + annotation
      if (isTRUE(input$cq_show_trendline) && nrow(d) >= 10) {
        mod <- lm(log10(value) ~ log10(Q), data = d)
        slope <- round(coef(mod)[2], 3)
        r2 <- round(summary(mod)$r.squared, 3)

        x_range <- range(log10(d$Q))
        x_seq <- seq(x_range[1], x_range[2], length.out = 50)
        y_seq <- coef(mod)[1] + coef(mod)[2] * x_seq

        p <- p %>%
          add_trace(
            x = x_seq,
            y = y_seq,
            type = "scatter",
            mode = "lines",
            name = paste0(trace_name, " fit"),
            line = list(color = clr, width = 2, dash = "dash"),
            hoverinfo = "skip",
            showlegend = FALSE
          )

        # place b and R² at the right end of the trendline, stacked vertically
        cq_annotations <- c(cq_annotations, list(
          list(
            x = x_range[2], y = tail(y_seq, 1),
            text = paste0("b = ", slope, "<br>R\u00b2 = ", r2),
            showarrow = FALSE,
            xanchor = "left", yanchor = "middle",
            font = list(color = clr, size = 11),
            xshift = 6
          )
        ))
      }
    }

    p %>%
      layout(
        xaxis = list(title = "log\u2081\u2080(Discharge, cms)", gridcolor = "#d4e3f0"),
        yaxis = list(title = "log\u2081\u2080(Concentration)", gridcolor = "#d4e3f0"),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = list(orientation = "h", y = -0.15),
        margin = list(r = 70),
        annotations = cq_annotations
      )
  })

  # --- C-Q Slope Histogram ----------------------------------------------------

  output$cq_histogram <- renderPlotly({
    req(input$cq_hist_solute)

    slopes <- cq_slopes_data() %>%
      filter(variable == input$cq_hist_solute)

    if (nrow(slopes) == 0) {
      return(
        plotly_empty() %>%
          layout(title = list(
            text = "No C-Q slopes available for this solute",
            font = list(color = "#666", size = 14)
          ))
      )
    }

    solute_label <- names(cq_solute_choices)[cq_solute_choices == input$cq_hist_solute]
    med_slope <- median(slopes$cq_slope, na.rm = TRUE)

    # y-range for annotation placement
    hist_obj <- hist(slopes$cq_slope, plot = FALSE)
    y_max <- max(hist_obj$counts) * 1.1

    plot_ly(
      x = slopes$cq_slope,
      type = "histogram",
      marker = list(color = "#6b9bd1", line = list(color = "#5a7fa8", width = 1)),
      name = solute_label,
      hovertemplate = "Slope: %{x:.2f}<br>Count: %{y}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = paste0("C-Q Slope Distribution — ", solute_label),
          font = list(size = 14, color = "#2d2926")
        ),
        xaxis = list(title = "C-Q Slope", gridcolor = "#d4e3f0"),
        yaxis = list(title = "Number of Sites", gridcolor = "#d4e3f0"),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        shapes = list(
          list(
            type = "line",
            x0 = -0.1, x1 = -0.1,
            y0 = 0, y1 = y_max,
            line = list(color = "#2d2926", width = 1.5, dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0.1, x1 = 0.1,
            y0 = 0, y1 = y_max,
            line = list(color = "#2d2926", width = 1.5, dash = "dash")
          )
        ),
        annotations = list(
          list(
            x = -0.1,
            y = y_max * 0.95,
            text = "\u2190 Dilution",
            showarrow = FALSE,
            xanchor = "right",
            font = list(size = 12, color = "#666"),
            xshift = -6
          ),
          list(
            x = 0,
            y = y_max * 0.95,
            text = "Chemostatic",
            showarrow = FALSE,
            xanchor = "center",
            font = list(size = 11, color = "#999")
          ),
          list(
            x = 0.1,
            y = y_max * 0.95,
            text = "Enrichment \u2192",
            showarrow = FALSE,
            xanchor = "left",
            font = list(size = 12, color = "#666"),
            xshift = 6
          )
        )
      )
  })
}

shinyApp(ui = ui, server = server)
