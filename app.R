# Teaching Modules Shiny App

suppressPackageStartupMessages({
  if (!require("librarian")) {
    options(repos = c(CRAN = "https://cloud.r-project.org"))
    install.packages("librarian")
  }
  librarian::shelf(shiny, bslib, dplyr, ggplot2, leaflet, plotly, viridis)
})

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

# Define color palette - earth-toned scheme
module_colors <- c(
  "primary"   = "#6b9bd1",  # soft blue
  "secondary" = "#5a7fa8",  # deeper blue
  "success"   = "#7fb069",  # sage green
  "danger"    = "#d67e7e",  # soft red/coral
  "warning"   = "#e6c79c"   # warm tan
)

# UI
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
    tags$style(HTML("
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
    "))
  ),

  # Overview Tab with Map
  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Dataset Overview"),
        tableOutput("overview_table"),
        hr(),
        h4("Map Controls"),
        selectInput("map_color_by", "Color sites by:",
                   choices = c("Climate Zone" = "ClimateZ",
                             "Snow Fraction" = "snow_fraction",
                             "RBI (Flashiness)" = "RBI",
                             "Major Land Use" = "major_land",
                             "LTER Network" = "LTER"),
                   selected = "ClimateZ"),
        checkboxGroupInput("map_lter", "Filter by LTER:",
                         choices = NULL,
                         selected = NULL),
        checkboxInput("map_show_complete", "Show only complete data sites",
                     value = TRUE)
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Study Sites Across North America"),
          leafletOutput("site_map", height = 600)
        ),
        card(
          card_header("About This Module"),
          p("This interactive module explores river hydrology metrics and their relationship
            to watershed characteristics across North American rivers."),
          br(),
          h4("Key Metrics:"),
          tags$ul(
            tags$li(strong("RBI (Richards-Baker Flashiness Index):"),
                   "Measures how rapidly streamflow changes over time"),
            tags$li(strong("Recession Curve Slope (RCS):"),
                   "Describes how quickly discharge decreases after peak flow"),
            tags$li(strong("Climate & Land Use:"),
                   "Köppen-Geiger classification, precipitation, snow fraction, land cover")
          )
        )
      )
    )
  ),

  # Activity 1 Tab
  nav_panel(
    "Activity 1: Snow Influence on Flashiness",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Compare Snow Influence"),
        p("Explore how snow affects river flashiness (RBI) and recession behavior (RCS)",
          style = "font-size: 0.9em; color: #666;"),
        hr(),

        # Controls for RCS vs RBI tab
        conditionalPanel(
          condition = "input.activity1_tab == 'RCS vs RBI by Snow'",
          h4("Filter Scatter Plot"),
          p("Show/hide snow categories on the plot:",
            style = "font-size: 0.85em; color: #666;"),
          checkboxGroupInput("show_snow_categories", "Display:",
                            choices = c("Low (0-40 days)" = "Low (0-40 days)",
                                      "Medium (40-80 days)" = "Medium (40-80 days)",
                                      "High (80+ days)" = "High (80+ days)"),
                            selected = c("Low (0-40 days)", "Medium (40-80 days)", "High (80+ days)")),
          hr(),
          h4("Highlight Specific Sites"),
          p("Select sites to highlight on the plot:",
            style = "font-size: 0.85em; color: #666;"),
          selectInput("highlight_sites", "Select sites:",
                     choices = NULL,
                     multiple = TRUE)
        ),

        # Controls for Hydrograph tab
        conditionalPanel(
          condition = "input.activity1_tab == 'Hydrographs'",
          h4("Select Sites to Compare"),
          p("Choose sites from different snow categories to compare their discharge patterns.",
            style = "font-size: 0.85em; color: #666;"),
          selectInput("low_snow_sites", "Low snow sites (0-40 days):",
                     choices = NULL,
                     multiple = TRUE),
          selectInput("medium_snow_sites", "Medium snow sites (40-80 days):",
                     choices = NULL,
                     multiple = TRUE),
          selectInput("high_snow_sites", "High snow sites (80+ days):",
                     choices = NULL,
                     multiple = TRUE),
          p("Select at least 2 sites total to compare",
            style = "font-size: 0.85em; color: #666; margin-top: 8px;")
        )
      ),

      navset_card_tab(
        id = "activity1_tab",
        nav_panel("RCS vs RBI by Snow",
          layout_columns(
            col_widths = c(12, 12),
            card(
              full_screen = TRUE,
              card_header("How does snow influence flashiness and recession patterns?"),
              plotlyOutput("rcs_rbi_plot", height = 600)
            ),
            card(
              full_screen = TRUE,
              card_header("Selected Sites Detail"),
              plotlyOutput("rcs_rbi_highlight_plot", height = 500)
            )
          )
        ),
        nav_panel("Hydrographs",
          card(
            full_screen = TRUE,
            card_header("Compare Discharge Patterns"),
            plotlyOutput("hydrograph_plot", height = 600)
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Load harmonized data
  harmonized_complete <- reactive({
    read.csv(file.path(data_path, "harmonized_north_america_complete.csv"),
             stringsAsFactors = FALSE)
  })

  harmonized_partial <- reactive({
    read.csv(file.path(data_path, "harmonized_north_america_partial.csv"),
             stringsAsFactors = FALSE)
  })

  discharge_data <- reactive({
    # Get list of North American LTER sites from harmonized data
    na_lter <- c("Canada", "USGS", "AND", "ARC", "BcCZO", "BNZ", "ColoradoAlpine",
                 "CZO-Catalina Jemez", "Catalina Jemez", "EastRiverSFA", "GRO", "HBR",
                 "Ipswitch(Carey)", "KNZ", "LMP", "LMP(Wymore)", "LUQ", "NWT", "PIE",
                 "Sagehen", "Sagehen(Sullivan)", "UMR", "UMR(Jankowski)",
                 "WalkerBranch", "Walker Branch")

    read.csv(file.path(data_path, "20260106_masterdata_discharge.csv"),
             stringsAsFactors = FALSE) %>%
      filter(LTER %in% na_lter) %>%
      mutate(
        Date = as.Date(Date),
        Stream_ID = paste(LTER, Stream_Name, sep = "_"),
        Stream_ID = stringr::str_trim(Stream_ID),
        Stream_ID = stringr::str_replace_all(Stream_ID, "\\s+", "_")
      )
  })

  # Overview table
  output$overview_table <- renderTable({
    data.frame(
      Metric = c("Total North American Sites",
                 "Sites with Discharge Data",
                 "Sites with Complete Data",
                 "LTER Networks Represented"),
      Count = c(nrow(harmonized_partial()),
               sum(!is.na(harmonized_partial()$RBI)),
               nrow(harmonized_complete()),
               length(unique(harmonized_complete()$LTER)))
    )
  })

  # Update LTER choices for maps
  observe({
    lter_choices <- sort(unique(harmonized_complete()$LTER))
    updateCheckboxGroupInput(session, "map_lter",
                            choices = lter_choices,
                            selected = lter_choices)
  })

  # Add snow category to harmonized data
  harmonized_with_categories <- reactive({
    harmonized_complete() %>%
      filter(!is.na(RBI), !is.na(recession_slope), !is.na(mean_snow_days)) %>%
      mutate(
        snow_cat = case_when(
          mean_snow_days < 40 ~ "Low (0-40 days)",
          mean_snow_days >= 40 & mean_snow_days < 80 ~ "Medium (40-80 days)",
          mean_snow_days >= 80 ~ "High (80+ days)",
          TRUE ~ "Unknown"
        ),
        snow_cat = factor(snow_cat, levels = c("Low (0-40 days)", "Medium (40-80 days)", "High (80+ days)", "Unknown"))
      )
  })

  # Update site choices for each snow category
  observe({
    site_data <- harmonized_with_categories()

    # Low snow sites
    low_sites <- site_data %>%
      filter(snow_cat == "Low (0-40 days)") %>%
      arrange(Stream_Name)

    low_choices <- setNames(
      low_sites$Stream_ID,
      paste0(low_sites$Stream_Name, " [", low_sites$LTER, ", ", round(low_sites$mean_snow_days, 0), " days]")
    )

    # Medium snow sites
    medium_sites <- site_data %>%
      filter(snow_cat == "Medium (40-80 days)") %>%
      arrange(Stream_Name)

    medium_choices <- setNames(
      medium_sites$Stream_ID,
      paste0(medium_sites$Stream_Name, " [", medium_sites$LTER, ", ", round(medium_sites$mean_snow_days, 0), " days]")
    )

    # High snow sites
    high_sites <- site_data %>%
      filter(snow_cat == "High (80+ days)") %>%
      arrange(Stream_Name)

    high_choices <- setNames(
      high_sites$Stream_ID,
      paste0(high_sites$Stream_Name, " [", high_sites$LTER, ", ", round(high_sites$mean_snow_days, 0), " days]")
    )

    # All sites for highlighting
    all_sites <- site_data %>%
      arrange(snow_cat, Stream_Name)

    all_choices <- setNames(
      all_sites$Stream_ID,
      paste0(all_sites$Stream_Name, " [", all_sites$LTER, ", ", round(all_sites$mean_snow_days, 0), " days]")
    )

    updateSelectInput(session, "low_snow_sites", choices = low_choices)
    updateSelectInput(session, "medium_snow_sites", choices = medium_choices)
    updateSelectInput(session, "high_snow_sites", choices = high_choices)
    updateSelectInput(session, "highlight_sites", choices = all_choices)
  })

  # Site map
  output$site_map <- renderLeaflet({
    req(input$map_lter, input$map_color_by)

    map_data <- harmonized_complete() %>%
      filter(LTER %in% input$map_lter) %>%
      filter(!is.na(Latitude), !is.na(Longitude))

    # High-contrast distinguishable color palette
    distinct_colors <- c(
      "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
      "#ffff33", "#a65628", "#f781bf", "#66c2a5", "#fc8d62",
      "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494",
      "#b3b3b3", "#1b9e77", "#d95f02", "#7570b3", "#e7298a"
    )

    # Create color palette based on selected variable
    color_var <- map_data[[input$map_color_by]]

    if (input$map_color_by %in% c("RBI", "snow_fraction")) {
      # Numeric variables - use continuous color scale
      pal <- colorNumeric(
        palette = c("#d67e7e", "#e6c79c", "#7fb069", "#6b9bd1", "#5a7fa8"),
        domain = color_var,
        na.color = "#cccccc"
      )
      legend_title <- gsub("_", " ", input$map_color_by)
    } else {
      # Categorical variables - use factor colors
      pal <- colorFactor(
        palette = distinct_colors,
        domain = color_var,
        na.color = "#cccccc"
      )
      legend_title <- gsub("_", " ", input$map_color_by)
    }

    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        fillColor = ~pal(color_var),
        color = "#2d2926",
        weight = 1,
        opacity = 0.8,
        fillOpacity = 0.7,
        popup = ~paste0("<b>", Stream_Name, "</b><br>",
                       "LTER: ", LTER, "<br>",
                       "RBI: ", round(RBI, 3), "<br>",
                       "RCS: ", round(recession_slope, 3), "<br>",
                       "Climate: ", ClimateZ, "<br>",
                       "Snow Fraction: ", round(snow_fraction, 3), "<br>",
                       "Mean Annual Precip: ", round(mean_annual_precip, 1), " mm"),
        label = ~Stream_Name
      ) %>%
      addLegend("bottomright",
               pal = pal,
               values = color_var,
               title = legend_title,
               opacity = 0.7)
  })

  # Hydrograph plot
  output$hydrograph_plot <- renderPlotly({
    # Combine all selected sites
    all_selected <- c(input$low_snow_sites, input$medium_snow_sites, input$high_snow_sites)

    if (is.null(all_selected) || length(all_selected) < 2) {
      return(plotly_empty() %>%
        layout(title = list(text = "Select at least 2 sites total to compare discharge patterns",
                           font = list(color = "#666", size = 14))))
    }

    # Get site info
    selected_sites <- harmonized_with_categories() %>%
      filter(Stream_ID %in% all_selected) %>%
      select(Stream_ID, Stream_Name, LTER, snow_cat, mean_snow_days)

    # Get discharge data
    plot_data <- discharge_data() %>%
      filter(Stream_ID %in% all_selected) %>%
      left_join(selected_sites, by = "Stream_ID") %>%
      mutate(site_label = paste0(Stream_Name, " (", gsub(" \\(.*", "", snow_cat), " snow)"))

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "No discharge data available for selected sites",
                           font = list(color = "#666", size = 14))))
    }

    # Colors matching snow categories
    snow_colors <- c(
      "Low (0-40 days)" = "#d67e7e",
      "Medium (40-80 days)" = "#e6c79c",
      "High (80+ days)" = "#6b9bd1"
    )

    # Assign colors based on snow category
    plot_data <- plot_data %>%
      mutate(line_color = snow_colors[as.character(snow_cat)])

    p <- ggplot(plot_data, aes(x = Date, y = Qcms, color = site_label, group = site_label)) +
      geom_line(linewidth = 0.7, alpha = 0.8) +
      labs(x = "Date", y = "Discharge (cms)", color = "Site") +
      theme_minimal(base_family = "Work Sans") +
      theme(
        plot.background = element_rect(fill = "#fefcfb", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
        panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
        text = element_text(color = "#2d2926"),
        axis.text = element_text(color = "#2d2926"),
        legend.position = "bottom"
      )

    ggplotly(p) %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff",
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # RCS vs RBI plot
  output$rcs_rbi_plot <- renderPlotly({
    req(input$show_snow_categories)

    plot_data <- harmonized_with_categories() %>%
      filter(snow_cat %in% input$show_snow_categories) %>%
      mutate(is_highlighted = Stream_ID %in% input$highlight_sites)

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "Select at least one snow category to display",
                           font = list(color = "#666", size = 14))))
    }

    # Create plot showing snow categories with highlighted sites
    p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                               color = snow_cat,
                               alpha = is_highlighted,
                               size = is_highlighted,
                               text = paste0("<b>", Stream_Name, "</b><br>",
                                           "LTER: ", LTER, "<br>",
                                           "Snow Category: ", snow_cat, "<br>",
                                           "Snow Days/Year: ", round(mean_snow_days, 0), "<br>",
                                           "RBI: ", round(RBI, 3), "<br>",
                                           "RCS: ", round(recession_slope, 3)))) +
      geom_point() +
      labs(x = "Richards-Baker Flashiness Index (RBI)\n(higher = more flashy)",
           y = "Recession Curve Slope (RCS)\n(higher = faster drainage)",
           color = "Snow Category",
           title = "Do low snow sites have different flashiness than high snow sites?") +
      theme_minimal(base_family = "Work Sans") +
      theme(
        plot.background = element_rect(fill = "#fefcfb", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
        panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
        text = element_text(color = "#2d2926"),
        axis.text = element_text(color = "#2d2926"),
        plot.title = element_text(size = 11, color = "#666", face = "italic")
      ) +
      scale_color_manual(values = c("Low (0-40 days)" = "#d67e7e",
                                    "Medium (40-80 days)" = "#e6c79c",
                                    "High (80+ days)" = "#6b9bd1")) +
      scale_alpha_manual(values = c("FALSE" = 0.4, "TRUE" = 1), guide = "none") +
      scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4), guide = "none")

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff",
        title = list(text = "Do low snow sites have different flashiness than high snow sites?",
                    font = list(size = 12, color = "#666"))
      )
  })

  # RCS vs RBI highlight plot (specific sites detail)
  output$rcs_rbi_highlight_plot <- renderPlotly({
    if (is.null(input$highlight_sites) || length(input$highlight_sites) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "Select sites from the dropdown to see detailed comparison",
                           font = list(color = "#666", size = 14))))
    }

    plot_data <- harmonized_with_categories() %>%
      filter(Stream_ID %in% input$highlight_sites)

    if (nrow(plot_data) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "No data for selected sites",
                           font = list(color = "#666", size = 14))))
    }

    # Create plot showing only selected sites
    p <- ggplot(plot_data, aes(x = RBI, y = recession_slope,
                               color = snow_cat,
                               text = paste0("<b>", Stream_Name, "</b><br>",
                                           "LTER: ", LTER, "<br>",
                                           "Snow Category: ", snow_cat, "<br>",
                                           "Snow Days/Year: ", round(mean_snow_days, 0), "<br>",
                                           "RBI: ", round(RBI, 3), "<br>",
                                           "RCS: ", round(recession_slope, 3)))) +
      geom_point(size = 5, alpha = 0.8) +
      geom_text(aes(label = Stream_Name), hjust = -0.1, vjust = 0, size = 3, show.legend = FALSE) +
      labs(x = "Richards-Baker Flashiness Index (RBI)\n(higher = more flashy)",
           y = "Recession Curve Slope (RCS)\n(higher = faster drainage)",
           color = "Snow Category") +
      theme_minimal(base_family = "Work Sans") +
      theme(
        plot.background = element_rect(fill = "#fefcfb", color = NA),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        panel.grid.major = element_line(color = "#d4e3f0", linewidth = 0.3),
        panel.grid.minor = element_line(color = "#d4e3f0", linewidth = 0.15),
        text = element_text(color = "#2d2926"),
        axis.text = element_text(color = "#2d2926")
      ) +
      scale_color_manual(values = c("Low (0-40 days)" = "#d67e7e",
                                    "Medium (40-80 days)" = "#e6c79c",
                                    "High (80+ days)" = "#6b9bd1"))

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = "#fefcfb",
        plot_bgcolor = "#ffffff"
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
