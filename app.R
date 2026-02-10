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
  librarian::shelf(shiny, bslib, dplyr, ggplot2, leaflet, plotly, viridis)
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

regime_colors <- c(
  "Rain-dominated" = "#d67e7e",
  "Snow-dominated" = "#6b9bd1"
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
    base_font = font_google("Work Sans", wght = "400..700"),
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
        h4("Map Controls"),
        selectInput(
          "map_color_by",
          "Color sites by:",
          choices = c(
            "Climate Zone" = "Name",
            "Snow Fraction" = "snow_fraction",
            "MAP (mm)" = "mean_annual_precip",
            "RBI" = "RBI",
            "RCS" = "recession_slope",
            "LULC" = "major_land",
            "LTER Network" = "LTER"
          ),
          selected = "Name"
        ),
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          h4("Filter by LTER:", style = "margin: 0;"),
          div(
            actionLink("lter_all", "All", style = "font-size: 0.85em; margin-right: 8px;"),
            actionLink("lter_none", "None", style = "font-size: 0.85em;")
          )
        ),
        checkboxGroupInput(
          "map_lter",
          label = NULL,
          choices = NULL,
          selected = NULL
        )
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Study Sites Across North America"),
          leafletOutput("site_map", height = 600)
        ),
        card(
          card_header("Key Metrics"),
          tags$ul(
            style = "font-size: 0.9em; line-height: 1.6; padding-left: 18px;",
            tags$li(HTML("<span style='font-weight:700;'>Richards-Baker Flashiness Index</span> (RBI): Measures how rapidly streamflow changes over time")),
            tags$li(HTML("<span style='font-weight:700;'>Recession Curve Slope</span> (RCS): Describes how quickly discharge decreases after peak flow")),
            tags$li(HTML("<span style='font-weight:700;'>Climate Zone</span>: Koppen-Geiger climate classification")),
            tags$li(HTML("<span style='font-weight:700;'>Mean Annual Precipitation</span> (MAP, mm): Average yearly precipitation across the watershed")),
            tags$li(HTML("<span style='font-weight:700;'>Snow Fraction</span>: Proportion of the year with snow cover")),
            tags$li(HTML("<span style='font-weight:700;'>Land-use / Land-cover</span> (LULC): Dominant land cover type within the watershed"))
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 1: Hydrographs & Subsurface",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Controls"),

        # --- scatter controls ---
        conditionalPanel(
          condition = "input.activity1_tab == 'RCS vs RBI'",
          p(
            "Show or hide precipitation regime categories:",
            style = "font-size: 0.85em; color: #666;"
          ),
          checkboxGroupInput(
            "show_regime_categories",
            "Display:",
            choices = c(
              "Rain-dominated" = "Rain-dominated",
              "Snow-dominated" = "Snow-dominated"
            ),
            selected = c("Rain-dominated", "Snow-dominated")
          ),
          hr(),
          h4("Highlight Sites"),
          p(
            "Pick sites to label on the plot:",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "highlight_rain",
            "Rain-dominated (fSnow < 25%):",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "highlight_snow_dom",
            HTML("Snow-dominated (fSnow &ge; 25%):"),
            choices = NULL,
            multiple = TRUE
          )
        ),

        # --- hydrograph controls ---
        conditionalPanel(
          condition = "input.activity1_tab == 'Hydrographs'",
          p(
            "Select at least 2 sites from different regimes, then click Plot.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "rain_dom_sites",
            "Rain-dominated (fSnow < 25%):",
            choices = NULL,
            multiple = TRUE
          ),
          selectInput(
            "snow_dom_sites",
            HTML("Snow-dominated (fSnow &ge; 25%):"),
            choices = NULL,
            multiple = TRUE
          ),
          actionButton(
            "plot_hydrographs",
            "Plot Hydrographs",
            class = "btn-primary mt-2 w-100"
          )
        )
      ),

      navset_card_tab(
        id = "activity1_tab",
        nav_panel(
          "RCS vs RBI",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header(
                "How does precipitation regime influence flashiness and recession?"
              ),
              plotlyOutput("rcs_rbi_plot", height = 600)
            ),
            card(
              card_header("Guide"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Richards-Baker Flashiness Index (RBI)</b>: Measures how
                  rapidly streamflow changes over time. Higher values indicate
                  a flashier basin that responds quickly to precipitation."
                )),
                tags$p(HTML(
                  "<b>Recession Curve Slope (RCS)</b>: Describes how quickly
                  discharge decreases after peak flow. Higher values indicate
                  faster drainage and less subsurface storage."
                )),
                hr(),
                tags$p(HTML(
                  "<b>Rain-dominated</b> (fSnow < 25%): Precipitation falls
                  mostly as rain; runoff responds directly to storm events."
                )),
                tags$p(HTML(
                  "<b>Snow-dominated</b> (fSnow &ge; 25%): Significant
                  snowpack buffers and delays runoff into spring melt."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to look for:</b> You should see an inverse
                    relationship between RCS and RBI. Why might flashier
                    basins have lower recession curve slopes? What does this
                    tell you about subsurface storage and streamflow
                    generation?"
                  )
                ),
                hr(),
                tags$p(
                  style = "font-size: 0.85em; color: #888;",
                  HTML(
                    "<em>fSnow = fraction of the year with snow cover
                    (snow days &divide; 365).</em>"
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "Hydrographs",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Compare Discharge Patterns"),
              plotlyOutput("hydrograph_plot", height = 600)
            ),
            card(
              card_header("Reading Hydrographs"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(
                  "Select two hydrographs from rain-dominated areas
                  (fSnow < 25%) and two from snow-dominated areas
                  (fSnow > 25%). Compare the flashiness and recession
                  behavior of all four."
                ),
                hr(),
                tags$p(HTML(
                  "<b>Flashiness:</b> How rapidly does the hydrograph
                  rise and fall? Rain-dominated basins tend to produce
                  sharper peaks and faster recessions."
                )),
                tags$p(HTML(
                  "<b>Recession:</b> How long does it take for discharge to
                  return to baseflow after a peak? The length of the recession
                  period reflects subsurface storage capacity."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to look for:</b> Given the precipitation regime
                    (snow vs rain), what can you say about the relationship
                    between precipitation type and flashiness? Between
                    precipitation type and recession behavior? What hypotheses
                    can you form about why you see these patterns?"
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 2: Mapping Stream Salinity",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Controls"),

        # chloride map controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Chloride Map'",
          p(
            "Use log scale to see variation when a few high-concentration
            sites dominate the color range.",
            style = "font-size: 0.85em; color: #666;"
          ),
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
          checkboxInput(
            "cl_log_scale",
            "Use log scale",
            value = FALSE
          )
        ),

        # seasonal plot controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Seasonal Cl & Discharge'",
          p(
            "Toggle discharge overlay to compare Cl with streamflow seasonality.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cl_site_select",
            "Choose a site:",
            choices = NULL
          ),
          checkboxInput(
            "cl_show_discharge",
            "Overlay monthly discharge",
            value = FALSE
          )
        )
      ),

      navset_card_tab(
        id = "activity2_tab",
        nav_panel(
          "Chloride Map",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Stream Chloride Across North America"),
              leafletOutput("cl_map", height = 600)
            ),
            card(
              card_header("About Stream Chloride"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "<b>Chloride (Cl<sup>&minus;</sup>)</b> is a conservative
                  tracer &mdash; it doesn't react or degrade in most
                  freshwater systems, making it useful for tracking sources
                  and transport."
                )),
                hr(),
                tags$p(HTML(
                  "<b>Common sources of elevated Cl:</b>"
                )),
                tags$ul(
                  tags$li("Road salt application"),
                  tags$li("Agricultural inputs"),
                  tags$li("Wastewater and septic"),
                  tags$li("Marine aerosol influence")
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to look for:</b> What areas of the US have
                    higher Cl? How do precipitation and land use correlate
                    with Cl concentration? Try coloring by % Urban or
                    % Cropland."
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "Seasonal Cl & Discharge",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Monthly Chloride & Discharge Patterns"),
              plotlyOutput("cl_seasonal_plot", height = 600)
            ),
            card(
              card_header("Seasonal Patterns"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(
                  "Choose a few sites from different regions. Look at
                  how Cl varies over the course of a year."
                ),
                hr(),
                tags$p(HTML(
                  "<b>Winter Cl peaks</b> often indicate road salt
                  runoff, especially in urban or suburban catchments."
                )),
                tags$p(HTML(
                  "<b>Summer Cl peaks</b> may reflect concentration during
                  low-flow periods when dilution is reduced."
                )),
                hr(),
                tags$p(HTML(
                  "Toggle <b>discharge overlay</b> to see the relationship
                  between seasonal Cl and the hydrograph. Do they appear
                  to be related?"
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to look for:</b> When do you see high Cl
                    and when do you see low Cl? What is the relationship
                    between seasonal Cl concentration and streamflow?
                    What does that suggest about hydrologic and land use
                    processes driving salinity?"
                  )
                )
              )
            )
          )
        )
      )
    )
  ),

  nav_panel(
    "Activity 3: Exploring C-Q Relationships",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h4("Controls"),
        # C-Q scatter controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Relationships'",
          p(
            "Select up to 3 sites and 2 solutes to compare.",
            style = "font-size: 0.85em; color: #666;"
          ),
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
          )
        ),

        # histogram controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Slope Distribution'",
          p(
            "Compare C-Q slope distributions for Cl and NO3 across all sites.",
            style = "font-size: 0.85em; color: #666;"
          ),
          checkboxGroupInput(
            "cq_hist_solutes",
            "Show:",
            choices = c("Chloride (Cl)" = "Cl", "Nitrate (NO3)" = "NO3"),
            selected = c("Cl", "NO3")
          )
        )
      ),

      navset_card_tab(
        id = "activity3_tab",
        nav_panel(
          "C-Q Relationships",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header(HTML("log<sub>10</sub>(Concentration) vs log<sub>10</sub>(Discharge)")),
              plotlyOutput("cq_scatter_plot", height = 600)
            ),
            card(
              card_header("C-Q Framework"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(HTML(
                  "The C-Q framework follows the power-law model from
                  <a href='https://doi.org/10.1002/hyp.7315' target='_blank'>
                  Godsey et al. (2009)</a>:"
                )),
                tags$p(
                  style = "text-align: center; font-size: 1.05em; margin: 6px 0;",
                  HTML("<em>C = a Q<sup>b</sup></em>")
                ),
                tags$p(HTML(
                  "The exponent <em>b</em> is the C-Q slope, estimated via
                  log-log regression: log(C) = log(a) + <em>b</em> &middot; log(Q).
                  It tells us how solutes are stored and mobilized."
                )),
                hr(),
                tags$p(HTML("<b>Interpreting C-Q slopes:</b>")),
                tags$ul(
                  tags$li(HTML("<b>Positive slope</b> = enrichment (concentration rises with flow)")),
                  tags$li(HTML("<b>Slope near 0</b> = chemostatic (concentration stable)")),
                  tags$li(HTML("<b>Negative slope</b> = dilution (concentration falls with flow)"))
                ),
                hr(),
                tags$p(HTML(
                  "<b>Cl</b> is a <em>conservative</em> tracer &mdash; it
                  doesn't react in most freshwater systems."
                )),
                tags$p(HTML(
                  "<b>NO<sub>3</sub></b> is <em>non-conservative</em> &mdash;
                  it is actively cycled by biological and chemical processes."
                )),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to look for:</b> How do the C-Q slopes for
                    Cl compare to NO<sub>3</sub> at the same site? What does
                    that tell us about the different processes controlling
                    storage and transport of each solute?"
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          "C-Q Slope Distribution",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Distribution of C-Q Slopes Across All Sites"),
              plotlyOutput("cq_histogram", height = 600)
            ),
            card(
              card_header("Reading the Histogram"),
              tags$div(
                style = "font-size: 0.88em; line-height: 1.6; padding: 8px;",
                tags$p(
                  "Each bar represents a group of sites with similar C-Q
                  slopes. Both Cl and NO3 are shown so you can compare
                  their distributions directly."
                ),
                hr(),
                tags$p(HTML(
                  "The dashed lines at <b>&plusmn;0.1</b> mark the boundaries
                  between behavioral regimes:"
                )),
                tags$ul(
                  tags$li(HTML("Left of &minus;0.1: <b>Dilution</b> &mdash; concentration decreases with flow")),
                  tags$li(HTML("Between &plusmn;0.1: <b>Chemostatic</b> &mdash; concentration is stable")),
                  tags$li(HTML("Right of +0.1: <b>Enrichment</b> &mdash; concentration increases with flow"))
                ),
                hr(),
                tags$p(
                  style = "color: #444;",
                  HTML(
                    "<b>What to look for:</b> Which solute has a larger
                    range of slopes? Which is more likely to be chemostatic?
                    What does this tell you about the processes controlling
                    storage and transport of Cl compared to
                    NO<sub>3</sub>?"
                  )
                )
              )
            )
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

  # bin sites by snow days and precipitation regime
  harmonized_with_categories <- reactive({
    harmonized_complete() %>%
      filter(!is.na(RBI), !is.na(recession_slope),
             !is.na(mean_snow_days), !is.na(snow_fraction)) %>%
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
        ),
        precip_regime = if_else(
          snow_fraction < 0.25,
          "Rain-dominated",
          "Snow-dominated"
        ),
        precip_regime = factor(
          precip_regime,
          levels = c("Rain-dominated", "Snow-dominated")
        )
      )
  })

  output$overview_table <- renderTable({
    data.frame(
      Metric = c(
        "Sites on Map",
        "LTER Networks"
      ),
      Count = c(
        nrow(harmonized_complete()),
        length(unique(harmonized_complete()$LTER))
      )
    )
  })

  # only show LTERs that have sites with coordinates in the complete dataset
  lter_with_data <- reactive({
    harmonized_complete() %>%
      filter(!is.na(Latitude), !is.na(Longitude)) %>%
      pull(LTER) %>%
      unique() %>%
      sort()
  })

  observe({
    choices <- lter_with_data()
    updateCheckboxGroupInput(session, "map_lter", choices = choices, selected = choices)
  })

  observeEvent(input$lter_all, {
    updateCheckboxGroupInput(session, "map_lter", selected = lter_with_data())
  })

  observeEvent(input$lter_none, {
    updateCheckboxGroupInput(session, "map_lter", selected = character(0))
  })

  # populate the per-regime site dropdowns in Activity 1
  observe({
    site_data <- harmonized_with_categories()

    make_choices <- function(df) {
      df <- df %>% arrange(Stream_Name)
      setNames(
        df$Stream_ID,
        paste0(df$Stream_Name, " [", df$LTER, ", ",
               round(df$snow_fraction * 100, 0), "% snow]")
      )
    }

    rain_choices <- make_choices(filter(site_data, precip_regime == "Rain-dominated"))
    snow_dom_choices <- make_choices(filter(site_data, precip_regime == "Snow-dominated"))

    updateSelectInput(session, "rain_dom_sites", choices = rain_choices)
    updateSelectInput(session, "snow_dom_sites", choices = snow_dom_choices)
    updateSelectInput(session, "highlight_rain", choices = rain_choices)
    updateSelectInput(session, "highlight_snow_dom", choices = snow_dom_choices)
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

    map_data <- map_data %>% filter(!is.na(.data[[input$map_color_by]]))

    # clean up major_land labels: remove underscores, title case
    if (input$map_color_by == "major_land") {
      map_data <- map_data %>%
        mutate(major_land = gsub("_", " ", major_land) %>% tools::toTitleCase())
    }

    color_var <- map_data[[input$map_color_by]]

    legend_titles <- c(
      "Name" = "Climate Zone",
      "snow_fraction" = "Snow Fraction",
      "mean_annual_precip" = "MAP (mm)",
      "RBI" = "RBI",
      "recession_slope" = "RCS",
      "major_land" = "LULC",
      "LTER" = "LTER Network"
    )
    legend_title <- legend_titles[[input$map_color_by]]

    if (input$map_color_by %in% c("RBI", "recession_slope", "snow_fraction", "mean_annual_precip")) {
      pal <- colorNumeric(
        palette = c("#d67e7e", "#e6c79c", "#7fb069", "#6b9bd1", "#5a7fa8"),
        domain = color_var
      )
    } else {
      pal <- colorFactor(
        palette = distinct_colors,
        domain = color_var
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
          Name,
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
        "bottomleft",
        pal = pal,
        values = color_var,
        title = legend_title,
        opacity = 0.7
      )
  })

  # --- Hydrograph ----------------------------------------------------------

  hydrograph_data <- eventReactive(input$plot_hydrographs, {
    all_selected <- c(input$rain_dom_sites, input$snow_dom_sites)

    if (length(all_selected) < 2) return(NULL)

    selected_sites <- harmonized_with_categories() %>%
      filter(Stream_ID %in% all_selected) %>%
      select(Stream_ID, Stream_Name, LTER, precip_regime, snow_fraction)

    discharge_data() %>%
      filter(Stream_ID %in% all_selected) %>%
      left_join(selected_sites, by = c("Stream_ID", "Stream_Name", "LTER")) %>%
      mutate(
        site_label = paste0(Stream_Name, " (", precip_regime, ")")
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

  all_highlighted <- reactive({
    c(input$highlight_rain, input$highlight_snow_dom)
  })

  output$rcs_rbi_plot <- renderPlotly({
    req(input$show_regime_categories)

    plot_data <- harmonized_with_categories() %>%
      filter(precip_regime %in% input$show_regime_categories) %>%
      mutate(is_highlighted = Stream_ID %in% all_highlighted())

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select at least one category to display",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    hover_text <- paste0(
      "<b>", plot_data$Stream_Name, "</b><br>",
      "LTER: ", plot_data$LTER, "<br>",
      "Regime: ", plot_data$precip_regime, "<br>",
      "fSnow: ", round(plot_data$snow_fraction * 100, 0), "%<br>",
      "Snow Days/Year: ", round(plot_data$mean_snow_days, 0), "<br>",
      "RBI: ", round(plot_data$RBI, 3), "<br>",
      "RCS: ", round(plot_data$recession_slope, 3)
    )

    p <- ggplot(
      plot_data,
      aes(x = RBI, y = recession_slope,
          color = precip_regime, text = hover_text)
    ) +
      geom_point(size = 3, alpha = 0.5) +
      labs(
        x = "Richards-Baker Flashiness Index (RBI)\n(higher = more flashy)",
        y = "Recession Curve Slope (RCS)\n(higher = faster drainage)",
        color = "Precipitation Regime"
      ) +
      base_plot_theme +
      scale_color_manual(values = regime_colors)

    if (any(plot_data$is_highlighted)) {
      highlight_df <- filter(plot_data, is_highlighted)
      highlight_text <- paste0(
        "<b>", highlight_df$Stream_Name, "</b><br>",
        "LTER: ", highlight_df$LTER, "<br>",
        "Regime: ", highlight_df$precip_regime, "<br>",
        "fSnow: ", round(highlight_df$snow_fraction * 100, 0), "%<br>",
        "RBI: ", round(highlight_df$RBI, 3), "<br>",
        "RCS: ", round(highlight_df$recession_slope, 3)
      )
      p <- p +
        geom_point(
          data = highlight_df,
          aes(text = highlight_text),
          size = 5, alpha = 1, show.legend = FALSE
        ) +
        geom_text(
          data = highlight_df,
          aes(label = Stream_Name),
          hjust = -0.1, vjust = 0, size = 3,
          show.legend = FALSE
        )
    }

    ggplotly(p, tooltip = "text") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = list(orientation = "h", y = -0.15),
        title = list(
          text = "Do rain- and snow-dominated sites differ in flashiness?",
          font = list(size = 12, color = "#666")
        )
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
    legend_labels <- c(
      "mean_Cl_uM" = "Mean Cl (uM)",
      "land_cropland" = "% Cropland",
      "land_urban_and_built_up_land" = "% Urban",
      "mean_annual_precip" = "Precip (mm/yr)"
    )
    legend_title <- legend_labels[[color_var_name]]

    map_data <- map_data %>% filter(!is.na(.data[[color_var_name]]))
    color_vals <- map_data[[color_var_name]]

    use_log <- isTRUE(input$cl_log_scale) & all(color_vals > 0, na.rm = TRUE)

    if (use_log) {
      pal <- colorNumeric(
        viridis(256),
        domain = log10(color_vals)
      )
      fill_colors <- pal(log10(color_vals))
      legend_title <- paste0(legend_title, " (log\u2081\u2080)")
    } else {
      # quantile breaks so outliers don't crush the range
      breaks <- unique(quantile(color_vals, probs = seq(0, 1, length.out = 7), na.rm = TRUE))
      pal <- colorBin(
        viridis(max(length(breaks) - 1, 1)),
        domain = color_vals,
        bins = breaks
      )
      fill_colors <- pal(color_vals)
    }

    m <- leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6,
        fillColor = fill_colors,
        color = "#2d2926",
        weight = 1,
        opacity = 0.6,
        fillOpacity = 0.5,
        popup = ~ paste0(
          "<b>", Stream_Name, "</b><br>",
          "LTER: ", LTER, "<br>",
          "Mean Cl: ", round(mean_Cl_uM, 1), " uM<br>",
          "% Cropland: ", round(land_cropland, 1), "<br>",
          "% Urban: ", round(land_urban_and_built_up_land, 1), "<br>",
          "Precip: ", round(mean_annual_precip, 0), " mm/yr"
        ),
        label = ~Stream_Name
      )

    if (use_log) {
      m <- m %>% addLegend(
        "bottomleft", pal = pal,
        values = log10(color_vals),
        title = legend_title,
        opacity = 0.7,
        labFormat = labelFormat(transform = function(x) round(10^x, 1))
      )
    } else {
      m <- m %>% addLegend(
        "bottomleft", pal = pal,
        values = color_vals,
        title = legend_title,
        opacity = 0.7
      )
    }

    m
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

  cq_solute_choices <- c(
    "Chloride (Cl)" = "Cl",
    "Nitrate (NO3)" = "NO3"
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

  # populate solute dropdown with available solutes
  observe({
    available <- unique(cq_slopes_data()$variable)
    scatter_choices <- cq_solute_choices[cq_solute_choices %in% available]
    updateSelectInput(session, "cq_solutes", choices = scatter_choices)
  })

  # enforce max 3 sites
  observe({
    if (length(input$cq_sites) > 3) {
      updateSelectInput(session, "cq_sites", selected = input$cq_sites[1:3])
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
    req(input$cq_hist_solutes)

    slopes <- cq_slopes_data() %>%
      filter(variable %in% input$cq_hist_solutes)

    if (nrow(slopes) == 0) {
      return(
        plotly_empty() %>%
          layout(title = list(
            text = "No C-Q slopes available",
            font = list(color = "#666", size = 14)
          ))
      )
    }

    # y-range for annotation placement (use combined data)
    hist_obj <- hist(slopes$cq_slope, plot = FALSE)
    y_max <- max(hist_obj$counts) * 1.1

    solute_colors <- c("Cl" = "#6b9bd1", "NO3" = "#7fb069")

    p <- plot_ly()
    for (sol in input$cq_hist_solutes) {
      sol_data <- filter(slopes, variable == sol)
      sol_label <- names(cq_solute_choices)[cq_solute_choices == sol]
      p <- p %>%
        add_histogram(
          x = sol_data$cq_slope,
          name = sol_label,
          marker = list(
            color = paste0(solute_colors[[sol]], "99"),
            line = list(color = solute_colors[[sol]], width = 1)
          ),
          hovertemplate = paste0(
            sol_label, "<br>Slope: %{x:.2f}<br>Count: %{y}<extra></extra>"
          )
        )
    }

    p %>%
      layout(
        barmode = "overlay",
        title = list(
          text = "C-Q Slope Distribution \u2014 Cl vs NO3",
          font = list(size = 14, color = "#2d2926")
        ),
        xaxis = list(title = "C-Q Slope", gridcolor = "#d4e3f0"),
        yaxis = list(title = "Number of Sites", gridcolor = "#d4e3f0"),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = list(orientation = "h", y = -0.15),
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
