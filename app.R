# HydroViz teaching app
#
# Interactive app for exploring hydrology, snow, chloride, and C-Q
# patterns across North American sites.
#
# The app reads the pre-processed files built by prep_data.R.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(leaflet)
  library(leaflet.extras)
  library(plotly)
  library(terra)
  library(viridis)
})

# App data lives in the repo by default, but can be overridden for local testing
# or deployment with HYDRO_MODULES_APP_DATA_DIR.
data_path <- Sys.getenv("HYDRO_MODULES_APP_DATA_DIR", unset = "data")

# Shared palette used across the maps, plots, and UI.
module_colors <- c(
  "primary" = "#2f6c8f",
  "secondary" = "#5a86a6",
  "success" = "#7a9e63",
  "danger" = "#c98066",
  "warning" = "#d5b066"
)

precip_palette <- c(
  "#eef7fd",
  "#d9ecf7",
  "#bfdcf0",
  "#9fc9e4",
  "#7db4d7",
  "#5d9fc8",
  "#3f88b5",
  "#2f729b"
)

activity2_precip_raster_breaks <- c(
  0, 400, 600, 1000, 1500, 2000, 2500, 3000, 4000, 5000, Inf
)

activity2_precip_raster_colors <- c(
  "#a55f47",
  "#c27f4e",
  "#d9a25b",
  "#ebc96a",
  "#bfd169",
  "#83b865",
  "#4f9b6b",
  "#3f8d7d",
  "#3b7692",
  "#315d86"
)

activity2_precip_raster_labels <- c(
  "0 - 400",
  "400 - 600",
  "600 - 1,000",
  "1,000 - 1,500",
  "1,500 - 2,000",
  "2,000 - 2,500",
  "2,500 - 3,000",
  "3,000 - 4,000",
  "4,000 - 5,000",
  "> 5,000"
)

activity2_cropland_breaks <- c(0.5, 2, 10, 25, 50, Inf)
activity2_cropland_colors <- c(
  "#f1e6b2",
  "#ddc56d",
  "#bf9c3d",
  "#946f22",
  "#654612"
)
activity2_cropland_labels <- c(
  "0.5 - 2",
  "2 - 10",
  "10 - 25",
  "25 - 50",
  "> 50"
)

activity2_impervious_breaks <- c(0.1, 5, 10, 50, Inf)
activity2_impervious_colors <- c(
  "#dfe5e8",
  "#7f8c96",
  "#66727c",
  "#353d44"
)
activity2_impervious_labels <- c(
  "< 5",
  "5 - 10",
  "10 - 50",
  "> 50"
)

snow_palette <- c(
  "#d8eff8",
  "#c1e2f3",
  "#a5d2eb",
  "#83bde0",
  "#63a5d1",
  "#448bbd",
  "#2f6fa3",
  "#1f5484"
)

land_use_colors <- c(
  "Cropland" = "#d7b63f",
  "Grassland / Shrubland" = "#9fc86b",
  "Forest" = "#2f6b3b",
  "Wetland / Marsh" = "#6f90a8",
  "Tidal Wetland" = "#58748e",
  "Impervious" = "#c3c8cc",
  "Bare" = "#ffffff",
  "Water" = "#3f73b5",
  "Salt Water" = "#7da7d6",
  "Ice / Snow" = "#e7f2f8",
  "Tundra" = "#b9c7a0",
  "Other / Unclassified" = "#b9b3ab"
)

climate_zone_colors <- c(
  "Arid" = "#c77b63",
  "Semi-Arid" = "#d8a160",
  "Mediterranean" = "#b8a55d",
  "Humid Subtropical" = "#6f996f",
  "Tropical" = "#4f8c7e",
  "Humid Continental" = "#6f93b1",
  "Subarctic" = "#5b78a0",
  "Tundra" = "#8fb3c2"
)

about_logo_items <- list(
  list(
    src = "about_assets/oregon_state_university.webp",
    alt = "Oregon State University logo",
    label = "Oregon State University",
    logo_class = "logo-osu"
  ),
  list(
    src = "about_assets/cu_cires.png",
    alt = "University of Colorado Boulder and CIRES logo",
    label = "CU Boulder / CIRES",
    logo_class = "logo-cucires"
  ),
  list(
    src = "about_assets/usgs.png",
    alt = "United States Geological Survey logo",
    label = "USGS",
    logo_class = "logo-usgs"
  ),
  list(
    src = "about_assets/nsf.png",
    alt = "National Science Foundation logo",
    label = "NSF",
    logo_class = "logo-nsf"
  ),
  list(
    src = "about_assets/nceas.png",
    alt = "National Center for Ecological Analysis and Synthesis logo",
    label = "NCEAS",
    logo_class = "logo-nceas"
  ),
  list(
    src = "about_assets/powell_center.jpg",
    alt = "Powell Center logo",
    label = "Powell Center",
    logo_class = "logo-powell"
  ),
  list(
    src = "about_assets/umrr.jpg",
    alt = "Upper Mississippi River Restoration Program logo",
    label = "UMRR",
    logo_class = "logo-umrr"
  ),
  list(
    src = "about_assets/environment_canada.jpg",
    alt = "Environment Canada logo",
    label = "Environment Canada",
    logo_class = "logo-environment-canada"
  ),
  list(
    src = "about_assets/cuahsi.png",
    alt = "CUAHSI logo",
    label = "CUAHSI",
    logo_class = "logo-cuahsi"
  )
)

activity2_cl_accent <- "#975379"
activity2_q_accent <- "#7f878d"
activity2_site_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
activity3_no3_accent <- "#355c8a"
chloride_excluded_stream_names <- c("DMF Brazos River")

solute_colors <- c(
  "Cl" = activity2_cl_accent,
  "NO3" = activity3_no3_accent
)

# Shared ggplot styling used in several figures.
base_plot_theme <- theme_minimal(base_family = "Work Sans") +
  theme(
    plot.background = element_rect(fill = "#fcfbf7", color = NA),
    panel.background = element_rect(fill = "#ffffff", color = NA),
    panel.grid.major = element_line(color = "#d7e3ea", linewidth = 0.32),
    panel.grid.minor = element_line(color = "#e7eef3", linewidth = 0.18),
    text = element_text(color = "#24323d"),
    axis.text = element_text(color = "#31424c"),
    axis.title = element_text(face = "plain")
  )

plotly_bg <- list(paper_bgcolor = "#fcfbf7", plot_bgcolor = "#ffffff")
plotly_modebar_remove <- c(
  "select2d",
  "lasso2d",
  "hoverCompareCartesian",
  "hoverClosestCartesian",
  "autoScale2d",
  "toggleSpikelines"
)

polish_plotly <- function(p, register_click = FALSE) {
  if (isTRUE(register_click)) {
    p <- event_register(p, "plotly_click")
  }

  p %>%
    layout(
      font = list(family = "Work Sans, sans-serif", color = "#24323d"),
      hoverlabel = list(
        bgcolor = "rgba(255,255,255,0.98)",
        bordercolor = "#bfd0db",
        font = list(color = "#24323d", size = 12)
      )
    ) %>%
    config(
      displaylogo = FALSE,
      responsive = TRUE,
      scrollZoom = FALSE,
      modeBarButtonsToRemove = plotly_modebar_remove
    )
}

right_side_legend <- function(font_size = 11) {
  list(
    orientation = "v",
    x = 1.02,
    y = 1,
    xanchor = "left",
    yanchor = "top",
    bgcolor = "rgba(255,255,255,0.88)",
    bordercolor = "#d4e3f0",
    borderwidth = 1,
    font = list(size = font_size, color = "#24323d")
  )
}

# Stop early if a required app data file is missing.
required_data_files <- c(
  "harmonized_complete.rds",
  "harmonized_partial.rds",
  "discharge.rds",
  "cl_monthly.rds",
  "cq_paired.rds",
  "cq_slopes.rds"
)
missing_data_files <- required_data_files[
  !file.exists(file.path(data_path, required_data_files))
]

if (length(missing_data_files) > 0) {
  stop(
    paste(
      "Missing app data files in", normalizePath(data_path, mustWork = FALSE), ":",
      paste(missing_data_files, collapse = ", "),
      "\nRun prep_data.R first or set HYDRO_MODULES_APP_DATA_DIR."
    )
  )
}

format_site_name_token <- function(token) {
  if (!grepl("[[:alpha:]]", token)) {
    return(token)
  }

  if (grepl("[[:digit:]]", token)) {
    return(token)
  }

  if (identical(token, toupper(token)) && nchar(token) <= 3) {
    return(token)
  }

  token_lower <- tolower(token)
  if (grepl("^mc[[:alpha:]]", token_lower) && nchar(token_lower) > 2) {
    return(paste0(
      "Mc",
      toupper(substr(token_lower, 3, 3)),
      substr(token_lower, 4, nchar(token_lower))
    ))
  }

  paste0(toupper(substr(token_lower, 1, 1)), substr(token_lower, 2, nchar(token_lower)))
}

format_site_name <- function(site_names) {
  vapply(
    as.character(site_names),
    function(site_name) {
      if (is.na(site_name) || trimws(site_name) == "") {
        return(site_name)
      }

      pieces <- regmatches(
        site_name,
        gregexpr("[[:alnum:]]+|[^[:alnum:]]+", site_name, perl = TRUE)
      )[[1]]

      paste0(
        vapply(
          pieces,
          function(piece) {
            if (grepl("^[[:alnum:]]+$", piece)) {
              format_site_name_token(piece)
            } else {
              piece
            }
          },
          character(1)
        ),
        collapse = ""
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}

format_stream_name_columns <- function(data) {
  if (is.data.frame(data) && "Stream_Name" %in% names(data)) {
    stream_names <- as.character(data$Stream_Name)
    unique_stream_names <- unique(stream_names)
    formatted_stream_names <- format_site_name(unique_stream_names)
    data$Stream_Name <- formatted_stream_names[match(stream_names, unique_stream_names)]
  }

  data
}

read_app_data <- function(file_name) {
  readRDS(file.path(data_path, file_name)) %>%
    format_stream_name_columns()
}

build_monthly_discharge <- function(discharge_df) {
  discharge_df %>%
    mutate(month = as.integer(format(Date, "%m"))) %>%
    group_by(Stream_ID, Stream_Name, LTER, month) %>%
    summarise(mean_Q_cms = mean(Qcms, na.rm = TRUE), .groups = "drop")
}

build_daily_average_discharge <- function(discharge_df) {
  discharge_df %>%
    filter(!is.na(Date), !is.na(Qcms)) %>%
    mutate(
      month = as.integer(format(Date, "%m")),
      day = as.integer(format(Date, "%d")),
      hydrograph_date = as.Date(paste0("2000-", sprintf("%02d-%02d", month, day))),
      day_of_year = as.integer(format(hydrograph_date, "%j")),
      month_day_label = format(hydrograph_date, "%b %d")
    ) %>%
    group_by(
      Stream_ID,
      Stream_Name,
      LTER,
      month,
      day,
      day_of_year,
      month_day_label
    ) %>%
    summarise(mean_Q_cms = mean(Qcms, na.rm = TRUE), .groups = "drop") %>%
    arrange(Stream_ID, day_of_year)
}

month_labels <- c(
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec"
)
month_start_days <- as.integer(format(
  as.Date(sprintf("2000-%02d-01", seq_len(12))),
  "%j"
))
month_keys <- tolower(month_labels)
days_in_month <- c(
  "jan" = 31,
  "feb" = 28,
  "mar" = 31,
  "apr" = 30,
  "may" = 31,
  "jun" = 30,
  "jul" = 31,
  "aug" = 31,
  "sep" = 30,
  "oct" = 31,
  "nov" = 30,
  "dec" = 31
)

extract_monthly_site_values <- function(site_row, prefix, suffix) {
  vapply(
    month_keys,
    function(key) {
      col_name <- paste0(prefix, key, suffix)
      if (col_name %in% names(site_row)) {
        as.numeric(site_row[[col_name]][1])
      } else {
        NA_real_
      }
    },
    numeric(1)
  )
}

clean_land_use_label <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^land_", "", x)

  dplyr::case_when(
    is.na(x) | x == "" ~ "Other / Unclassified",
    x %in% c(
      "deciduous_broadleaf_forest",
      "evergreen_needleleaf_forest",
      "mixed_forest",
      "evergreen_broadleaf_forest",
      "deciduous_needleleaf_forest",
      "Forest"
    ) ~ "Forest",
    x %in% c("shrubland_grassland", "Grassland_Shrubland") ~ "Grassland / Shrubland",
    x %in% c("cropland", "Cropland") ~ "Cropland",
    x %in% c("urban_and_built_up_land", "Impervious") ~ "Impervious",
    x %in% c("wetland", "Wetland_Marsh") ~ "Wetland / Marsh",
    x %in% c("Tidal_Wetland") ~ "Tidal Wetland",
    x %in% c("Water") ~ "Water",
    x %in% c("Salt_Water") ~ "Salt Water",
    x %in% c("Ice_Snow", "tundra") ~ "Ice / Snow",
    x %in% c("Bare", "barren_or_sparsely_vegetated") ~ "Bare",
    TRUE ~ tools::toTitleCase(gsub("_", " ", x))
  )
}

named_color_lookup <- function(values, palette, default = "#b9b3ab") {
  vapply(
    as.character(values),
    function(value) {
      if (!is.na(value) && value %in% names(palette)) {
        unname(palette[[value]])
      } else {
        default
      }
    },
    character(1)
  )
}

land_use_legend_order <- c(
  "Cropland",
  "Grassland / Shrubland",
  "Forest",
  "Wetland / Marsh",
  "Tidal Wetland",
  "Impervious",
  "Bare",
  "Water",
  "Salt Water",
  "Ice / Snow",
  "Tundra",
  "Other / Unclassified"
)

land_use_legend_levels <- function(values) {
  present_levels <- unique(as.character(values[!is.na(values)]))
  c(
    intersect(land_use_legend_order, present_levels),
    sort(setdiff(present_levels, land_use_legend_order))
  )
}

load_activity2_raster <- function(file_path) {
  if (!file.exists(file_path)) {
    return(NULL)
  }
  raster_layer <- terra::rast(file_path)

  # Crop to North America so the map opens on the teaching region.
  north_america_extent <- terra::ext(-179, -50, 5, 85)
  terra::crop(raster_layer, north_america_extent)
}

activity2_map_bounds <- list(
  xmin = -179,
  ymin = 5,
  xmax = -50,
  ymax = 85
)

activity2_initial_map_view <- list(
  lng = -103,
  lat = 49,
  zoom = 3
)

activity2_landcover_focus_bounds <- list(
  xmin = -135,
  ymin = 15,
  xmax = -60,
  ymax = 63
)

activity2_background_focus_bounds <- list(
  "none" = activity2_landcover_focus_bounds,
  "map" = activity2_map_bounds,
  "cropland" = activity2_landcover_focus_bounds,
  "impervious" = activity2_landcover_focus_bounds
)

# Load the largest data object once at startup; read the rest on demand.
discharge_global <- read_app_data("discharge.rds")

activity2_background_specs <- list(
  "map" = list(
    label = "MAP (mm)",
    file_name = "activity2_map_precip_mm.tif",
    fallback_file = file.path("raw_inputs", "na_1981_2010_annual_precip.tif"),
    colors = activity2_precip_raster_colors,
    breaks = activity2_precip_raster_breaks,
    labels = activity2_precip_raster_labels
  ),
  "cropland" = list(
    label = "% Cropland",
    file_name = "activity2_map_cropland_pct.tif",
    colors = activity2_cropland_colors,
    breaks = activity2_cropland_breaks,
    labels = activity2_cropland_labels
  ),
  "impervious" = list(
    label = "% Impervious",
    file_name = "activity2_map_impervious_pct.tif",
    colors = activity2_impervious_colors,
    breaks = activity2_impervious_breaks,
    labels = activity2_impervious_labels
  )
)

load_activity2_background_rasters <- function(base_dir, specs) {
  lapply(specs, function(spec) {
    primary_path <- file.path(base_dir, spec$file_name)
    raster_layer <- load_activity2_raster(primary_path)

    if (is.null(raster_layer) && !is.null(spec$fallback_file)) {
      raster_layer <- load_activity2_raster(file.path(base_dir, spec$fallback_file))
    }

    raster_layer
  })
}

activity2_background_rasters_global <- load_activity2_background_rasters(
  base_dir = data_path,
  specs = activity2_background_specs
)

activity2_background_choices <- setNames(
  c(
    "none",
    names(activity2_background_specs)[
      vapply(activity2_background_rasters_global, Negate(is.null), logical(1))
    ]
  ),
  c(
    "",
    vapply(
      activity2_background_specs[
        names(activity2_background_specs)[
          vapply(activity2_background_rasters_global, Negate(is.null), logical(1))
        ]
      ],
      `[[`,
      character(1),
      "label"
    )
  )
)


# --- UI -------------------------------------------------------------------

ui <- page_navbar(
  title = tags$div(
    class = "app-title-block",
    tags$span("HydroViz", class = "app-title-kicker"),
    tags$span("Hydrology Modules", class = "app-title-main")
  ),
  theme = bs_theme(
    base_font = font_google("Work Sans", wght = "400..700"),
    heading_font = font_google("Work Sans", wght = "500..700"),
    bg = "#fcfbf7",
    fg = "#24323d",
    navbar_bg = "#ffffff",
    navbar_fg = "#24323d",
    primary = module_colors[["primary"]],
    secondary = module_colors[["secondary"]],
    success = module_colors[["success"]],
    danger = module_colors[["danger"]],
    "card-bg" = "#ffffff",
    "card-border-color" = "#d7e3ea"
  ),

  header = tags$head(
    tags$style(HTML(
      "
      :root {
        --hydro-paper: #f3f7fa;
        --hydro-canvas: #fcfbf7;
        --hydro-card: rgba(255,255,255,0.94);
        --hydro-card-strong: #ffffff;
        --hydro-ink: #24323d;
        --hydro-muted: #5d6d76;
        --hydro-line: #d7e3ea;
        --hydro-line-strong: #bfd0db;
        --hydro-blue: #2f6c8f;
        --hydro-blue-soft: rgba(47,108,143,0.14);
        --hydro-green-soft: rgba(122,158,99,0.13);
        --hydro-sand-soft: rgba(213,176,102,0.14);
        --hydro-shadow: 0 18px 42px rgba(53,79,92,0.08);
        --hydro-shadow-strong: 0 22px 48px rgba(53,79,92,0.12);
      }

      body {
        background:
          radial-gradient(circle at 8% 2%, rgba(47,108,143,0.15), transparent 28%),
          radial-gradient(circle at 88% 10%, rgba(122,158,99,0.13), transparent 26%),
          linear-gradient(180deg, var(--hydro-paper) 0%, #f8f5ee 48%, var(--hydro-canvas) 100%) !important;
        font-family: 'Work Sans', sans-serif !important;
        color: var(--hydro-ink) !important;
        min-height: 100vh;
      }

      body::before {
        content: '';
        position: fixed;
        right: 4%;
        bottom: 7%;
        width: 22rem;
        height: 22rem;
        background: radial-gradient(circle, var(--hydro-sand-soft), transparent 68%);
        filter: blur(10px);
        pointer-events: none;
        z-index: 0;
      }

      .bslib-page-navbar,
      .container-fluid,
      .navbar,
      .main {
        position: relative;
        z-index: 1;
      }

      #map, .leaflet-container {
        background: #f7fafc !important;
      }

      .card {
        border: 1px solid var(--hydro-line) !important;
        box-shadow: var(--hydro-shadow) !important;
        border-radius: 18px !important;
        background: var(--hydro-card) !important;
        overflow: hidden !important;
        backdrop-filter: blur(10px);
      }

      .card:hover {
        transform: translateY(-2px);
        box-shadow: var(--hydro-shadow-strong) !important;
      }

      .card-header {
        background: linear-gradient(180deg, rgba(243,248,251,0.98), rgba(236,244,248,0.94)) !important;
        border-bottom: 1px solid var(--hydro-line) !important;
        color: var(--hydro-ink) !important;
        font-weight: 700 !important;
        letter-spacing: 0.01em;
        border-radius: 18px 18px 0 0 !important;
      }

      .bslib-value-box {
        border: 1px solid var(--hydro-line) !important;
        box-shadow: var(--hydro-shadow) !important;
        border-radius: 18px !important;
        background: var(--hydro-card-strong) !important;
      }

      .sidebar {
        background: rgba(255,255,255,0.9) !important;
        border: 1px solid var(--hydro-line) !important;
        box-shadow: var(--hydro-shadow) !important;
        border-radius: 18px !important;
        backdrop-filter: blur(8px);
      }

      @media (min-width: 992px) {
        .sidebar {
          position: sticky;
          top: 5.4rem;
          max-height: calc(100vh - 7rem);
          overflow-y: auto;
        }
      }

      .navbar {
        box-shadow: 0 10px 28px rgba(53,79,92,0.08) !important;
        background: rgba(255,255,255,0.84) !important;
        border-bottom: 1px solid rgba(215,227,234,0.9) !important;
        backdrop-filter: blur(14px);
      }

      .app-title-block {
        display: flex;
        flex-direction: column;
        gap: 0.05rem;
        line-height: 1.02;
      }

      .app-title-kicker {
        font-size: 0.72rem;
        text-transform: uppercase;
        letter-spacing: 0.18em;
        font-weight: 700;
        color: #5f8da9;
      }

      .app-title-main {
        font-size: 1.14rem;
        font-weight: 700;
        letter-spacing: -0.01em;
        color: var(--hydro-ink);
      }

      .navbar-brand {
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
      }

      .navbar-nav .nav-link {
        border-radius: 999px;
        padding: 0.58rem 0.95rem !important;
        margin: 0 0.12rem;
        font-weight: 600;
        color: #52636c !important;
        transition: all 0.24s ease;
      }

      .navbar-nav .nav-link:hover {
        background: rgba(47,108,143,0.1) !important;
        color: #24465b !important;
        transform: translateY(-1px);
      }

      .navbar-nav .nav-link.active {
        color: #24465b !important;
        background: linear-gradient(135deg, rgba(47,108,143,0.18), rgba(47,108,143,0.08)) !important;
        border-bottom: none !important;
        box-shadow: inset 0 0 0 1px rgba(47,108,143,0.2);
      }

      .nav-tabs {
        gap: 0.45rem;
        padding: 0.35rem 0.35rem 0.15rem;
        border-bottom: none !important;
      }

      .nav-tabs .nav-link {
        border: none !important;
        border-radius: 999px !important;
        padding: 0.55rem 0.95rem !important;
        background: rgba(243,248,251,0.92);
        color: #5b6b74;
        font-weight: 600;
      }

      .nav-tabs .nav-link.active {
        background: linear-gradient(135deg, #2f6c8f, #5a86a6) !important;
        color: #ffffff !important;
        box-shadow: 0 10px 22px rgba(47,108,143,0.18);
      }

      .form-label,
      .control-label,
      .sidebar h4 {
        font-weight: 700 !important;
        color: #344650 !important;
        letter-spacing: 0.01em;
      }

      .card p,
      .card li {
        line-height: 1.66;
      }

      .sidebar p {
        font-size: 0.93rem !important;
        line-height: 1.6 !important;
        color: #55656e !important;
      }

      .form-select,
      .form-control,
      .selectize-input,
      .selectize-dropdown {
        border-radius: 12px !important;
      }

      .form-select,
      .form-control,
      .selectize-input {
        border: 1px solid var(--hydro-line-strong) !important;
        box-shadow: none !important;
        background: #ffffff !important;
        min-height: 46px;
      }

      .form-select:focus,
      .form-control:focus,
      .selectize-input.focus {
        border-color: #7ea8c4 !important;
        box-shadow: 0 0 0 0.22rem rgba(47,108,143,0.16) !important;
      }

      .selectize-dropdown {
        border: 1px solid var(--hydro-line) !important;
        box-shadow: 0 18px 32px rgba(53,79,92,0.12) !important;
      }

      .btn-primary {
        background: linear-gradient(135deg, #2f6c8f, #5a86a6) !important;
        border: none !important;
        border-radius: 999px !important;
        font-weight: 600 !important;
        padding: 0.55rem 1rem !important;
        box-shadow: 0 10px 24px rgba(47,108,143,0.22);
      }

      .btn-outline-secondary {
        border-radius: 999px !important;
        border-color: var(--hydro-line-strong) !important;
        color: #405560 !important;
        background: #ffffff !important;
      }

      .btn-outline-secondary:hover {
        background: #eef5f8 !important;
        color: var(--hydro-ink) !important;
        border-color: #9fb8c8 !important;
      }

      .leaflet-container,
      .html-widget,
      .js-plotly-plot {
        border-radius: 14px !important;
      }

      .leaflet-container {
        background: #edf2f4 !important;
        box-shadow: inset 0 0 0 1px rgba(215,227,234,0.75);
      }

      .leaflet-control-zoom a,
      .leaflet-bar a {
        border-radius: 10px !important;
        border: none !important;
        color: #264557 !important;
        background: #ffffff !important;
        box-shadow: 0 8px 18px rgba(53,79,92,0.12);
      }

      .leaflet-control-attribution {
        background: rgba(255,255,255,0.88) !important;
        border-radius: 10px 0 0 0;
      }

      .custom-legend {
        background: rgba(255,255,255,0.94);
        border: 1px solid var(--hydro-line);
        border-radius: 14px;
        box-shadow: 0 12px 28px rgba(53,79,92,0.14);
        padding: 0.8rem 0.9rem;
        color: var(--hydro-ink);
        max-width: 168px;
      }

      .custom-legend-wide {
        max-width: 248px;
      }

      .custom-legend-title {
        font-weight: 700;
        font-size: 0.88rem;
        margin-bottom: 0.55rem;
        line-height: 1.18;
        white-space: normal;
      }

      .custom-legend-body {
        display: flex;
        align-items: stretch;
        gap: 0.65rem;
      }

      .custom-legend-ramp {
        width: 16px;
        min-width: 16px;
        height: 144px;
        border-radius: 999px;
        box-shadow: inset 0 0 0 1px rgba(36,50,61,0.12);
      }

      .custom-legend-labels {
        height: 144px;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        font-size: 0.8rem;
        color: #4f616b;
      }

      .custom-legend-labels span {
        line-height: 1;
      }

      .custom-legend-list {
        display: flex;
        flex-direction: column;
        gap: 0.42rem;
        max-height: 220px;
        overflow-y: auto;
        padding-right: 0.15rem;
      }

      .custom-legend-item {
        display: flex;
        align-items: center;
        gap: 0.55rem;
        font-size: 0.8rem;
        color: #4f616b;
        line-height: 1.2;
      }

      .custom-legend-swatch {
        width: 14px;
        min-width: 14px;
        height: 14px;
        border-radius: 999px;
        box-shadow: inset 0 0 0 1px rgba(36,50,61,0.14);
      }

      .site-toggle-legend .control-label {
        display: block;
        margin: 0 0 0.45rem;
        color: var(--hydro-ink);
        font-weight: 700;
      }

      .site-toggle-legend .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 10px 14px;
        width: 100%;
      }

      .site-toggle-legend .form-check,
      .site-toggle-legend .checkbox {
        position: relative;
        min-height: 76px;
        margin: 0;
        padding: 0.64rem 0.75rem 0.64rem 2rem;
        background: rgba(255,255,255,0.78);
        border: 1px solid #e1ebf0;
        border-radius: 12px;
      }

      .site-toggle-legend .form-check-input,
      .site-toggle-legend .checkbox input[type='checkbox'] {
        position: absolute;
        top: 0.82rem;
        left: 0.72rem;
        margin: 0;
      }

      .site-toggle-legend .form-check-label,
      .site-toggle-legend .checkbox label {
        display: block;
        width: 100%;
        min-width: 0;
        padding-left: 0;
        font-weight: 400;
      }

      @media (max-width: 720px) {
        .site-toggle-legend .shiny-options-group {
          grid-template-columns: 1fr;
        }
      }

      .about-copy {
        font-size: 0.97rem;
        line-height: 1.68;
        color: #42545f;
        padding: 0.2rem 0;
      }

      .about-copy p {
        margin-bottom: 0.9rem;
      }

      .about-copy p:last-child {
        margin-bottom: 0;
      }

      .about-top-grid {
        display: grid;
        grid-template-columns: minmax(0, 1.45fr) minmax(260px, 0.95fr);
        gap: 1rem;
      }

      .about-reference-block {
        margin-top: 1rem;
        padding-top: 1rem;
        border-top: 1px solid rgba(193,206,214,0.62);
      }

      .about-reference-section + .about-reference-section {
        margin-top: 1rem;
      }

      .about-reference-heading {
        margin: 0 0 0.28rem;
        color: #5f8da9;
        font-size: 0.82rem;
        font-weight: 700;
      }

      .about-reference-title {
        margin: 0;
        color: #5f8da9;
        font-size: 0.92rem;
        line-height: 1.5;
      }

      .about-reference-title strong {
        color: inherit;
        font-weight: 700;
      }

      .about-reference-title a {
        color: #24323d;
        font-weight: 700;
        text-decoration: none;
      }

      .about-reference-title a:hover {
        text-decoration: underline;
      }

      .about-reference-title .about-link-chip {
        color: #24323d;
        font-size: inherit;
        font-weight: 700;
      }

      .about-citation {
        font-size: 0.98rem;
        color: #55656e;
        line-height: 1.58;
      }

      .about-section-subtitle {
        margin: 0 0 0.5rem;
        color: #24323d;
        font-size: 0.96rem;
        font-weight: 700;
      }

      .about-logo-grid {
        display: grid;
        grid-template-columns: repeat(5, minmax(110px, 1fr));
        align-items: center;
        justify-items: center;
        gap: 0.8rem 1rem;
        width: 100%;
      }

      .about-logo-card {
        display: flex;
        align-items: center;
        justify-content: center;
        min-height: 64px;
        padding: 0.1rem 0.2rem;
        width: 100%;
      }

      .about-logo-card img {
        max-width: 100%;
        max-height: 56px;
        width: auto;
        height: auto;
        object-fit: contain;
      }

      .about-logo-card .logo-nceas {
        max-height: 70px;
      }

      .about-logo-card .logo-cucires {
        max-height: 76px;
      }

      .about-logo-card .logo-nsf,
      .about-logo-card .logo-powell {
        max-height: 74px;
      }

      .about-logo-card .logo-umrr,
      .about-logo-card .logo-environment-canada {
        max-height: 80px;
      }

      .about-logo-card .logo-powell {
        max-height: 84px;
      }

      .about-logo-card .logo-umrr {
        max-height: 92px;
      }

      .about-logo-card .logo-environment-canada {
        max-height: 90px;
      }

      .about-logo-card .logo-cuahsi {
        max-height: 62px;
      }

      .about-profile {
        display: grid;
        grid-template-columns: 154px 1fr;
        gap: 0.62rem;
        align-items: start;
      }

      .about-profile-media {
        display: grid;
        gap: 0.32rem;
        align-content: start;
        width: 154px;
        min-width: 154px;
        max-width: 154px;
      }

      .about-profile-photo {
        width: 100%;
        aspect-ratio: 4 / 4.8;
        border-radius: 16px;
        object-fit: cover;
        border: 1px solid rgba(215,227,234,0.92);
        box-shadow: 0 10px 24px rgba(53,79,92,0.1);
        background: #f2f6f8;
      }

      .about-profile-placeholder {
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 1.2rem;
        background: linear-gradient(180deg, #f6f8fa, #eef3f6);
        color: #6a7982;
        font-size: 0.92rem;
        font-weight: 600;
        text-align: center;
      }

      .about-profile-name {
        margin: 0 0 0.25rem;
        color: #24323d;
        font-size: 1.12rem;
        font-weight: 700;
      }

      .about-profile-role {
        margin: 0 0 0.65rem;
        color: #5f8da9;
        font-size: 0.86rem;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
      }

      .about-profile-pronouns {
        margin: 0 0 0.4rem;
        color: #6c7b84;
        font-size: 0.86rem;
        font-style: italic;
      }

      .about-profile-text {
        color: #4f616b;
        font-size: 0.95rem;
        line-height: 1.62;
      }

      .about-profile-text p:last-child {
        margin-bottom: 0;
      }

      .about-link-row {
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        gap: 0.45rem;
      }

      .about-link-row-offset {
        padding-left: 0.4rem;
      }

      .about-link-chip {
        display: inline-flex;
        align-items: center;
        gap: 0.42rem;
        padding: 0;
        color: #385160;
        font-size: 0.74rem;
        font-weight: 400;
        line-height: 1.2;
        text-decoration: none !important;
        transition: color 0.2s ease, transform 0.2s ease;
      }

      .about-link-chip:hover {
        color: #24323d;
        transform: translateY(-1px);
      }

      .about-link-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 16px;
        height: 16px;
      }

      .about-link-icon svg {
        width: 16px;
        height: 16px;
        fill: currentColor;
      }

      .about-link-badge {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 18px;
        height: 18px;
        border-radius: 999px;
        color: #2f6c8f;
        font-size: 0.68rem;
        font-weight: 800;
        letter-spacing: 0.02em;
      }

      .about-profile-contact {
        margin-top: auto;
        padding-top: 0.65rem;
        border-top: 1px solid rgba(215,227,234,0.9);
        color: #4f616b;
        font-size: 0.9rem;
        line-height: 1.52;
      }

      .about-profile-contact p:last-child {
        margin-bottom: 0;
      }

      .about-layout {
        display: grid;
        grid-template-columns: minmax(0, 1.34fr) minmax(430px, 0.74fr);
        grid-template-areas: 'left people';
        column-gap: 1rem;
        align-items: stretch;
      }

      .about-layout > div {
        min-height: 0;
      }

      .about-left-stack {
        grid-area: left;
        display: flex;
        flex-direction: column;
        gap: 0.08rem;
      }

      .about-layout > div > .card,
      .about-layout > div > .bslib-card,
      .about-layout > div .card,
      .about-layout > div .bslib-card {
        height: 100%;
      }

      .about-layout > div .card-body,
      .about-layout > div .bslib-card-body {
        display: flex;
        flex-direction: column;
        height: 100%;
        min-height: 0;
      }

      .about-card-people .card-body,
      .about-card-people .bslib-card-body {
        padding: 0.72rem 0.82rem;
      }

      .about-card-people {
        grid-area: people;
        align-self: stretch;
      }

      .about-people-stack {
        display: flex;
        flex-direction: column;
        gap: 0;
        height: 100%;
      }

      .about-people-stack > .about-profile {
        padding-top: 0.55rem;
      }

      .about-people-stack .about-profile {
        height: auto;
      }

      .about-people-divider {
        margin: 1.9rem 0;
        border: 0;
        border-top: 1px solid rgba(215,227,234,0.9);
      }

      .about-profile {
        height: 100%;
      }

      .about-profile-text {
        display: flex;
        flex-direction: column;
        height: 100%;
      }

      .about-profile-contact {
        margin-top: auto;
      }

      @media (max-width: 767px) {
        .about-layout {
          grid-template-columns: 1fr;
          grid-template-areas: 'left' 'people';
          align-items: start;
          min-height: 0;
        }

        .about-profile {
          grid-template-columns: 1fr;
        }

        .about-profile-photo {
          max-width: 260px;
        }

        .about-logo-panel {
          height: auto;
          width: auto;
        }

        .about-logo-grid {
          grid-template-columns: repeat(auto-fit, minmax(92px, 1fr));
          gap: 0.5rem 0.65rem;
        }
      }

      a:focus-visible,
      button:focus-visible,
      .nav-link:focus-visible,
      .leaflet-bar a:focus-visible,
      .selectize-input.focus,
      .form-select:focus-visible,
      .form-control:focus-visible {
        outline: 3px solid #d5b066 !important;
        outline-offset: 2px !important;
      }

      .js-plotly-plot .plotly .modebar {
        opacity: 0;
        transition: opacity 0.2s ease;
      }

      .js-plotly-plot:hover .plotly .modebar {
        opacity: 1;
      }

      .js-plotly-plot .plotly .modebar-group {
        background: rgba(255,255,255,0.9) !important;
        border-radius: 999px !important;
        box-shadow: 0 10px 22px rgba(53,79,92,0.12);
      }

      hr {
        border-top: 1px solid var(--hydro-line);
        opacity: 0.85;
      }

      .recalculating {
        opacity: 0.55;
        transition: opacity 0.18s ease;
      }

      .shiny-output-error-validation {
        margin-top: 0.75rem;
        padding: 0.85rem 1rem;
        border-radius: 12px;
        background: #f5f8fa;
        color: #51616b;
        border: 1px solid var(--hydro-line);
      }

      .visually-hidden {
        position: absolute !important;
        width: 1px !important;
        height: 1px !important;
        padding: 0 !important;
        margin: -1px !important;
        overflow: hidden !important;
        clip: rect(0, 0, 0, 0) !important;
        white-space: nowrap !important;
        border: 0 !important;
      }

      .card, .btn, .bslib-value-box, .nav-link, .nav-tabs .nav-link {
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
            "LULC" = "major_land",
            "MAP (mm)" = "mean_annual_precip",
            "MAT (°C)" = "mean_annual_temp",
            "Mean Annual ET (kg/m²)" = "mean_annual_evapotrans",
            "Mean Peak Snow Cover (%)" = "mean_peak_snow_prop_area",
            "RBI" = "RBI"
          ),
          selected = "Name"
        )
      ),

      layout_columns(
        col_widths = c(8, 4),
        card(
          card_header("Study Sites Across North America"),
          tags$p(
            "Interactive map of study sites across North America. Use the map control to switch between climate, land cover, snow, and hydrograph metrics.",
            class = "visually-hidden"
          ),
          leafletOutput("site_map", height = 600)
        ),
        card(
          card_header("Key Metrics"),
          tags$ul(
            style = "font-size: 0.9em; line-height: 1.6; padding-left: 18px;",
            tags$li(HTML(
              "<span style='font-weight:700;'>Climate Zone</span>: Koppen-Geiger climate classification"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Land-use / Land-cover</span> (LULC): Dominant land cover type within the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Annual Precipitation</span> (MAP, mm): Average yearly precipitation across the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Annual Temperature</span> (MAT, °C): Average yearly temperature across the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Annual Evapotranspiration</span> (kg/m²): Average yearly evapotranspiration across the watershed"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Mean Peak Snow Cover</span> (%): Average of the annual maximum percent of watershed area covered by snow"
            )),
            tags$li(HTML(
              "<span style='font-weight:700;'>Richards-Baker Flashiness Index</span> (RBI): Measures how rapidly streamflow changes over time"
            ))
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
        p(
          "Start by picking four sites in the ",
          tags$strong("Precipitation & Snow Cover"),
          " panel: two with lower mean peak snow-cover values and two with higher mean peak snow-cover values. ",
          "Those same sites stay highlighted in the hydrographs and the snow-cover vs RBI comparison.",
          style = "font-size: 0.85em; color: #666;"
        ),
        conditionalPanel(
          condition = "input.activity1_tab == 'Snow Cover vs RBI'",
          selectInput(
            "snow_rbi_color_by",
            "Color full-site plot by:",
            choices = c(
              "Mean Peak Snow Cover (%)" = "mean_peak_snow_prop_area",
              "MAP (mm)" = "mean_annual_precip",
              "Land Use" = "major_land"
            ),
            selected = "mean_peak_snow_prop_area"
          )
        ),
        conditionalPanel(
          condition = "input.activity1_tab == 'Average Hydrographs'",
          checkboxInput(
            "hydrograph_log_scale",
            "Log-scale discharge axis",
            value = FALSE
          )
        ),
        uiOutput("selected_sites_display"),
        actionButton(
          "clear_sites",
          "Clear selections",
          class = "btn-outline-secondary btn-sm mt-2 w-100"
        ),
        tags$div(
          class = "mt-3",
          tags$h5("Definitions"),
          tags$table(
            class = "table table-sm",
            style = "font-size: 0.8em; line-height: 1.35;",
            tags$tbody(
              tags$tr(
                tags$th(scope = "row", "Snow Cover"),
                tags$td("Long-term average of the maximum proportion of watershed area covered by snow")
              ),
              tags$tr(
                tags$th(scope = "row", "Annual Precipitation"),
                tags$td("Long-term average of the total amount of precipitation that falls in the watershed")
              ),
              tags$tr(
                tags$th(scope = "row", "RBI"),
                tags$td("Richards-Baker Flashiness index (RBI). Describes how \"flashy\" a watershed is: lower values indicate more stable flow and higher values indicate more rapid response to precipitation inputs")
              )
            )
          )
        )
      ),

      navset_card_tab(
        id = "activity1_tab",
        nav_panel(
          "Precipitation & Snow Cover",
          div(
            style = "display: flex; flex-direction: column; gap: 1rem;",
            card(
              full_screen = TRUE,
              card_header("Use Precipitation and Mean Peak Snow Cover to Choose Four Sites"),
              tags$p(
                "Scatterplot of mean annual precipitation and mean peak snow cover for the full site set. Select up to four sites to compare across Activity 1.",
                class = "visually-hidden"
              ),
              plotlyOutput("hydroclimate_selector_plot", height = 520)
            ),
            card(
              full_screen = TRUE,
              card_header("Seasonal Precipitation and Snow Cover for Selected Sites"),
              tags$p(
                "Monthly precipitation and snow-cover profile for the selected sites.",
                class = "visually-hidden"
              ),
              div(
                style = "display: flex; flex-direction: column; gap: 0.35rem;",
                uiOutput("hydroclimate_profile_site_toggles"),
                plotlyOutput("hydroclimate_profile", height = 400)
              )
            )
          )
        ),
        nav_panel(
          "Average Hydrographs",
          div(
            style = "display: flex; flex-direction: column; gap: 1rem;",
            card(
              full_screen = TRUE,
              card_header("Compare Average Daily Discharge Patterns"),
              div(
                style = "display: flex; flex-direction: column; gap: 0.35rem;",
                plotlyOutput("hydrograph_grid", height = 650),
                uiOutput("hydrograph_grid_legend")
              )
            )
          )
        ),
        nav_panel(
          "Snow Cover vs RBI",
          card(
            full_screen = TRUE,
            card_header(
              "Mean Peak Snow Cover vs RBI Across the Full Site Set"
            ),
            tags$p(
              "Scatterplot of mean peak snow cover and Richards-Baker flashiness index for all sites.",
              class = "visually-hidden"
            ),
            plotlyOutput("snow_rbi_plot", height = 700)
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
          selectInput(
            "cl_map_background",
            "Background Map:",
            choices = activity2_background_choices,
            selected = "none"
          ),
          p(
            "Choose a North America background map and compare it with the
            mean chloride points plotted on top. Hover over a marker for site
            information, and click up to four markers to carry them into the
            seasonal chloride and discharge panel.",
            style = "font-size: 0.85em; color: #666;"
          )
        ),

        # seasonal plot controls
        conditionalPanel(
          condition = "input.activity2_tab == 'Seasonal Cl & Discharge'",
          p(
            "Use the map to select up to four sites, then toggle selected sites
            on and off here to compare one set of lines at a time.",
            style = "font-size: 0.85em; color: #666;"
          ),
          checkboxInput(
            "cl_show_discharge",
            "Overlay monthly discharge",
            value = FALSE
          )
        ),
        uiOutput("activity2_selected_sites_display"),
        actionButton(
          "clear_cl_sites",
          "Clear selections",
          class = "btn-outline-secondary btn-sm mt-2 w-100"
        )
      ),

      navset_card_tab(
        id = "activity2_tab",
        nav_panel(
          "Chloride Map",
          card(
            full_screen = TRUE,
            card_header("Stream Chloride Across North America"),
            tags$p(
              "Interactive chloride map with switchable North America raster backgrounds for MAP, cropland cover, and impervious cover, with chloride points plotted on top.",
              class = "visually-hidden"
            ),
            leafletOutput("cl_map", height = 600)
          )
        ),
        nav_panel(
          "Seasonal Cl & Discharge",
          card(
            full_screen = TRUE,
            card_header("Monthly Chloride & Discharge Patterns"),
            div(
              style = "display: flex; flex-direction: column; gap: 0.35rem;",
              uiOutput("cl_seasonal_site_toggles"),
              plotlyOutput("cl_seasonal_plot", height = 600)
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

        conditionalPanel(
          condition = "input.activity3_tab == 'Site Map'",
          p(
            "Select a site from the map.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cq_map_color_by",
            "Color sites by:",
            choices = c(
              "Climate Zone" = "Name",
              "LULC" = "major_land",
              "MAP (mm)" = "mean_annual_precip",
              "MAT (°C)" = "mean_annual_temp",
              "Mean Annual ET (kg/m²)" = "mean_annual_evapotrans",
              "Mean Peak Snow Cover (%)" = "mean_peak_snow_prop_area",
              "RBI" = "RBI"
            ),
            selected = "Name"
          ),
          uiOutput("cq_map_selected_site_label")
        ),

        # time series controls
        conditionalPanel(
          condition = "input.activity3_tab == 'Average Seasonal Hydrograph'",
          p(
            "Start with one site, identify low- and high-flow periods, and
            then overlay Cl or NO3 to compare concentration with seasonality.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cq_ts_site",
            "Select a site:",
            choices = NULL
          ),
          checkboxGroupInput(
            "cq_ts_solutes",
            "Overlay concentration:",
            choices = c("Chloride (Cl)" = "Cl", "Nitrate (NO3)" = "NO3"),
            selected = character(0)
          ),
          checkboxInput(
            "cq_ts_normalize",
            "Normalize chemistry (z-score)",
            value = FALSE
          )
        ),

        # C-Q scatter controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Relationships'",
          p(
            "Use one site at a time and compare how Cl and NO3 relate to
            discharge at that site.",
            style = "font-size: 0.85em; color: #666;"
          ),
          selectInput(
            "cq_sites",
            "Select a site:",
            choices = NULL
          ),
          checkboxGroupInput(
            "cq_solutes",
            "Solutes:",
            choices = c("Chloride (Cl)" = "Cl", "Nitrate (NO3)" = "NO3"),
            selected = character(0)
          ),
          tags$div(
            style = paste(
              "margin-top: 1rem; padding: 0.85rem 0.95rem;",
              "border: 1px solid #d7e3ea; border-radius: 12px;",
              "background: rgba(255,255,255,0.78);",
              "font-size: 0.84rem; line-height: 1.5; color: #31424c;"
            ),
            tags$div(
              "C-Q slope interpretation",
              style = "font-weight: 700; margin-bottom: 0.35rem; color: #24323d;"
            ),
            tags$div("Dilution < -0.1"),
            tags$div("-0.1 < Chemostatic < 0.1"),
            tags$div("Mobilizing > 0.1")
          )
        ),

        # histogram controls
        conditionalPanel(
          condition = "input.activity3_tab == 'C-Q Slope Distribution'",
          p(
            "Compare the national C-Q slope distributions for Cl and NO3.",
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
          "Site Map",
          layout_columns(
            col_widths = c(8, 4),
            card(
              full_screen = TRUE,
              card_header("Choose a Site from the Map"),
              tags$p(
                "Interactive map of Activity 3 sites. Click a site to carry it into the Activity 3 tabs.",
                class = "visually-hidden"
              ),
              leafletOutput("cq_site_map", height = 600)
            ),
            card(
              card_header("Key Metrics"),
              tags$ul(
                style = "font-size: 0.9em; line-height: 1.6; padding-left: 18px;",
                tags$li(HTML(
                  "<span style='font-weight:700;'>Climate Zone</span>: Koppen-Geiger climate classification"
                )),
                tags$li(HTML(
                  "<span style='font-weight:700;'>Land-use / Land-cover</span> (LULC): Dominant land cover type within the watershed"
                )),
                tags$li(HTML(
                  "<span style='font-weight:700;'>Mean Annual Precipitation</span> (MAP, mm): Average yearly precipitation across the watershed"
                )),
                tags$li(HTML(
                  "<span style='font-weight:700;'>Mean Annual Temperature</span> (MAT, °C): Average yearly temperature across the watershed"
                )),
                tags$li(HTML(
                  "<span style='font-weight:700;'>Mean Annual Evapotranspiration</span> (kg/m²): Average yearly evapotranspiration across the watershed"
                )),
                tags$li(HTML(
                  "<span style='font-weight:700;'>Mean Peak Snow Cover</span> (%): Average of the annual maximum percent of watershed area covered by snow"
                )),
                tags$li(HTML(
                  "<span style='font-weight:700;'>Richards-Baker Flashiness Index</span> (RBI): Measures how rapidly streamflow changes over time"
                ))
              )
            )
          )
        ),
        nav_panel(
          "Average Seasonal Hydrograph",
          card(
            full_screen = TRUE,
            card_header("Average Monthly Discharge & Concentration"),
            tags$div(
              style = "display: flex; flex-direction: column; gap: 0.5rem;",
              plotlyOutput("cq_timeseries_plot", height = 600),
              uiOutput("cq_timeseries_plot_legend")
            )
          )
        ),
        nav_panel(
          "C-Q Relationships",
          tags$div(
            style = "display: flex; flex-direction: column; gap: 1rem;",
            card(
              full_screen = TRUE,
              card_header(HTML(
                "log<sub>10</sub>(Concentration) vs log<sub>10</sub>(Discharge)"
              )),
              div(
                style = "display: flex; flex-direction: column; gap: 0.5rem;",
                plotlyOutput("cq_scatter_plot", height = 600),
                uiOutput("cq_scatter_legend")
              )
            ),
            card(
              card_header("Selected Trendline Fits"),
              uiOutput("cq_fit_summaries")
            )
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
  ),

  nav_panel(
    "About",
    tags$div(
      class = "about-layout",
      tags$div(
        class = "about-left-stack",
        tags$div(
          class = "about-card-main",
          card(
            card_header("About These Modules"),
            tags$div(
              class = "about-copy",
              tags$p(
                "The development of these modules was supported by the CUAHSI ",
                "HydroInformatics Innovation Fellowship."
              ),
              tags$p(
                "Datasets included in these modules come from the United States ",
                "Geological Survey (USGS), Upper Mississippi River Restoration ",
                "Program, the Long Term Ecological Research Network, and ",
                "Environment Canada."
              ),
              tags$p(
                "The datasets from these agencies have been compiled by ",
                "working group participants of a long-running synthesis group ",
                "funded by the National Center for Analysis and Synthesis ",
                "(NCEAS), ",
                "USGS Powell Center, and the National Science Foundation: ",
                tags$a(
                  tags$strong("Global River Chemistry synthesis group"),
                  href = "https://global-river-chem.github.io"
                )
              ),
              hr(),
              tags$p("Resources", class = "about-section-subtitle"),
              tags$div(
                class = "about-reference-section",
                tags$p(
                  class = "about-reference-title",
                  tags$strong("Published dataset: "),
                  tags$a(
                    "GlASS - Global Aggregation of Stream Silica",
                    href = "https://www.nature.com/articles/s41597-025-05937-2"
                  )
                ),
                tags$p(
                  class = "about-citation",
                  HTML(
                    "Jankowski, K. J., Johnson, K., Lyon, N. J., Bush, S. A., Julian, P., Sethna, L. R., McKnight, D. M., McDowell, W. H., Wymore, A. S., Kortelainen, P., Laudon, H., Heindel, R. C., Poste, A. E., Shogren, A., Worrall, F., Mosley, L., Sullivan, P. L., &amp; Carey, J. C. (2025). GlASS - Global Aggregation of Stream Silica. <em>Scientific Data, 12</em>, Article 1658."
                  )
                ),
                tags$p(
                  class = "about-reference-title",
                  tags$strong("Shiny app code: "),
                  tags$a(
                    href = "https://github.com/sidneyabush/hydro-modules",
                    class = "about-link-chip",
                    tags$span(
                      class = "about-link-icon",
                      HTML(
                        '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.5-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82a7.65 7.65 0 0 1 4 0c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8Z"/></svg>'
                      )
                    ),
                    "hydro-modules"
                  )
                )
              )
            )
          )
        ),
        tags$div(
          class = "about-logo-panel",
          card(
            card_header("Partners and Support"),
            tags$div(
              class = "about-logo-grid",
              lapply(about_logo_items, function(item) {
                tags$div(
                  class = "about-logo-card",
                  tags$img(
                    src = item$src,
                    alt = item$alt,
                    class = item$logo_class
                  )
                )
              })
            )
          )
        )
      ),
      tags$div(
        class = "about-card-people",
        card(
          card_header("People"),
          tags$div(
            class = "about-people-stack",
            tags$div(
              class = "about-profile",
              tags$div(
                class = "about-profile-media",
                tags$img(
                  src = "about_assets/keira_johnson.JPG",
                  alt = "Photo of Keira Johnson",
                  class = "about-profile-photo"
                ),
                tags$div(
                  class = "about-link-row about-link-row-offset",
                  tags$a(
                    href = "https://scholar.google.com/citations?user=pC1oFD0AAAAJ&hl=en&oi=ao",
                    class = "about-link-chip",
                    tags$span("GS", class = "about-link-badge"),
                    "Google Scholar"
                  )
                )
              ),
              tags$div(
                class = "about-profile-text",
                tags$h4("Keira Johnson, PhD", class = "about-profile-name"),
                tags$p("she/her/hers", class = "about-profile-pronouns"),
                tags$p("MODULE DEVELOPMENT", class = "about-profile-role"),
                tags$p(
                  "Keira Johnson is a postdoctoral fellow at the Cooperative ",
                  "Institute for Research in Environmental Sciences (CIRES) at ",
                  "University of Colorado Boulder. Her work focuses on ",
                  "understanding how water quality and quantity in river ",
                  "ecosystems are responding to climate and land use change."
                ),
                tags$div(
                  class = "about-profile-contact",
                  tags$p(
                    "For questions regarding the activities or synthesis project, ",
                    "please contact Keira Johnson at ",
                    tags$a(
                      "keira.johnson@colorado.edu",
                      href = "mailto:keira.johnson@colorado.edu"
                    )
                  )
                )
              )
            ),
            tags$hr(class = "about-people-divider"),
            tags$div(
              class = "about-profile",
              tags$div(
                class = "about-profile-media",
                tags$img(
                  src = "about_assets/sidney_bush.jpg",
                  alt = "Photo of Sidney Bush",
                  class = "about-profile-photo"
                ),
                tags$div(
                  class = "about-link-row about-link-row-offset",
                  tags$a(
                    href = "https://github.com/sidneyabush",
                    class = "about-link-chip",
                    tags$span(
                      class = "about-link-icon",
                      HTML(
                        '<svg viewBox="0 0 16 16" aria-hidden="true"><path d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.5-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82a7.65 7.65 0 0 1 4 0c1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8Z"/></svg>'
                      )
                    ),
                    "GitHub"
                  ),
                  tags$a(
                    href = "https://scholar.google.com/citations?user=y-LJGhcAAAAJ&hl=en&oi=ao",
                    class = "about-link-chip",
                    tags$span("GS", class = "about-link-badge"),
                    "Google Scholar"
                  ),
                  tags$a(
                    href = "https://www.linkedin.com/feed/",
                    class = "about-link-chip",
                    tags$span("in", class = "about-link-badge"),
                    "LinkedIn"
                  )
                )
              ),
              tags$div(
                class = "about-profile-text",
                tags$h4("Sidney A Bush, PhD", class = "about-profile-name"),
                tags$p("she/her/hers", class = "about-profile-pronouns"),
                tags$p("SHINYAPP DEVELOPMENT", class = "about-profile-role"),
                tags$p(
                  "Sidney Bush is a Research Scientist at Oregon State ",
                  "University. Her work combines catchment hydrology, ",
                  "critical-zone science, stream biogeochemistry, and ",
                  "large-scale data synthesis to understand how water and ",
                  "solutes move through watersheds, and how river-system ",
                  "function responds to climate variability, land use, and ",
                  "wildfire disturbance."
                ),
                tags$div(
                  class = "about-profile-contact",
                  tags$p(
                    "For questions regarding the Shiny app, please contact ",
                    "Sidney Bush at ",
                    tags$a(
                      "sidney.bush@oregonstate.edu",
                      href = "mailto:sidney.bush@oregonstate.edu"
                    )
                  )
                )
              )
            )
          )
        )
      ),
    )
  )
)

# --- Server ----------------------------------------------------------------

server <- function(input, output, session) {
  # Keep the core tables in a few shared reactives so the plots stay simple.
  harmonized_complete <- reactive({
    read_app_data("harmonized_complete.rds")
  })

  harmonized_partial <- reactive({
    read_app_data("harmonized_partial.rds")
  })

  discharge_data <- reactive({
    discharge_global
  })

  hydro_sites <- reactive({
    harmonized_complete() %>%
      filter(
        !is.na(RBI),
        !is.na(mean_peak_snow_prop_area)
      )
  })

  hydroclimate_sites <- reactive({
    hydro_sites() %>%
      filter(!is.na(mean_annual_precip), !is.na(mean_peak_snow_prop_area))
  })

  # Monthly discharge shows up in more than one activity, so build it once.
  discharge_monthly <- reactive({
    build_monthly_discharge(discharge_data())
  })

  hydro_site_colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")

  # Keep one shared site selection for all Activity 1 panels.
  selected_sites <- reactiveVal(character(0))

  toggle_selected_site <- function(site_id) {
    if (is.null(site_id) || site_id == "") {
      return()
    }
    current <- selected_sites()
    if (site_id %in% current) {
      selected_sites(setdiff(current, site_id))
    } else if (length(current) < 4) {
      selected_sites(c(current, site_id))
    }
  }

  observeEvent(event_data("plotly_click", source = "hydro_selector"), {
    click <- event_data("plotly_click", source = "hydro_selector")
    if (is.null(click)) {
      return()
    }
    toggle_selected_site(click$key)
  })

  observeEvent(event_data("plotly_click", source = "snow_rbi"), {
    click <- event_data("plotly_click", source = "snow_rbi")
    if (is.null(click)) {
      return()
    }
    toggle_selected_site(click$key)
  })

  observeEvent(input$clear_sites, {
    selected_sites(character(0))
  })

  # show selected sites in sidebar
  output$selected_sites_display <- renderUI({
    site_data <- hydro_sites()
    ids <- selected_sites()

    label <- if (length(ids) == 0) {
      tags$em("None", style = "color: #999;")
    } else {
      names <- site_data$Stream_Name[match(ids, site_data$Stream_ID)]
      tags$span(paste(names, collapse = ", "))
    }

    tags$div(
      style = "font-size: 0.85em; line-height: 1.6;",
      tags$div(
        tags$strong("Selected sites: ", style = "color: #2d2926;"),
        label,
        paste0(" (", length(ids), "/4)")
      )
    )
  })

  selected_site_palette <- reactive({
    ids <- selected_sites()
    setNames(hydro_site_colors[seq_len(length(ids))], ids)
  })

  build_hydroclimate_precip_key <- function(color) {
    tags$span(
      style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
      tags$span(
        style = paste0(
          "display: block; width: 28px; border-top: 3px solid ", color, ";"
        )
      ),
      tags$span(
        style = paste0(
          "position: absolute; left: 10px; top: -3px;",
          "width: 8px; height: 8px; border-radius: 50%;",
          "background: ", color, ";"
        )
      )
    )
  }

  build_hydroclimate_snow_key <- function(color) {
    tags$span(
      style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
      tags$span(
        style = paste0(
          "display: block; width: 28px; border-top: 2px dashed ", color, ";"
        )
      ),
      tags$span(
        style = paste0(
          "position: absolute; left: 10px; top: -5px;",
          "width: 8px; height: 8px; background: ", color, ";",
          "transform: rotate(45deg);"
        )
      )
    )
  }

  output$hydroclimate_profile_site_toggles <- renderUI({
    ids <- selected_sites()

    if (length(ids) == 0) {
      return(NULL)
    }

    site_meta <- hydroclimate_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    if (nrow(site_meta) == 0) {
      return(NULL)
    }

    palette <- selected_site_palette()
    choice_names <- lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      color <- palette[[row$Stream_ID]]
      label <- paste0(
        row$Stream_Name,
        " (Snow Cover: ",
        round(row$mean_peak_snow_prop_area * 100, 0),
        "%)"
      )

      tags$span(
        style = "display: flex; flex-direction: column; gap: 0.45rem; min-width: 0;",
        tags$span(
          style = "font-size: 0.84rem; color: #31424c; line-height: 1.3; white-space: normal;",
          label
        ),
        tags$span(
          style = "display: flex; align-items: center; gap: 1rem; flex-wrap: wrap;",
          tags$span(
            style = "display: inline-flex; align-items: center; gap: 8px; min-width: 0;",
            build_hydroclimate_precip_key(color),
            tags$span(
              style = "font-size: 0.8rem; color: #4f616b;",
              "Precipitation"
            )
          ),
          tags$span(
            style = "display: inline-flex; align-items: center; gap: 8px; min-width: 0;",
            build_hydroclimate_snow_key(color),
            tags$span(
              style = "font-size: 0.8rem; color: #4f616b;",
              "Snow Cover"
            )
          )
        )
      )
    })

    tags$div(
      class = "site-toggle-legend",
      checkboxGroupInput(
        "hydroclimate_profile_site_ids",
        "Display sites:",
        choiceNames = choice_names,
        choiceValues = site_meta$Stream_ID,
        selected = ids,
        width = "100%"
      )
    )
  })

  hydroclimate_profile_site_ids <- reactive({
    ids <- selected_sites()

    if (length(ids) == 0) {
      return(ids)
    }

    displayed_ids <- input$hydroclimate_profile_site_ids
    if (is.null(displayed_ids)) {
      return(ids)
    }

    intersect(ids, displayed_ids)
  })

  format_legend_number <- function(x, digits = 0) {
    formatC(x, format = "f", digits = digits, drop0trailing = TRUE)
  }

  build_numeric_legend <- function(title, values, legend_colors, digits = 0, n_breaks = 6) {
    value_range <- range(values, na.rm = TRUE)
    legend_breaks <- pretty(value_range, n = n_breaks)
    legend_breaks <- legend_breaks[
      legend_breaks >= value_range[1] &
        legend_breaks <= value_range[2]
    ]

    if (length(legend_breaks) < 2) {
      legend_breaks <- sort(unique(round(value_range, digits + 1)))
    }

    if (length(legend_breaks) < 2) {
      legend_breaks <- c(value_range[1], value_range[2])
    }

    legend_labels <- rev(format_legend_number(legend_breaks, digits = digits))

    as.character(
      tags$div(
        class = "custom-legend",
        tags$div(title, class = "custom-legend-title"),
        tags$div(
          class = "custom-legend-body",
          tags$div(
            class = "custom-legend-ramp",
            style = paste0(
              "background: linear-gradient(to top, ",
              paste(legend_colors, collapse = ", "),
              ");"
            )
          ),
          tags$div(
            class = "custom-legend-labels",
            lapply(legend_labels, tags$span)
          )
        )
      )
    )
  }

  build_custom_numeric_legend <- function(title, legend_colors, legend_labels) {
    legend_labels <- rev(legend_labels)

    as.character(
      tags$div(
        class = "custom-legend",
        tags$div(title, class = "custom-legend-title"),
        tags$div(
          class = "custom-legend-body",
          tags$div(
            class = "custom-legend-ramp",
            style = paste0(
              "background: linear-gradient(to top, ",
              paste(legend_colors, collapse = ", "),
              ");"
            )
          ),
          tags$div(
            class = "custom-legend-labels",
            lapply(legend_labels, tags$span)
          )
        )
      )
    )
  }

  build_categorical_legend <- function(title, legend_items, label_overrides = NULL, extra_class = NULL) {
    as.character(
      tags$div(
        class = trimws(paste("custom-legend", extra_class)),
        tags$div(title, class = "custom-legend-title"),
        tags$div(
          class = "custom-legend-list",
          lapply(
            names(legend_items),
            function(label) {
              display_label <- if (!is.null(label_overrides) && label %in% names(label_overrides)) {
                label_overrides[[label]]
              } else {
                label
              }
              tags$div(
                class = "custom-legend-item",
                tags$span(
                  class = "custom-legend-swatch",
                  style = paste0("background:", legend_items[[label]], ";")
                ),
                tags$span(display_label)
              )
            }
          )
        )
      )
    )
  }

  # --- Map -----------------------------------------------------------------
  activity3_available_sites <- reactive({
    has_q <- unique(discharge_data()$Stream_ID)

    cq_paired_data() %>%
      filter(variable %in% c("Cl", "NO3")) %>%
      count(Stream_ID, LTER, Stream_Name, variable, name = "n_paired") %>%
      filter(n_paired >= 3) %>%
      group_by(Stream_ID, LTER, Stream_Name) %>%
      summarise(
        has_cl = any(variable == "Cl"),
        has_no3 = any(variable == "NO3"),
        .groups = "drop"
      ) %>%
      filter(Stream_ID %in% has_q, has_cl, has_no3) %>%
      select(Stream_ID, LTER, Stream_Name) %>%
      distinct() %>%
      arrange(LTER, Stream_Name)
  })

  activity3_map_selected_site <- reactiveVal(NULL)

  build_overview_style_map <- function(map_data,
                                       selected_var,
                                       clickable = FALSE,
                                       highlighted_site_id = NULL,
                                       show_popups = TRUE,
                                       initial_view = NULL) {
    map_data <- map_data %>%
      filter(!is.na(Latitude), !is.na(Longitude)) %>%
      mutate(major_land_display = clean_land_use_label(major_land))

    map_data <- if (selected_var %in% c("major_land", "Name")) {
      map_data %>%
        filter(
          !is.na(.data[[selected_var]]),
          trimws(as.character(.data[[selected_var]])) != ""
        )
    } else {
      map_data %>%
        filter(is.finite(.data[[selected_var]]))
    }

    req(nrow(map_data) > 0)

    if (selected_var == "major_land") {
      map_data <- map_data %>%
        mutate(major_land = major_land_display)
    }

    color_var <- map_data[[selected_var]]
    if (selected_var %in% c("snow_cover", "mean_peak_snow_prop_area", "peak_snow_prop_area")) {
      color_var <- color_var * 100
    }

    numeric_legend_specs <- list(
      "mean_annual_precip" = list(colors = precip_palette, digits = 0),
      "mean_annual_temp" = list(colors = rev(RColorBrewer::brewer.pal(9, "RdYlBu")), digits = 1),
      "mean_annual_evapotrans" = list(colors = RColorBrewer::brewer.pal(9, "Oranges"), digits = 0),
      "snow_cover" = list(colors = snow_palette, digits = 0),
      "mean_peak_snow_prop_area" = list(colors = snow_palette, digits = 0),
      "peak_snow_prop_area" = list(colors = snow_palette, digits = 0),
      "RBI" = list(colors = RColorBrewer::brewer.pal(9, "Greens"), digits = 2),
      "recession_slope" = list(colors = RColorBrewer::brewer.pal(9, "Greens"), digits = 2)
    )
    is_numeric_map_var <- selected_var %in% names(numeric_legend_specs)
    numeric_spec <- if (is_numeric_map_var) numeric_legend_specs[[selected_var]] else NULL

    legend_titles <- c(
      "Name" = "Climate Zone",
      "snow_cover" = "Snow Cover (%)",
      "mean_annual_precip" = "MAP (mm)",
      "mean_annual_temp" = "MAT (°C)",
      "mean_annual_evapotrans" = "Mean Annual ET (kg/m²)",
      "mean_peak_snow_prop_area" = "Mean Peak Snow Cover (%)",
      "peak_snow_prop_area" = "Peak Snow Cover (%)",
      "RBI" = "RBI",
      "recession_slope" = "RCS",
      "major_land" = "LULC"
    )
    legend_title <- switch(
      selected_var,
      "mean_peak_snow_prop_area" = HTML("Mean Peak Snow Cover<br>(%)"),
      "peak_snow_prop_area" = HTML("Peak Snow<br>Cover (%)"),
      legend_titles[[selected_var]]
    )

    distinct_colors <- c(
      "#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00",
      "#ffff33", "#a65628", "#f781bf", "#66c2a5", "#fc8d62",
      "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494",
      "#b3b3b3", "#1b9e77", "#d95f02", "#7570b3", "#e7298a"
    )

    pal <- switch(
      selected_var,
      "mean_annual_precip" = colorNumeric(numeric_legend_specs[["mean_annual_precip"]]$colors, domain = color_var),
      "mean_annual_temp" = colorNumeric(numeric_legend_specs[["mean_annual_temp"]]$colors, domain = color_var),
      "mean_annual_evapotrans" = colorNumeric(numeric_legend_specs[["mean_annual_evapotrans"]]$colors, domain = color_var),
      "snow_cover" = colorNumeric(numeric_legend_specs[["snow_cover"]]$colors, domain = color_var),
      "mean_peak_snow_prop_area" = colorNumeric(numeric_legend_specs[["mean_peak_snow_prop_area"]]$colors, domain = color_var),
      "peak_snow_prop_area" = colorNumeric(numeric_legend_specs[["peak_snow_prop_area"]]$colors, domain = color_var),
      "RBI" = colorNumeric(numeric_legend_specs[["RBI"]]$colors, domain = color_var),
      "recession_slope" = colorNumeric(numeric_legend_specs[["recession_slope"]]$colors, domain = color_var),
      "Name" = function(values) {
        named_color_lookup(values, palette = climate_zone_colors, default = "#b9c7d3")
      },
      "major_land" = function(values) {
        named_color_lookup(values, palette = land_use_colors, default = land_use_colors[["Other / Unclassified"]])
      },
      colorFactor(
        palette = rep(distinct_colors, length.out = length(unique(color_var))),
        domain = color_var
      )
    )

    map_fill_color <- if (selected_var == "major_land") {
      named_color_lookup(color_var, palette = land_use_colors, default = land_use_colors[["Other / Unclassified"]])
    } else if (selected_var == "Name") {
      named_color_lookup(color_var, palette = climate_zone_colors, default = "#b9c7d3")
    } else {
      unname(pal(color_var))
    }

    map_data <- map_data %>%
      mutate(
        map_fill_color = map_fill_color,
        popup_html = paste0(
          "<b>", Stream_Name, "</b><br>",
          "LTER: ", LTER, "<br>",
          "LULC: ", major_land_display, "<br>",
          "RBI: ", round(RBI, 3), "<br>",
          "RCS: ", round(recession_slope, 3), "<br>",
          "Climate: ", Name, "<br>",
          "Snow Cover: ", round(snow_cover * 100, 0), "%<br>",
          "Mean Peak Snow Cover: ", round(mean_peak_snow_prop_area * 100, 0), "%<br>",
          "Peak Snow Cover: ", round(peak_snow_prop_area * 100, 0), "%<br>",
          "Mean Annual Precip: ", round(mean_annual_precip, 1), " mm<br>",
          "MAT: ", round(mean_annual_temp, 1), " °C<br>",
          "Mean Annual ET: ", round(mean_annual_evapotrans, 1), " kg/m²"
        )
      )

    lng_bounds <- range(map_data$Longitude, na.rm = TRUE) + c(-6, 6)
    lat_bounds <- range(map_data$Latitude, na.rm = TRUE) + c(-4, 4)

    m <- leaflet(
      map_data,
      options = leafletOptions(
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )
    ) %>%
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        options = tileOptions(opacity = 0.9)
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = tileOptions(opacity = 0.75)
      )

    if (!is.null(initial_view)) {
      m <- m %>%
        setView(
          lng = initial_view$lng,
          lat = initial_view$lat,
          zoom = initial_view$zoom
        )
    } else {
      m <- m %>%
        fitBounds(lng_bounds[1], lat_bounds[1], lng_bounds[2], lat_bounds[2])
    }

    m <- m %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE))

    if (selected_var %in% c("major_land", "Name")) {
      class_palette <- if (selected_var == "major_land") land_use_colors else climate_zone_colors
      class_var <- if (selected_var == "major_land") "major_land" else "Name"
      class_levels <- names(class_palette)[
        names(class_palette) %in% unique(as.character(map_data[[class_var]]))
      ]

      for (class_label in class_levels) {
        class_data <- map_data %>% filter(.data[[class_var]] == class_label)
        if (nrow(class_data) == 0) {
          next
        }

        class_color <- unname(class_palette[[class_label]])
        class_fill_opacity <- if (identical(class_label, "Bare")) 0.94 else 0.78

        marker_args <- list(
          data = class_data,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 6.5,
          stroke = TRUE,
          fill = TRUE,
          fillColor = class_color,
          color = "#7f878d",
          weight = 0.9,
          opacity = 0.85,
          fillOpacity = class_fill_opacity,
          label = ~Stream_Name,
          group = class_label
        )
        if (isTRUE(show_popups)) {
          marker_args$popup <- ~popup_html
        }
        if (isTRUE(clickable)) {
          marker_args$layerId <- class_data$Stream_ID
        }

        m <- do.call(addCircleMarkers, c(list(map = m), marker_args))
      }
    } else {
      marker_args <- list(
        map = m,
        data = map_data,
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6.5,
        stroke = TRUE,
        fill = TRUE,
        fillColor = ~map_fill_color,
        color = "#7f878d",
        weight = 0.9,
        opacity = 0.85,
        fillOpacity = 0.78,
        label = ~Stream_Name
      )
      if (isTRUE(show_popups)) {
        marker_args$popup <- ~popup_html
      }
      if (isTRUE(clickable)) {
        marker_args$layerId <- ~Stream_ID
      }

      m <- do.call(addCircleMarkers, marker_args)
    }

    if (!is.null(highlighted_site_id) && highlighted_site_id %in% map_data$Stream_ID) {
      selected_point <- map_data %>%
        filter(Stream_ID == highlighted_site_id) %>%
        slice(1)

      m <- m %>%
        addCircleMarkers(
          data = selected_point,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 10,
          stroke = TRUE,
          fill = FALSE,
          color = "#24323d",
          weight = 3.2,
          opacity = 0.96
        )
    }

    if (is_numeric_map_var) {
      m %>%
        addControl(
          html = build_numeric_legend(
            title = legend_title,
            values = color_var,
            legend_colors = numeric_spec$colors,
            digits = numeric_spec$digits
          ),
          position = "bottomleft"
        )
    } else {
      legend_levels <- if (selected_var == "major_land") {
        land_use_legend_levels(color_var)
      } else if (selected_var == "Name") {
        names(climate_zone_colors)[
          names(climate_zone_colors) %in% unique(as.character(color_var))
        ]
      } else {
        sort(unique(as.character(color_var)))
      }
      legend_items <- if (selected_var == "major_land") {
        setNames(
          named_color_lookup(
            legend_levels,
            palette = land_use_colors,
            default = land_use_colors[["Other / Unclassified"]]
          ),
          legend_levels
        )
      } else if (selected_var == "Name") {
        setNames(unname(climate_zone_colors[legend_levels]), legend_levels)
      } else {
        setNames(unname(pal(legend_levels)), legend_levels)
      }

      m %>%
        addControl(
          html = build_categorical_legend(
            title = legend_title,
            legend_items = legend_items,
            label_overrides = if (selected_var == "major_land") {
              c("Grassland / Shrubland" = "Grassland\u00A0/\u00A0Shrubland")
            } else {
              NULL
            },
            extra_class = if (selected_var == "major_land") {
              "custom-legend-wide"
            } else {
              NULL
            }
          ),
          position = "bottomleft"
        )
    }
  }

  output$site_map <- renderLeaflet({
    req(input$map_color_by)
    build_overview_style_map(
      map_data = harmonized_partial(),
      selected_var = input$map_color_by
    )
  })

  output$cq_site_map <- renderLeaflet({
    req(input$cq_map_color_by)

    map_site_ids <- activity3_available_sites()$Stream_ID
    map_data <- harmonized_partial() %>%
      filter(Stream_ID %in% map_site_ids)
    current_map_view <- isolate({
      center <- input$cq_site_map_center
      zoom <- input$cq_site_map_zoom
      if (is.null(center) || is.null(zoom)) {
        NULL
      } else {
        list(lng = center$lng, lat = center$lat, zoom = zoom)
      }
    })

    build_overview_style_map(
      map_data = map_data,
      selected_var = input$cq_map_color_by,
      clickable = TRUE,
      highlighted_site_id = activity3_map_selected_site(),
      show_popups = FALSE,
      initial_view = current_map_view
    )
  })

  observeEvent(input$cq_site_map_marker_click, {
    click <- input$cq_site_map_marker_click
    req(click$id)
    activity3_map_selected_site(click$id)
    updateSelectInput(session, "cq_ts_site", selected = click$id)
    updateSelectInput(session, "cq_sites", selected = click$id)
  })

  output$cq_map_selected_site_label <- renderUI({
    selected_id <- activity3_map_selected_site()

    if (is.null(selected_id) || !nzchar(selected_id)) {
      return(
        tags$div(
          style = "margin-top: 0.8rem; font-size: 0.84rem; color: #5d6d76;",
          tags$strong("Selected site: "),
          "none"
        )
      )
    }

    selected_site <- harmonized_partial() %>%
      filter(Stream_ID == selected_id) %>%
      select(Stream_Name, LTER) %>%
      distinct() %>%
      slice(1)

    if (nrow(selected_site) == 0) {
      return(NULL)
    }

    tags$div(
      style = "margin-top: 0.8rem; font-size: 0.84rem; color: #31424c;",
      tags$strong("Selected site: "),
      selected_site$Stream_Name,
      " [",
      selected_site$LTER,
      "]"
    )
  })

  # --- Activity 1: Hydroclimate selector -----------------------------------

  hydroclimate_profile_data <- reactive({
    ids <- hydroclimate_profile_site_ids()

    if (length(ids) < 1) {
      return(NULL)
    }

    site_meta <- hydroclimate_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    bind_rows(lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      tibble(
        Stream_ID = row$Stream_ID,
        Stream_Name = row$Stream_Name,
        LTER = row$LTER,
        mean_peak_snow_prop_area = row$mean_peak_snow_prop_area,
        month = 1:12,
        month_label = month_labels,
        precip_mm = extract_monthly_site_values(
          row,
          prefix = "precip_",
          suffix = "_mm_per_day"
        ) * unname(days_in_month[month_keys]),
        snow_cover = extract_monthly_site_values(
          row,
          prefix = "snow_",
          suffix = "_avg_prop_area"
        ),
        snow_cover_pct = extract_monthly_site_values(
          row,
          prefix = "snow_",
          suffix = "_avg_prop_area"
        ) * 100
      )
    }))
  })

  output$hydroclimate_selector_plot <- renderPlotly({
    plot_data <- hydroclimate_sites() %>%
      mutate(is_highlighted = Stream_ID %in% selected_sites())

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No hydroclimate data are available for the site selector",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    hover_text <- paste0(
      "<b>", plot_data$Stream_Name, "</b><br>",
      "LTER: ", plot_data$LTER, "<br>",
      "MAP: ", round(plot_data$mean_annual_precip, 0), " mm/yr<br>",
      "Snow Cover: ", round(plot_data$mean_peak_snow_prop_area * 100, 0), "%<br>",
      "RBI: ", round(plot_data$RBI, 3)
    )

    selector_label_x <- max(plot_data$mean_annual_precip, na.rm = TRUE) * 0.98
    selector_x_mid <- mean(range(plot_data$mean_annual_precip, na.rm = TRUE))
    selector_y_mid <- mean(range(plot_data$mean_peak_snow_prop_area, na.rm = TRUE))

    p <- ggplot(
      plot_data,
      aes(
        x = mean_annual_precip,
        y = mean_peak_snow_prop_area,
        text = hover_text,
        key = Stream_ID
      )
    ) +
      geom_hline(
        yintercept = 0.25,
        color = "#5d6d76",
        linewidth = 0.45,
        linetype = "dashed"
      ) +
      geom_point(
        shape = 21,
        color = "#7f878d",
        fill = module_colors[["secondary"]],
        size = 3.3,
        stroke = 0.28,
        alpha = 0.8
      ) +
      labs(
        x = "Mean Annual Precipitation (mm/yr)",
        y = "Mean Peak Snow Cover (%)"
      ) +
      base_plot_theme +
      scale_x_continuous(expand = expansion(mult = c(0.07, 0.22))) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0.08, 0.14))
      )

    selected_site_annotations <- list()
    if (any(plot_data$is_highlighted)) {
      highlighted <- plot_data %>%
        filter(is_highlighted) %>%
        mutate(
          selector_label = paste0(
            Stream_Name,
            " (Snow Cover: ",
            round(mean_peak_snow_prop_area * 100, 0),
            "%)"
          )
        )
      selected_site_annotations <- lapply(seq_len(nrow(highlighted)), function(i) {
        row <- highlighted[i, , drop = FALSE]
        is_right_side <- row$mean_annual_precip > selector_x_mid
        is_upper_half <- row$mean_peak_snow_prop_area > selector_y_mid

        list(
          x = row$mean_annual_precip,
          y = row$mean_peak_snow_prop_area,
          text = paste0("<b>", row$selector_label, "</b>"),
          showarrow = FALSE,
          xanchor = if (is_right_side) "right" else "left",
          yanchor = if (is_upper_half) "top" else "bottom",
          xshift = if (is_right_side) -8 else 8,
          yshift = if (is_upper_half) -8 else 8,
          font = list(size = 11, color = "#24323d"),
          bgcolor = "rgba(255,255,255,0.76)",
          bordercolor = "rgba(36,50,61,0.16)",
          borderwidth = 1,
          borderpad = 2
        )
      })
      p <- p +
        geom_point(
          data = highlighted,
          aes(
            x = mean_annual_precip,
            y = mean_peak_snow_prop_area
          ),
          shape = 21,
          color = "#7f878d",
          fill = module_colors[["primary"]],
          size = 5.2,
          stroke = 0.4,
          alpha = 1,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    }

    ggplotly(p, tooltip = "text", source = "hydro_selector") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        showlegend = FALSE,
        margin = list(r = 40),
        annotations = c(
          selected_site_annotations,
          list(
            list(
              x = selector_label_x,
              y = 0.25,
              text = "25% mean peak snow cover",
              showarrow = FALSE,
              xanchor = "right",
              yanchor = "bottom",
              yshift = 4,
              font = list(size = 11, color = "#5d6d76"),
              bgcolor = "rgba(255,255,255,0.78)",
              bordercolor = "rgba(93,109,118,0.18)",
              borderwidth = 1,
              borderpad = 3
            )
          )
        ),
        title = FALSE
      ) %>%
      polish_plotly(register_click = TRUE)
  })

  output$hydroclimate_profile <- renderPlotly({
    plot_data <- hydroclimate_profile_data()

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      empty_text <- if (length(selected_sites()) == 0) {
        "Select sites above to compare monthly precipitation and snow cover"
      } else {
        "Choose at least one displayed site to show monthly precipitation and snow cover"
      }

      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = empty_text,
              font = list(color = "#666", size = 13)
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            yaxis2 = list(visible = FALSE),
            paper_bgcolor = plotly_bg$paper_bgcolor,
            plot_bgcolor = plotly_bg$plot_bgcolor,
            showlegend = FALSE
          ) %>%
          polish_plotly()
      )
    }

    palette <- selected_site_palette()
    p <- plot_ly()
    for (site_id in hydroclimate_profile_site_ids()) {
      site_data <- filter(plot_data, Stream_ID == site_id)
      clr <- palette[[site_id]]
      if (nrow(site_data) == 0) {
        next
      }
      label <- paste0(
        site_data$Stream_Name[1],
        " (Snow Cover: ",
        round(site_data$mean_peak_snow_prop_area[1] * 100, 0),
        "%)"
      )

      p <- p %>%
        add_trace(
          data = site_data,
          x = ~month,
          y = ~precip_mm,
          type = "scatter",
          mode = "lines+markers",
          name = paste0(label, " — P"),
          showlegend = FALSE,
          line = list(color = clr, width = 2.5),
          marker = list(color = clr, size = 6),
          hovertemplate = paste0(
            label,
            "<br>Month: %{x}<br>",
            "Precipitation: %{y:.1f} mm/month<extra></extra>"
          )
        ) %>%
        add_trace(
          data = site_data,
          x = ~month,
          y = ~snow_cover_pct,
          type = "scatter",
          mode = "lines+markers",
          name = paste0(label, " — Snow Cover"),
          showlegend = FALSE,
          yaxis = "y2",
          line = list(color = clr, width = 2, dash = "dash"),
          marker = list(color = clr, size = 5, symbol = "diamond"),
          hovertemplate = paste0(
            label,
            "<br>Month: %{x}<br>",
            "Snow Cover: %{y:.0f}%<extra></extra>"
          )
        )
    }

    p %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        xaxis = list(
          title = "Month",
          tickmode = "array",
          tickvals = 1:12,
          ticktext = month_labels,
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = "Precipitation (mm/month)",
          gridcolor = "#d4e3f0"
        ),
        yaxis2 = list(
          title = "Snow Cover (%)",
          overlaying = "y",
          side = "right",
          showgrid = FALSE
        ),
        showlegend = FALSE,
        margin = list(r = 80, b = 55),
        hovermode = "closest"
      ) %>%
      polish_plotly()
  })

  # --- Hydrograph ----------------------------------------------------------

  # hydrograph data reacts to the shared site selections
  hydrograph_data <- reactive({
    all_selected <- selected_sites()

    if (length(all_selected) < 1) {
      return(NULL)
    }

    selected_site_meta <- hydro_sites() %>%
      filter(Stream_ID %in% all_selected) %>%
      select(
        Stream_ID,
        Stream_Name,
        LTER,
        mean_peak_snow_prop_area,
        RBI
      )

    discharge_data() %>%
      filter(Stream_ID %in% all_selected) %>%
      build_daily_average_discharge() %>%
      left_join(selected_site_meta, by = c("Stream_ID", "Stream_Name", "LTER")) %>%
      mutate(
        site_label = paste0(
          Stream_Name,
          " (Snow Cover: ",
          round(mean_peak_snow_prop_area * 100, 0),
          "%)"
        )
      )
  })

  # build the shared color map for both plots
  hydro_color_map <- reactive({
    selected_site_palette()
  })

  # --- Average daily hydrograph comparison ---
  output$hydrograph_grid <- renderPlotly({
    plot_data <- hydrograph_data()
    log_scale <- isTRUE(input$hydrograph_log_scale)

    if (is.null(plot_data) || nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select sites in the precipitation and snow panel to compare average daily hydrographs",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    if (log_scale) {
      plot_data <- plot_data %>%
        filter(is.finite(mean_Q_cms), mean_Q_cms > 0)

      if (nrow(plot_data) == 0) {
        return(
          plotly_empty() %>%
            layout(
              title = list(
                text = "No positive discharge values are available for log scaling",
                font = list(color = "#666", size = 14)
              )
            )
        )
      }
    }

    colors <- hydro_color_map()
    site_meta <- plot_data %>%
      select(Stream_ID, Stream_Name, LTER, site_label, RBI, mean_peak_snow_prop_area) %>%
      distinct() %>%
      mutate(order = match(Stream_ID, selected_sites())) %>%
      arrange(order)

    p <- plot_ly()
    for (i in seq_len(nrow(site_meta))) {
      row <- site_meta[i, ]
      d <- filter(plot_data, Stream_ID == row$Stream_ID)
      clr <- colors[[row$Stream_ID]]

      p <- p %>%
        add_trace(
          data = d,
          x = ~day_of_year,
          y = ~mean_Q_cms,
          customdata = ~month_day_label,
          type = "scatter",
          mode = "lines",
          name = row$site_label,
          showlegend = FALSE,
          line = list(color = clr, width = 3),
          hovertemplate = paste0(
            "<b>",
            row$site_label,
            "</b><br>Date: %{customdata}<br>",
            "Mean daily Q: %{y:.3f} cms<br>",
            "RBI: ",
            round(row$RBI, 3),
            "<extra></extra>"
          )
        )
    }

    p %>%
      layout(
        title = list(
          text = "Selected Sites: Average Daily Hydrographs",
          font = list(size = 17, color = "#24323d")
        ),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        xaxis = list(
          title = list(text = "Day of Year", font = list(size = 14)),
          tickmode = "array",
          tickvals = month_start_days,
          ticktext = month_labels,
          tickfont = list(size = 12),
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = list(
            text = if (log_scale) {
              "Mean Daily Discharge (cms, log scale)"
            } else {
              "Mean Daily Discharge (cms)"
            },
            font = list(size = 14)
          ),
          type = if (log_scale) "log" else "linear",
          tickfont = list(size = 12),
          gridcolor = "#d4e3f0"
        ),
        showlegend = FALSE,
        margin = list(t = 60, r = 40, b = 70, l = 60),
        hovermode = "closest",
        hoverdistance = 12
      ) %>%
      polish_plotly()
  })

  output$hydrograph_grid_legend <- renderUI({
    ids <- selected_sites()
    if (length(ids) == 0) {
      return(NULL)
    }

    site_meta <- hydro_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    colors <- hydro_color_map()

    build_hydro_key <- function(color) {
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px solid ", color, ";"
          )
        )
      )
    }

    legend_items <- lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      color <- colors[[row$Stream_ID]]
      label <- paste0(
        row$Stream_Name,
        " (Snow Cover: ",
        round(row$mean_peak_snow_prop_area * 100, 0),
        "%)"
      )

      tags$div(
        style = "display: flex; align-items: center; gap: 8px; min-width: 0;",
        build_hydro_key(color),
        tags$span(
          style = "font-size: 0.84rem; color: #31424c; line-height: 1.25; white-space: normal;",
          label
        )
      )
    })

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 20px;",
        "row-gap: 8px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      ),
      legend_items
    )
  })

  # --- Mean Peak Snow Cover vs RBI scatter for all Activity 1 sites --------

  all_highlighted <- reactive({
    selected_sites()
  })

  output$snow_rbi_plot <- renderPlotly({
    color_var_name <- if (is.null(input$snow_rbi_color_by)) {
      "mean_peak_snow_prop_area"
    } else {
      input$snow_rbi_color_by
    }
    color_var_label <- c(
      "mean_peak_snow_prop_area" = "Mean Peak Snow Cover (%)",
      "mean_annual_precip" = "MAP (mm)",
      "major_land" = "LULC"
    )[[color_var_name]]

    plot_data <- hydro_sites() %>%
      mutate(
        is_highlighted = Stream_ID %in% all_highlighted(),
        major_land_display = clean_land_use_label(major_land),
        color_value = if (color_var_name == "major_land") {
          major_land_display
        } else {
          .data[[color_var_name]]
        }
      ) %>%
      filter(!is.na(color_value))

    if (nrow(plot_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No sites are available for the Mean Peak Snow Cover/RBI comparison",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    hover_text <- paste0(
      "<b>",
      plot_data$Stream_Name,
      "</b><br>",
      "LTER: ",
      plot_data$LTER,
      "<br>",
      "Snow Cover: ",
      round(plot_data$mean_peak_snow_prop_area * 100, 0),
      "%<br>",
      "MAP: ",
      round(plot_data$mean_annual_precip, 0),
      " mm/yr<br>",
      "Land Use: ",
      plot_data$major_land_display,
      "<br>",
      "RBI: ",
      round(plot_data$RBI, 3)
    )

    snow_rbi_x_mid <- mean(range(plot_data$RBI, na.rm = TRUE))
    snow_rbi_y_mid <- mean(range(plot_data$mean_peak_snow_prop_area, na.rm = TRUE))

    p <- ggplot(
      plot_data,
      aes(
        x = RBI,
        y = mean_peak_snow_prop_area,
        fill = color_value,
        text = hover_text,
        key = Stream_ID
      )
    ) +
      geom_point(
        shape = 21,
        color = "#7f878d",
        size = 3,
        stroke = 0.28,
        alpha = 0.78
      ) +
      labs(
        x = "RBI",
        y = "Mean Peak Snow Cover (%)",
        fill = color_var_label
      ) +
      base_plot_theme +
      scale_x_continuous(expand = expansion(mult = c(0.07, 0.22))) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0.08, 0.14))
      )

    if (color_var_name == "major_land") {
      land_levels <- land_use_legend_levels(plot_data$color_value)
      fallback_colors <- c(
        "#1b9e77",
        "#d95f02",
        "#7570b3",
        "#e7298a",
        "#66a61e",
        "#e6ab02",
        "#a6761d",
        "#666666"
      )
      land_palette <- setNames(
        rep(fallback_colors, length.out = length(land_levels)),
        land_levels
      )
      matched_levels <- intersect(names(land_use_colors), names(land_palette))
      land_palette[matched_levels] <- land_use_colors[matched_levels]

      p <- p +
        scale_fill_manual(
          values = land_palette,
          breaks = land_levels,
          na.translate = FALSE
        )
    } else if (color_var_name == "mean_peak_snow_prop_area") {
      p <- p +
        scale_fill_gradientn(
          colours = snow_palette,
          labels = scales::percent_format(accuracy = 1)
        )
    } else if (color_var_name == "mean_annual_precip") {
      p <- p +
        scale_fill_gradientn(
          colours = precip_palette,
          labels = scales::label_number(big.mark = ",", accuracy = 1)
        )
    } else {
      p <- p +
        scale_fill_viridis_c()
    }

    label_annotations <- list()
    if (any(plot_data$is_highlighted)) {
      highlight_df <- filter(plot_data, is_highlighted) %>%
        mutate(
          hover = paste0(
            "<b>",
            Stream_Name,
            "</b><br>",
            "LTER: ",
            LTER,
            "<br>",
            "Snow Cover: ",
            round(mean_peak_snow_prop_area * 100, 0),
            "%<br>",
            "RBI: ",
            round(RBI, 3)
          )
        )
      label_annotations <- lapply(seq_len(nrow(highlight_df)), function(i) {
        row <- highlight_df[i, , drop = FALSE]
        is_right_side <- row$RBI > snow_rbi_x_mid
        is_upper_half <- row$mean_peak_snow_prop_area > snow_rbi_y_mid

        list(
          x = row$RBI,
          y = row$mean_peak_snow_prop_area,
          text = paste0("<b>", row$Stream_Name, "</b>"),
          showarrow = FALSE,
          xanchor = if (is_right_side) "right" else "left",
          yanchor = if (is_upper_half) "top" else "bottom",
          xshift = if (is_right_side) -8 else 8,
          yshift = if (is_upper_half) -8 else 8,
          font = list(size = 11, color = "#24323d"),
          bgcolor = "rgba(255,255,255,0.76)",
          bordercolor = "rgba(36,50,61,0.16)",
          borderwidth = 1,
          borderpad = 2
        )
      })
      p <- p +
        geom_point(
          data = highlight_df,
          aes(
            x = RBI,
            y = mean_peak_snow_prop_area,
            fill = color_value,
            text = hover
          ),
          shape = 21,
          color = "#7f878d",
          size = 5,
          stroke = 0.4,
          alpha = 1,
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    }

    ggplotly(p, tooltip = "text", source = "snow_rbi") %>%
      layout(
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = right_side_legend(font_size = 10),
        annotations = label_annotations,
        margin = list(r = 170),
        title = FALSE
      ) %>%
      polish_plotly(register_click = TRUE)
  })

  # --- Activity 2: Stream Salinity ------------------------------------------

  # sites from harmonized partial that have Cl data
  cl_sites <- reactive({
    harmonized_partial() %>%
      filter(!is.na(mean_Cl_mgL), !is.na(Latitude), !is.na(Longitude)) %>%
      filter(!Stream_Name %in% chloride_excluded_stream_names)
  })

  cl_monthly <- reactive({
    read_app_data("cl_monthly.rds")
  })

  activity2_selected_sites <- reactiveVal(character(0))

  activity2_selected_site_palette <- reactive({
    ids <- activity2_selected_sites()
    setNames(activity2_site_colors[seq_len(length(ids))], ids)
  })

  toggle_activity2_selected_site <- function(site_id) {
    if (is.null(site_id) || site_id == "") {
      return()
    }

    current <- activity2_selected_sites()
    if (site_id %in% current) {
      activity2_selected_sites(setdiff(current, site_id))
    } else if (length(current) < 4) {
      activity2_selected_sites(c(current, site_id))
    } else {
      showNotification("Select up to four sites.", type = "message", duration = 2)
    }
  }

  observeEvent(input$cl_map_marker_click, {
    click <- input$cl_map_marker_click
    req(click$id)
    toggle_activity2_selected_site(click$id)
  })

  observeEvent(input$clear_cl_sites, {
    activity2_selected_sites(character(0))
  })

  output$activity2_selected_sites_display <- renderUI({
    ids <- activity2_selected_sites()

    if (length(ids) == 0) {
      return(
        tags$p(
          "Selected sites: none",
          style = "font-size: 0.85em; color: #666; margin-top: 0.9rem;"
        )
      )
    }

    site_data <- cl_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    tags$div(
      style = "font-size: 0.85em; color: #444; margin-top: 0.9rem;",
      tags$strong("Selected sites: ", style = "color: #2d2926;"),
      tags$ol(
        style = "padding-left: 1.1rem; margin-bottom: 0;",
        lapply(site_data$Stream_Name, tags$li)
      )
    )
  })

  # --- Chloride Map ---------------------------------------------------------
  output$cl_map <- renderLeaflet({
    marker_data <- cl_sites()
    req(nrow(marker_data) > 0)

    leaflet(
      options = leafletOptions(
        preferCanvas = TRUE,
        worldCopyJump = FALSE
      )
    ) %>%
      addProviderTiles(
        providers$CartoDB.PositronNoLabels,
        options = tileOptions(opacity = 0.58)
      ) %>%
      addProviderTiles(
        providers$CartoDB.PositronOnlyLabels,
        options = tileOptions(opacity = 0.68)
      ) %>%
      setView(
        lng = activity2_initial_map_view$lng,
        lat = activity2_initial_map_view$lat,
        zoom = activity2_initial_map_view$zoom
      ) %>%
      addScaleBar(position = "topleft", options = scaleBarOptions(imperial = FALSE))
  })

  outputOptions(output, "cl_map", suspendWhenHidden = FALSE)

  observe({
    req(identical(input$activity2_tab, "Chloride Map"))
    marker_data <- cl_sites()
    req(nrow(marker_data) > 0)

    available_backgrounds <- names(activity2_background_rasters_global)[
      vapply(activity2_background_rasters_global, Negate(is.null), logical(1))
    ]

    background_key <- input$cl_map_background
    valid_background_keys <- c("none", available_backgrounds)
    if (is.null(background_key) || !background_key %in% valid_background_keys) {
      background_key <- "none"
    }

    has_background <- background_key %in% available_backgrounds
    background_spec <- NULL
    display_background_raster <- NULL
    background_pal <- NULL
    background_opacity <- NULL

    if (has_background) {
      background_spec <- activity2_background_specs[[background_key]]
      background_raster <- activity2_background_rasters_global[[background_key]]
      req(!is.null(background_raster))

      display_background_raster <- background_raster
      if (identical(background_key, "cropland")) {
        display_background_raster <- terra::ifel(
          background_raster <= 0.5,
          NA,
          background_raster
        )
      } else if (identical(background_key, "impervious")) {
        display_background_raster <- terra::ifel(
          background_raster <= 0.1,
          NA,
          background_raster
        )
      }

      background_vals <- terra::values(display_background_raster, mat = FALSE)
      background_vals <- background_vals[is.finite(background_vals)]
      background_pal <- colorBin(
        palette = background_spec$colors,
        domain = background_vals,
        bins = background_spec$breaks,
        na.color = "transparent",
        right = FALSE
      )
      background_opacity <- if (identical(background_key, "map")) 0.84 else 0.72
    }

    cl_point_palette <- c(
      "#f0c7df",
      "#e4a7ca",
      "#d486b2",
      "#c06598",
      "#a64c80",
      "#8b3b68",
      "#702f53",
      "#54243f"
    )
    linear_cl_values <- marker_data$mean_Cl_mgL[
      is.finite(marker_data$mean_Cl_mgL)
    ]
    req(length(linear_cl_values) > 0)
    linear_cl_domain <- c(0, max(linear_cl_values))

    marker_data <- marker_data %>%
      mutate(mean_Cl_color_value = mean_Cl_mgL)

    cl_pal <- colorNumeric(
      palette = cl_point_palette,
      domain = linear_cl_domain
    )
    cl_legend_title <- "Mean Cl (mg/L)"
    cl_legend_values <- c(0, linear_cl_values)
    selected_ids <- activity2_selected_sites()
    selected_marker_data <- marker_data %>%
      filter(Stream_ID %in% selected_ids)

    if (nrow(selected_marker_data) > 0) {
      selection_palette <- activity2_selected_site_palette()
      selected_marker_data <- selected_marker_data %>%
        mutate(selected_color = unname(selection_palette[Stream_ID]))
    }

    map_proxy <- leafletProxy("cl_map", data = marker_data) %>%
      clearImages() %>%
      clearMarkers() %>%
      clearControls()

    if (has_background) {
      map_proxy <- map_proxy %>%
        addRasterImage(
          display_background_raster,
          colors = background_pal,
          opacity = background_opacity,
          project = TRUE,
          method = "bilinear",
          maxBytes = 40 * 1024 * 1024
        ) %>%
        addControl(
          html = build_categorical_legend(
            title = background_spec$label,
            legend_items = setNames(
              rev(background_spec$colors),
              rev(background_spec$labels)
            )
          ),
          position = "bottomleft"
        )
    }

    map_proxy <- map_proxy %>%
      addScaleBar(position = "topleft", options = scaleBarOptions(imperial = FALSE)) %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        radius = 6.5,
        fillColor = ~cl_pal(mean_Cl_color_value),
        color = "#7f878d",
        weight = 0.9,
        opacity = 0.85,
        fillOpacity = 0.78,
        layerId = ~Stream_ID,
        label = ~ lapply(
          paste0(
            "<b>",
            Stream_Name,
            "</b><br>",
            "Mean Cl: ",
            round(mean_Cl_mgL, 1),
            " mg/L"
          ),
          HTML
        ),
        labelOptions = labelOptions(direction = "auto", opacity = 0.96)
      )

    if (nrow(selected_marker_data) > 0) {
      map_proxy <- map_proxy %>%
        addCircleMarkers(
          data = selected_marker_data,
          lng = ~Longitude,
          lat = ~Latitude,
          radius = 9,
          fillColor = ~cl_pal(mean_Cl_color_value),
          color = ~selected_color,
          weight = 3,
          opacity = 1,
          fillOpacity = 0.92,
          layerId = ~Stream_ID,
          label = ~ lapply(
            paste0(
              "<b>",
              Stream_Name,
              "</b><br>",
              "Mean Cl: ",
              round(mean_Cl_mgL, 1),
              " mg/L<br>Selected"
            ),
            HTML
          ),
          labelOptions = labelOptions(direction = "auto", opacity = 0.96)
        )
    }

    map_proxy %>%
      addControl(
        html = build_numeric_legend(
          title = cl_legend_title,
          values = cl_legend_values,
          legend_colors = cl_point_palette,
          digits = 0,
          n_breaks = 4
        ),
        position = "bottomright"
      )
  })

  # --- Seasonal Cl & Discharge plot -----------------------------------------

  build_activity2_cl_key <- function(color) {
    tags$span(
      style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
      tags$span(
        style = paste0(
          "display: block; width: 28px; border-top: 3px solid ", color, ";"
        )
      ),
      tags$span(
        style = paste0(
          "position: absolute; left: 10px; top: -3px;",
          "width: 8px; height: 8px; border-radius: 50%;",
          "background: ", color, ";"
        )
      )
    )
  }

  build_activity2_q_key <- function(color) {
    tags$span(
      style = "display: inline-flex; align-items: center; width: 34px;",
      tags$span(
        style = paste0(
          "display: block; width: 28px; border-top: 3px dashed ", color, ";"
        )
      )
    )
  }

  output$cl_seasonal_site_toggles <- renderUI({
    ids <- activity2_selected_sites()

    if (length(ids) == 0) {
      return(
        tags$p(
          "Select up to four sites on the chloride map to compare seasonal chloride and discharge.",
          style = "font-size: 0.85rem; color: #666; margin: 0 10px 0.35rem;"
        )
      )
    }

    site_meta <- cl_sites() %>%
      filter(Stream_ID %in% ids) %>%
      mutate(order = match(Stream_ID, ids)) %>%
      arrange(order)

    if (nrow(site_meta) == 0) {
      return(NULL)
    }

    colors <- activity2_selected_site_palette()
    q_site_ids <- unique(discharge_monthly()$Stream_ID)
    show_q <- isTRUE(input$cl_show_discharge)

    choice_names <- lapply(seq_len(nrow(site_meta)), function(i) {
      row <- site_meta[i, , drop = FALSE]
      color <- colors[[row$Stream_ID]]
      label <- paste0(row$Stream_Name, " [", row$LTER, "]")
      has_q <- row$Stream_ID %in% q_site_ids

      tags$div(
        style = "display: flex; flex-direction: column; gap: 0.42rem; min-width: 0;",
        tags$span(
          style = "font-size: 0.84rem; color: #31424c; line-height: 1.25; white-space: normal;",
          label
        ),
        tags$div(
          style = "display: flex; align-items: center; gap: 0.8rem; flex-wrap: wrap;",
          tags$div(
            style = "display: flex; align-items: center; gap: 6px;",
            build_activity2_cl_key(color),
            tags$span(style = "font-size: 0.78rem; color: #4f616b;", "Cl")
          ),
          if (show_q && has_q) {
            tags$div(
              style = "display: flex; align-items: center; gap: 6px;",
              build_activity2_q_key(color),
              tags$span(style = "font-size: 0.78rem; color: #4f616b;", "Q")
            )
          } else if (show_q) {
            tags$span(style = "font-size: 0.78rem; color: #6d767c;", "No Q")
          } else {
            NULL
          }
        )
      )
    })

    tags$div(
      class = "site-toggle-legend",
      checkboxGroupInput(
        "cl_seasonal_site_ids",
        "Display sites:",
        choiceNames = choice_names,
        choiceValues = site_meta$Stream_ID,
        selected = ids,
        width = "100%"
      )
    )
  })

  cl_seasonal_site_ids <- reactive({
    ids <- activity2_selected_sites()
    if (length(ids) == 0) {
      return(character(0))
    }

    displayed_ids <- input$cl_seasonal_site_ids
    if (is.null(displayed_ids)) {
      return(ids)
    }

    intersect(ids, displayed_ids)
  })

  output$cl_seasonal_plot <- renderPlotly({
    selected_ids <- activity2_selected_sites()
    displayed_ids <- cl_seasonal_site_ids()

    if (length(selected_ids) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Select up to four sites on the chloride map to compare seasonal chloride and discharge",
              font = list(color = "#666", size = 14)
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            yaxis2 = list(visible = FALSE),
            paper_bgcolor = plotly_bg$paper_bgcolor,
            plot_bgcolor = plotly_bg$plot_bgcolor,
            showlegend = FALSE
          ) %>%
          polish_plotly()
      )
    }

    if (length(displayed_ids) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "Choose at least one displayed site to show seasonal chloride and discharge",
              font = list(color = "#666", size = 14)
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            yaxis2 = list(visible = FALSE),
            paper_bgcolor = plotly_bg$paper_bgcolor,
            plot_bgcolor = plotly_bg$plot_bgcolor,
            showlegend = FALSE
          ) %>%
          polish_plotly()
      )
    }

    site_meta <- cl_sites() %>%
      filter(Stream_ID %in% displayed_ids) %>%
      mutate(order = match(Stream_ID, displayed_ids)) %>%
      arrange(order)

    cl_data <- cl_monthly() %>%
      filter(Stream_ID %in% displayed_ids) %>%
      arrange(Stream_ID, month)

    if (nrow(cl_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No chloride data for the displayed sites",
              font = list(color = "#666", size = 14)
            )
          ) %>%
          polish_plotly()
      )
    }

    q_data <- discharge_monthly() %>%
      filter(Stream_ID %in% displayed_ids) %>%
      arrange(Stream_ID, month)
    show_q <- isTRUE(input$cl_show_discharge) && nrow(q_data) > 0
    colors <- activity2_selected_site_palette()
    p <- plot_ly()

    for (i in seq_len(nrow(site_meta))) {
      row <- site_meta[i, , drop = FALSE]
      site_id <- row$Stream_ID
      color <- colors[[site_id]]
      site_label <- paste0(row$Stream_Name, " [", row$LTER, "]")
      cl_site_data <- cl_data %>% filter(Stream_ID == site_id)

      if (nrow(cl_site_data) > 0) {
        p <- p %>%
          add_trace(
            data = cl_site_data,
            x = ~month,
            y = ~mean_Cl_mgL,
            type = "scatter",
            mode = "lines+markers",
            name = paste0(site_label, " Cl"),
            showlegend = FALSE,
            line = list(color = color, width = 3),
            marker = list(color = color, size = 7),
            hovertemplate = paste0(
              "<b>",
              site_label,
              "</b><br>Month: %{x}<br>",
              "Mean Cl: %{y:.1f} mg/L<br>",
              "<extra></extra>"
            )
          )
      }

      if (show_q) {
        q_site_data <- q_data %>% filter(Stream_ID == site_id)
        if (nrow(q_site_data) > 0) {
          p <- p %>%
            add_trace(
              data = q_site_data,
              x = ~month,
              y = ~mean_Q_cms,
              type = "scatter",
              mode = "lines",
              name = paste0(site_label, " Q"),
              showlegend = FALSE,
              yaxis = "y2",
              line = list(color = color, width = 3, dash = "dash"),
              hovertemplate = paste0(
                "<b>",
                site_label,
                "</b><br>Month: %{x}<br>",
                "Mean Q: %{y:.3f} cms<br>",
                "<extra></extra>"
              )
            )
        }
      }
    }

    missing_q_sites <- if (isTRUE(input$cl_show_discharge)) {
      setdiff(displayed_ids, unique(q_data$Stream_ID))
    } else {
      character(0)
    }
    missing_q_note <- if (length(missing_q_sites) > 0) {
      "<br><sup>Some displayed sites do not have monthly discharge in the local data.</sup>"
    } else {
      ""
    }

    y2_config <- if (show_q) {
      list(
        title = list(
          text = "Mean Discharge (cms)",
          font = list(color = activity2_q_accent)
        ),
        overlaying = "y",
        side = "right",
        showgrid = FALSE,
        tickfont = list(color = activity2_q_accent)
      )
    } else {
      list(overlaying = "y", side = "right", visible = FALSE)
    }

    p %>%
      layout(
        title = list(
          text = paste0("Selected Sites: Monthly Chloride", if (show_q) " & Discharge" else "", missing_q_note),
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
          title = list(
            text = "Mean Chloride (mg/L)",
            font = list(color = activity2_cl_accent)
          ),
          gridcolor = "#d4e3f0",
          tickfont = list(color = activity2_cl_accent)
        ),
        yaxis2 = y2_config,
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        margin = list(r = if (show_q) 90 else 40),
        showlegend = FALSE,
        hovermode = "x unified"
      ) %>%
      polish_plotly()
  })

  # --- Activity 3: C-Q Analysis -----------------------------------------------

  cq_paired_data <- reactive({
    read_app_data("cq_paired.rds")
  })

  cq_slopes_data <- reactive({
    read_app_data("cq_slopes.rds")
  })

  cq_solute_choices <- c(
    "Chloride (Cl)" = "Cl",
    "Nitrate (NO3)" = "NO3"
  )

  # populate site dropdown — only sites that have C-Q slopes AND discharge data
  observe({
    sites <- activity3_available_sites()
    choices <- setNames(
      sites$Stream_ID,
      paste0(sites$Stream_Name, " [", sites$LTER, "]")
    )
    updateSelectInput(session, "cq_sites", choices = choices)
    updateSelectInput(session, "cq_ts_site", choices = choices)
  })

  # update solute checkboxes to only show available solutes
  observe({
    available_site_ids <- activity3_available_sites()$Stream_ID
    available <- cq_slopes_data() %>%
      filter(Stream_ID %in% available_site_ids) %>%
      pull(variable) %>%
      unique()
    scatter_choices <- cq_solute_choices[cq_solute_choices %in% available]
    updateCheckboxGroupInput(
      session,
      "cq_solutes",
      choices = scatter_choices,
      selected = character(0)
    )
  })

  # --- C-Q Monthly Hydrograph -------------------------------------------------

  output$cq_timeseries_plot <- renderPlotly({
    req(input$cq_ts_site)

    site_id <- input$cq_ts_site
    has_conc <- length(input$cq_ts_solutes) > 0

    # average monthly discharge for this site
    q_data <- discharge_monthly() %>%
      filter(Stream_ID == site_id) %>%
      arrange(month)

    if (nrow(q_data) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No discharge data for this site",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    site_name <- q_data$Stream_Name[1]
    site_lter <- q_data$LTER[1]

    p <- plot_ly() %>%
      add_trace(
        data = q_data,
        x = ~month,
        y = ~mean_Q_cms,
        type = "scatter",
        mode = "lines",
        name = "Mean Q (cms)",
        yaxis = if (has_conc) "y2" else "y",
        line = list(color = activity2_q_accent, width = 3.2, dash = "dash"),
        hovertemplate = "Month: %{x}<br>Mean Q: %{y:.4f} cms<extra></extra>"
      )

    if (length(input$cq_ts_solutes) > 0) {
      chem <- cq_paired_data() %>%
        filter(Stream_ID == site_id, variable %in% input$cq_ts_solutes) %>%
        mutate(month = as.integer(format(date, "%m"))) %>%
        group_by(Stream_ID, Stream_Name, LTER, variable, month) %>%
        summarise(mean_value = mean(value, na.rm = TRUE), n_obs = n(), .groups = "drop") %>%
        group_by(variable) %>%
        mutate(
          plot_value = if (isTRUE(input$cq_ts_normalize)) {
            value_sd <- sd(mean_value, na.rm = TRUE)
            if (is.finite(value_sd) && value_sd > 0) {
              (mean_value - mean(mean_value, na.rm = TRUE)) / value_sd
            } else {
              rep(0, dplyr::n())
            }
          } else {
            mean_value
          }
        ) %>%
        ungroup()

      for (sol in input$cq_ts_solutes) {
        sol_data <- filter(chem, variable == sol)
        if (nrow(sol_data) == 0) {
          next
        }
        sol_label <- names(cq_solute_choices)[cq_solute_choices == sol]
        p <- p %>%
          add_trace(
            data = sol_data,
            x = ~month,
            y = ~plot_value,
            type = "scatter",
            mode = "lines+markers",
            name = sol_label,
            yaxis = "y",
            line = list(color = solute_colors[[sol]], width = 2),
            marker = list(color = solute_colors[[sol]], size = 6, opacity = 0.8),
            hovertemplate = paste0(
              sol_label,
              "<br>Month: %{x}<br>",
              if (isTRUE(input$cq_ts_normalize)) {
                "Z-score: %{y:.2f}<br>Mean conc: %{customdata:.2f}<extra></extra>"
              } else {
                "Mean conc: %{y:.2f}<extra></extra>"
              }
            ),
            customdata = ~mean_value
          )
      }
    }

    chemistry_axis_title <- if (isTRUE(input$cq_ts_normalize)) {
      "Normalized Concentration (z-score)"
    } else {
      "Concentration (mg/L)"
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
          gridcolor = "#d4e3f0",
          zeroline = FALSE
        ),
        yaxis = list(
          title = list(
            text = if (has_conc) chemistry_axis_title else "Mean Discharge (cms)",
            font = list(color = if (has_conc) "#2d2926" else activity2_q_accent)
          ),
          gridcolor = "#d4e3f0",
          tickfont = list(color = if (has_conc) "#2d2926" else activity2_q_accent),
          zeroline = FALSE
        ),
        yaxis2 = if (has_conc) {
          list(
            title = list(
              text = "Mean Discharge (cms)",
              font = list(color = activity2_q_accent)
            ),
            overlaying = "y",
            side = "right",
            showgrid = FALSE,
            tickfont = list(color = activity2_q_accent),
            zeroline = FALSE
          )
        } else {
          list(overlaying = "y", side = "right", visible = FALSE)
        },
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        showlegend = FALSE,
        hovermode = "closest",
        margin = list(r = if (has_conc) 95 else 55)
      ) %>%
      polish_plotly()
  })

  output$cq_timeseries_plot_legend <- renderUI({
    req(input$cq_ts_site)

    solute_entries <- lapply(input$cq_ts_solutes, function(sol) {
      sol_label <- names(cq_solute_choices)[cq_solute_choices == sol]
      tags$div(
        style = "display: flex; align-items: center; gap: 8px;",
        tags$span(
          style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
          tags$span(
            style = paste0(
              "display: block; width: 28px; border-top: 3px solid ",
              solute_colors[[sol]],
              ";"
            )
          ),
          tags$span(
            style = paste0(
              "position: absolute; left: 10px; top: -3px;",
              "width: 8px; height: 8px; border-radius: 50%;",
              "background: ",
              solute_colors[[sol]],
              ";"
            )
          )
        ),
        tags$span(
          style = "font-size: 0.84rem; color: #31424c;",
          sol_label
        )
      )
    })

    discharge_entry <- tags$div(
      style = "display: flex; align-items: center; gap: 8px;",
      tags$span(
        style = "display: inline-flex; align-items: center; width: 34px;",
        tags$span(
          style = paste0(
            "display: block; width: 28px; border-top: 3px dashed ",
            activity2_q_accent,
            ";"
          )
        )
      ),
      tags$span(
        style = "font-size: 0.84rem; color: #31424c;",
        "Mean Discharge (cms)"
      )
    )

    legend_entries <- c(solute_entries, list(discharge_entry))

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 20px;",
        "row-gap: 8px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      ),
      legend_entries
    )
  })

  cq_trendline_summaries <- reactive({
    req(input$cq_sites, input$cq_solutes)

    paired <- cq_paired_data() %>%
      filter(Stream_ID == input$cq_sites, variable %in% input$cq_solutes)

    if (nrow(paired) == 0) {
      return(NULL)
    }

    paired %>%
      group_by(Stream_ID, Stream_Name, LTER, variable) %>%
      group_modify(~ {
        if (nrow(.x) < 3) {
          return(data.frame(
            n_obs = nrow(.x),
            intercept = NA_real_,
            slope = NA_real_,
            r2 = NA_real_
          ))
        }

        mod <- lm(log10(value) ~ log10(Q), data = .x)
        data.frame(
          n_obs = nrow(.x),
          intercept = unname(coef(mod)[1]),
          slope = unname(coef(mod)[2]),
          r2 = summary(mod)$r.squared
        )
      }) %>%
      ungroup() %>%
      mutate(
        solute_label = names(cq_solute_choices)[match(variable, cq_solute_choices)],
        solute_color = unname(solute_colors[variable])
      ) %>%
      arrange(variable)
  })

  # --- C-Q Scatter Plot -------------------------------------------------------

  output$cq_scatter_plot <- renderPlotly({
    req(input$cq_sites, input$cq_solutes)

    paired <- cq_paired_data() %>%
      filter(Stream_ID == input$cq_sites, variable %in% input$cq_solutes)

    if (nrow(paired) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No paired C-Q data for selected sites/solutes",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    combos <- paired %>%
      select(Stream_ID, Stream_Name, LTER, variable) %>%
      distinct() %>%
      mutate(solute_order = match(variable, input$cq_solutes)) %>%
      arrange(solute_order)

    p <- plot_ly()

    for (i in seq_len(nrow(combos))) {
      row <- combos[i, ]
      d <- paired %>%
        filter(Stream_ID == row$Stream_ID, variable == row$variable)

      solute_label <- names(cq_solute_choices)[
        cq_solute_choices == row$variable
      ]
      trace_name <- paste0(row$Stream_Name, " — ", solute_label)
      clr <- unname(solute_colors[[row$variable]])

      p <- p %>%
        add_trace(
          data = d,
          x = ~ log10(Q),
          y = ~ log10(value),
          type = "scatter",
          mode = "markers",
          name = trace_name,
          marker = list(
            color = clr,
            size = 9.4,
            opacity = 0.88,
            symbol = "circle",
            line = list(color = clr, width = 1)
          ),
          hovertemplate = paste0(
            row$Stream_Name,
            "<br>",
            solute_label,
            "<br>",
            "Q: %{customdata:.4f} cms<br>",
            "C: %{meta:.2f}<br>",
            "<extra></extra>"
          ),
          customdata = d$Q,
          meta = d$value,
          showlegend = FALSE
        )

      if (nrow(d) >= 3) {
        mod <- lm(log10(value) ~ log10(Q), data = d)

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
            line = list(color = clr, width = 2.5),
            hoverinfo = "skip",
            showlegend = FALSE
          )
      }
    }

    p %>%
      layout(
        xaxis = list(
          title = "log\u2081\u2080(Discharge, cms)",
          gridcolor = "#d4e3f0"
        ),
        yaxis = list(
          title = "log\u2081\u2080(Concentration)",
          gridcolor = "#d4e3f0"
        ),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        showlegend = FALSE,
        margin = list(r = 30, b = 20)
      ) %>%
      polish_plotly()
  })

  output$cq_scatter_legend <- renderUI({
    req(input$cq_sites, input$cq_solutes)

    selected_site <- cq_slopes_data() %>%
      filter(Stream_ID == input$cq_sites) %>%
      select(Stream_ID, Stream_Name) %>%
      distinct()

    if (nrow(selected_site) == 0) {
      return(NULL)
    }

    legend_entries <- lapply(input$cq_solutes, function(sol) {
      solute_label <- names(cq_solute_choices)[cq_solute_choices == sol]
      clr <- unname(solute_colors[[sol]])

      tags$div(
        style = "display: flex; align-items: center; gap: 10px;",
        tags$span(
          style = "display: inline-flex; align-items: center; width: 34px; position: relative;",
          tags$span(
            style = paste0(
              "display: block; width: 28px; border-top: 3px solid ",
              clr,
              ";"
            )
          ),
          tags$span(
            style = paste0(
              "position: absolute; left: 9px; top: -4px;",
              "width: 10px; height: 10px; border-radius: 50%;",
              "background: ", clr, ";"
            )
          )
        ),
        tags$span(
          style = "font-size: 0.84rem; color: #31424c;",
          solute_label
        )
      )
    })

    tags$div(
      style = paste(
        "display: grid;",
        "grid-template-columns: repeat(2, minmax(0, 1fr));",
        "column-gap: 20px;",
        "row-gap: 8px;",
        "padding: 0 10px 10px 10px;",
        "border-top: 1px solid #e1ebf0;"
      )
      ,
      legend_entries
    )
  })

  output$cq_fit_summaries <- renderUI({
    trendlines <- cq_trendline_summaries()

    if (is.null(trendlines) || nrow(trendlines) == 0) {
      return(
        tags$p(
          "No fitted C-Q lines are available for the current site and solute selection.",
          style = "color: #5d6d76; margin-bottom: 0;"
        )
      )
    }

    tags$div(
      style = paste(
        "overflow-x: auto;",
        "border: 1px solid #d7e3ea;",
        "border-radius: 12px;",
        "background: rgba(255,255,255,0.86);"
      ),
      tags$table(
        style = paste(
          "width: 100%;",
          "border-collapse: collapse;",
          "font-size: 0.84rem;",
          "line-height: 1.35;"
        ),
        tags$thead(
          tags$tr(
            style = "background: #f4f7f9; color: #24323d;",
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Site"),
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Solute"),
            tags$th(style = "text-align:left; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Fit"),
            tags$th(style = "text-align:right; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "Slope"),
            tags$th(style = "text-align:right; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "R²"),
            tags$th(style = "text-align:right; padding: 10px 12px; border-bottom: 1px solid #d7e3ea;", "n")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(trendlines)), function(i) {
            row <- trendlines[i, ]

            if (is.na(row$slope) || is.na(row$intercept) || is.na(row$r2)) {
              fit_text <- "Not enough paired observations to fit a line"
              slope_text <- "\u2014"
              r2_text <- "\u2014"
            } else {
              fit_text <- sprintf(
                "log10(C) = %.3f + %.3f x log10(Q)",
                row$intercept,
                row$slope
              )
              slope_text <- sprintf("%.3f", row$slope)
              r2_text <- sprintf("%.3f", row$r2)
            }

            tags$tr(
              style = "border-bottom: 1px solid #e5edf2;",
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; color: #24323d;",
                row$Stream_Name
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; color: #24323d;",
                tags$div(
                  style = "display: flex; align-items: center; gap: 0.45rem;",
                  tags$span(
                    style = paste0(
                      "display:inline-block;",
                      "width:12px;",
                      "height:12px;",
                      "border-radius:999px;",
                      "background:", row$solute_color, ";"
                    )
                  ),
                  tags$span(row$solute_label)
                )
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; font-family: 'SFMono-Regular', 'Menlo', monospace; color: #24323d;",
                fit_text
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; text-align: right; color: #24323d;",
                slope_text
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; text-align: right; color: #24323d;",
                r2_text
              ),
              tags$td(
                style = "padding: 10px 12px; vertical-align: top; text-align: right; color: #24323d;",
                as.integer(row$n_obs)
              )
            )
          })
        )
      )
    )
  })

  # --- C-Q Slope Histogram ----------------------------------------------------

  output$cq_histogram <- renderPlotly({
    req(input$cq_hist_solutes)

    available_site_ids <- activity3_available_sites()$Stream_ID
    slopes <- cq_slopes_data() %>%
      filter(
        Stream_ID %in% available_site_ids,
        variable %in% input$cq_hist_solutes
      )

    if (nrow(slopes) == 0) {
      return(
        plotly_empty() %>%
          layout(
            title = list(
              text = "No C-Q slopes available",
              font = list(color = "#666", size = 14)
            )
          )
      )
    }

    # y-range for annotation placement (use combined data)
    bin_edges <- seq(
      floor(min(slopes$cq_slope, na.rm = TRUE) / 0.025) * 0.025,
      ceiling(max(slopes$cq_slope, na.rm = TRUE) / 0.025) * 0.025,
      by = 0.025
    )
    hist_obj <- hist(slopes$cq_slope, breaks = bin_edges, plot = FALSE)
    y_max <- max(hist_obj$counts) * 1.1

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
          xbins = list(
            start = min(bin_edges),
            end = max(bin_edges),
            size = 0.025
          ),
          hovertemplate = paste0(
            sol_label,
            "<br>Slope: %{x:.2f}<br>Count: %{y}<extra></extra>"
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
        xaxis = list(
          title = "C-Q Slope",
          gridcolor = "#d4e3f0",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "Number of Sites",
          gridcolor = "#d4e3f0",
          zeroline = FALSE
        ),
        paper_bgcolor = plotly_bg$paper_bgcolor,
        plot_bgcolor = plotly_bg$plot_bgcolor,
        legend = right_side_legend(font_size = 10),
        margin = list(r = 170),
        shapes = list(
          list(
            type = "line",
            x0 = -0.1,
            x1 = -0.1,
            y0 = 0,
            y1 = y_max,
            line = list(color = "#2d2926", width = 1.5, dash = "dash")
          ),
          list(
            type = "line",
            x0 = 0.1,
            x1 = 0.1,
            y0 = 0,
            y1 = y_max,
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
            font = list(size = 8, color = "#999")
          ),
          list(
            x = 0.1,
            y = y_max * 0.95,
            text = "Mobilizing \u2192",
            showarrow = FALSE,
            xanchor = "left",
            font = list(size = 12, color = "#666"),
            xshift = 6
          )
        )
      ) %>%
      polish_plotly()
  })
}

shinyApp(ui = ui, server = server)
