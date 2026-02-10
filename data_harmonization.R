# Data Harmonization for Teaching Modules
# Adapted from LTER-SiSyn-Spatial-Controls harmonization workflow
#
# Reads raw chemistry, discharge, climate, spatial driver, and land use CSVs,
# then builds everything the Shiny app needs:
#   - harmonized_north_america_partial.csv  (all NA sites, some fields may be NA)
#   - harmonized_north_america_complete.csv (only sites with RBI + RCS + climate + slope)
#   - discharge_north_america.csv           (pre-filtered discharge so the app doesn't load 920 MB)
#   - cl_monthly_summary.csv               (monthly mean Cl per site for Activity 2)
#   - cq_paired_obs.csv                    (same-day chem+discharge pairs for Activity 3)
#   - cq_slopes.csv                        (C-Q slopes per site×solute for Activity 3)
#
# Update data_path below to point at your local data directory.

rm(list = ls())

if (!require("librarian")) install.packages("librarian")
librarian::shelf(dplyr, ggplot2, data.table, lubridate, tidyr, stringr, readr)

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Hydrology_Lab/CUAHSI-teaching-modules-shiny/data"

# standardizes the LTER + Stream_Name combo into a single join key
create_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = paste(LTER, Stream_Name, sep = "_"),
      Stream_ID = str_trim(Stream_ID),
      Stream_ID = str_replace_all(Stream_ID, "\\s+", "_")
    )
}

north_american_lter <- c(
  "Canada", "USGS", "AND", "ARC", "BcCZO", "BNZ", "ColoradoAlpine",
  "CZO-Catalina Jemez", "Catalina Jemez", "EastRiverSFA", "GRO", "HBR",
  "Ipswitch(Carey)", "KNZ", "LMP", "LMP(Wymore)", "LUQ", "NWT", "PIE",
  "Sagehen", "Sagehen(Sullivan)", "UMR", "UMR(Jankowski)",
  "WalkerBranch", "Walker Branch"
)


# --- Load and filter raw data ---------------------------------------------

chem_na <- read.csv(file.path(data_path, "20260105_masterdata_chem.csv"),
                    stringsAsFactors = FALSE) %>%
  filter(LTER %in% north_american_lter) %>%
  create_stream_id()

discharge_na <- read.csv(file.path(data_path, "20260106_masterdata_discharge.csv"),
                         stringsAsFactors = FALSE) %>%
  filter(LTER %in% north_american_lter) %>%
  create_stream_id() %>%
  rename(Q = Qcms) %>%
  mutate(Date = as.Date(Date))


# --- Compute discharge metrics --------------------------------------------

# RBI: sum of absolute day-to-day changes / total discharge
# only keep sites with at least a year of data
rbi_results <- discharge_na %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  arrange(Date) %>%
  mutate(abs_dQ = abs(Q - lag(Q))) %>%
  filter(!is.na(abs_dQ)) %>%
  summarise(
    n_days = n(),
    total_discharge = sum(Q, na.rm = TRUE),
    total_change = sum(abs_dQ, na.rm = TRUE),
    RBI = total_change / total_discharge,
    .groups = "drop"
  ) %>%
  filter(n_days >= 365)

# RCS: fit log-log regression on recession limbs
# keep only days where flow dropped but not too abruptly (Q_t / Q_t-1 >= 0.7)
Q_diff <- discharge_na %>%
  arrange(Stream_ID, Date) %>%
  group_by(Stream_ID) %>%
  mutate(
    dQ = Q - lag(Q),
    change_dQ = Q / lag(Q),
    dQ_dt = dQ / as.numeric(Date - lag(Date))
  ) %>%
  filter(!is.na(dQ_dt), change_dQ >= 0.7)

recession_data <- Q_diff %>%
  filter(dQ < 0) %>%
  mutate(recession_slope = -dQ_dt) %>%
  filter(is.finite(recession_slope), recession_slope > 0)

recession_slopes <- recession_data %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  summarise(
    n_recession_days = n(),
    recession_slope = if (n_recession_days >= 50) {
      tryCatch({
        lm_model <- lm(log(recession_slope) ~ log(Q), data = pick(everything()))
        unname(coef(lm_model)[2])
      }, error = function(e) NA_real_)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  filter(!is.na(recession_slope), recession_slope >= 0)

discharge_metrics <- rbi_results %>%
  left_join(recession_slopes %>% select(Stream_ID, recession_slope, n_recession_days),
            by = "Stream_ID")


# --- Merge site info + climate + spatial drivers + LULC --------------------

sites_info <- chem_na %>%
  select(Stream_ID, LTER, Stream_Name) %>%
  distinct()

sites_with_discharge <- sites_info %>%
  left_join(discharge_metrics, by = c("Stream_ID", "LTER", "Stream_Name"))

kg_data <- read.csv(file.path(data_path,
  "Driver_Variables/Data Release 2/Data Harmonization/Additional files needed/Koeppen_Geiger_2.csv"),
  stringsAsFactors = FALSE) %>%
  create_stream_id() %>%
  select(Stream_ID, ClimateZ, Latitude, Longitude, Name)

spatial_drivers_raw <- read.csv(file.path(data_path,
  "Driver_Variables/Data Release 2/all-data_si-extract_2_20250325.csv"),
  stringsAsFactors = FALSE) %>%
  create_stream_id()

spatial_drivers <- spatial_drivers_raw %>%
  select(Stream_ID, LTER, Stream_Name,
         basin_slope_mean_degree, basin_slope_median_degree,
         elevation_mean_m, elevation_median_m,
         starts_with("precip_"), starts_with("temp_"),
         starts_with("evapotrans_"),
         starts_with("land_"), major_land, major_rock, major_soil)

# average annual precip and snow days from the yearly columns in spatial drivers
snow_precip_data <- spatial_drivers_raw %>%
  rowwise() %>%
  mutate(
    mean_annual_precip = mean(c_across(matches("precip_[0-9]{4}_mm_per_day")),
                              na.rm = TRUE) * 365,
    mean_snow_days = mean(c_across(matches("snow_[0-9]{4}_num_days")),
                          na.rm = TRUE),
    snow_fraction = mean_snow_days / 365
  ) %>%
  ungroup() %>%
  select(Stream_ID, mean_annual_precip, mean_snow_days, snow_fraction)

lulc_data <- read.csv(file.path(data_path, "DSi_LULC_filled_interpolated_Simple.csv"),
                      stringsAsFactors = FALSE) %>%
  filter(Year >= 2002, Year <= 2022) %>%
  mutate(
    LandClass_sum = if_else(
      is.na(LandClass_sum) | LandClass_sum == 0,
      LandClass_sum,
      LandClass_sum * 100
    )
  ) %>%
  filter(Simple_Class != "Filled_Value") %>%
  pivot_wider(names_from = Simple_Class, values_from = LandClass_sum,
              names_prefix = "land_")

lulc_avg <- lulc_data %>%
  mutate(Stream_ID = paste0(Stream_Name, "_", Stream_Name)) %>%
  group_by(Stream_Name) %>%
  summarise(across(starts_with("land_"), ~ mean(.x, na.rm = TRUE)),
            .groups = "drop") %>%
  mutate(
    major_land_lulc = apply(select(., starts_with("land_")), 1, function(x) {
      if (all(is.na(x))) NA_character_
      else names(x)[which.max(x)]
    })
  )


# --- Assemble and write outputs -------------------------------------------

harmonized_data <- sites_with_discharge %>%
  left_join(kg_data, by = "Stream_ID") %>%
  left_join(spatial_drivers, by = c("Stream_ID", "LTER", "Stream_Name")) %>%
  left_join(lulc_avg, by = "Stream_Name") %>%
  left_join(snow_precip_data, by = "Stream_ID") %>%
  # drop sites outside North America (catches some Russian GRO sites)
  filter(
    is.na(Longitude) | (Longitude >= -170 & Longitude <= -50),
    is.na(Latitude)  | (Latitude >= 15 & Latitude <= 85)
  )

write.csv(harmonized_data,
          file.path(data_path, "harmonized_north_america_partial.csv"),
          row.names = FALSE)

# complete cases: only sites that have all the key fields the app needs
complete_cases <- harmonized_data %>%
  filter(!is.na(RBI), !is.na(recession_slope),
         !is.na(ClimateZ), !is.na(basin_slope_mean_degree))

write.csv(complete_cases,
          file.path(data_path, "harmonized_north_america_complete.csv"),
          row.names = FALSE)


# --- Pre-filter discharge for the app ----------------------------------------
# The raw discharge file is ~920 MB. Save just the NA-filtered subset so the
# app doesn't have to load the full thing on every startup.

discharge_na_export <- discharge_na %>%
  rename(Qcms = Q) %>%
  select(Qcms, Date, LTER, Stream_Name, Stream_ID)

write.csv(discharge_na_export,
          file.path(data_path, "discharge_north_america.csv"),
          row.names = FALSE)


# --- Pre-compute Cl summaries for Activity 2 --------------------------------
# Avoids loading the full 345 MB chem file at app startup

cl_data <- chem_na %>%
  filter(variable == "Cl", !is.na(value)) %>%
  mutate(date = as.Date(date),
         month = month(date))

# site-level summary: mean, median, observation count
cl_site_stats <- cl_data %>%
  group_by(Stream_ID, LTER, Stream_Name) %>%
  summarise(
    mean_Cl_uM = mean(value, na.rm = TRUE),
    median_Cl_uM = median(value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

# monthly averages per site (for seasonal plot)
cl_monthly <- cl_data %>%
  group_by(Stream_ID, LTER, Stream_Name, month) %>%
  summarise(
    mean_Cl_uM = mean(value, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

write.csv(cl_monthly,
          file.path(data_path, "cl_monthly_summary.csv"),
          row.names = FALSE)

# add site-level mean Cl to the harmonized partial file
harmonized_with_cl <- harmonized_data %>%
  left_join(cl_site_stats %>% select(Stream_ID, mean_Cl_uM, median_Cl_uM, n_cl_obs = n_obs),
            by = "Stream_ID")

write.csv(harmonized_with_cl,
          file.path(data_path, "harmonized_north_america_partial.csv"),
          row.names = FALSE)


# --- Pre-compute C-Q paired observations and slopes for Activity 3 ----------
# Pairs same-day chemistry + discharge for 10 target solutes, then fits
# log-log regressions to get C-Q slopes per site×solute.

cq_solutes <- c("Cl", "NO3", "SO4", "Ca", "Mg", "Na", "K", "DSi", "PO4", "DOC")

# paired observations: inner join chem + discharge on Stream_ID + date
cq_chem <- chem_na %>%
  filter(variable %in% cq_solutes, !is.na(value), value > 0) %>%
  mutate(date = as.Date(date)) %>%
  select(Stream_ID, LTER, Stream_Name, date, variable, value)

# average duplicate discharge dates per site before joining
cq_discharge <- discharge_na %>%
  filter(Q > 0) %>%
  group_by(Stream_ID, Date) %>%
  summarise(Q = mean(Q), .groups = "drop") %>%
  rename(date = Date)

cq_paired <- cq_chem %>%
  inner_join(cq_discharge, by = c("Stream_ID", "date"))

write.csv(cq_paired,
          file.path(data_path, "cq_paired_obs.csv"),
          row.names = FALSE)


# slopes: log-log regression per site×solute (min 10 observations)
fit_cq_slope <- function(df) {
  empty <- data.frame(n_paired_obs = integer(0), cq_slope = numeric(0), r_squared = numeric(0))
  if (nrow(df) < 10) return(empty)
  mod <- tryCatch(
    lm(log10(value) ~ log10(Q), data = df),
    error = function(e) NULL
  )
  if (is.null(mod)) return(empty)
  data.frame(
    n_paired_obs = nrow(df),
    cq_slope = unname(coef(mod)[2]),
    r_squared = summary(mod)$r.squared
  )
}

cq_slopes <- cq_paired %>%
  group_by(Stream_ID, LTER, Stream_Name, variable) %>%
  group_modify(~ fit_cq_slope(.x)) %>%
  ungroup()

write.csv(cq_slopes,
          file.path(data_path, "cq_slopes.csv"),
          row.names = FALSE)
