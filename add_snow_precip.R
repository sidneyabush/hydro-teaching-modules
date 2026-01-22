if (!require("librarian")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("librarian")
}
librarian::shelf(dplyr, tidyr)

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

create_stream_id <- function(df) {
  df %>%
    mutate(
      Stream_ID = paste(LTER, Stream_Name, sep = "_"),
      Stream_ID = str_trim(Stream_ID),
      Stream_ID = str_replace_all(Stream_ID, "\\s+", "_")
    )
}

spatial_drivers_raw <- read.csv(file.path(data_path, "Driver_Variables/Data Release 2/all-data_si-extract_2_20250325.csv"),
                               stringsAsFactors = FALSE) %>%
  create_stream_id()

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

harmonized_partial <- read.csv(file.path(data_path, "harmonized_north_america_partial.csv"),
                               stringsAsFactors = FALSE) %>%
  left_join(snow_precip_data, by = "Stream_ID")

harmonized_complete <- read.csv(file.path(data_path, "harmonized_north_america_complete.csv"),
                                stringsAsFactors = FALSE) %>%
  left_join(snow_precip_data, by = "Stream_ID")

write.csv(harmonized_partial,
          file.path(data_path, "harmonized_north_america_partial.csv"),
          row.names = FALSE)

write.csv(harmonized_complete,
          file.path(data_path, "harmonized_north_america_complete.csv"),
          row.names = FALSE)
