# Pre-process raw CSVs into compact .rds files for the Shiny app.
# Run this once locally whenever the source data changes.
# The app reads only from data/ in the repo — no external paths needed.

library(dplyr)
library(data.table)

raw_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Hydrology_Lab/CUAHSI-teaching-modules-shiny/data"
out_path <- "data"
dir.create(out_path, showWarnings = FALSE)

# --- 1. Harmonized site data (complete + partial) ---
complete <- read.csv(file.path(raw_path, "harmonized_north_america_complete.csv"),
                     stringsAsFactors = FALSE)
partial <- read.csv(file.path(raw_path, "harmonized_north_america_partial.csv"),
                    stringsAsFactors = FALSE)

saveRDS(complete, file.path(out_path, "harmonized_complete.rds"))
saveRDS(partial, file.path(out_path, "harmonized_partial.rds"))
cat("harmonized:", nrow(complete), "complete,", nrow(partial), "partial\n")

# --- 2. Discharge — only keep sites that appear in harmonized_complete ---
keep_sites <- unique(complete$Stream_ID)
discharge <- fread(file.path(raw_path, "discharge_north_america.csv")) %>%
  as.data.frame() %>%
  filter(Stream_ID %in% keep_sites) %>%
  mutate(Date = as.Date(Date)) %>%
  select(Stream_ID, Stream_Name, LTER, Date, Qcms)

saveRDS(discharge, file.path(out_path, "discharge.rds"))
cat("discharge:", nrow(discharge), "rows,",
    length(unique(discharge$Stream_ID)), "sites\n")

# --- 3. Cl monthly summary ---
cl_monthly <- read.csv(file.path(raw_path, "cl_monthly_summary.csv"),
                       stringsAsFactors = FALSE)
saveRDS(cl_monthly, file.path(out_path, "cl_monthly.rds"))
cat("cl_monthly:", nrow(cl_monthly), "rows\n")

# --- 4. CQ paired observations — Cl and NO3 only ---
cq_paired <- read.csv(file.path(raw_path, "cq_paired_obs.csv"),
                      stringsAsFactors = FALSE) %>%
  filter(variable %in% c("Cl", "NO3")) %>%
  mutate(date = as.Date(date))

saveRDS(cq_paired, file.path(out_path, "cq_paired.rds"))
cat("cq_paired:", nrow(cq_paired), "rows\n")

# --- 5. CQ slopes — Cl and NO3 only ---
cq_slopes <- read.csv(file.path(raw_path, "cq_slopes.csv"),
                      stringsAsFactors = FALSE) %>%
  filter(variable %in% c("Cl", "NO3"))

saveRDS(cq_slopes, file.path(out_path, "cq_slopes.rds"))
cat("cq_slopes:", nrow(cq_slopes), "rows\n")

cat("\nDone! All .rds files written to", out_path, "\n")
