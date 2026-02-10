# River Hydrology Teaching Module

Interactive Shiny app for exploring river hydrology metrics (RBI, recession slope) and their relationship to watershed characteristics across North American LTER and USGS sites. Built for CUAHSI teaching workshops.

## Setup

### 1. Get the data

Data files live in Google Drive and need to be downloaded locally. Update the `data_path` variable at the top of `data_harmonization.R` and `app.R` to point at your local copy.

The raw inputs you'll need:
- `20260105_masterdata_chem.csv` — chemistry observations
- `20260106_masterdata_discharge.csv` — daily discharge
- `Driver_Variables/` folder — climate classifications, spatial drivers
- `DSi_LULC_filled_interpolated_Simple.csv` — land use / land cover

Original data source: [Google Drive](https://drive.google.com/drive/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-)

### 2. Run the harmonization

```r
source("data_harmonization.R")
```

This filters to North American sites, computes discharge metrics, merges in climate/spatial/land-use data, and pre-computes everything the app needs:
- `harmonized_north_america_partial.csv` — all sites, some fields may be NA
- `harmonized_north_america_complete.csv` — only sites with RBI + RCS + climate + slope
- `discharge_north_america.csv` — pre-filtered discharge (~269 MB vs 920 MB raw) so the app loads fast
- `cl_monthly_summary.csv` — monthly mean Cl per site (Activity 2)
- `cq_paired_obs.csv` — same-day chemistry + discharge pairs for 10 solutes (Activity 3)
- `cq_slopes.csv` — log-log C-Q regression slopes per site×solute (Activity 3)

### 3. Run the app

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click "Run App".

## Project structure

```
teaching-modules-shiny/
├── app.R                    # Shiny app
├── data_harmonization.R     # Builds the harmonized datasets from raw CSVs
├── .gitignore
└── README.md
```

## What's in the app

- **Overview tab** — interactive map of all study sites, colored by climate zone, snow fraction, RBI, RCS, land use, or LTER network
- **Activity 1: Snow Influence on Flashiness** — explores how snow influences river flashiness (RBI) and recession behavior (RCS), with scatter plots and side-by-side hydrograph comparisons
- **Activity 2: Stream Salinity** — chloride heatmap across North America, seasonal Cl patterns with optional discharge overlay
- **Activity 3: C-Q Analysis** — concentration-discharge relationships for 10 solutes (Cl, NO3, SO4, Ca, Mg, Na, K, DSi, PO4, DOC). Includes a log-log scatter with trendlines (slope + R²) for up to 3 sites × 2 solutes, and a histogram showing how C-Q slopes are distributed across all sites for a given solute
