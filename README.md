# River Hydrology Teaching Module

Interactive Shiny app for exploring river hydrology metrics (RBI, recession slope) and their relationship to watershed characteristics across North American LTER and USGS sites. Built for CUAHSI teaching workshops.

## Setup

### 1. Get the data

Data files live in Google Drive and need to be downloaded locally. Set the `DATA_PATH` environment variable to wherever you put them:

```r
Sys.setenv(DATA_PATH = "/path/to/your/data")
```

If unset, both scripts default to a `data/` folder next to the code.

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

This filters to North American sites, computes RBI and recession curve slopes, merges in climate/spatial/snow/land-use data, and writes two CSVs:
- `harmonized_north_america_partial.csv` — all sites, some fields may be NA
- `harmonized_north_america_complete.csv` — only sites with RBI + RCS + climate + slope

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
- **Activity 1** — explores how snow influences river flashiness (RBI) and recession behavior (RCS), with scatter plots and side-by-side hydrograph comparisons
