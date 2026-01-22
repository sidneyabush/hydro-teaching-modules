# Teaching Modules Shiny App

Interactive Shiny application for teaching modules.

## Setup

### 1. Install Required Packages

```r
install.packages(c("shiny", "googledrive"))
```

### 2. Download Data from Google Drive

The data files are stored in Google Drive and are downloaded to Box.

**Run the data download script:**

```r
source("data_download.R")
```

This will:
- Prompt you to authenticate with Google (OAuth)
- Download all files from the Google Drive folder
- Save them to Box: `/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data/`

**Note:** You only need to download the data once. The authentication token will be cached locally.

### 3. Run the Shiny App

```r
shiny::runApp()
```

Or open `app.R` in RStudio and click "Run App".

## Project Structure

```
teaching-modules-shiny/
├── app.R                  # Main Shiny application
├── data_download.R        # Script to download data from Google Drive to Box
├── .gitignore            # Excludes sensitive info
└── README.md             # This file
```

## Data Location

Data files are stored in Box (not in this repository):
`/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data/`

## Data Source

Original data is in Google Drive:
https://drive.google.com/drive/folders/1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-

## Notes

- Data files are stored in Box, not in the git repository
- Download data using `data_download.R` (downloads to Box location)
- OAuth authentication token is cached locally (excluded from git)
