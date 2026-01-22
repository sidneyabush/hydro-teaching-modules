# Google Drive Data Download Script
# Downloads data files from Google Drive to Box
# Uses OAuth authentication

if (!require("googledrive")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  install.packages("googledrive")
}
library(googledrive)

drive_auth()

gdrive_folder_id <- "1hbkUsTdo4WAEUnlPReOUuXdeeXm92mg-"

data_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/CUAHSI-teaching-modules-shiny/data"

if (!dir.exists(data_path)) {
  dir.create(data_path, recursive = TRUE)
}

download_files_recursive <- function(folder_id, local_path = data_path, depth = 0) {
  items <- drive_ls(as_id(folder_id))

  if (nrow(items) == 0) {
    return()
  }

  for (i in 1:nrow(items)) {
    item_name <- items$name[i]
    item_id <- items$id[i]
    item_type <- items$drive_resource[[i]]$mimeType

    if (item_type == "application/vnd.google-apps.folder") {
      subfolder_path <- file.path(local_path, item_name)
      if (!dir.exists(subfolder_path)) {
        dir.create(subfolder_path, recursive = TRUE)
      }
      download_files_recursive(item_id, subfolder_path, depth + 1)
    } else {
      tryCatch({
        drive_download(
          file = as_id(item_id),
          path = file.path(local_path, item_name),
          overwrite = TRUE
        )
      }, error = function(e) {})
    }
  }
}

download_specific_file <- function(file_path, local_path = data_path) {
  path_parts <- strsplit(file_path, "/")[[1]]
  current_folder_id <- gdrive_folder_id

  for (i in 1:(length(path_parts) - 1)) {
    folder_name <- path_parts[i]
    items <- drive_ls(as_id(current_folder_id))
    folder_match <- items[items$name == folder_name, ]

    if (nrow(folder_match) == 0) {
      stop(paste0("Folder not found: ", folder_name))
    }

    current_folder_id <- folder_match$id[1]

    local_subfolder <- file.path(local_path, paste(path_parts[1:i], collapse = "/"))
    if (!dir.exists(local_subfolder)) {
      dir.create(local_subfolder, recursive = TRUE)
    }
  }

  file_name <- path_parts[length(path_parts)]
  files <- drive_ls(as_id(current_folder_id))
  file_match <- files[files$name == file_name, ]

  if (nrow(file_match) == 0) {
    stop(paste0("File not found: ", file_name))
  }

  local_file_path <- file.path(local_path, file_path)

  drive_download(
    file = as_id(file_match$id[1]),
    path = local_file_path,
    overwrite = TRUE
  )
}

download_file_from_subfolder <- function(folder_name, file_name, local_path = data_path) {
  items <- drive_ls(as_id(gdrive_folder_id))
  folder_match <- items[items$name == folder_name, ]

  if (nrow(folder_match) == 0) {
    stop(paste0("Folder not found: ", folder_name))
  }

  files <- drive_ls(as_id(folder_match$id[1]))
  file_match <- files[files$name == file_name, ]

  if (nrow(file_match) == 0) {
    stop(paste0("File not found: ", file_name))
  }

  drive_download(
    file = as_id(file_match$id[1]),
    path = file.path(local_path, file_name),
    overwrite = TRUE
  )
}

download_root_file <- function(file_name, local_path = data_path) {
  items <- drive_ls(as_id(gdrive_folder_id))
  file_match <- items[items$name == file_name, ]

  if (nrow(file_match) == 0) {
    stop(paste0("File not found in root: ", file_name))
  }

  drive_download(
    file = as_id(file_match$id[1]),
    path = file.path(local_path, file_name),
    overwrite = TRUE
  )
}

# Download specific files
download_file_from_subfolder("Master_Chemistry", "20260105_masterdata_chem.csv")
download_root_file("20260106_masterdata_discharge.csv")

# To download ALL files recursively, uncomment:
# download_files_recursive(gdrive_folder_id)
