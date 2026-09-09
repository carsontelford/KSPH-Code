#### 02b Append Static Raster Covariates ####

# This script is the companion static-covariate step for the main 02 Google
# Earth Engine extraction notebook. Use it for covariates that are static over
# time and already exist as local .tif/.tiff rasters, rather than covariates
# pulled from Google Earth Engine.
#
# Run the main 02_predGrid_trainSet_extraction.ipynb notebook first. After it
# has created data/dataset2.csv and data/prediction_grid_covariates_2020_2025.csv,
# run this 02b script to append local static covariate columns to both tables.
#
# Expected inputs:
#   data/dataset2.csv
#     One row per training point-year with id, year, latitude, longitude,
#     outcome, type, country, and all Google Earth Engine covariates.
#
#   data/prediction_grid_covariates_2020_2025.csv
#     One row per prediction grid cell-year with grid_id, x, y, longitude,
#     latitude, year, and all Google Earth Engine covariates.
#
#   Static Covariates/*.tif
#     Static raster covariates stored in the KSPH Code repo folder by default.
#     Each raster is sampled at the point location, and the sanitized raster
#     file name becomes the covariate column name.
#     To add more static covariates later, copy the additional .tif/.tiff files
#     into this folder and rerun this script. New rasters will be extracted and
#     cached; existing cached rasters will be skipped unless
#     OVERWRITE_STATIC_COVARIATES <- TRUE.
#
# Expected outputs:
#   data/dataset2.csv
#   data/prediction_grid_covariates_2020_2025.csv
#     The same tables, overwritten with static covariate columns appended.
#
#   data/static_covariate_exports/*.csv
#     Cached per-raster extraction tables. If a matching cache exists and
#     OVERWRITE_STATIC_COVARIATES is FALSE, the extraction is skipped.
#
# Notes for future analysts:
#   - Add new .tif/.tiff rasters to the Static Covariates folder and rerun.
#   - The Static Covariates folder is tracked in Git, but the raster files
#     inside it are ignored so large local covariates are not pushed to GitHub.
#   - Existing cached covariates are reused automatically.
#   - If you replace a raster but keep the same file name, set
#     OVERWRITE_STATIC_COVARIATES <- TRUE to refresh the cached values.


#### Configuration ####

get_current_script_path <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  }

  for (frame in rev(sys.frames())) {
    if (!is.null(frame$ofile)) {
      return(normalizePath(frame$ofile, winslash = "/", mustWork = TRUE))
    }
  }

  NA_character_
}

find_code_dir <- function() {
  script_path <- get_current_script_path()
  candidates <- c(
    if (!is.na(script_path)) normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE),
    file.path(getwd(), "KSPH Code"),
    getwd()
  )

  candidates <- unique(normalizePath(candidates, winslash = "/", mustWork = FALSE))
  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "config", "predictor_list.csv")) &&
      dir.exists(file.path(candidate, "R_python_code"))
    ) {
      return(candidate)
    }
  }

  stop(
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/02b_append_static_covariates.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/02b_append_static_covariates.R') from the parent folder.",
    call. = FALSE
  )
}

CODE_DIR <- find_code_dir()
PROJECT_DIR <- normalizePath(file.path(CODE_DIR, ".."), winslash = "/", mustWork = TRUE)

# Help R find packages installed in the per-user Windows library, e.g. R/win-library/4.6.
LOCALAPPDATA_DIR <- normalizePath(Sys.getenv("LOCALAPPDATA"), winslash = "/", mustWork = FALSE)
WINDOWS_USER_R_LIB <- file.path(
  LOCALAPPDATA_DIR,
  "R",
  "win-library",
  paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
)
if (dir.exists(WINDOWS_USER_R_LIB) && !WINDOWS_USER_R_LIB %in% .libPaths()) {
  .libPaths(c(WINDOWS_USER_R_LIB, .libPaths()))
}

TRAINING_CSV <- file.path(CODE_DIR, "data", "dataset2.csv")
PREDICTION_GRID_CSV <- file.path(CODE_DIR, "data", "prediction_grid_covariates_2020_2025.csv")
PREDICTOR_LIST_CSV <- file.path(CODE_DIR, "config", "predictor_list.csv")

# By default, the static raster folder lives inside the KSPH Code repo:
#   KSPH Code/Static Covariates
# To use a different folder without editing this script, set a system
# environment variable named STATIC_COVARIATE_DIR before running R.
STATIC_COVARIATE_DIR <- Sys.getenv(
  "STATIC_COVARIATE_DIR",
  unset = file.path(CODE_DIR, "Static Covariates")
)
STATIC_COVARIATE_DIR <- normalizePath(STATIC_COVARIATE_DIR, winslash = "/", mustWork = FALSE)

STATIC_COVARIATE_CACHE_DIR <- file.path(CODE_DIR, "data", "static_covariate_exports")
STATIC_COVARIATE_MANIFEST_CSV <- file.path(
  STATIC_COVARIATE_CACHE_DIR,
  "static_covariate_manifest.csv"
)

OVERWRITE_STATIC_COVARIATES <- FALSE
MAKE_INPUT_BACKUPS <- TRUE
UPDATE_PREDICTOR_LIST <- TRUE

STATIC_RASTER_EXTENSIONS <- "\\.(tif|tiff)$"
RECURSIVE_STATIC_RASTER_SEARCH <- FALSE
STATIC_RASTER_LAYER <- 1L

TRAINING_KEY_COLUMN <- "id"
PREDICTION_GRID_KEY_COLUMN <- "grid_id"
LON_COLUMN <- "longitude"
LAT_COLUMN <- "latitude"

TRAINING_BASE_COLUMNS <- c("id", "year", "latitude", "longitude", "outcome", "type", "country")
PREDICTION_BASE_COLUMNS <- c("grid_id", "grid_batch", "x", "y", "year", "longitude", "latitude", "country")


#### Helpers ####

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      sprintf(
        "Package '%s' is required. Install it before running this script.",
        package
      ),
      call. = FALSE
    )
  }
}

make_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
}

sanitize_covariate_name <- function(path) {
  name <- tools::file_path_sans_ext(basename(path))
  name <- tolower(name)
  name <- gsub("[^a-z0-9]+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) {
    stop("Static raster has a blank covariate name after sanitizing: ", path, call. = FALSE)
  }
  name
}

discover_static_rasters <- function(static_covariate_dir) {
  if (!dir.exists(static_covariate_dir)) {
    stop(
      "Static covariate folder does not exist: ", static_covariate_dir,
      "\nCreate this folder or update STATIC_COVARIATE_DIR.",
      call. = FALSE
    )
  }

  raster_paths <- list.files(
    static_covariate_dir,
    pattern = STATIC_RASTER_EXTENSIONS,
    full.names = TRUE,
    recursive = RECURSIVE_STATIC_RASTER_SEARCH,
    ignore.case = TRUE
  )
  raster_paths <- sort(normalizePath(raster_paths, winslash = "/", mustWork = TRUE))

  if (length(raster_paths) == 0) {
    stop("No .tif/.tiff static covariate rasters found in: ", static_covariate_dir, call. = FALSE)
  }

  covariate_names <- vapply(raster_paths, sanitize_covariate_name, character(1))
  duplicated_names <- unique(covariate_names[duplicated(covariate_names)])
  if (length(duplicated_names) > 0) {
    stop(
      "Static raster names are not unique after sanitizing: ",
      paste(duplicated_names, collapse = ", "),
      "\nRename one of the source files so each raster creates a unique covariate column.",
      call. = FALSE
    )
  }

  data.frame(
    covariate = covariate_names,
    raster_path = raster_paths,
    stringsAsFactors = FALSE
  )
}

check_input_table <- function(df, table_name, key_column) {
  required <- c(key_column, LON_COLUMN, LAT_COLUMN)
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop(
      table_name, " is missing required columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  df[[LON_COLUMN]] <- as.numeric(df[[LON_COLUMN]])
  df[[LAT_COLUMN]] <- as.numeric(df[[LAT_COLUMN]])
  if (any(!is.finite(df[[LON_COLUMN]]) | !is.finite(df[[LAT_COLUMN]]))) {
    stop(table_name, " has non-finite longitude/latitude values.", call. = FALSE)
  }

  df
}

unique_point_table <- function(df, key_column, table_name) {
  point_columns <- c(key_column, LON_COLUMN, LAT_COLUMN)
  points <- unique(df[point_columns])

  duplicated_keys <- unique(points[[key_column]][duplicated(points[[key_column]])])
  if (length(duplicated_keys) > 0) {
    stop(
      table_name, " has key values linked to more than one coordinate: ",
      paste(head(duplicated_keys, 10), collapse = ", "),
      if (length(duplicated_keys) > 10) ", ..." else "",
      call. = FALSE
    )
  }

  points
}

cache_file_path <- function(target_name, covariate_name) {
  file.path(STATIC_COVARIATE_CACHE_DIR, paste0(target_name, "_", covariate_name, ".csv"))
}

is_cache_compatible <- function(cache_df, points_df, key_column, covariate_name) {
  required <- c(key_column, covariate_name)
  if (!all(required %in% names(cache_df))) {
    return(FALSE)
  }

  identical(sort(as.character(cache_df[[key_column]])), sort(as.character(points_df[[key_column]])))
}

read_compatible_cache <- function(cache_path, points_df, key_column, covariate_name) {
  if (!file.exists(cache_path) || OVERWRITE_STATIC_COVARIATES) {
    return(NULL)
  }

  cache_df <- utils::read.csv(cache_path, stringsAsFactors = FALSE)
  if (!is_cache_compatible(cache_df, points_df, key_column, covariate_name)) {
    message("Cached table is not compatible with current points, refreshing: ", cache_path)
    return(NULL)
  }

  cache_df[c(key_column, covariate_name)]
}

extract_static_raster_values <- function(points_df, raster_path, covariate_name, key_column) {
  raster <- terra::rast(raster_path)
  if (terra::nlyr(raster) < STATIC_RASTER_LAYER) {
    stop("Requested STATIC_RASTER_LAYER is not available in: ", raster_path, call. = FALSE)
  }

  raster <- raster[[STATIC_RASTER_LAYER]]
  points <- terra::vect(
    points_df,
    geom = c(LON_COLUMN, LAT_COLUMN),
    crs = "EPSG:4326",
    keepgeom = FALSE
  )

  if (!terra::same.crs(raster, points)) {
    points <- terra::project(points, terra::crs(raster))
  }

  values <- terra::extract(raster, points, ID = FALSE)
  output <- points_df[key_column]
  output[[covariate_name]] <- values[[1]]
  output
}

extract_or_read_static_covariate <- function(points_df, raster_path, covariate_name, key_column, target_name) {
  cache_path <- cache_file_path(target_name, covariate_name)
  cached <- read_compatible_cache(cache_path, points_df, key_column, covariate_name)
  if (!is.null(cached)) {
    message(target_name, " | ", covariate_name, " | cached | rows=", format(nrow(cached), big.mark = ","))
    return(list(data = cached, cache_path = cache_path, status = "cached"))
  }

  message(target_name, " | ", covariate_name, " | extracting from ", basename(raster_path))
  extracted <- extract_static_raster_values(points_df, raster_path, covariate_name, key_column)
  utils::write.csv(extracted, cache_path, row.names = FALSE)
  message(target_name, " | ", covariate_name, " | wrote cache | rows=", format(nrow(extracted), big.mark = ","))

  list(data = extracted, cache_path = cache_path, status = "extracted")
}

drop_existing_static_columns <- function(df, covariate_names) {
  existing <- intersect(covariate_names, names(df))
  if (length(existing) > 0) {
    df <- df[setdiff(names(df), existing)]
  }
  df
}

append_covariate_tables <- function(df, extraction_tables, key_column) {
  for (covariate_name in names(extraction_tables)) {
    df <- merge(
      df,
      extraction_tables[[covariate_name]],
      by = key_column,
      all.x = TRUE,
      sort = FALSE
    )
  }

  df
}

restore_column_order <- function(df, original_columns, covariate_names) {
  ordered_columns <- c(original_columns, covariate_names)
  extra_columns <- setdiff(names(df), ordered_columns)
  df[c(ordered_columns[ordered_columns %in% names(df)], extra_columns)]
}

make_input_backup <- function(path) {
  if (!MAKE_INPUT_BACKUPS || !file.exists(path)) {
    return(invisible(FALSE))
  }

  backup_path <- file.path(
    dirname(path),
    paste0(tools::file_path_sans_ext(basename(path)), "_pre_static_backup.csv")
  )
  if (!file.exists(backup_path)) {
    file.copy(path, backup_path)
    message("Wrote one-time input backup: ", backup_path)
  }

  invisible(TRUE)
}

write_updated_table <- function(df, output_csv) {
  make_dir(dirname(output_csv))
  utils::write.csv(df, output_csv, row.names = FALSE, na = "")
  message("Wrote updated table: ", output_csv)
}

append_static_covariates_to_table <- function(input_csv, output_csv, target_name, key_column, raster_index) {
  if (!file.exists(input_csv)) {
    stop("Input CSV does not exist: ", input_csv, call. = FALSE)
  }

  df <- utils::read.csv(input_csv, stringsAsFactors = FALSE)
  df <- check_input_table(df, basename(input_csv), key_column)

  covariate_names <- raster_index$covariate
  original_columns <- setdiff(names(df), covariate_names)
  df <- drop_existing_static_columns(df, covariate_names)

  points_df <- unique_point_table(df, key_column, basename(input_csv))
  message(
    target_name, " | unique extraction points: ",
    format(nrow(points_df), big.mark = ","),
    " from table rows: ",
    format(nrow(df), big.mark = ",")
  )

  extraction_tables <- list()
  manifest_rows <- vector("list", nrow(raster_index))
  for (i in seq_len(nrow(raster_index))) {
    covariate_name <- raster_index$covariate[i]
    result <- extract_or_read_static_covariate(
      points_df = points_df,
      raster_path = raster_index$raster_path[i],
      covariate_name = covariate_name,
      key_column = key_column,
      target_name = target_name
    )
    extraction_tables[[covariate_name]] <- result$data

    manifest_rows[[i]] <- data.frame(
      target = target_name,
      covariate = covariate_name,
      raster_path = raster_index$raster_path[i],
      cache_path = result$cache_path,
      status = result$status,
      rows = nrow(result$data),
      nonmissing = sum(!is.na(result$data[[covariate_name]])),
      stringsAsFactors = FALSE
    )
  }

  df <- append_covariate_tables(df, extraction_tables, key_column)
  df <- restore_column_order(df, original_columns, covariate_names)

  make_input_backup(input_csv)
  write_updated_table(df, output_csv)

  do.call(rbind, manifest_rows)
}

update_predictor_list <- function(covariate_names) {
  if (!UPDATE_PREDICTOR_LIST) {
    return(invisible(NULL))
  }

  if (!file.exists(PREDICTOR_LIST_CSV)) {
    message("Predictor list not found, skipping update: ", PREDICTOR_LIST_CSV)
    return(invisible(NULL))
  }

  predictor_list <- utils::read.csv(PREDICTOR_LIST_CSV, stringsAsFactors = FALSE)
  required_columns <- c("predictor", "group", "buffer", "scaled", "include_in_model")
  missing_required <- setdiff(required_columns, names(predictor_list))
  if (length(missing_required) > 0) {
    message(
      "Predictor list is missing expected columns, skipping update: ",
      paste(missing_required, collapse = ", ")
    )
    return(invisible(NULL))
  }

  missing_covariates <- setdiff(covariate_names, predictor_list$predictor)
  if (length(missing_covariates) == 0) {
    message("Predictor list already includes all static covariates.")
    return(invisible(NULL))
  }

  additions <- data.frame(
    predictor = missing_covariates,
    group = "static_local",
    buffer = "point",
    scaled = FALSE,
    include_in_model = TRUE,
    stringsAsFactors = FALSE
  )

  predictor_list <- rbind(predictor_list, additions)
  utils::write.csv(predictor_list, PREDICTOR_LIST_CSV, row.names = FALSE)
  message(
    "Added static covariates to predictor list: ",
    paste(missing_covariates, collapse = ", ")
  )

  invisible(NULL)
}

check_training_prediction_static_match <- function(training_csv, prediction_grid_csv, covariate_names) {
  training_header <- names(utils::read.csv(training_csv, nrows = 0, stringsAsFactors = FALSE))
  prediction_header <- names(utils::read.csv(prediction_grid_csv, nrows = 0, stringsAsFactors = FALSE))

  missing_training <- setdiff(covariate_names, training_header)
  missing_prediction <- setdiff(covariate_names, prediction_header)
  if (length(missing_training) > 0 || length(missing_prediction) > 0) {
    stop(
      "Static covariates were not appended consistently.\n",
      "Missing from training: ", paste(missing_training, collapse = ", "),
      "\nMissing from prediction grid: ", paste(missing_prediction, collapse = ", "),
      call. = FALSE
    )
  }

  training_predictors <- setdiff(training_header, TRAINING_BASE_COLUMNS)
  prediction_predictors <- setdiff(prediction_header, PREDICTION_BASE_COLUMNS)
  if (!identical(training_predictors, prediction_predictors)) {
    stop(
      "Training and prediction-grid predictor columns are not identical after appending static covariates.\n",
      "Missing from training: ", paste(setdiff(prediction_predictors, training_predictors), collapse = ", "),
      "\nExtra in training: ", paste(setdiff(training_predictors, prediction_predictors), collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


#### 1. Discover Static Raster Covariates ####

require_package("terra")
make_dir(STATIC_COVARIATE_CACHE_DIR)

message("KSPH Code directory: ", CODE_DIR)
message("Static covariate folder: ", STATIC_COVARIATE_DIR)
message("Static covariate cache folder: ", STATIC_COVARIATE_CACHE_DIR)
message("Overwrite cached static covariates: ", OVERWRITE_STATIC_COVARIATES)

static_rasters <- discover_static_rasters(STATIC_COVARIATE_DIR)
message("Static rasters found: ", nrow(static_rasters))
print(static_rasters)


#### 2. Append Static Covariates To Training Dataset ####

training_manifest <- append_static_covariates_to_table(
  input_csv = TRAINING_CSV,
  output_csv = TRAINING_CSV,
  target_name = "training",
  key_column = TRAINING_KEY_COLUMN,
  raster_index = static_rasters
)


#### 3. Append Static Covariates To Prediction Grid Dataset ####

prediction_manifest <- append_static_covariates_to_table(
  input_csv = PREDICTION_GRID_CSV,
  output_csv = PREDICTION_GRID_CSV,
  target_name = "predgrid",
  key_column = PREDICTION_GRID_KEY_COLUMN,
  raster_index = static_rasters
)


#### 4. Save Manifest And Update Predictor List ####

manifest <- rbind(training_manifest, prediction_manifest)
utils::write.csv(manifest, STATIC_COVARIATE_MANIFEST_CSV, row.names = FALSE)
message("Wrote static covariate manifest: ", STATIC_COVARIATE_MANIFEST_CSV)

update_predictor_list(static_rasters$covariate)
check_training_prediction_static_match(TRAINING_CSV, PREDICTION_GRID_CSV, static_rasters$covariate)

message("Static covariate append complete.")
