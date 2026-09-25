#### 03 Add Post-Extraction Covariates ####

# This script is the companion post-extraction covariate step for the main 02
# Google Earth Engine extraction notebook. It does two things:
#   1. appends static local raster covariates that already exist as
#      .tif/.tiff files, and
#   2. creates derived interaction covariates used by the descriptive and
#      modeling scripts.
#
# Run the main 02_predGrid_trainSet_extraction.ipynb notebook first. After it
# has created analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv and
# analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2001_2025.csv,
# run this 03 script to add local static covariate columns and derived
# covariate columns to both tables.
#
# Expected inputs:
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv
#     One row per training point-year with id, year, latitude, longitude,
#     outcome, type, country, and all Google Earth Engine covariates.
#
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2001_2025.csv
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
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2001_2025.csv
#     The same tables, overwritten with static and derived covariate columns
#     appended.
#
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/static_covariate_exports/*.csv
#     Cached per-raster extraction tables. If a matching cache exists and
#     OVERWRITE_STATIC_COVARIATES is FALSE, the extraction is skipped.
#
# Notes for future analysts:
#   - Add new .tif/.tiff rasters to the Static Covariates folder and rerun.
#   - The Static Covariates folder is tracked in Git, but the raster files
#     inside it are ignored so large local covariates are not pushed to GitHub.
#   - Existing cached covariates are reused only when
#     OVERWRITE_STATIC_COVARIATES <- FALSE.
#   - If you replace a raster but keep the same file name, set
#     OVERWRITE_STATIC_COVARIATES <- TRUE to refresh the cached values.
#   - Derived covariates are rebuilt every time this script runs. Current
#     derived covariates are:
#       forest_cover_x_log_pop_density_<scale>
#       forest_edge_x_log_pop_density_<scale>
#     where log population density means log1p(pop_density_<scale>).


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
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/03_add_covars.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/03_add_covars.R') from the parent folder.",
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

sanitize_path_component <- function(value) {
  value <- trimws(as.character(value))
  value <- gsub("[^A-Za-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) {
    stop("Analysis folder name cannot be blank after sanitizing.", call. = FALSE)
  }
  value
}

# STUDY_AREA_ANALYSIS_NAME chooses which study-area extraction products to
# update. Static covariates are appended to:
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2001_2025.csv
#
# Example for a separate DRC run:
#   STUDY_AREA_ANALYSIS_NAME <- "drc"
STUDY_AREA_ANALYSIS_NAME <- "equatorial_africa"
ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
DATA_DIR <- file.path(ANALYSIS_DIR, "data")

# Keep FALSE for production-style runs. Set TRUE only for a one-time migration
# check if you intentionally need to read old root-level KSPH Code/data files.
ALLOW_LEGACY_PATH_FALLBACK <- FALSE

# Optional migration fallback: if enabled and the new analysis data folder does
# not contain completed equatorial Africa covariate files, use old root-level
# files. DRC and other new study-area analyses never fall back.
LEGACY_DATA_DIR <- file.path(CODE_DIR, "data")
if (
  isTRUE(ALLOW_LEGACY_PATH_FALLBACK) &&
  identical(ACTIVE_STUDY_AREA_ANALYSIS_NAME, "equatorial_africa") &&
    (!file.exists(file.path(DATA_DIR, "dataset2.csv")) ||
       !file.exists(file.path(DATA_DIR, "prediction_grid_covariates_2001_2025.csv"))) &&
    file.exists(file.path(LEGACY_DATA_DIR, "dataset2.csv")) &&
    file.exists(file.path(LEGACY_DATA_DIR, "prediction_grid_covariates_2001_2025.csv"))
) {
  message("Using legacy root-level data folder because the equatorial Africa analysis data folder is not complete yet: ", LEGACY_DATA_DIR)
  DATA_DIR <- LEGACY_DATA_DIR
}

TRAINING_CSV <- file.path(DATA_DIR, "dataset2.csv")
PREDICTION_GRID_CSV <- file.path(DATA_DIR, "prediction_grid_covariates_2001_2025.csv")
PREDICTION_GRID_POINTS_CSV <- file.path(DATA_DIR, "prediction_grid_5km.csv")
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

STATIC_COVARIATE_CACHE_DIR <- file.path(DATA_DIR, "static_covariate_exports")
STATIC_COVARIATE_MANIFEST_CSV <- file.path(
  STATIC_COVARIATE_CACHE_DIR,
  "static_covariate_manifest.csv"
)

# Set TRUE for production reruns after changing the prediction grid resolution
# or replacing any static raster files. Once the production 5 km tables are
# complete, analysts can set this back to FALSE to reuse the cache.
OVERWRITE_STATIC_COVARIATES <- TRUE
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
DERIVED_SPATIAL_SCALES <- c("0_10km", "10_25km", "25_50km")
DEPRECATED_DERIVED_COVARIATES <- as.vector(outer(
  c("forest_cover_x_pop_density_", "forest_edge_x_pop_density_"),
  DERIVED_SPATIAL_SCALES,
  paste0
))


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
  if (length(raster_paths) == 0) {
    return(data.frame(
      covariate = character(),
      raster_path = character(),
      stringsAsFactors = FALSE
    ))
  }

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

validate_prediction_grid_coverage <- function(prediction_df, key_column) {
  if (!file.exists(PREDICTION_GRID_POINTS_CSV)) {
    return(invisible(TRUE))
  }

  grid_points <- utils::read.csv(PREDICTION_GRID_POINTS_CSV, stringsAsFactors = FALSE)
  if (!key_column %in% names(grid_points)) {
    stop("Prediction-grid points file is missing key column: ", key_column, call. = FALSE)
  }

  expected_ids <- unique(as.character(grid_points[[key_column]]))
  observed_ids <- unique(as.character(prediction_df[[key_column]]))
  missing_ids <- setdiff(expected_ids, observed_ids)
  extra_ids <- setdiff(observed_ids, expected_ids)

  if (length(missing_ids) > 0 || length(extra_ids) > 0) {
    stop(
      "Prediction-grid covariate table does not match the full prediction grid before static covariates are appended.\n",
      "Expected unique grid cells from ", basename(PREDICTION_GRID_POINTS_CSV), ": ", format(length(expected_ids), big.mark = ","), "\n",
      "Observed unique grid cells in ", basename(PREDICTION_GRID_CSV), ": ", format(length(observed_ids), big.mark = ","), "\n",
      "Missing grid cells: ", format(length(missing_ids), big.mark = ","), "\n",
      "Extra grid cells: ", format(length(extra_ids), big.mark = ","), "\n",
      "This usually means the prediction-grid Drive exports were only partially merged. Re-run the 02 prediction-grid merge after all batches are synced.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

restore_column_order <- function(df, original_columns, covariate_names) {
  ordered_columns <- c(original_columns, covariate_names)
  extra_columns <- setdiff(names(df), ordered_columns)
  df[c(ordered_columns[ordered_columns %in% names(df)], extra_columns)]
}

derived_covariate_specs <- function(scales = DERIVED_SPATIAL_SCALES) {
  rows <- lapply(scales, function(scale) {
    data.frame(
      covariate = c(
        paste0("forest_cover_x_log_pop_density_", scale),
        paste0("forest_edge_x_log_pop_density_", scale)
      ),
      left = c(
        paste0("forest_cover_prop_", scale),
        paste0("frag_edge_prop_", scale)
      ),
      right = paste0("pop_density_", scale),
      right_transform = "log1p",
      group = c("forest_population_interaction", "edge_population_interaction"),
      buffer = scale,
      scaled = TRUE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

add_derived_covariates <- function(df, specs, table_name) {
  for (i in seq_len(nrow(specs))) {
    output_column <- specs$covariate[i]
    left_column <- specs$left[i]
    right_column <- specs$right[i]
    missing_inputs <- setdiff(c(left_column, right_column), names(df))
    if (length(missing_inputs) > 0) {
      stop(
        table_name, " is missing input column(s) required for ", output_column,
        ": ", paste(missing_inputs, collapse = ", "),
        call. = FALSE
      )
    }
    left_values <- suppressWarnings(as.numeric(df[[left_column]]))
    right_values <- suppressWarnings(as.numeric(df[[right_column]]))
    if ("right_transform" %in% names(specs) && identical(specs$right_transform[i], "log1p")) {
      negative_rows <- is.finite(right_values) & right_values < 0
      if (any(negative_rows)) {
        warning(
          table_name, " has ", sum(negative_rows), " negative values in ",
          right_column, "; setting them to NA before log1p for ", output_column,
          call. = FALSE
        )
        right_values[negative_rows] <- NA_real_
      }
      right_values <- log1p(right_values)
    }
    df[[output_column]] <- left_values * right_values
  }
  df
}

add_derived_covariates_to_table <- function(input_csv, output_csv, target_name, specs) {
  if (!file.exists(input_csv)) {
    stop("Input CSV does not exist: ", input_csv, call. = FALSE)
  }
  df <- utils::read.csv(input_csv, stringsAsFactors = FALSE)
  original_columns <- setdiff(names(df), c(specs$covariate, DEPRECATED_DERIVED_COVARIATES))
  df <- df[original_columns]
  df <- add_derived_covariates(df, specs, basename(input_csv))
  df <- restore_column_order(df, original_columns, specs$covariate)
  write_updated_table(df, output_csv)
  message(
    target_name, " | derived covariates added: ",
    paste(specs$covariate, collapse = ", ")
  )
  invisible(specs$covariate)
}

make_input_backup <- function(path) {
  if (!MAKE_INPUT_BACKUPS || !file.exists(path)) {
    return(invisible(FALSE))
  }

  backup_path <- file.path(
    dirname(path),
    paste0(tools::file_path_sans_ext(basename(path)), "_pre_add_covars_backup.csv")
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
  if (identical(target_name, "predgrid")) {
    validate_prediction_grid_coverage(df, key_column)
  }

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

empty_static_manifest <- function() {
  data.frame(
    target = character(),
    covariate = character(),
    raster_path = character(),
    cache_path = character(),
    status = character(),
    rows = integer(),
    nonmissing = integer(),
    stringsAsFactors = FALSE
  )
}

update_predictor_list <- function(covariate_names, group = "static_local", buffer = "point", scaled = FALSE) {
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
    message("Predictor list already includes all requested covariates.")
    return(invisible(NULL))
  }

  covariate_meta <- data.frame(
    predictor = covariate_names,
    group = rep(group, length.out = length(covariate_names)),
    buffer = rep(buffer, length.out = length(covariate_names)),
    scaled = rep(scaled, length.out = length(covariate_names)),
    include_in_model = TRUE,
    stringsAsFactors = FALSE
  )
  additions <- covariate_meta[match(missing_covariates, covariate_meta$predictor), , drop = FALSE]

  predictor_list <- rbind(predictor_list, additions)
  utils::write.csv(predictor_list, PREDICTOR_LIST_CSV, row.names = FALSE)
  message(
    "Added covariates to predictor list: ",
    paste(missing_covariates, collapse = ", ")
  )

  invisible(NULL)
}

remove_predictors_from_predictor_list <- function(covariate_names) {
  if (!UPDATE_PREDICTOR_LIST || length(covariate_names) == 0 || !file.exists(PREDICTOR_LIST_CSV)) {
    return(invisible(NULL))
  }

  predictor_list <- utils::read.csv(PREDICTOR_LIST_CSV, stringsAsFactors = FALSE)
  if (!"predictor" %in% names(predictor_list)) {
    return(invisible(NULL))
  }
  keep <- !predictor_list$predictor %in% covariate_names
  removed <- predictor_list$predictor[!keep]
  if (length(removed) == 0) {
    return(invisible(NULL))
  }
  predictor_list <- predictor_list[keep, , drop = FALSE]
  utils::write.csv(predictor_list, PREDICTOR_LIST_CSV, row.names = FALSE)
  message("Removed deprecated covariates from predictor list: ", paste(removed, collapse = ", "))
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

make_dir(STATIC_COVARIATE_CACHE_DIR)

message("KSPH Code directory: ", CODE_DIR)
message("Study-area analysis: ", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
message("Analysis data directory: ", DATA_DIR)
message("Static covariate folder: ", STATIC_COVARIATE_DIR)
message("Static covariate cache folder: ", STATIC_COVARIATE_CACHE_DIR)
message("Overwrite cached static covariates: ", OVERWRITE_STATIC_COVARIATES)

static_rasters <- discover_static_rasters(STATIC_COVARIATE_DIR)
message("Static rasters found: ", nrow(static_rasters))
print(static_rasters)
if (nrow(static_rasters) > 0) {
  require_package("terra")
}


#### 2. Append Static Covariates To Training Dataset ####

if (nrow(static_rasters) > 0) {
  training_manifest <- append_static_covariates_to_table(
    input_csv = TRAINING_CSV,
    output_csv = TRAINING_CSV,
    target_name = "training",
    key_column = TRAINING_KEY_COLUMN,
    raster_index = static_rasters
  )
} else {
  message("No static rasters found; skipping static covariate extraction for training.")
  training_manifest <- empty_static_manifest()
}


#### 3. Append Static Covariates To Prediction Grid Dataset ####

if (nrow(static_rasters) > 0) {
  prediction_manifest <- append_static_covariates_to_table(
    input_csv = PREDICTION_GRID_CSV,
    output_csv = PREDICTION_GRID_CSV,
    target_name = "predgrid",
    key_column = PREDICTION_GRID_KEY_COLUMN,
    raster_index = static_rasters
  )
} else {
  message("No static rasters found; skipping static covariate extraction for prediction grid.")
  prediction_manifest <- empty_static_manifest()
}


#### 4. Add Derived Interaction Covariates ####

derived_specs <- derived_covariate_specs()
message("Derived covariates to create: ", nrow(derived_specs))
print(derived_specs[c("covariate", "left", "right", "right_transform")])

training_derived_covariates <- add_derived_covariates_to_table(
  input_csv = TRAINING_CSV,
  output_csv = TRAINING_CSV,
  target_name = "training",
  specs = derived_specs
)

prediction_derived_covariates <- add_derived_covariates_to_table(
  input_csv = PREDICTION_GRID_CSV,
  output_csv = PREDICTION_GRID_CSV,
  target_name = "predgrid",
  specs = derived_specs
)

if (!identical(training_derived_covariates, prediction_derived_covariates)) {
  stop("Derived covariate names differ between training and prediction grid outputs.", call. = FALSE)
}


#### 5. Save Manifest And Update Predictor List ####

manifest <- rbind(training_manifest, prediction_manifest)
utils::write.csv(manifest, STATIC_COVARIATE_MANIFEST_CSV, row.names = FALSE)
message("Wrote static covariate manifest: ", STATIC_COVARIATE_MANIFEST_CSV)

update_predictor_list(static_rasters$covariate)
remove_predictors_from_predictor_list(DEPRECATED_DERIVED_COVARIATES)
update_predictor_list(
  covariate_names = derived_specs$covariate,
  group = derived_specs$group,
  buffer = derived_specs$buffer,
  scaled = derived_specs$scaled
)
check_training_prediction_static_match(
  TRAINING_CSV,
  PREDICTION_GRID_CSV,
  c(static_rasters$covariate, derived_specs$covariate)
)

message("Post-extraction covariate add complete.")
