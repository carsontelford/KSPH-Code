#### 08 Final Full-Data SuperLearner Historical-Anomaly Predictions ####

# Purpose:
#   Fit the final tuned SuperLearner ensemble using all available training years
#   and predict every prediction-grid year currently available. The outputs are
#   annual probability rasters, annual relative-odds-ratio (ROR) rasters, and
#   historical ROR anomaly rasters.
#
#   The historical anomaly is calculated as:
#       ROR in pixel-year / mean ROR for that same pixel across all available
#       prediction years.
#
#   This is intentionally separate from script 06. Script 06 is the prospective
#   forward-stepwise validation workflow. This script is the final descriptive
#   mapping workflow after model tuning and validation have been completed.


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
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    active_doc <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) NA_character_)
    if (!is.na(active_doc) && nzchar(active_doc)) {
      return(normalizePath(active_doc, winslash = "/", mustWork = TRUE))
    }
  }
  NA_character_
}

find_code_dir <- function() {
  script_path <- get_current_script_path()
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  script_dir <- if (!is.na(script_path)) dirname(script_path) else NA_character_
  wd_parents <- normalizePath(file.path(wd, c(".", "..", "../..", "../../..")), winslash = "/", mustWork = FALSE)
  script_candidates <- if (!is.na(script_dir)) {
    normalizePath(file.path(script_dir, c(".", "..", "../..")), winslash = "/", mustWork = FALSE)
  } else {
    character(0)
  }
  candidates <- unique(normalizePath(c(script_candidates, wd_parents, file.path(wd_parents, "KSPH Code")), winslash = "/", mustWork = FALSE))
  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "config", "predictor_list.csv")) &&
      dir.exists(file.path(candidate, "R_python_code"))
    ) {
      return(candidate)
    }
  }
  stop(
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/08_train_predict_full_superlearner_historical_anomaly.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/08_train_predict_full_superlearner_historical_anomaly.R') from the parent folder.",
    call. = FALSE
  )
}

CODE_DIR <- find_code_dir()
source(file.path(CODE_DIR, "R_python_code", "modeling_helpers.R"))

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

STUDY_AREA_ANALYSIS_NAME <- "equatorial_africa"

# Run both final products by default. To run only one, remove the other row.
FINAL_ANALYSES_TO_RUN <- data.frame(
  subanalysis_name = c("", ""),
  training_type_filter = c("", "Z"),
  stringsAsFactors = FALSE
)

TUNING_MODEL_FAMILY_NAME <- "superlearner"
FINAL_MODEL_FAMILY_NAME <- "final_sl"
MAP_SET_NAME <- "maps"
RANDOM_SEED <- 20260923

OUTCOME_COLUMN <- "outcome"
EVENT_VALUE <- 1L
CONTROL_VALUE <- 0L
TRAINING_BASE_COLUMNS <- c("id", "year", "latitude", "longitude", "outcome", "type", "country")
PREDICTION_BASE_COLUMNS <- c("grid_id", "grid_batch", "x", "y", "year", "longitude", "latitude", "country")
RASTER_CRS <- "EPSG:4326"

SL_INTERNAL_FOLDS <- 3
SL_LIBRARY <- c("SL.glm", "SL.rpart_tuned", "SL.ranger_tuned")
SL_METHOD <- "method.NNloglik"
RANGER_THREADS <- 1
USE_CLASS_WEIGHTS <- FALSE
CONTROL_RATIO <- 50L
CONTROL_SAMPLE_REPLACE <- TRUE
FINAL_RESAMPLED_ENSEMBLE_SIZE <- 50L

HANSEN_ZERO_FILL_PREFIXES <- c(
  "forest_cover_prop_",
  "flsy_prop_",
  "fl1yp_prop_",
  "fl2yp_prop_",
  "frag_edge_prop_"
)
LATEST_AVAILABLE_COVARIATE_FILLS <- list()
MAX_PREDICTION_GRID_MISSING_PROP <- 0.20

TOP_PERCENTILE <- 0.99
ODDS_EPSILON <- 1e-6
RASTER_WRITE_OPTIONS <- list(
  datatype = "FLT4S",
  gdal = c("COMPRESS=LZW")
)

# Prediction-grid covariate files are discovered automatically. Backup files are
# ignored. If multiple files contain the same year, the year with the larger grid
# is retained; ties use the newest file. Years whose grid geometry does not match
# the active production grid are skipped with a warning.
PREDICTION_GRID_FILE_PATTERN <- "^prediction_grid_covariates_[0-9]{4}_[0-9]{4}\\.csv$"

OVERWRITE_FINAL_MODELS <- TRUE
OVERWRITE_PREDICTION_TABLES <- TRUE
OVERWRITE_RASTERS <- TRUE
WRITE_COMBINED_SUMMARY_TABLE <- TRUE

ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
DATA_DIR <- file.path(ANALYSIS_DIR, "data")
ANALYSIS_MODEL_DIR <- file.path(ANALYSIS_DIR, "models")
ANALYSIS_OUTPUT_DIR <- file.path(ANALYSIS_DIR, "outputs")
TRAINING_CSV <- file.path(DATA_DIR, "dataset2.csv")


#### Helpers ####

candidate_from_tuned_settings <- function(best_settings) {
  as.list(best_settings[1, , drop = FALSE])
}

prediction_grid_source_paths <- function() {
  paths <- list.files(DATA_DIR, pattern = PREDICTION_GRID_FILE_PATTERN, full.names = TRUE)
  paths <- paths[!grepl("backup|pre_static|pre_add_covars|DO_NOT_USE", basename(paths), ignore.case = TRUE)]
  if (length(paths) == 0) {
    stop("No prediction-grid covariate CSV files were found in: ", DATA_DIR, call. = FALSE)
  }
  paths[order(file.info(paths)$mtime, decreasing = TRUE)]
}

same_grid <- function(a, b) {
  if (!all(c("grid_id", "x", "y", "longitude", "latitude") %in% names(a)) ||
      !all(c("grid_id", "x", "y", "longitude", "latitude") %in% names(b))) {
    return(FALSE)
  }
  a_key <- a[order(a$grid_id), c("grid_id", "x", "y", "longitude", "latitude"), drop = FALSE]
  b_key <- b[order(b$grid_id), c("grid_id", "x", "y", "longitude", "latitude"), drop = FALSE]
  if (!identical(a_key$grid_id, b_key$grid_id)) {
    return(FALSE)
  }
  numeric_columns <- c("x", "y", "longitude", "latitude")
  max_abs_diff <- max(abs(as.matrix(a_key[numeric_columns]) - as.matrix(b_key[numeric_columns])), na.rm = TRUE)
  is.finite(max_abs_diff) && max_abs_diff < 1e-9
}

read_available_prediction_grid <- function() {
  paths <- prediction_grid_source_paths()
  message("Prediction-grid source files:")
  message("  ", paste(basename(paths), collapse = "\n  "))

  best_by_year <- list()
  best_meta <- data.frame()

  for (path in paths) {
    message("Reading prediction-grid covariates: ", path)
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
    if (!"year" %in% names(df)) {
      warning("Skipping prediction-grid file without a year column: ", path, call. = FALSE)
      next
    }
    df$year <- as.integer(df$year)
    source_mtime <- file.info(path)$mtime
    for (year in sort(unique(df$year))) {
      year_df <- df[df$year == year, , drop = FALSE]
      candidate_score <- nrow(year_df)
      existing <- best_meta[best_meta$year == year, , drop = FALSE]
      replace_existing <- nrow(existing) == 0 ||
        candidate_score > existing$rows ||
        (candidate_score == existing$rows && source_mtime > existing$source_mtime)
      if (replace_existing) {
        best_by_year[[as.character(year)]] <- year_df
        best_meta <- best_meta[best_meta$year != year, , drop = FALSE]
        best_meta <- rbind(
          best_meta,
          data.frame(
            year = year,
            rows = candidate_score,
            source_file = path,
            source_mtime = source_mtime,
            stringsAsFactors = FALSE
          )
        )
      }
    }
    rm(df)
    gc()
  }

  if (length(best_by_year) == 0) {
    stop("No usable prediction-grid rows were found.", call. = FALSE)
  }

  best_meta <- best_meta[order(best_meta$year), , drop = FALSE]
  reference_year <- best_meta$year[which.max(best_meta$rows)]
  reference_grid <- best_by_year[[as.character(reference_year)]]
  keep_years <- integer(0)
  dropped <- data.frame()

  for (year in best_meta$year) {
    year_df <- best_by_year[[as.character(year)]]
    if (same_grid(reference_grid, year_df)) {
      keep_years <- c(keep_years, year)
    } else {
      dropped <- rbind(
        dropped,
        data.frame(
          year = year,
          rows = nrow(year_df),
          source_file = best_meta$source_file[best_meta$year == year],
          reason = paste0("grid_does_not_match_reference_year_", reference_year),
          stringsAsFactors = FALSE
        )
      )
    }
  }

  if (nrow(dropped) > 0) {
    warning(
      "Skipped prediction-grid year(s) because their grid geometry did not match the active production grid: ",
      paste(dropped$year, collapse = ", "),
      ". Re-extract those year(s) at the current grid resolution before using them in the historical baseline.",
      call. = FALSE
    )
  }

  retained <- do.call(rbind, best_by_year[as.character(sort(keep_years))])
  retained <- retained[order(retained$year, retained$grid_id), , drop = FALSE]
  attr(retained, "source_meta") <- best_meta[best_meta$year %in% keep_years, , drop = FALSE]
  attr(retained, "dropped_meta") <- dropped
  retained
}

final_model_path <- function(final_model_dir) {
  file.path(final_model_dir, "full_superlearner_fit.rds")
}

prediction_table_path <- function(prediction_table_dir, year) {
  file.path(prediction_table_dir, sprintf("preds_%s.csv", year))
}

summary_table_path <- function(prediction_table_dir, year) {
  file.path(prediction_table_dir, sprintf("summ_%s.csv", year))
}

annual_summary_raster_path <- function(annual_raster_dir, year) {
  file.path(annual_raster_dir, sprintf("prob_%s.tif", year))
}

ror_raster_path <- function(ror_dir, year) {
  file.path(ror_dir, sprintf("ror_%s.tif", year))
}

historical_mean_ror_path <- function(historical_dir, years) {
  file.path(historical_dir, sprintf("hist_mean_ror_%s_%s.tif", min(years), max(years)))
}

historical_anomaly_raster_path <- function(anomaly_dir, year) {
  file.path(anomaly_dir, sprintf("hist_anom_%s.tif", year))
}

prepare_final_training_data <- function(dataset2, predictor_names, training_type_filter) {
  training_df <- filter_training_dataset_by_type(dataset2, training_type_filter, OUTCOME_COLUMN, EVENT_VALUE)
  missing_predictors <- setdiff(predictor_names, names(training_df))
  if (length(missing_predictors) > 0) {
    stop("Training data are missing predictors: ", paste(missing_predictors, collapse = ", "), call. = FALSE)
  }
  training_df <- as_numeric_predictors(training_df, predictor_names)
  training_df <- fill_hansen_na_with_zero(training_df)
  imputation_values <- fit_imputation_values(training_df, predictor_names)
  training_df <- apply_imputation_values(training_df, predictor_names, imputation_values, "final training data")
  training_df <- log_transform_population_predictors(training_df, predictor_names, "final training data")
  training_df[[OUTCOME_COLUMN]] <- as.integer(training_df[[OUTCOME_COLUMN]])

  n_event <- sum(training_df[[OUTCOME_COLUMN]] == EVENT_VALUE)
  n_control <- sum(training_df[[OUTCOME_COLUMN]] == CONTROL_VALUE)
  if (n_event == 0 || n_control == 0) {
    stop(
      "Final training data need at least one event and one control. Events=", n_event,
      "; controls=", n_control,
      call. = FALSE
    )
  }
  list(training_df = training_df, imputation_values = imputation_values)
}

prepare_year_prediction_grid <- function(prediction_grid, predictor_names, imputation_values, year) {
  year_df <- prediction_grid[prediction_grid$year == year, , drop = FALSE]
  if (nrow(year_df) == 0) {
    stop("Prediction grid has no rows for year ", year, ".", call. = FALSE)
  }
  prepare_prediction_grid_data(
    year_df,
    predictor_names,
    imputation_values,
    prediction_years = year,
    fill_rules = LATEST_AVAILABLE_COVARIATE_FILLS
  )
}

fit_final_superlearner_ensemble <- function(training_df, predictor_names, best_candidate, subanalysis_seed) {
  fit_superlearner_resample_ensemble(
    training_df = training_df,
    predictor_names = predictor_names,
    candidate = best_candidate,
    ensemble_size = FINAL_RESAMPLED_ENSEMBLE_SIZE,
    control_ratio = CONTROL_RATIO,
    replace_controls = CONTROL_SAMPLE_REPLACE,
    outcome_column = OUTCOME_COLUMN,
    event_value = EVENT_VALUE,
    control_value = CONTROL_VALUE,
    seed_base = subanalysis_seed
  )
}

write_annual_rasters <- function(summary_table, year, annual_raster_dir, ror_dir) {
  summary_raster <- prediction_summary_to_raster_stack(summary_table, raster_crs = RASTER_CRS)
  annual_path <- annual_summary_raster_path(annual_raster_dir, year)
  make_parent_dir(annual_path)
  terra::writeRaster(summary_raster, annual_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
  message("  Wrote annual probability raster: ", annual_path)

  ror_raster <- build_ror_raster(summary_raster, top_percentile = TOP_PERCENTILE, epsilon = ODDS_EPSILON)
  ror_path <- ror_raster_path(ror_dir, year)
  make_parent_dir(ror_path)
  terra::writeRaster(ror_raster, ror_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
  message("  Wrote ROR raster: ", ror_path)
}

build_historical_mean_ror <- function(years, ror_dir, historical_dir) {
  years <- sort(as.integer(years))
  first_ror <- terra::rast(ror_raster_path(ror_dir, years[1]))
  value_layers <- names(first_ror)[!grepl("_top1pct$", names(first_ror))]
  historical_layers <- list()
  for (layer in value_layers) {
    layer_stack <- do.call(c, lapply(years, function(year) terra::rast(ror_raster_path(ror_dir, year))[[layer]]))
    historical_mean <- terra::app(layer_stack, mean, na.rm = TRUE)
    names(historical_mean) <- paste0("historical_mean_", layer)
    historical_layers <- c(historical_layers, list(historical_mean))
  }
  historical_raster <- do.call(c, historical_layers)
  output_path <- historical_mean_ror_path(historical_dir, years)
  make_parent_dir(output_path)
  terra::writeRaster(historical_raster, output_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
  message("Wrote historical mean ROR raster: ", output_path)
  historical_raster
}

build_historical_anomaly_rasters <- function(years, ror_dir, historical_dir, anomaly_dir) {
  years <- sort(as.integer(years))
  historical_raster <- build_historical_mean_ror(years, ror_dir, historical_dir)
  historical_lookup <- stats::setNames(seq_along(names(historical_raster)), names(historical_raster))

  for (year in years) {
    current_ror <- terra::rast(ror_raster_path(ror_dir, year))
    value_layers <- names(current_ror)[!grepl("_top1pct$", names(current_ror))]
    anomaly_layers <- list()
    for (layer in value_layers) {
      historical_layer_name <- paste0("historical_mean_", layer)
      historical_mean <- historical_raster[[historical_lookup[[historical_layer_name]]]]
      ratio <- terra::ifel(
        is.na(historical_mean),
        NA_real_,
        terra::ifel(historical_mean == 0, NA_real_, current_ror[[layer]] / historical_mean)
      )
      names(ratio) <- paste0("histROR_anomaly_", sub("^ROR_", "", layer))
      top <- top_percentile_binary_raster(ratio, paste0(names(ratio), "_top1pct"), TOP_PERCENTILE)
      anomaly_layers <- c(anomaly_layers, list(ratio, top))
    }
    anomaly_raster <- do.call(c, anomaly_layers)
    output_path <- historical_anomaly_raster_path(anomaly_dir, year)
    make_parent_dir(output_path)
    terra::writeRaster(anomaly_raster, output_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
    message("Wrote historical ROR anomaly raster: ", output_path)
  }
}

run_final_analysis <- function(subanalysis_name, training_type_filter, prediction_grid_all, dataset2_all, prediction_years) {
  active_subanalysis_name <- derive_subanalysis_name(subanalysis_name, training_type_filter)
  tuning_model_dir <- file.path(ANALYSIS_MODEL_DIR, TUNING_MODEL_FAMILY_NAME, active_subanalysis_name)
  tuned_settings_rds <- file.path(tuning_model_dir, "superlearner_tuned_settings.rds")
  predictor_names_rds <- file.path(tuning_model_dir, "predictor_names.rds")

  if (!file.exists(tuned_settings_rds)) {
    stop("Could not find tuned SuperLearner settings for ", active_subanalysis_name, ": ", tuned_settings_rds, call. = FALSE)
  }
  if (!file.exists(predictor_names_rds)) {
    stop("Could not find tuned predictor list for ", active_subanalysis_name, ": ", predictor_names_rds, call. = FALSE)
  }

  best_settings <- readRDS(tuned_settings_rds)
  best_candidate <- candidate_from_tuned_settings(best_settings)
  predictor_names <- readRDS(predictor_names_rds)

  final_model_dir <- file.path(ANALYSIS_MODEL_DIR, FINAL_MODEL_FAMILY_NAME, active_subanalysis_name)
  final_output_dir <- file.path(ANALYSIS_OUTPUT_DIR, FINAL_MODEL_FAMILY_NAME, active_subanalysis_name)
  map_output_dir <- file.path(final_output_dir, MAP_SET_NAME, "pred")
  prediction_table_dir <- file.path(map_output_dir, "tbl")
  annual_raster_dir <- file.path(map_output_dir, "prob")
  ror_dir <- file.path(map_output_dir, "ror")
  historical_dir <- file.path(map_output_dir, "hist_mean")
  anomaly_dir <- file.path(map_output_dir, "hist_anom")
  run_index_csv <- file.path(final_output_dir, "run_index.csv")
  prediction_grid_missingness_report_csv <- file.path(
    final_output_dir,
    sprintf("pred_grid_missing_%s_%s.csv", min(prediction_years), max(prediction_years))
  )
  combined_summary_table_csv <- file.path(
    prediction_table_dir,
    sprintf("summ_all_%s_%s.csv", min(prediction_years), max(prediction_years))
  )

  make_dir(final_model_dir)
  make_dir(prediction_table_dir)
  make_dir(annual_raster_dir)
  make_dir(ror_dir)
  make_dir(historical_dir)
  make_dir(anomaly_dir)

  check_prediction_grid_predictor_missingness(
    prediction_df = prediction_grid_all,
    predictor_names = predictor_names,
    prediction_years = prediction_years,
    output_csv = prediction_grid_missingness_report_csv,
    max_prediction_missing_prop = MAX_PREDICTION_GRID_MISSING_PROP,
    fill_rules = LATEST_AVAILABLE_COVARIATE_FILLS
  )

  prepared <- prepare_final_training_data(dataset2_all, predictor_names, training_type_filter)
  training_df <- prepared$training_df
  imputation_values <- prepared$imputation_values

  message("\nRunning final full-data analysis: ", active_subanalysis_name)
  message("Tuned candidate: ", best_settings$candidate_id)
  message("Prediction years: ", paste(prediction_years, collapse = ", "))
  message("Training rows: ", format(nrow(training_df), big.mark = ","))
  message("Training events: ", sum(training_df[[OUTCOME_COLUMN]] == EVENT_VALUE))
  message("Training controls: ", sum(training_df[[OUTCOME_COLUMN]] == CONTROL_VALUE))
  message("Predictors: ", length(predictor_names))

  model_path <- final_model_path(final_model_dir)
  if (!file.exists(model_path) || OVERWRITE_FINAL_MODELS) {
    sl_ensemble <- fit_final_superlearner_ensemble(
      training_df,
      predictor_names,
      best_candidate,
      subanalysis_seed = RANDOM_SEED + match(active_subanalysis_name, unique(c("all_types", "type_Z", active_subanalysis_name))) * 10000L
    )
    model_object <- list(
      models = sl_ensemble$models,
      sample_summary = sl_ensemble$sample_summary,
      predictor_names = predictor_names,
      imputation_values = imputation_values,
      best_settings = best_settings,
      ensemble_size = FINAL_RESAMPLED_ENSEMBLE_SIZE,
      control_ratio = CONTROL_RATIO,
      control_sample_replace = CONTROL_SAMPLE_REPLACE,
      training_start_year = min(training_df$year, na.rm = TRUE),
      training_end_year = max(training_df$year, na.rm = TRUE),
      prediction_years = prediction_years,
      training_rows = nrow(training_df),
      training_events = sum(training_df[[OUTCOME_COLUMN]] == EVENT_VALUE),
      training_controls = sum(training_df[[OUTCOME_COLUMN]] == CONTROL_VALUE),
      created_at = Sys.time()
    )
    saveRDS(model_object, model_path)
    message("Saved final model: ", model_path)
    rm(sl_ensemble)
  } else {
    model_object <- readRDS(model_path)
    message("Loaded existing final model: ", model_path)
  }

  annual_summary_tables <- vector("list", length(prediction_years))
  run_records <- vector("list", length(prediction_years))

  for (index in seq_along(prediction_years)) {
    year <- as.integer(prediction_years[index])
    message("\nPredicting final full-data map for ", year, "...")
    target_grid <- prepare_year_prediction_grid(prediction_grid_all, predictor_names, imputation_values, year)

    prediction_path <- prediction_table_path(prediction_table_dir, year)
    if (!file.exists(prediction_path) || OVERWRITE_PREDICTION_TABLES) {
      prediction_table <- make_superlearner_prediction_table(target_grid, predictor_names, model_object)
      make_parent_dir(prediction_path)
      utils::write.csv(prediction_table, prediction_path, row.names = FALSE)
      message("  Saved prediction table: ", prediction_path)
    } else {
      prediction_table <- utils::read.csv(prediction_path, stringsAsFactors = FALSE)
      message("  Loaded existing prediction table: ", prediction_path)
    }

    summary_table <- summarize_single_prediction_column(prediction_table)
    summary_path <- summary_table_path(prediction_table_dir, year)
    make_parent_dir(summary_path)
    utils::write.csv(summary_table, summary_path, row.names = FALSE)
    message("  Saved summary table: ", summary_path)
    write_annual_rasters(summary_table, year, annual_raster_dir, ror_dir)

    annual_summary_tables[[index]] <- summary_table
    run_records[[index]] <- data.frame(
      subanalysis = active_subanalysis_name,
      prediction_year = year,
      training_start_year = min(training_df$year, na.rm = TRUE),
      training_end_year = max(training_df$year, na.rm = TRUE),
      training_rows = nrow(training_df),
      training_events = sum(training_df[[OUTCOME_COLUMN]] == EVENT_VALUE),
      training_controls = sum(training_df[[OUTCOME_COLUMN]] == CONTROL_VALUE),
      ensemble_size = FINAL_RESAMPLED_ENSEMBLE_SIZE,
      control_ratio = CONTROL_RATIO,
      model_path = model_path,
      prediction_table_path = prediction_path,
      summary_table_path = summary_path,
      annual_summary_raster_path = annual_summary_raster_path(annual_raster_dir, year),
      ror_raster_path = ror_raster_path(ror_dir, year),
      stringsAsFactors = FALSE
    )

    rm(target_grid, prediction_table, summary_table)
    gc()
  }

  if (isTRUE(WRITE_COMBINED_SUMMARY_TABLE)) {
    make_parent_dir(combined_summary_table_csv)
    utils::write.csv(do.call(rbind, annual_summary_tables), combined_summary_table_csv, row.names = FALSE)
    message("Saved combined summary table: ", combined_summary_table_csv)
  }

  build_historical_anomaly_rasters(prediction_years, ror_dir, historical_dir, anomaly_dir)

  run_index <- do.call(rbind, run_records)
  make_parent_dir(run_index_csv)
  utils::write.csv(run_index, run_index_csv, row.names = FALSE)
  message("Saved run index: ", run_index_csv)
}


#### 1. Read Inputs ####

require_package("SuperLearner")
require_package("rpart")
require_package("ranger")
require_package("terra")

if (!file.exists(TRAINING_CSV)) {
  stop("Could not find training dataset: ", TRAINING_CSV, call. = FALSE)
}

dataset2_all <- utils::read.csv(TRAINING_CSV, stringsAsFactors = FALSE)
prediction_grid_all <- read_available_prediction_grid()
prediction_years <- sort(unique(as.integer(prediction_grid_all$year)))
if (length(prediction_years) == 0) {
  stop("No prediction years were found in the retained prediction grid.", call. = FALSE)
}

message("Study-area analysis: ", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
message("Available prediction years: ", paste(prediction_years, collapse = ", "))
message("Prediction-grid rows retained: ", format(nrow(prediction_grid_all), big.mark = ","))
message("Unique grid cells: ", format(length(unique(prediction_grid_all$grid_id)), big.mark = ","))


#### 2. Fit Final Models And Predict Available Years ####

for (row_index in seq_len(nrow(FINAL_ANALYSES_TO_RUN))) {
  run_final_analysis(
    subanalysis_name = FINAL_ANALYSES_TO_RUN$subanalysis_name[row_index],
    training_type_filter = FINAL_ANALYSES_TO_RUN$training_type_filter[row_index],
    prediction_grid_all = prediction_grid_all,
    dataset2_all = dataset2_all,
    prediction_years = prediction_years
  )
  gc()
}

message("Done.")
