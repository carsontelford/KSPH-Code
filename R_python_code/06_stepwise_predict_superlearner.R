#### 06 Stepwise SuperLearner Prediction ####

# Purpose:
#   Apply the tuned SuperLearner settings from script 05 in an annual
#   forward-stepping workflow:
#     train 2001-2020 -> predict 2021
#     train 2001-2021 -> predict 2022
#     train 2001-2022 -> predict 2023
#     train 2001-2023 -> predict 2024
#     train 2001-2024 -> predict 2025
#
# This script creates prediction tables, annual probability rasters, annual
# relative-odds-ratio rasters, and 1-year ROR-change rasters. Each target-year
# model is a 50-fit sampled SuperLearner ensemble: all events are retained, 50
# controls per event are sampled for each ensemble fit, and prediction summaries
# are calculated across the 50 fitted SuperLearners.


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
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/06_stepwise_predict_superlearner.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/06_stepwise_predict_superlearner.R') from the parent folder.",
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

# Must match the settings used in 05_tune_superlearner_cv.R.
SUBANALYSIS_NAME <- ""
TRAINING_TYPE_FILTER <- "Z"

MODEL_FAMILY_NAME <- "superlearner"
STEPWISE_MODEL_FAMILY_NAME <- "stepwise_superlearner"
TEMPORAL_START_YEAR <- 2001L
TEMPORAL_TARGET_YEARS <- 2021:2025
RANDOM_SEED <- 20260910

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

OVERWRITE_STEPWISE_MODELS <- FALSE
OVERWRITE_PREDICTION_TABLES <- TRUE
OVERWRITE_RASTERS <- TRUE

ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
ACTIVE_SUBANALYSIS_NAME <- derive_subanalysis_name(SUBANALYSIS_NAME, TRAINING_TYPE_FILTER)
ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
DATA_DIR <- file.path(ANALYSIS_DIR, "data")
ANALYSIS_MODEL_DIR <- file.path(ANALYSIS_DIR, "models")
ANALYSIS_OUTPUT_DIR <- file.path(ANALYSIS_DIR, "outputs")

TRAINING_CSV <- file.path(DATA_DIR, "dataset2.csv")
PREDICTION_GRID_CSV <- file.path(DATA_DIR, "prediction_grid_covariates_2001_2025.csv")

TUNING_MODEL_DIR <- file.path(ANALYSIS_MODEL_DIR, MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME)
TUNED_SETTINGS_RDS <- file.path(TUNING_MODEL_DIR, "superlearner_tuned_settings.rds")
PREDICTOR_NAMES_RDS <- file.path(TUNING_MODEL_DIR, "predictor_names.rds")

STEPWISE_MODEL_DIR <- file.path(ANALYSIS_MODEL_DIR, STEPWISE_MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME)
STEPWISE_OUTPUT_DIR <- file.path(ANALYSIS_OUTPUT_DIR, STEPWISE_MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME)
MAP_SET_NAME <- sprintf("maps_%s_%s", min(TEMPORAL_TARGET_YEARS), max(TEMPORAL_TARGET_YEARS))
MAP_OUTPUT_DIR <- file.path(STEPWISE_OUTPUT_DIR, MAP_SET_NAME, "pred")

PREDICTION_TABLE_DIR <- file.path(MAP_OUTPUT_DIR, "tables")
ANNUAL_SUMMARY_RASTER_DIR <- file.path(MAP_OUTPUT_DIR, "annual_rasters")
ROR_ESTIMATE_DIR <- file.path(MAP_OUTPUT_DIR, "ror")
ROR_CHANGE_DIR <- file.path(MAP_OUTPUT_DIR, "ror_1yr")

RUN_INDEX_CSV <- file.path(STEPWISE_OUTPUT_DIR, "stepwise_runs.csv")
COMBINED_MODEL_PREDICTION_TABLE_CSV <- file.path(
  PREDICTION_TABLE_DIR,
  sprintf("stepwise_preds_%s_%s.csv", min(TEMPORAL_TARGET_YEARS), max(TEMPORAL_TARGET_YEARS))
)
COMBINED_SUMMARY_TABLE_CSV <- file.path(
  PREDICTION_TABLE_DIR,
  sprintf("stepwise_summaries_%s_%s.csv", min(TEMPORAL_TARGET_YEARS), max(TEMPORAL_TARGET_YEARS))
)
PREDICTION_GRID_MISSINGNESS_REPORT_CSV <- file.path(
  STEPWISE_OUTPUT_DIR,
  sprintf("prediction_grid_predictor_missingness_%s_%s.csv", min(TEMPORAL_TARGET_YEARS), max(TEMPORAL_TARGET_YEARS))
)


#### Helpers ####

candidate_from_tuned_settings <- function(best_settings) {
  as.list(best_settings[1, , drop = FALSE])
}

temporal_run_name <- function(target_year) {
  sprintf("train_%s_%s_predict_%s", TEMPORAL_START_YEAR, as.integer(target_year) - 1L, as.integer(target_year))
}

stepwise_model_path <- function(target_year) {
  file.path(STEPWISE_MODEL_DIR, temporal_run_name(target_year), "superlearner_fit.rds")
}

stepwise_prediction_table_path <- function(target_year) {
  file.path(PREDICTION_TABLE_DIR, sprintf("prediction_grid_superlearner_predictions_%s.csv", target_year))
}

stepwise_summary_table_path <- function(target_year) {
  file.path(PREDICTION_TABLE_DIR, sprintf("prediction_grid_superlearner_summaries_%s.csv", target_year))
}

annual_summary_raster_path <- function(target_year) {
  file.path(ANNUAL_SUMMARY_RASTER_DIR, sprintf("event_probability_summary_%s.tif", target_year))
}

ror_raster_path <- function(target_year) {
  file.path(ROR_ESTIMATE_DIR, sprintf("event_probability_ROR_%s.tif", target_year))
}

ror_change_raster_path <- function(target_year) {
  file.path(ROR_CHANGE_DIR, sprintf("chgROR_1yr_%s_over_%s.tif", target_year, as.integer(target_year) - 1L))
}

prepare_window_training_data <- function(dataset2, predictor_names, target_year) {
  training_window <- dataset2[
    dataset2$year >= TEMPORAL_START_YEAR & dataset2$year < target_year,
    ,
    drop = FALSE
  ]
  training_window <- filter_training_dataset_by_type(training_window, TRAINING_TYPE_FILTER, OUTCOME_COLUMN, EVENT_VALUE)
  missing_predictors <- setdiff(predictor_names, names(training_window))
  if (length(missing_predictors) > 0) {
    stop("Training data are missing predictors: ", paste(missing_predictors, collapse = ", "), call. = FALSE)
  }
  training_window <- as_numeric_predictors(training_window, predictor_names)
  training_window <- fill_hansen_na_with_zero(training_window)
  imputation_values <- fit_imputation_values(training_window, predictor_names)
  training_window <- apply_imputation_values(training_window, predictor_names, imputation_values, "stepwise training window")
  training_window <- log_transform_population_predictors(training_window, predictor_names, "stepwise training window")
  training_window[[OUTCOME_COLUMN]] <- as.integer(training_window[[OUTCOME_COLUMN]])

  n_event <- sum(training_window[[OUTCOME_COLUMN]] == EVENT_VALUE)
  n_control <- sum(training_window[[OUTCOME_COLUMN]] == CONTROL_VALUE)
  if (n_event == 0 || n_control == 0) {
    stop(
      "Training window for target year ", target_year,
      " needs at least one event and one control. Events=", n_event,
      "; controls=", n_control,
      call. = FALSE
    )
  }

  list(training_df = training_window, imputation_values = imputation_values)
}

prepare_target_prediction_grid <- function(prediction_grid, predictor_names, imputation_values, target_year) {
  target_grid <- prediction_grid[prediction_grid$year == target_year, , drop = FALSE]
  if (nrow(target_grid) == 0) {
    stop("Prediction grid has no rows for target year ", target_year, ".", call. = FALSE)
  }
  prepare_prediction_grid_data(
    target_grid,
    predictor_names,
    imputation_values,
    prediction_years = target_year,
    fill_rules = LATEST_AVAILABLE_COVARIATE_FILLS
  )
}

fit_stepwise_superlearner_ensemble <- function(training_df, predictor_names, best_candidate, target_year) {
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
    seed_base = RANDOM_SEED + as.integer(target_year) * 1000L
  )
}

write_stepwise_rasters <- function(summary_table, target_year) {
  summary_raster <- prediction_summary_to_raster_stack(summary_table, raster_crs = RASTER_CRS)
  annual_path <- annual_summary_raster_path(target_year)
  make_parent_dir(annual_path)
  terra::writeRaster(summary_raster, annual_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
  message("  Wrote annual probability raster: ", annual_path)

  ror_raster <- build_ror_raster(summary_raster, top_percentile = TOP_PERCENTILE, epsilon = ODDS_EPSILON)
  ror_path <- ror_raster_path(target_year)
  make_parent_dir(ror_path)
  terra::writeRaster(ror_raster, ror_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
  message("  Wrote ROR raster: ", ror_path)
}


#### 1. Read Tuned Settings And Input Tables ####

require_package("SuperLearner")
require_package("rpart")
require_package("ranger")
require_package("terra")

if (!file.exists(TUNED_SETTINGS_RDS)) {
  stop("Could not find tuned SuperLearner settings. Run 05_tune_superlearner_cv.R first: ", TUNED_SETTINGS_RDS, call. = FALSE)
}
if (!file.exists(PREDICTOR_NAMES_RDS)) {
  stop("Could not find tuned predictor list. Run 05_tune_superlearner_cv.R first: ", PREDICTOR_NAMES_RDS, call. = FALSE)
}
if (!file.exists(TRAINING_CSV)) {
  stop("Could not find training dataset: ", TRAINING_CSV, call. = FALSE)
}
if (!file.exists(PREDICTION_GRID_CSV)) {
  stop("Could not find prediction-grid dataset: ", PREDICTION_GRID_CSV, call. = FALSE)
}

make_dir(STEPWISE_MODEL_DIR)
make_dir(PREDICTION_TABLE_DIR)
make_dir(ANNUAL_SUMMARY_RASTER_DIR)
make_dir(ROR_ESTIMATE_DIR)
make_dir(ROR_CHANGE_DIR)

best_settings <- readRDS(TUNED_SETTINGS_RDS)
best_candidate <- candidate_from_tuned_settings(best_settings)
predictor_names <- readRDS(PREDICTOR_NAMES_RDS)
dataset2_all <- utils::read.csv(TRAINING_CSV, stringsAsFactors = FALSE)
prediction_grid_all <- utils::read.csv(PREDICTION_GRID_CSV, stringsAsFactors = FALSE)

missing_years <- setdiff(TEMPORAL_TARGET_YEARS, sort(unique(prediction_grid_all$year)))
if (length(missing_years) > 0) {
  stop("Prediction grid is missing target year(s): ", paste(missing_years, collapse = ", "), call. = FALSE)
}

check_prediction_grid_predictor_missingness(
  prediction_df = prediction_grid_all,
  predictor_names = predictor_names,
  prediction_years = TEMPORAL_TARGET_YEARS,
  output_csv = PREDICTION_GRID_MISSINGNESS_REPORT_CSV,
  max_prediction_missing_prop = MAX_PREDICTION_GRID_MISSING_PROP,
  fill_rules = LATEST_AVAILABLE_COVARIATE_FILLS
)

message("Study-area analysis: ", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
message("Sub-analysis: ", ACTIVE_SUBANALYSIS_NAME)
message("Tuned candidate: ", best_settings$candidate_id)
message("Target years: ", paste(TEMPORAL_TARGET_YEARS, collapse = ", "))
message("Predictors: ", length(predictor_names))
message("Final sampled ensemble size: ", FINAL_RESAMPLED_ENSEMBLE_SIZE)
message("Controls sampled per event per ensemble fit: ", CONTROL_RATIO)
message("Stepwise output directory: ", STEPWISE_OUTPUT_DIR)


#### 2. Fit Stepwise Models And Predict Annual Grids ####

run_records <- vector("list", length(TEMPORAL_TARGET_YEARS))
annual_prediction_tables <- vector("list", length(TEMPORAL_TARGET_YEARS))
annual_summary_tables <- vector("list", length(TEMPORAL_TARGET_YEARS))

for (index in seq_along(TEMPORAL_TARGET_YEARS)) {
  target_year <- as.integer(TEMPORAL_TARGET_YEARS[index])
  run_name <- temporal_run_name(target_year)
  message("\nRunning ", run_name, "...")

  prepared_window <- prepare_window_training_data(dataset2_all, predictor_names, target_year)
  training_window <- prepared_window$training_df
  imputation_values <- prepared_window$imputation_values
  target_grid <- prepare_target_prediction_grid(prediction_grid_all, predictor_names, imputation_values, target_year)

  model_path <- stepwise_model_path(target_year)
  make_parent_dir(model_path)
  if (!file.exists(model_path) || OVERWRITE_STEPWISE_MODELS) {
    sl_ensemble <- fit_stepwise_superlearner_ensemble(training_window, predictor_names, best_candidate, target_year)
    model_object <- list(
      models = sl_ensemble$models,
      sample_summary = sl_ensemble$sample_summary,
      predictor_names = predictor_names,
      imputation_values = imputation_values,
      best_settings = best_settings,
      ensemble_size = FINAL_RESAMPLED_ENSEMBLE_SIZE,
      control_ratio = CONTROL_RATIO,
      control_sample_replace = CONTROL_SAMPLE_REPLACE,
      training_start_year = TEMPORAL_START_YEAR,
      training_end_year = target_year - 1L,
      prediction_year = target_year,
      training_rows = nrow(training_window),
      training_events = sum(training_window[[OUTCOME_COLUMN]] == EVENT_VALUE),
      training_controls = sum(training_window[[OUTCOME_COLUMN]] == CONTROL_VALUE),
      created_at = Sys.time()
    )
    saveRDS(model_object, model_path)
    message("  Saved model: ", model_path)
    rm(sl_ensemble)
  } else {
    model_object <- readRDS(model_path)
    message("  Loaded existing model: ", model_path)
  }

  prediction_path <- stepwise_prediction_table_path(target_year)
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
  summary_path <- stepwise_summary_table_path(target_year)
  make_parent_dir(summary_path)
  utils::write.csv(summary_table, summary_path, row.names = FALSE)
  message("  Saved summary table: ", summary_path)

  write_stepwise_rasters(summary_table, target_year)

  annual_prediction_tables[[index]] <- prediction_table
  annual_summary_tables[[index]] <- summary_table
  run_records[[index]] <- data.frame(
    run_name = run_name,
    training_start_year = TEMPORAL_START_YEAR,
    training_end_year = target_year - 1L,
    prediction_year = target_year,
    training_rows = nrow(training_window),
    training_events = sum(training_window[[OUTCOME_COLUMN]] == EVENT_VALUE),
    training_controls = sum(training_window[[OUTCOME_COLUMN]] == CONTROL_VALUE),
    ensemble_size = FINAL_RESAMPLED_ENSEMBLE_SIZE,
    control_ratio = CONTROL_RATIO,
    model_path = model_path,
    prediction_table_path = prediction_path,
    summary_table_path = summary_path,
    annual_summary_raster_path = annual_summary_raster_path(target_year),
    ror_raster_path = ror_raster_path(target_year),
    stringsAsFactors = FALSE
  )

  rm(prepared_window, training_window, target_grid, model_object, prediction_table, summary_table)
  gc()
}


#### 3. Save Combined Stepwise Tables ####

run_index <- do.call(rbind, run_records)
make_parent_dir(RUN_INDEX_CSV)
make_parent_dir(COMBINED_MODEL_PREDICTION_TABLE_CSV)
make_parent_dir(COMBINED_SUMMARY_TABLE_CSV)
utils::write.csv(run_index, RUN_INDEX_CSV, row.names = FALSE)
utils::write.csv(do.call(rbind, annual_prediction_tables), COMBINED_MODEL_PREDICTION_TABLE_CSV, row.names = FALSE)
utils::write.csv(do.call(rbind, annual_summary_tables), COMBINED_SUMMARY_TABLE_CSV, row.names = FALSE)
message("Saved run index: ", RUN_INDEX_CSV)
message("Saved combined prediction table: ", COMBINED_MODEL_PREDICTION_TABLE_CSV)
message("Saved combined summary table: ", COMBINED_SUMMARY_TABLE_CSV)


#### 4. Build 1-Year ROR Change Rasters ####

for (target_year in TEMPORAL_TARGET_YEARS[-1]) {
  previous_year <- as.integer(target_year) - 1L
  current_path <- ror_raster_path(target_year)
  previous_path <- ror_raster_path(previous_year)
  if (!file.exists(current_path) || !file.exists(previous_path)) {
    stop("Cannot build ROR change raster because an input is missing: ", current_path, " or ", previous_path, call. = FALSE)
  }
  current_ror <- terra::rast(current_path)
  previous_ror <- terra::rast(previous_path)
  change_raster <- build_ratio_change_raster(
    current_ror,
    previous_ror,
    prefix = "chgROR_1yr",
    top_percentile = TOP_PERCENTILE
  )
  output_path <- ror_change_raster_path(target_year)
  make_parent_dir(output_path)
  terra::writeRaster(change_raster, output_path, overwrite = OVERWRITE_RASTERS, wopt = RASTER_WRITE_OPTIONS)
  message("Wrote 1-year ROR change raster: ", output_path)
}

message("Done.")
