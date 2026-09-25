#### 07 Evaluate Stepwise SuperLearner Predictions ####

# Purpose:
#   Evaluate the forward-stepping predictions from script 06 at observed
#   training-data locations/years from 2021-2025.
#
# Important note:
#   The full prediction grid is unlabeled, so sensitivity, specificity, PPV,
#   NPV, and confusion matrices cannot be calculated for every grid cell. This
#   script evaluates cells where the training dataset provides an observed
#   presence/pseudo-absence label in a prediction year.
#
# Decision rules evaluated:
#   1. predicted probability >= the tuned threshold from script 05
#   2. cell is in the top 1% of annual mean ROR
#   3. cell is in the top 1% of 1-year mean ROR increase
#   4. cell is in the top 1% of either annual mean ROR or 1-year mean ROR increase


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
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/07_evaluate_stepwise_predictions.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/07_evaluate_stepwise_predictions.R') from the parent folder.",
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

# Must match scripts 05 and 06.
SUBANALYSIS_NAME <- ""
TRAINING_TYPE_FILTER <- "Z"

MODEL_FAMILY_NAME <- "superlearner"
STEPWISE_MODEL_FAMILY_NAME <- "stepwise_superlearner"
EVALUATION_YEARS <- 2021:2025
RASTER_CRS <- "EPSG:4326"

OUTCOME_COLUMN <- "outcome"
EVENT_VALUE <- 1L
CONTROL_VALUE <- 0L
TRAINING_BASE_COLUMNS <- c("id", "year", "latitude", "longitude", "outcome", "type", "country")

ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
ACTIVE_SUBANALYSIS_NAME <- derive_subanalysis_name(SUBANALYSIS_NAME, TRAINING_TYPE_FILTER)
ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
DATA_DIR <- file.path(ANALYSIS_DIR, "data")
ANALYSIS_MODEL_DIR <- file.path(ANALYSIS_DIR, "models")
ANALYSIS_OUTPUT_DIR <- file.path(ANALYSIS_DIR, "outputs")

TRAINING_CSV <- file.path(DATA_DIR, "dataset2.csv")
TUNING_MODEL_DIR <- file.path(ANALYSIS_MODEL_DIR, MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME)
TUNED_SETTINGS_RDS <- file.path(TUNING_MODEL_DIR, "superlearner_tuned_settings.rds")

STEPWISE_OUTPUT_DIR <- file.path(ANALYSIS_OUTPUT_DIR, STEPWISE_MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME)
MAP_SET_NAME <- sprintf("maps_%s_%s", min(EVALUATION_YEARS), max(EVALUATION_YEARS))
MAP_OUTPUT_DIR <- file.path(STEPWISE_OUTPUT_DIR, MAP_SET_NAME, "pred")
ANNUAL_SUMMARY_RASTER_DIR <- file.path(MAP_OUTPUT_DIR, "annual_rasters")
ROR_ESTIMATE_DIR <- file.path(MAP_OUTPUT_DIR, "ror")
ROR_CHANGE_DIR <- file.path(MAP_OUTPUT_DIR, "ror_1yr")

EVALUATION_DIR <- file.path(STEPWISE_OUTPUT_DIR, "evaluation")
POINT_PREDICTIONS_CSV <- file.path(EVALUATION_DIR, "stepwise_point_predictions_2021_2025.csv")
MISSING_EVENT_PREDICTIONS_CSV <- file.path(EVALUATION_DIR, "stepwise_missing_event_predictions.csv")
BINARY_METRICS_OVERALL_CSV <- file.path(EVALUATION_DIR, "stepwise_binary_metrics_overall.csv")
BINARY_METRICS_BY_YEAR_CSV <- file.path(EVALUATION_DIR, "stepwise_binary_metrics_by_year.csv")
CONTINUOUS_METRICS_OVERALL_CSV <- file.path(EVALUATION_DIR, "stepwise_continuous_auc_metrics_overall.csv")
CONTINUOUS_METRICS_BY_YEAR_CSV <- file.path(EVALUATION_DIR, "stepwise_continuous_auc_metrics_by_year.csv")
CONFUSION_MATRIX_TXT <- file.path(EVALUATION_DIR, "stepwise_caret_confusion_matrices.txt")


#### Helpers ####

annual_summary_raster_path <- function(year) {
  file.path(ANNUAL_SUMMARY_RASTER_DIR, sprintf("event_probability_summary_%s.tif", year))
}

ror_raster_path <- function(year) {
  file.path(ROR_ESTIMATE_DIR, sprintf("event_probability_ROR_%s.tif", year))
}

ror_change_raster_path <- function(year) {
  file.path(ROR_CHANGE_DIR, sprintf("chgROR_1yr_%s_over_%s.tif", year, as.integer(year) - 1L))
}

extract_layers_at_points <- function(raster_path, df, layer_names) {
  if (!file.exists(raster_path)) {
    stop("Missing raster needed for evaluation: ", raster_path, call. = FALSE)
  }
  raster <- terra::rast(raster_path)
  missing_layers <- setdiff(layer_names, names(raster))
  if (length(missing_layers) > 0) {
    stop("Raster is missing expected layer(s): ", paste(missing_layers, collapse = ", "), "\n", raster_path, call. = FALSE)
  }
  points <- terra::vect(df, geom = c("longitude", "latitude"), crs = RASTER_CRS, keepgeom = FALSE)
  values <- terra::extract(raster[[layer_names]], points)
  values$ID <- NULL
  values
}

build_evaluation_points <- function(dataset2) {
  dataset2 <- filter_training_dataset_by_type(dataset2, TRAINING_TYPE_FILTER, OUTCOME_COLUMN, EVENT_VALUE)
  dataset2 <- dataset2[dataset2$year %in% EVALUATION_YEARS, , drop = FALSE]
  dataset2[[OUTCOME_COLUMN]] <- as.integer(dataset2[[OUTCOME_COLUMN]])
  required <- c("year", "longitude", "latitude", OUTCOME_COLUMN)
  missing <- setdiff(required, names(dataset2))
  if (length(missing) > 0) {
    stop("Training dataset is missing evaluation columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  dataset2
}

add_year_predictions <- function(df_year, year) {
  probability <- extract_layers_at_points(
    annual_summary_raster_path(year),
    df_year,
    "pred_mean"
  )
  ror <- extract_layers_at_points(
    ror_raster_path(year),
    df_year,
    c("ROR_mean", "ROR_mean_top1pct")
  )
  if (year == min(EVALUATION_YEARS)) {
    change <- data.frame(
      chgROR_1yr_mean = NA_real_,
      chgROR_1yr_mean_top1pct = 0
    )
  } else {
    change <- extract_layers_at_points(
      ror_change_raster_path(year),
      df_year,
      c("chgROR_1yr_mean", "chgROR_1yr_mean_top1pct")
    )
  }
  cbind(df_year, probability, ror, change)
}

safe_caret_metrics <- function(truth, predicted) {
  if (length(unique(truth[!is.na(truth)])) < 2 || length(unique(predicted[!is.na(predicted)])) < 1) {
    return(data.frame(
      accuracy = NA_real_,
      sensitivity = NA_real_,
      specificity = NA_real_,
      ppv = NA_real_,
      npv = NA_real_,
      f1 = NA_real_,
      balanced_accuracy = NA_real_
    ))
  }
  caret_confusion_summary(truth, predicted, EVENT_VALUE, CONTROL_VALUE)
}

evaluate_rule <- function(df, rule_column, year = NA_integer_) {
  keep <- !is.na(df[[rule_column]]) & !is.na(df[[OUTCOME_COLUMN]])
  truth <- df[[OUTCOME_COLUMN]][keep]
  predicted <- as.integer(df[[rule_column]][keep])
  manual <- binary_metrics(truth, predicted = predicted, event_value = EVENT_VALUE, control_value = CONTROL_VALUE)
  caret <- safe_caret_metrics(truth, predicted)
  data.frame(
    year = year,
    rule = rule_column,
    n = length(truth),
    events = sum(truth == EVENT_VALUE),
    controls = sum(truth == CONTROL_VALUE),
    manual,
    caret_accuracy = caret$accuracy,
    caret_sensitivity = caret$sensitivity,
    caret_specificity = caret$specificity,
    caret_ppv = caret$ppv,
    caret_npv = caret$npv,
    caret_f1 = caret$f1,
    caret_balanced_accuracy = caret$balanced_accuracy,
    stringsAsFactors = FALSE
  )
}

continuous_auc_row <- function(df, score_column, year = NA_integer_) {
  auc <- manual_auc(df[[OUTCOME_COLUMN]], df[[score_column]], EVENT_VALUE, CONTROL_VALUE)
  data.frame(
    year = year,
    score = score_column,
    n = sum(is.finite(df[[score_column]]) & !is.na(df[[OUTCOME_COLUMN]])),
    events = sum(df[[OUTCOME_COLUMN]] == EVENT_VALUE, na.rm = TRUE),
    controls = sum(df[[OUTCOME_COLUMN]] == CONTROL_VALUE, na.rm = TRUE),
    roc_auc = auc$roc_auc,
    pr_auc = auc$pr_auc,
    stringsAsFactors = FALSE
  )
}


#### 1. Read Inputs And Extract Predictions At Labeled Points ####

require_package("terra")
require_package("caret")
make_dir(EVALUATION_DIR)

if (!file.exists(TRAINING_CSV)) {
  stop("Could not find training dataset: ", TRAINING_CSV, call. = FALSE)
}
if (!file.exists(TUNED_SETTINGS_RDS)) {
  stop("Could not find tuned SuperLearner settings. Run 05_tune_superlearner_cv.R first: ", TUNED_SETTINGS_RDS, call. = FALSE)
}

best_settings <- readRDS(TUNED_SETTINGS_RDS)
dataset2 <- utils::read.csv(TRAINING_CSV, stringsAsFactors = FALSE)
eval_points <- build_evaluation_points(dataset2)

year_tables <- lapply(EVALUATION_YEARS, function(year) {
  df_year <- eval_points[eval_points$year == year, , drop = FALSE]
  if (nrow(df_year) == 0) {
    warning("No labeled evaluation rows for year ", year, ".")
    return(NULL)
  }
  message("Extracting stepwise predictions for labeled points in ", year, "...")
  add_year_predictions(df_year, year)
})
point_predictions <- do.call(rbind, year_tables[!vapply(year_tables, is.null, logical(1))])

missing_event_predictions <- point_predictions[
  point_predictions[[OUTCOME_COLUMN]] == EVENT_VALUE & is.na(point_predictions$pred_mean),
  intersect(
    c("id", "year", "latitude", "longitude", "type", "country", "pred_mean", "ROR_mean", "chgROR_1yr_mean"),
    names(point_predictions)
  ),
  drop = FALSE
]
if (nrow(missing_event_predictions) > 0) {
  utils::write.csv(missing_event_predictions, MISSING_EVENT_PREDICTIONS_CSV, row.names = FALSE)
  warning(
    nrow(missing_event_predictions),
    " event row(s) have missing extracted predictions and will be excluded from confusion matrices. ",
    "See: ",
    MISSING_EVENT_PREDICTIONS_CSV,
    call. = FALSE
  )
}

point_predictions$rule_probability_tuned_threshold <- as.integer(point_predictions$pred_mean >= best_settings$threshold)
point_predictions$rule_top1_ror <- as.integer(point_predictions$ROR_mean_top1pct == 1)
point_predictions$rule_top1_ror_1yr_change <- as.integer(point_predictions$chgROR_1yr_mean_top1pct == 1)
point_predictions$rule_top1_ror_or_1yr_change <- as.integer(
  point_predictions$rule_top1_ror == 1 | point_predictions$rule_top1_ror_1yr_change == 1
)

utils::write.csv(point_predictions, POINT_PREDICTIONS_CSV, row.names = FALSE)
message("Saved labeled point predictions: ", POINT_PREDICTIONS_CSV)


#### 2. Confusion Matrices And Binary Metrics ####

rule_columns <- c(
  "rule_probability_tuned_threshold",
  "rule_top1_ror",
  "rule_top1_ror_1yr_change",
  "rule_top1_ror_or_1yr_change"
)

overall_binary <- do.call(rbind, lapply(rule_columns, function(rule) evaluate_rule(point_predictions, rule)))
by_year_binary <- do.call(rbind, lapply(EVALUATION_YEARS, function(year) {
  df_year <- point_predictions[point_predictions$year == year, , drop = FALSE]
  do.call(rbind, lapply(rule_columns, function(rule) evaluate_rule(df_year, rule, year = year)))
}))

utils::write.csv(overall_binary, BINARY_METRICS_OVERALL_CSV, row.names = FALSE)
utils::write.csv(by_year_binary, BINARY_METRICS_BY_YEAR_CSV, row.names = FALSE)
print(overall_binary)
message("Saved overall binary metrics: ", BINARY_METRICS_OVERALL_CSV)
message("Saved by-year binary metrics: ", BINARY_METRICS_BY_YEAR_CSV)

confusion_output <- capture.output({
  for (rule in rule_columns) {
    print_caret_confusion_matrix(
      truth = point_predictions[[OUTCOME_COLUMN]],
      predicted = point_predictions[[rule]],
      label = rule,
      event_value = EVENT_VALUE
    )
  }
})
writeLines(confusion_output)
writeLines(confusion_output, CONFUSION_MATRIX_TXT)
message("Saved caret confusion matrices: ", CONFUSION_MATRIX_TXT)


#### 3. Continuous AUC And PR-AUC Metrics ####

score_columns <- c("pred_mean", "ROR_mean", "chgROR_1yr_mean")
overall_auc <- do.call(rbind, lapply(score_columns, function(score) continuous_auc_row(point_predictions, score)))
by_year_auc <- do.call(rbind, lapply(EVALUATION_YEARS, function(year) {
  df_year <- point_predictions[point_predictions$year == year, , drop = FALSE]
  do.call(rbind, lapply(score_columns, function(score) continuous_auc_row(df_year, score, year = year)))
}))

utils::write.csv(overall_auc, CONTINUOUS_METRICS_OVERALL_CSV, row.names = FALSE)
utils::write.csv(by_year_auc, CONTINUOUS_METRICS_BY_YEAR_CSV, row.names = FALSE)
print(overall_auc)
message("Saved overall continuous metrics: ", CONTINUOUS_METRICS_OVERALL_CSV)
message("Saved by-year continuous metrics: ", CONTINUOUS_METRICS_BY_YEAR_CSV)

message("Done.")
