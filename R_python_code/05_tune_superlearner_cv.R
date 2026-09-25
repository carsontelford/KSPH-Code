#### 05 Tune SuperLearner With Cross-Validation ####

# Purpose:
#   Tune the production SuperLearner hyperparameters with cross-validation.
#   Each outer-fold fit emulates the earlier BRT workflow by repeatedly
#   sampling 50 controls per case, fitting one SuperLearner per sample, and
#   averaging the held-out predictions across the sampled ensemble. This script
#   stops after saving tuned settings and CV performance outputs; prediction
#   maps are generated in script 06.


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
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/05_tune_superlearner_cv.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/05_tune_superlearner_cv.R') from the parent folder.",
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

# Leave both blank for all event types. To tune a type-specific model, set
# TRAINING_TYPE_FILTER <- "Z"; outputs will go to superlearner/type_Z.
SUBANALYSIS_NAME <- ""
TRAINING_TYPE_FILTER <- "Z"

MODEL_FAMILY_NAME <- "superlearner"
RANDOM_SEED <- 20260910
OUTCOME_COLUMN <- "outcome"
EVENT_VALUE <- 1L
CONTROL_VALUE <- 0L
TRAINING_BASE_COLUMNS <- c("id", "year", "latitude", "longitude", "outcome", "type", "country")

# Outer CV strategy:
# - "leave_year_out" holds out all rows from one calendar year at a time.
# - "one_event_per_fold" creates one fold for each eligible event row and
#   randomly assigns controls as evenly as possible across those folds.
# - "stratified_kfold" uses N_FOLDS below.
CV_FOLD_STRATEGY <- "leave_year_out"
N_FOLDS <- 10
ALLOW_FEWER_FOLDS_IF_FEW_EVENTS <- TRUE
SL_INTERNAL_FOLDS <- 3
SL_LIBRARY <- c("SL.glm", "SL.rpart_tuned", "SL.ranger_tuned")
SL_METHOD <- "method.NNloglik"
RANGER_THREADS <- 1
USE_CLASS_WEIGHTS <- FALSE
CONTROL_RATIO <- 50L
CONTROL_SAMPLE_REPLACE <- TRUE
CV_RESAMPLED_ENSEMBLE_SIZE <- 5L

# Each row is a candidate set of hyperparameters for the two tuned learners.
# SL.glm has no tunable hyperparameters in this first production pass.
SL_TUNING_GRID <- data.frame(
  candidate_id = c("sampled_fast_balanced", "sampled_fast_flexible"),
  rpart_cp = c(0.008, 0.003),
  rpart_maxdepth = c(4L, 6L),
  rpart_minbucket = c(15L, 8L),
  ranger_num_trees = c(75L, 125L),
  ranger_mtry_fraction = c(0.40, 0.60),
  ranger_min_node_size = c(15L, 8L),
  stringsAsFactors = FALSE
)

THRESHOLD_GRID <- sort(unique(c(
  seq(0.001, 0.050, by = 0.001),
  seq(0.055, 0.200, by = 0.005),
  seq(0.250, 0.500, by = 0.050)
)))
THRESHOLD_SELECTION_METRIC <- "sens_spec_product"

DROP_HIGH_MISSING_PREDICTORS <- TRUE
MAX_TRAINING_MISSING_PROP <- 0.20
HANSEN_ZERO_FILL_PREFIXES <- c(
  "forest_cover_prop_",
  "flsy_prop_",
  "fl1yp_prop_",
  "fl2yp_prop_",
  "frag_edge_prop_"
)
LATEST_AVAILABLE_COVARIATE_FILLS <- list()

OVERWRITE_CV_OUTPUTS <- TRUE

ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
ACTIVE_SUBANALYSIS_NAME <- derive_subanalysis_name(SUBANALYSIS_NAME, TRAINING_TYPE_FILTER)
ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
DATA_DIR <- file.path(ANALYSIS_DIR, "data")
ANALYSIS_MODEL_DIR <- file.path(ANALYSIS_DIR, "models")
ANALYSIS_OUTPUT_DIR <- file.path(ANALYSIS_DIR, "outputs")

TRAINING_CSV <- file.path(DATA_DIR, "dataset2.csv")
PREDICTOR_LIST_CSV <- file.path(CODE_DIR, "config", "predictor_list.csv")

MODEL_DIR <- file.path(ANALYSIS_MODEL_DIR, MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME)
OUTPUT_DIR <- file.path(ANALYSIS_OUTPUT_DIR, MODEL_FAMILY_NAME, ACTIVE_SUBANALYSIS_NAME, "tuning")

PREDICTOR_NAMES_RDS <- file.path(MODEL_DIR, "predictor_names.rds")
PREDICTOR_NAMES_CSV <- file.path(MODEL_DIR, "predictor_names.csv")
PREDICTOR_MISSINGNESS_REPORT_CSV <- file.path(MODEL_DIR, "predictor_missingness_report.csv")
IMPUTATION_VALUES_RDS <- file.path(MODEL_DIR, "full_training_predictor_imputation_values.rds")
IMPUTATION_VALUES_CSV <- file.path(MODEL_DIR, "full_training_predictor_imputation_values.csv")
FOLD_ASSIGNMENT_CSV <- file.path(OUTPUT_DIR, "cv_fold_assignments.csv")
CV_PREDICTIONS_CSV <- file.path(OUTPUT_DIR, "superlearner_cv_predictions.csv")
CV_METRICS_CSV <- file.path(OUTPUT_DIR, "superlearner_cv_metrics_by_candidate.csv")
BEST_SETTINGS_RDS <- file.path(MODEL_DIR, "superlearner_tuned_settings.rds")
BEST_SETTINGS_CSV <- file.path(MODEL_DIR, "superlearner_tuned_settings.csv")
BEST_CONFUSION_MATRIX_TXT <- file.path(OUTPUT_DIR, "best_candidate_caret_confusion_matrix.txt")


#### Helpers ####

candidate_from_grid <- function(row_index) {
  as.list(SL_TUNING_GRID[row_index, , drop = FALSE])
}

cross_validate_superlearner_candidate <- function(training_df, predictor_names, fold_id, candidate) {
  X <- training_df[, predictor_names, drop = FALSE]
  y <- training_df[[OUTCOME_COLUMN]]
  cv_pred <- rep(NA_real_, nrow(training_df))
  folds <- sort(unique(fold_id))

  for (fold_index in seq_along(folds)) {
    fold <- folds[fold_index]
    message("    fold ", fold, " of ", length(folds))
    train_rows <- which(fold_id != fold)
    valid_rows <- which(fold_id == fold)
    if (
      sum(y[train_rows] == EVENT_VALUE, na.rm = TRUE) == 0 ||
        sum(y[train_rows] == CONTROL_VALUE, na.rm = TRUE) == 0
    ) {
      stop("Training fold ", fold, " does not contain both classes.", call. = FALSE)
    }
    fold_prediction_matrix <- matrix(NA_real_, nrow = length(valid_rows), ncol = CV_RESAMPLED_ENSEMBLE_SIZE)
    for (ensemble_id in seq_len(CV_RESAMPLED_ENSEMBLE_SIZE)) {
      message("      sampled ensemble fit ", ensemble_id, " of ", CV_RESAMPLED_ENSEMBLE_SIZE)
      set.seed(RANDOM_SEED + fold_index * 10000L + ensemble_id)
      sampled_relative_rows <- sample_case_control_rows(
        y[train_rows],
        control_ratio = CONTROL_RATIO,
        replace_controls = CONTROL_SAMPLE_REPLACE,
        event_value = EVENT_VALUE,
        control_value = CONTROL_VALUE
      )
      sampled_train_rows <- train_rows[sampled_relative_rows]
      y_sampled <- y[sampled_train_rows]
      obs_weights <- make_observation_weights(y_sampled, EVENT_VALUE, CONTROL_VALUE)
      fold_fit <- fit_superlearner_model(
        X = X[sampled_train_rows, , drop = FALSE],
        y = y_sampled,
        candidate = candidate,
        obs_weights = obs_weights
      )
      fold_prediction_matrix[, ensemble_id] <- predict_superlearner_probability(
        fold_fit,
        X[valid_rows, , drop = FALSE]
      )
      rm(fold_fit)
      gc()
    }
    cv_pred[valid_rows] <- rowMeans(fold_prediction_matrix, na.rm = TRUE)
    rm(fold_prediction_matrix)
    gc()
  }

  best_threshold <- find_best_threshold(
    truth = y,
    probability = cv_pred,
    threshold_grid = THRESHOLD_GRID,
    metric = THRESHOLD_SELECTION_METRIC
  )
  list(cv_pred = cv_pred, best_threshold = best_threshold)
}

candidate_cv_metrics <- function(training_df, cv_pred, best_threshold) {
  y <- training_df[[OUTCOME_COLUMN]]
  predicted <- ifelse(cv_pred >= best_threshold$threshold, EVENT_VALUE, CONTROL_VALUE)
  manual <- best_threshold
  caret <- caret_confusion_summary(y, predicted, EVENT_VALUE, CONTROL_VALUE)
  auc <- manual_auc(y, cv_pred, EVENT_VALUE, CONTROL_VALUE)
  cbind(
    manual,
    caret_accuracy = caret$accuracy,
    caret_sensitivity = caret$sensitivity,
    caret_specificity = caret$specificity,
    caret_ppv = caret$ppv,
    caret_npv = caret$npv,
    caret_f1 = caret$f1,
    roc_auc = auc$roc_auc,
    pr_auc = auc$pr_auc
  )
}

select_best_tuning_result <- function(tuning_results) {
  tuning_results <- tuning_results[order(
    -tuning_results$selection_metric,
    -tuning_results$sensitivity,
    -tuning_results$specificity,
    -tuning_results$f1,
    -tuning_results$ppv,
    tuning_results$threshold
  ), , drop = FALSE]
  row.names(tuning_results) <- NULL
  tuning_results[1, , drop = FALSE]
}


#### 1. Read And Prepare Training Data ####

require_package("SuperLearner")
require_package("rpart")
require_package("ranger")
require_package("caret")

make_dir(MODEL_DIR)
make_dir(OUTPUT_DIR)

if (!file.exists(TRAINING_CSV)) {
  stop("Could not find training dataset: ", TRAINING_CSV, call. = FALSE)
}

dataset2_raw <- utils::read.csv(TRAINING_CSV, stringsAsFactors = FALSE)

prepared <- prepare_model_training_data(
  training_df = dataset2_raw,
  prediction_df = NULL,
  predictor_list_csv = PREDICTOR_LIST_CSV,
  training_base_columns = TRAINING_BASE_COLUMNS,
  outcome_column = OUTCOME_COLUMN,
  event_value = EVENT_VALUE,
  training_type_filter = TRAINING_TYPE_FILTER,
  predictor_missingness_report_csv = PREDICTOR_MISSINGNESS_REPORT_CSV,
  max_training_missing_prop = MAX_TRAINING_MISSING_PROP,
  fill_rules = LATEST_AVAILABLE_COVARIATE_FILLS
)

dataset2 <- prepared$training_df
predictor_names <- prepared$predictor_names
imputation_values <- prepared$imputation_values

saveRDS(predictor_names, PREDICTOR_NAMES_RDS)
utils::write.csv(data.frame(predictor = predictor_names), PREDICTOR_NAMES_CSV, row.names = FALSE)
saveRDS(imputation_values, IMPUTATION_VALUES_RDS)
utils::write.csv(
  data.frame(predictor = names(imputation_values), imputation_value = as.numeric(imputation_values)),
  IMPUTATION_VALUES_CSV,
  row.names = FALSE
)

message("Study-area analysis: ", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
message("Sub-analysis: ", ACTIVE_SUBANALYSIS_NAME)
message("Training rows: ", format(nrow(dataset2), big.mark = ","))
message("Events: ", sum(dataset2[[OUTCOME_COLUMN]] == EVENT_VALUE))
message("Controls: ", sum(dataset2[[OUTCOME_COLUMN]] == CONTROL_VALUE))
message("Predictors retained: ", length(predictor_names))
message("Class weights enabled: ", USE_CLASS_WEIGHTS)


#### 2. Create CV Folds ####

if (identical(CV_FOLD_STRATEGY, "leave_year_out")) {
  fold_id <- make_leave_year_out_fold_ids(
    years = dataset2$year,
    y = dataset2[[OUTCOME_COLUMN]],
    event_value = EVENT_VALUE,
    control_value = CONTROL_VALUE
  )
} else if (identical(CV_FOLD_STRATEGY, "one_event_per_fold")) {
  fold_id <- make_one_event_per_fold_ids(
    dataset2[[OUTCOME_COLUMN]],
    RANDOM_SEED,
    event_value = EVENT_VALUE,
    control_value = CONTROL_VALUE
  )
} else if (identical(CV_FOLD_STRATEGY, "stratified_kfold")) {
  fold_id <- make_stratified_fold_ids(
    dataset2[[OUTCOME_COLUMN]],
    N_FOLDS,
    RANDOM_SEED,
    event_value = EVENT_VALUE,
    control_value = CONTROL_VALUE,
    allow_fewer_folds = ALLOW_FEWER_FOLDS_IF_FEW_EVENTS
  )
} else {
  stop("Unknown CV_FOLD_STRATEGY: ", CV_FOLD_STRATEGY, call. = FALSE)
}

message("CV fold strategy: ", CV_FOLD_STRATEGY)
message("Outer CV folds: ", length(unique(fold_id)))
fold_assignments <- data.frame(
  row_id = seq_len(nrow(dataset2)),
  id = if ("id" %in% names(dataset2)) dataset2$id else seq_len(nrow(dataset2)),
  year = dataset2$year,
  outcome = dataset2[[OUTCOME_COLUMN]],
  type = if ("type" %in% names(dataset2)) dataset2$type else NA_character_,
  fold_strategy = CV_FOLD_STRATEGY,
  fold = fold_id,
  stringsAsFactors = FALSE
)
utils::write.csv(fold_assignments, FOLD_ASSIGNMENT_CSV, row.names = FALSE)
print(table(fold_assignments$fold, fold_assignments$outcome))
message("Saved fold assignments: ", FOLD_ASSIGNMENT_CSV)


#### 3. Cross-Validate Candidate Hyperparameters ####

if (!file.exists(CV_METRICS_CSV) || OVERWRITE_CV_OUTPUTS) {
  tuning_results <- data.frame()
  cv_prediction_list <- vector("list", nrow(SL_TUNING_GRID))

  for (candidate_index in seq_len(nrow(SL_TUNING_GRID))) {
    candidate <- candidate_from_grid(candidate_index)
    message(
      "Evaluating candidate ", candidate_index, " of ", nrow(SL_TUNING_GRID),
      ": ", candidate$candidate_id
    )
    cv_result <- cross_validate_superlearner_candidate(dataset2, predictor_names, fold_id, candidate)
    metrics <- candidate_cv_metrics(dataset2, cv_result$cv_pred, cv_result$best_threshold)
    candidate_result <- cbind(SL_TUNING_GRID[candidate_index, , drop = FALSE], metrics)
    candidate_result$threshold_selection_metric <- THRESHOLD_SELECTION_METRIC
    candidate_result$cv_ensemble_size <- CV_RESAMPLED_ENSEMBLE_SIZE
    candidate_result$control_ratio <- CONTROL_RATIO
    tuning_results <- rbind(tuning_results, candidate_result)
    cv_prediction_list[[candidate_index]] <- data.frame(
      candidate_id = candidate$candidate_id,
      row_id = seq_len(nrow(dataset2)),
      id = if ("id" %in% names(dataset2)) dataset2$id else seq_len(nrow(dataset2)),
      year = dataset2$year,
      outcome = dataset2[[OUTCOME_COLUMN]],
      type = if ("type" %in% names(dataset2)) dataset2$type else NA_character_,
      fold = fold_id,
      cv_ensemble_size = CV_RESAMPLED_ENSEMBLE_SIZE,
      control_ratio = CONTROL_RATIO,
      cv_probability = cv_result$cv_pred,
      stringsAsFactors = FALSE
    )
  }

  best_settings <- select_best_tuning_result(tuning_results)
  saveRDS(best_settings, BEST_SETTINGS_RDS)
  utils::write.csv(best_settings, BEST_SETTINGS_CSV, row.names = FALSE)
  utils::write.csv(tuning_results, CV_METRICS_CSV, row.names = FALSE)
  utils::write.csv(do.call(rbind, cv_prediction_list), CV_PREDICTIONS_CSV, row.names = FALSE)
} else {
  tuning_results <- utils::read.csv(CV_METRICS_CSV, stringsAsFactors = FALSE)
  best_settings <- readRDS(BEST_SETTINGS_RDS)
}

print(tuning_results)
message("Saved CV metrics: ", CV_METRICS_CSV)
message("Saved CV predictions: ", CV_PREDICTIONS_CSV)
message("Saved tuned SuperLearner settings: ", BEST_SETTINGS_CSV)


#### 4. Print Best Candidate Confusion Matrix ####

cv_predictions <- utils::read.csv(CV_PREDICTIONS_CSV, stringsAsFactors = FALSE)
best_candidate_id <- as.character(best_settings$candidate_id)
best_predictions <- cv_predictions[cv_predictions$candidate_id == best_candidate_id, , drop = FALSE]
best_predicted_class <- ifelse(best_predictions$cv_probability >= best_settings$threshold, EVENT_VALUE, CONTROL_VALUE)

confusion_output <- capture.output(
  print_caret_confusion_matrix(
    truth = best_predictions$outcome,
    predicted = best_predicted_class,
    label = paste0("best SuperLearner CV candidate: ", best_candidate_id),
    event_value = EVENT_VALUE
  )
)
writeLines(confusion_output)
writeLines(confusion_output, BEST_CONFUSION_MATRIX_TXT)

message(
  "Best candidate: ", best_candidate_id,
  " | threshold: ", signif(best_settings$threshold, 4),
  " | threshold metric: ", THRESHOLD_SELECTION_METRIC,
  " | F1: ", signif(best_settings$f1, 4),
  " | sensitivity: ", signif(best_settings$sensitivity, 4),
  " | specificity: ", signif(best_settings$specificity, 4),
  " | PPV: ", signif(best_settings$ppv, 4),
  " | ROC AUC: ", signif(best_settings$roc_auc, 4),
  " | PR AUC: ", signif(best_settings$pr_auc, 4)
)
message("Done.")
