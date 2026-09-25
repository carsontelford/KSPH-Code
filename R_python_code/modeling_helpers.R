#### Shared Modeling Helpers ####

# Shared utilities for the production SuperLearner workflow. These helpers are
# sourced by scripts 04-07; analysts should run the numbered scripts rather
# than this file directly.

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required. Install it before running this script.", package),
      call. = FALSE
    )
  }
}

load_package <- function(package) {
  require_package(package)
  suppressPackageStartupMessages(
    base::library(package, character.only = TRUE)
  )
  invisible(TRUE)
}

trim_nonempty_values <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character(0))
  }
  values <- trimws(as.character(values))
  values[!is.na(values) & nzchar(values)]
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

derive_subanalysis_name <- function(subanalysis_name, training_type_filter) {
  active_subanalysis <- trim_nonempty_values(subanalysis_name)
  active_filter <- trim_nonempty_values(training_type_filter)
  if (length(active_subanalysis) > 1) {
    stop("SUBANALYSIS_NAME must be a single value.", call. = FALSE)
  }
  if (length(active_subanalysis) == 1) {
    return(sanitize_path_component(active_subanalysis))
  }
  if (length(active_filter) > 0) {
    return(paste0(
      "type_",
      paste(vapply(active_filter, sanitize_path_component, character(1)), collapse = "_")
    ))
  }
  "all_types"
}

make_dir <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

make_parent_dir <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
}

existing_dir_or_first <- function(paths) {
  existing <- paths[dir.exists(paths)]
  if (length(existing) > 0) {
    return(existing[1])
  }
  paths[1]
}

safe_divide <- function(numerator, denominator, fallback = NA_real_) {
  out <- rep(fallback, length(numerator))
  ok <- is.finite(numerator) & is.finite(denominator) & denominator != 0
  out[ok] <- numerator[ok] / denominator[ok]
  out
}

humanize_predictor_name <- function(x) {
  x <- gsub("_0_10km$", " (0-10 km)", x)
  x <- gsub("_10_25km$", " (10-25 km)", x)
  x <- gsub("_25_50km$", " (25-50 km)", x)
  x <- gsub("^forest_cover_prop", "Forest cover", x)
  x <- gsub("^forest_cover_x_log_pop_density", "Forest cover x log population density", x)
  x <- gsub("^flsy_prop", "Forest loss, same year", x)
  x <- gsub("^fl1yp_prop", "Forest loss, 1-year lag", x)
  x <- gsub("^fl2yp_prop", "Forest loss, 2-year lag", x)
  x <- gsub("^frag_edge_prop", "Forest edge", x)
  x <- gsub("^forest_edge_x_log_pop_density", "Forest edge x log population density", x)
  x <- gsub("^pop_density", "Population density", x)
  x <- gsub("^precip_mm", "Annual precipitation", x)
  x <- gsub("^precip_anomaly_mm", "Precipitation anomaly", x)
  x <- gsub("^precip_anomaly_z", "Precipitation anomaly z", x)
  x <- gsub("^temp_k", "Annual temperature", x)
  x <- gsub("^temp_anomaly_k", "Temperature anomaly", x)
  x <- gsub("^temp_anomaly_z", "Temperature anomaly z", x)
  x <- gsub("^pet_mm", "Annual PET", x)
  x <- gsub("^ndvi_mean", "Annual NDVI", x)
  x <- gsub("^ndvi_anomaly", "NDVI anomaly", x)
  x <- gsub("^ndvi_anomaly_z", "NDVI anomaly z", x)
  x <- gsub("^elevation_m", "Elevation", x)
  x <- gsub("_", " ", x)
  x
}

filter_training_dataset_by_type <- function(df, training_type_filter, outcome_column, event_value) {
  active_filter <- trim_nonempty_values(training_type_filter)
  if (length(active_filter) == 0) {
    return(df)
  }
  if (!"type" %in% names(df)) {
    stop("TRAINING_TYPE_FILTER was set, but the training dataset has no 'type' column.", call. = FALSE)
  }

  event_rows <- df[[outcome_column]] == event_value
  keep_rows <- !event_rows | df$type %in% active_filter
  filtered <- df[keep_rows, , drop = FALSE]
  if (sum(filtered[[outcome_column]] == event_value, na.rm = TRUE) == 0) {
    stop(
      "TRAINING_TYPE_FILTER removed all event rows. Requested type value(s): ",
      paste(active_filter, collapse = ", "),
      call. = FALSE
    )
  }
  if (sum(filtered[[outcome_column]] != event_value, na.rm = TRUE) == 0) {
    stop("No control rows remain after applying TRAINING_TYPE_FILTER.", call. = FALSE)
  }
  filtered
}

read_predictor_names <- function(predictor_list_csv, training_df, prediction_df = NULL, training_base_columns, prediction_base_columns = character(0)) {
  training_predictors <- setdiff(names(training_df), training_base_columns)
  if (is.null(prediction_df)) {
    shared_predictors <- training_predictors
  } else {
    prediction_predictors <- setdiff(names(prediction_df), prediction_base_columns)
    shared_predictors <- intersect(training_predictors, prediction_predictors)
  }

  if (file.exists(predictor_list_csv)) {
    predictor_list <- utils::read.csv(predictor_list_csv, stringsAsFactors = FALSE)
    if ("include_in_model" %in% names(predictor_list)) {
      include <- tolower(as.character(predictor_list$include_in_model)) %in% c("true", "1", "yes", "y")
      predictor_list <- predictor_list[include, , drop = FALSE]
    }
    if ("predictor" %in% names(predictor_list)) {
      ordered <- predictor_list$predictor[predictor_list$predictor %in% shared_predictors]
      extra <- setdiff(shared_predictors, ordered)
      return(c(ordered, extra))
    }
  }

  shared_predictors
}

as_numeric_predictors <- function(df, predictor_names) {
  for (predictor in predictor_names) {
    df[[predictor]] <- suppressWarnings(as.numeric(df[[predictor]]))
    df[[predictor]][!is.finite(df[[predictor]])] <- NA_real_
  }
  df
}

fill_hansen_na_with_zero <- function(df) {
  prefixes <- get0(
    "HANSEN_ZERO_FILL_PREFIXES",
    ifnotfound = c("forest_cover_prop_", "flsy_prop_", "fl1yp_prop_", "fl2yp_prop_", "frag_edge_prop_")
  )
  columns <- names(df)[vapply(
    names(df),
    function(column) any(startsWith(column, prefixes)),
    logical(1)
  )]

  for (column in columns) {
    missing_rows <- is.na(df[[column]])
    if (any(missing_rows)) {
      df[[column]][missing_rows] <- 0
    }
  }
  df
}

fill_prediction_covariates_from_reference_years <- function(df, fill_rules) {
  if (length(fill_rules) == 0 || !"year" %in% names(df)) {
    return(df)
  }
  key_columns <- intersect(c("grid_id", "id", "longitude", "latitude", "x", "y"), names(df))
  for (covariate in names(fill_rules)) {
    if (!covariate %in% names(df)) {
      next
    }
    for (target_year_name in names(fill_rules[[covariate]])) {
      target_year <- as.integer(target_year_name)
      reference_year <- as.integer(fill_rules[[covariate]][[target_year_name]])
      target_rows <- which(df$year == target_year)
      reference_rows <- which(df$year == reference_year)
      if (length(target_rows) == 0 || length(reference_rows) == 0) {
        next
      }
      if ("grid_id" %in% key_columns) {
        reference_lookup <- data.frame(
          key = as.character(df$grid_id[reference_rows]),
          value = df[[covariate]][reference_rows],
          stringsAsFactors = FALSE
        )
        target_key <- as.character(df$grid_id[target_rows])
      } else {
        reference_lookup <- data.frame(
          key = paste(df$longitude[reference_rows], df$latitude[reference_rows], sep = "_"),
          value = df[[covariate]][reference_rows],
          stringsAsFactors = FALSE
        )
        target_key <- paste(df$longitude[target_rows], df$latitude[target_rows], sep = "_")
      }
      reference_lookup <- reference_lookup[is.finite(reference_lookup$value), , drop = FALSE]
      reference_lookup <- reference_lookup[!duplicated(reference_lookup$key), , drop = FALSE]
      matched_reference <- match(target_key, reference_lookup$key)
      fill_values <- reference_lookup$value[matched_reference]
      fillable <- is.na(df[[covariate]][target_rows]) & is.finite(fill_values)
      df[[covariate]][target_rows[fillable]] <- fill_values[fillable]
    }
  }
  df
}

screen_predictors_by_missingness <- function(
  training_df,
  prediction_df = NULL,
  predictor_names,
  output_csv,
  max_training_missing_prop,
  max_prediction_missing_prop = NULL
) {
  training_missing_count <- vapply(training_df[predictor_names], function(x) sum(is.na(x)), integer(1))
  training_missing_prop <- training_missing_count / nrow(training_df)
  if (is.null(prediction_df)) {
    prediction_missing_count <- rep(NA_integer_, length(predictor_names))
    prediction_missing_prop <- rep(NA_real_, length(predictor_names))
    prediction_grid_rows <- NA_integer_
    prediction_included <- rep(TRUE, length(predictor_names))
  } else {
    prediction_missing_count <- vapply(prediction_df[predictor_names], function(x) sum(is.na(x)), integer(1))
    prediction_missing_prop <- prediction_missing_count / nrow(prediction_df)
    prediction_grid_rows <- nrow(prediction_df)
    prediction_included <- prediction_missing_prop <= max_prediction_missing_prop
  }

  report <- data.frame(
    predictor = predictor_names,
    label = vapply(predictor_names, humanize_predictor_name, character(1)),
    training_rows = nrow(training_df),
    training_missing_count = as.integer(training_missing_count),
    training_missing_prop = as.numeric(training_missing_prop),
    prediction_grid_rows = prediction_grid_rows,
    prediction_grid_missing_count = as.integer(prediction_missing_count),
    prediction_grid_missing_prop = as.numeric(prediction_missing_prop),
    included_in_model = training_missing_prop <= max_training_missing_prop &
      prediction_included,
    stringsAsFactors = FALSE
  )
  report$drop_reason <- ""
  report$drop_reason[report$training_missing_prop > max_training_missing_prop] <- paste0(
    report$drop_reason[report$training_missing_prop > max_training_missing_prop],
    "training_missing_gt_", max_training_missing_prop
  )
  if (!is.null(prediction_df)) {
    report$drop_reason[report$prediction_grid_missing_prop > max_prediction_missing_prop] <- paste0(
      ifelse(nzchar(report$drop_reason[report$prediction_grid_missing_prop > max_prediction_missing_prop]), ";", ""),
      "prediction_grid_missing_gt_", max_prediction_missing_prop
    )
  }

  make_parent_dir(output_csv)
  utils::write.csv(report, output_csv, row.names = FALSE)
  kept <- report$predictor[report$included_in_model]
  if (length(kept) == 0) {
    stop("Predictor missingness screen removed all predictors.", call. = FALSE)
  }
  message("Saved predictor missingness report: ", output_csv)
  dropped <- report$predictor[!report$included_in_model]
  if (length(dropped) > 0) {
    message("Dropped predictors above missingness threshold: ", paste(dropped, collapse = ", "))
  }
  kept
}

check_prediction_grid_predictor_missingness <- function(
  prediction_df,
  predictor_names,
  prediction_years,
  output_csv,
  max_prediction_missing_prop,
  fill_rules = list()
) {
  missing_predictors <- setdiff(predictor_names, names(prediction_df))
  if (length(missing_predictors) > 0) {
    stop("Prediction grid is missing predictors: ", paste(missing_predictors, collapse = ", "), call. = FALSE)
  }
  prediction_screen <- prediction_df[prediction_df$year %in% prediction_years, , drop = FALSE]
  if (nrow(prediction_screen) == 0) {
    stop("Prediction grid has no rows for missingness screening years: ", paste(prediction_years, collapse = ", "), call. = FALSE)
  }
  prediction_screen <- as_numeric_predictors(prediction_screen, predictor_names)
  prediction_screen <- fill_hansen_na_with_zero(prediction_screen)
  prediction_screen <- fill_prediction_covariates_from_reference_years(prediction_screen, fill_rules)

  missing_count <- vapply(prediction_screen[predictor_names], function(x) sum(is.na(x)), integer(1))
  missing_prop <- missing_count / nrow(prediction_screen)
  report <- data.frame(
    predictor = predictor_names,
    label = vapply(predictor_names, humanize_predictor_name, character(1)),
    prediction_grid_rows = nrow(prediction_screen),
    prediction_grid_missing_count = as.integer(missing_count),
    prediction_grid_missing_prop = as.numeric(missing_prop),
    passes_prediction_grid_screen = missing_prop <= max_prediction_missing_prop,
    stringsAsFactors = FALSE
  )
  report$drop_reason <- ""
  report$drop_reason[report$prediction_grid_missing_prop > max_prediction_missing_prop] <- paste0(
    "prediction_grid_missing_gt_", max_prediction_missing_prop
  )

  make_parent_dir(output_csv)
  utils::write.csv(report, output_csv, row.names = FALSE)
  message("Saved prediction-grid predictor missingness report: ", output_csv)

  failed <- report$predictor[!report$passes_prediction_grid_screen]
  if (length(failed) > 0) {
    stop(
      "Prediction-grid missingness screen failed for retained predictor(s): ",
      paste(failed, collapse = ", "),
      ". Review ", output_csv,
      call. = FALSE
    )
  }
  invisible(report)
}

fit_imputation_values <- function(df, predictor_names) {
  zero_impute_prefixes <- get0("ZERO_IMPUTE_PREDICTOR_PREFIXES", ifnotfound = c("pop_density_"))
  stats::setNames(
    vapply(predictor_names, function(predictor) {
      if (any(startsWith(predictor, zero_impute_prefixes))) {
        return(0)
      }
      values <- df[[predictor]]
      finite_values <- values[is.finite(values)]
      if (length(finite_values) == 0) {
        stop("Predictor has no finite training values after missingness screening: ", predictor, call. = FALSE)
      }
      stats::median(finite_values, na.rm = TRUE)
    }, numeric(1)),
    predictor_names
  )
}

apply_imputation_values <- function(df, predictor_names, imputation_values, label) {
  for (predictor in predictor_names) {
    missing_rows <- is.na(df[[predictor]])
    if (any(missing_rows)) {
      fill_value <- imputation_values[[predictor]]
      df[[predictor]][missing_rows] <- fill_value
      message(
        "Filled ", format(sum(missing_rows), big.mark = ","), " remaining missing ",
        predictor, " values in ", label, " with ", format(fill_value, scientific = FALSE), "."
      )
    }
  }
  df
}

log_transform_population_predictors <- function(df, predictor_names, label) {
  prefixes <- get0("LOG1P_POPULATION_PREDICTOR_PREFIXES", ifnotfound = c("pop_density_"))
  columns <- predictor_names[vapply(
    predictor_names,
    function(predictor) any(startsWith(predictor, prefixes)),
    logical(1)
  )]
  columns <- intersect(columns, names(df))
  for (column in columns) {
    values <- suppressWarnings(as.numeric(df[[column]]))
    negative_rows <- is.finite(values) & values < 0
    if (any(negative_rows)) {
      warning(
        "Found ", sum(negative_rows), " negative values in ", column,
        " for ", label, "; setting them to NA before log1p transform.",
        call. = FALSE
      )
      values[negative_rows] <- NA_real_
    }
    df[[column]] <- log1p(values)
  }
  if (length(columns) > 0) {
    message("Applied log1p transform to population predictor(s) in ", label, ": ", paste(columns, collapse = ", "))
  }
  df
}

prepare_model_training_data <- function(
  training_df,
  prediction_df = NULL,
  predictor_list_csv,
  training_base_columns,
  prediction_base_columns = character(0),
  outcome_column,
  event_value,
  training_type_filter,
  prediction_years = NULL,
  predictor_missingness_report_csv,
  max_training_missing_prop,
  max_prediction_missing_prop = NULL,
  fill_rules = list()
) {
  training_df <- filter_training_dataset_by_type(training_df, training_type_filter, outcome_column, event_value)
  predictor_names <- read_predictor_names(predictor_list_csv, training_df, prediction_df, training_base_columns, prediction_base_columns)
  if (length(predictor_names) == 0) {
    stop("No eligible predictor columns were found in the training data.", call. = FALSE)
  }

  training_df <- as_numeric_predictors(training_df, predictor_names)
  training_df <- fill_hansen_na_with_zero(training_df)
  if (is.null(prediction_df)) {
    prediction_screen <- NULL
  } else {
    prediction_screen <- prediction_df[prediction_df$year %in% prediction_years, , drop = FALSE]
    prediction_screen <- as_numeric_predictors(prediction_screen, predictor_names)
    prediction_screen <- fill_hansen_na_with_zero(prediction_screen)
    prediction_screen <- fill_prediction_covariates_from_reference_years(prediction_screen, fill_rules)
  }
  predictor_names <- screen_predictors_by_missingness(
    training_df,
    prediction_screen,
    predictor_names,
    predictor_missingness_report_csv,
    max_training_missing_prop,
    max_prediction_missing_prop
  )
  imputation_values <- fit_imputation_values(training_df, predictor_names)
  training_df <- apply_imputation_values(training_df, predictor_names, imputation_values, "training data")
  training_df <- log_transform_population_predictors(training_df, predictor_names, "training data")
  training_df[[outcome_column]] <- as.integer(training_df[[outcome_column]])

  list(
    training_df = training_df,
    predictor_names = predictor_names,
    imputation_values = imputation_values
  )
}

prepare_prediction_grid_data <- function(prediction_grid, predictor_names, imputation_values, prediction_years, fill_rules = list()) {
  missing_predictors <- setdiff(predictor_names, names(prediction_grid))
  if (length(missing_predictors) > 0) {
    stop("Prediction grid is missing predictors: ", paste(missing_predictors, collapse = ", "), call. = FALSE)
  }
  prediction_grid <- prediction_grid[prediction_grid$year %in% prediction_years, , drop = FALSE]
  prediction_grid <- as_numeric_predictors(prediction_grid, predictor_names)
  prediction_grid <- fill_hansen_na_with_zero(prediction_grid)
  prediction_grid <- fill_prediction_covariates_from_reference_years(prediction_grid, fill_rules)
  prediction_grid <- apply_imputation_values(prediction_grid, predictor_names, imputation_values, "prediction grid")
  log_transform_population_predictors(prediction_grid, predictor_names, "prediction grid")
}

make_stratified_fold_ids <- function(y, k, seed, event_value = 1L, control_value = 0L, allow_fewer_folds = TRUE) {
  set.seed(seed)
  y <- as.integer(y)
  event_rows <- sample(which(y == event_value))
  control_rows <- sample(which(y == control_value))
  if (length(event_rows) == 0 || length(control_rows) == 0) {
    stop("Cross-validation requires at least one event and one control.", call. = FALSE)
  }
  effective_k <- as.integer(k)
  if (length(event_rows) < effective_k) {
    if (!isTRUE(allow_fewer_folds)) {
      stop("Each fold needs at least one event. Reduce N_FOLDS or add event rows.", call. = FALSE)
    }
    effective_k <- length(event_rows)
    warning(
      "Only ", length(event_rows), " event rows are available, so using ",
      effective_k, " folds instead of ", k, "."
    )
  }
  if (length(control_rows) < effective_k) {
    effective_k <- length(control_rows)
    warning("Only ", length(control_rows), " control rows are available, so using ", effective_k, " folds.")
  }

  fold_id <- rep(NA_integer_, length(y))
  fold_id[event_rows] <- rep(seq_len(effective_k), length.out = length(event_rows))
  fold_id[control_rows] <- rep(seq_len(effective_k), length.out = length(control_rows))
  fold_id
}

make_one_event_per_fold_ids <- function(y, seed, event_value = 1L, control_value = 0L) {
  set.seed(seed)
  y <- as.integer(y)
  event_rows <- sample(which(y == event_value))
  control_rows <- sample(which(y == control_value))
  if (length(event_rows) == 0 || length(control_rows) == 0) {
    stop("Cross-validation requires at least one event and one control.", call. = FALSE)
  }

  n_folds <- length(event_rows)
  fold_id <- rep(NA_integer_, length(y))
  fold_id[event_rows] <- seq_len(n_folds)
  fold_id[control_rows] <- rep(seq_len(n_folds), length.out = length(control_rows))
  fold_id
}

make_leave_year_out_fold_ids <- function(years, y, event_value = 1L, control_value = 0L) {
  years <- as.integer(years)
  y <- as.integer(y)
  if (any(is.na(years))) {
    stop("Leave-year-out cross-validation requires non-missing year values.", call. = FALSE)
  }
  fold_years <- sort(unique(years))
  if (length(fold_years) < 2) {
    stop("Leave-year-out cross-validation requires at least two years.", call. = FALSE)
  }
  if (sum(y == event_value, na.rm = TRUE) == 0 || sum(y == control_value, na.rm = TRUE) == 0) {
    stop("Cross-validation requires at least one event and one control.", call. = FALSE)
  }

  for (fold_year in fold_years) {
    train_rows <- years != fold_year
    n_event <- sum(y[train_rows] == event_value, na.rm = TRUE)
    n_control <- sum(y[train_rows] == control_value, na.rm = TRUE)
    if (n_event == 0 || n_control == 0) {
      stop(
        "Leave-year-out fold ", fold_year,
        " would leave the training data without both classes. Events=", n_event,
        "; controls=", n_control,
        call. = FALSE
      )
    }
  }

  years
}

make_observation_weights <- function(y, event_value = 1L, control_value = 0L) {
  if (!isTRUE(get0("USE_CLASS_WEIGHTS", ifnotfound = FALSE))) {
    return(rep(1, length(y)))
  }
  n_event <- sum(y == event_value)
  n_control <- sum(y == control_value)
  weights <- rep(NA_real_, length(y))
  weights[y == event_value] <- length(y) / (2 * n_event)
  weights[y == control_value] <- length(y) / (2 * n_control)
  weights / mean(weights)
}

sample_case_control_rows <- function(
  y,
  control_ratio = 50L,
  replace_controls = TRUE,
  event_value = 1L,
  control_value = 0L
) {
  y <- as.integer(y)
  event_rows <- which(y == event_value)
  control_rows <- which(y == control_value)
  if (length(event_rows) == 0 || length(control_rows) == 0) {
    stop("Case-control sampling requires at least one event row and one control row.", call. = FALSE)
  }
  n_controls <- as.integer(length(event_rows) * control_ratio)
  if (!isTRUE(replace_controls) && n_controls > length(control_rows)) {
    stop(
      "Not enough control rows to sample ", n_controls,
      " controls without replacement. Set replace_controls = TRUE or reduce control_ratio.",
      call. = FALSE
    )
  }
  sampled_controls <- sample(control_rows, n_controls, replace = replace_controls)
  sample(c(event_rows, sampled_controls))
}

binary_metrics <- function(truth, probability = NULL, predicted = NULL, threshold = NA_real_, event_value = 1L, control_value = 0L) {
  if (is.null(predicted)) {
    predicted <- ifelse(probability >= threshold, event_value, control_value)
  }
  keep <- !is.na(truth) & !is.na(predicted)
  truth <- as.integer(truth[keep])
  predicted <- as.integer(predicted[keep])
  tp <- sum(predicted == event_value & truth == event_value)
  fp <- sum(predicted == event_value & truth == control_value)
  fn <- sum(predicted == control_value & truth == event_value)
  tn <- sum(predicted == control_value & truth == control_value)
  sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
  specificity <- ifelse(tn + fp > 0, tn / (tn + fp), NA_real_)
  ppv <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)
  npv <- ifelse(tn + fn > 0, tn / (tn + fn), NA_real_)
  accuracy <- ifelse(tp + tn + fp + fn > 0, (tp + tn) / (tp + tn + fp + fn), NA_real_)
  f1 <- ifelse(is.finite(ppv + sensitivity) && ppv + sensitivity > 0, 2 * ppv * sensitivity / (ppv + sensitivity), NA_real_)
  data.frame(
    threshold = threshold,
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    sensitivity = sensitivity,
    specificity = specificity,
    ppv = ppv,
    npv = npv,
    accuracy = accuracy,
    f1 = f1
  )
}

find_best_threshold <- function(truth, probability, threshold_grid, metric = "f1") {
  keep <- is.finite(probability) & !is.na(truth)
  if (!any(keep)) {
    stop("No finite probabilities are available for threshold tuning.", call. = FALSE)
  }
  results <- do.call(
    rbind,
    lapply(threshold_grid, function(threshold) {
      binary_metrics(truth[keep], probability[keep], threshold = threshold)
    })
  )
  metric <- tolower(metric)
  if (metric == "sens_spec_product") {
    results$selection_metric <- results$sensitivity * results$specificity
  } else if (metric == "sens_ppv_product") {
    results$selection_metric <- results$sensitivity * results$ppv
  } else if (metric == "specificity") {
    results$selection_metric <- results$specificity
  } else if (metric == "sensitivity") {
    results$selection_metric <- results$sensitivity
  } else {
    results$selection_metric <- results$f1
  }
  results <- results[order(
    -results$selection_metric,
    -results$sensitivity,
    -results$specificity,
    -results$f1,
    -results$ppv,
    results$threshold
  ), , drop = FALSE]
  row.names(results) <- NULL
  results[1, , drop = FALSE]
}

manual_auc <- function(truth, score, event_value = 1L, control_value = 0L) {
  keep <- is.finite(score) & !is.na(truth)
  truth <- as.integer(truth[keep])
  score <- as.numeric(score[keep])
  if (length(unique(truth)) < 2) {
    return(list(roc_auc = NA_real_, pr_auc = NA_real_))
  }
  order_index <- order(score, decreasing = TRUE)
  truth <- truth[order_index]
  positives <- truth == event_value
  negatives <- truth == control_value
  n_pos <- sum(positives)
  n_neg <- sum(negatives)
  tp <- cumsum(positives)
  fp <- cumsum(negatives)
  recall <- tp / n_pos
  precision <- tp / pmax(tp + fp, 1)
  fpr <- fp / n_neg
  tpr <- recall
  roc_x <- c(0, fpr, 1)
  roc_y <- c(0, tpr, 1)
  pr_x <- c(0, recall)
  pr_y <- c(1, precision)
  roc_auc <- sum(diff(roc_x) * (head(roc_y, -1) + tail(roc_y, -1)) / 2)
  pr_auc <- sum(diff(pr_x) * (head(pr_y, -1) + tail(pr_y, -1)) / 2)
  list(roc_auc = roc_auc, pr_auc = pr_auc)
}

caret_confusion_summary <- function(truth, predicted, event_value = 1L, control_value = 0L) {
  require_package("caret")
  truth_factor <- factor(ifelse(truth == event_value, "event", "control"), levels = c("event", "control"))
  pred_factor <- factor(ifelse(predicted == event_value, "event", "control"), levels = c("event", "control"))
  cm <- caret::confusionMatrix(pred_factor, truth_factor, positive = "event")
  data.frame(
    accuracy = unname(cm$overall["Accuracy"]),
    sensitivity = unname(cm$byClass["Sensitivity"]),
    specificity = unname(cm$byClass["Specificity"]),
    ppv = unname(cm$byClass["Pos Pred Value"]),
    npv = unname(cm$byClass["Neg Pred Value"]),
    f1 = unname(cm$byClass["F1"]),
    balanced_accuracy = unname(cm$byClass["Balanced Accuracy"])
  )
}

print_caret_confusion_matrix <- function(truth, predicted, label, event_value = 1L) {
  require_package("caret")
  truth_factor <- factor(ifelse(truth == event_value, "event", "control"), levels = c("event", "control"))
  pred_factor <- factor(ifelse(predicted == event_value, "event", "control"), levels = c("event", "control"))
  message("\nCaret confusion matrix: ", label)
  print(caret::confusionMatrix(pred_factor, truth_factor, positive = "event"))
}

.SL_TUNING_ENV <- new.env(parent = emptyenv())

set_sl_tuning_params <- function(candidate) {
  .SL_TUNING_ENV$rpart_cp <- as.numeric(candidate$rpart_cp)
  .SL_TUNING_ENV$rpart_maxdepth <- as.integer(candidate$rpart_maxdepth)
  .SL_TUNING_ENV$rpart_minbucket <- as.integer(candidate$rpart_minbucket)
  .SL_TUNING_ENV$ranger_num_trees <- as.integer(candidate$ranger_num_trees)
  .SL_TUNING_ENV$ranger_mtry_fraction <- as.numeric(candidate$ranger_mtry_fraction)
  .SL_TUNING_ENV$ranger_min_node_size <- as.integer(candidate$ranger_min_node_size)
  invisible(TRUE)
}

event_factor <- function(y, event_value = 1L) {
  factor(ifelse(y == event_value, "event", "control"), levels = c("control", "event"))
}

probability_from_two_class_matrix <- function(pred_matrix, event_level = "event") {
  if (is.null(dim(pred_matrix))) {
    return(as.numeric(pred_matrix))
  }
  if (event_level %in% colnames(pred_matrix)) {
    return(as.numeric(pred_matrix[, event_level]))
  }
  rep(NA_real_, nrow(pred_matrix))
}

SL.rpart_tuned <- function(Y, X, newX, family, obsWeights, id, ...) {
  data <- data.frame(Y = event_factor(Y), as.data.frame(X), check.names = FALSE)
  fit <- rpart::rpart(
    Y ~ .,
    data = data,
    method = "class",
    weights = obsWeights,
    control = rpart::rpart.control(
      cp = .SL_TUNING_ENV$rpart_cp,
      maxdepth = .SL_TUNING_ENV$rpart_maxdepth,
      minbucket = .SL_TUNING_ENV$rpart_minbucket
    )
  )
  pred <- probability_from_two_class_matrix(predict(fit, newdata = as.data.frame(newX), type = "prob"))
  fit <- list(object = fit)
  class(fit) <- "SL.rpart_tuned"
  list(pred = pred, fit = fit)
}

predict.SL.rpart_tuned <- function(object, newdata, ...) {
  probability_from_two_class_matrix(predict(object$object, newdata = as.data.frame(newdata), type = "prob"))
}

SL.ranger_tuned <- function(Y, X, newX, family, obsWeights, id, ...) {
  X <- as.data.frame(X)
  newX <- as.data.frame(newX)
  mtry <- max(1L, min(ncol(X), round(.SL_TUNING_ENV$ranger_mtry_fraction * ncol(X))))
  data <- data.frame(Y = event_factor(Y), X, check.names = FALSE)
  fit <- ranger::ranger(
    dependent.variable.name = "Y",
    data = data,
    probability = TRUE,
    num.trees = .SL_TUNING_ENV$ranger_num_trees,
    mtry = mtry,
    min.node.size = .SL_TUNING_ENV$ranger_min_node_size,
    case.weights = obsWeights,
    num.threads = get0("RANGER_THREADS", ifnotfound = 1L),
    seed = get0("RANDOM_SEED", ifnotfound = 20260910L)
  )
  pred <- probability_from_two_class_matrix(predict(fit, data = newX)$predictions)
  fit <- list(object = fit)
  class(fit) <- "SL.ranger_tuned"
  list(pred = pred, fit = fit)
}

predict.SL.ranger_tuned <- function(object, newdata, ...) {
  probability_from_two_class_matrix(predict(object$object, data = as.data.frame(newdata))$predictions)
}

fit_superlearner_model <- function(X, y, candidate, obs_weights) {
  require_package("SuperLearner")
  require_package("rpart")
  require_package("ranger")
  set_sl_tuning_params(candidate)
  All <- function(Y, X, family, obsWeights, id, ...) {
    rep(TRUE, ncol(X))
  }
  sl_library <- get0("SL_LIBRARY", ifnotfound = c("SL.glm", "SL.rpart_tuned", "SL.ranger_tuned"))
  sl_env <- environment()
  for (learner in unique(as.character(sl_library))) {
    if (
      !exists(learner, envir = sl_env, inherits = TRUE) &&
        exists(learner, envir = asNamespace("SuperLearner"), inherits = FALSE)
    ) {
      assign(learner, get(learner, envir = asNamespace("SuperLearner")), envir = sl_env)
    }
  }
  internal_folds <- min(
    get0("SL_INTERNAL_FOLDS", ifnotfound = 5L),
    sum(y == get0("EVENT_VALUE", ifnotfound = 1L)),
    sum(y == get0("CONTROL_VALUE", ifnotfound = 0L))
  )
  internal_folds <- max(2L, as.integer(internal_folds))
  SuperLearner::SuperLearner(
    Y = y,
    X = as.data.frame(X),
    family = stats::binomial(),
    SL.library = sl_library,
    method = get0("SL_METHOD", ifnotfound = "method.NNloglik"),
    obsWeights = obs_weights,
    cvControl = list(V = internal_folds),
    env = sl_env,
    verbose = FALSE
  )
}

fit_superlearner_resample_ensemble <- function(
  training_df,
  predictor_names,
  candidate,
  ensemble_size,
  control_ratio,
  replace_controls = TRUE,
  outcome_column = "outcome",
  event_value = 1L,
  control_value = 0L,
  seed_base = NULL
) {
  models <- vector("list", ensemble_size)
  sample_records <- vector("list", ensemble_size)
  y_all <- training_df[[outcome_column]]

  for (ensemble_id in seq_len(ensemble_size)) {
    if (!is.null(seed_base)) {
      set.seed(as.integer(seed_base) + ensemble_id)
    }
    sampled_rows <- sample_case_control_rows(
      y_all,
      control_ratio = control_ratio,
      replace_controls = replace_controls,
      event_value = event_value,
      control_value = control_value
    )
    sampled_df <- training_df[sampled_rows, , drop = FALSE]
    y_sampled <- sampled_df[[outcome_column]]
    obs_weights <- make_observation_weights(y_sampled, event_value, control_value)
    models[[ensemble_id]] <- fit_superlearner_model(
      X = sampled_df[, predictor_names, drop = FALSE],
      y = y_sampled,
      candidate = candidate,
      obs_weights = obs_weights
    )
    sample_records[[ensemble_id]] <- data.frame(
      ensemble_id = ensemble_id,
      sampled_rows = length(sampled_rows),
      sampled_events = sum(y_sampled == event_value),
      sampled_controls = sum(y_sampled == control_value),
      stringsAsFactors = FALSE
    )
  }

  list(
    models = models,
    sample_summary = do.call(rbind, sample_records),
    ensemble_size = ensemble_size,
    control_ratio = control_ratio,
    replace_controls = replace_controls
  )
}

predict_superlearner_probability <- function(model, X) {
  load_package("SuperLearner")
  as.numeric(stats::predict(model, newdata = as.data.frame(X), onlySL = TRUE)$pred)
}

predict_superlearner_ensemble_matrix <- function(model_object, X) {
  if (!is.null(model_object$models)) {
    if (!is.list(model_object$models) || length(model_object$models) == 0) {
      stop("The SuperLearner ensemble object does not contain a non-empty model list.", call. = FALSE)
    }
    predictions <- lapply(seq_along(model_object$models), function(model_index) {
      predict_superlearner_probability(model_object$models[[model_index]], X)
    })
    out <- do.call(cbind, predictions)
    colnames(out) <- sprintf("pred_model_%03d", seq_len(ncol(out)))
    return(out)
  }
  if (is.null(model_object$model) && !inherits(model_object, "SuperLearner")) {
    stop("Expected a SuperLearner model object or an object with a 'models' ensemble list.", call. = FALSE)
  }
  single_model <- if (!is.null(model_object$model)) model_object$model else model_object
  out <- matrix(
    predict_superlearner_probability(single_model, X),
    ncol = 1
  )
  colnames(out) <- "pred_superlearner"
  out
}

prediction_metadata_columns <- function(df) {
  candidates <- c("grid_id", "grid_batch", "x", "y", "year", "longitude", "latitude", "country")
  candidates[candidates %in% names(df)]
}

make_superlearner_prediction_table <- function(prediction_grid, predictor_names, model_object) {
  metadata_columns <- prediction_metadata_columns(prediction_grid)
  prediction_table <- prediction_grid[, metadata_columns, drop = FALSE]
  prediction_matrix <- predict_superlearner_ensemble_matrix(
    model_object,
    prediction_grid[, predictor_names, drop = FALSE]
  )
  prediction_table <- cbind(prediction_table, as.data.frame(prediction_matrix, check.names = FALSE))
  prediction_table
}

summarize_single_prediction_column <- function(prediction_table, prediction_column = "pred_superlearner") {
  metadata_columns <- prediction_metadata_columns(prediction_table)
  out <- prediction_table[, metadata_columns, drop = FALSE]
  prediction_columns <- grep("^pred_model_[0-9]+$", names(prediction_table), value = TRUE)
  if (length(prediction_columns) == 0) {
    prediction_columns <- prediction_column
  }
  missing_prediction_columns <- setdiff(prediction_columns, names(prediction_table))
  if (length(missing_prediction_columns) > 0) {
    stop("Prediction table is missing prediction column(s): ", paste(missing_prediction_columns, collapse = ", "), call. = FALSE)
  }
  prediction_values <- as.matrix(prediction_table[, prediction_columns, drop = FALSE])
  out$pred_min <- apply(prediction_values, 1, min, na.rm = TRUE)
  out$pred_max <- apply(prediction_values, 1, max, na.rm = TRUE)
  out$pred_mean <- rowMeans(prediction_values, na.rm = TRUE)
  out$pred_median <- apply(prediction_values, 1, stats::median, na.rm = TRUE)
  out
}

infer_grid_resolution <- function(values) {
  values <- sort(unique(round(values, 10)))
  diffs <- diff(values)
  diffs <- diffs[is.finite(diffs) & diffs > 0]
  if (length(diffs) == 0) {
    stop("Could not infer raster grid resolution from prediction coordinates.", call. = FALSE)
  }
  stats::median(diffs)
}

make_prediction_template <- function(df_year, raster_crs = "EPSG:4326") {
  x_res <- infer_grid_resolution(df_year$x)
  y_res <- infer_grid_resolution(df_year$y)
  terra::rast(
    xmin = min(df_year$x, na.rm = TRUE) - x_res / 2,
    xmax = max(df_year$x, na.rm = TRUE) + x_res / 2,
    ymin = min(df_year$y, na.rm = TRUE) - y_res / 2,
    ymax = max(df_year$y, na.rm = TRUE) + y_res / 2,
    resolution = c(x_res, y_res),
    crs = raster_crs
  )
}

prediction_summary_to_raster_stack <- function(df_year, raster_crs = "EPSG:4326") {
  require_package("terra")
  required <- c("x", "y", "pred_min", "pred_max", "pred_mean", "pred_median")
  missing <- setdiff(required, names(df_year))
  if (length(missing) > 0) {
    stop("Prediction summary table is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  template <- make_prediction_template(df_year, raster_crs)
  points <- terra::vect(df_year, geom = c("x", "y"), crs = raster_crs, keepgeom = FALSE)
  raster_stack <- terra::rasterize(
    points,
    template,
    field = c("pred_min", "pred_max", "pred_mean", "pred_median"),
    fun = "mean"
  )
  names(raster_stack) <- c("pred_min", "pred_max", "pred_mean", "pred_median")
  raster_stack
}

probability_to_odds <- function(probability, epsilon = 1e-6) {
  if (inherits(probability, "SpatRaster")) {
    bounded_probability <- terra::ifel(
      is.na(probability),
      NA_real_,
      terra::ifel(
        probability < epsilon,
        epsilon,
        terra::ifel(probability > 1 - epsilon, 1 - epsilon, probability)
      )
    )
    return(bounded_probability / (1 - bounded_probability))
  }

  bounded_probability <- pmin(pmax(probability, epsilon), 1 - epsilon)
  bounded_probability / (1 - bounded_probability)
}

raster_quantile <- function(raster_layer, probability = 0.99) {
  quantile_value <- terra::global(
    raster_layer,
    fun = function(values, ...) {
      values <- values[is.finite(values)]
      if (length(values) == 0) {
        return(NA_real_)
      }
      as.numeric(stats::quantile(values, probs = probability, na.rm = TRUE, names = FALSE))
    }
  )[1, 1]

  as.numeric(quantile_value)
}

top_percentile_binary_raster <- function(raster_layer, layer_name, top_percentile = 0.99) {
  threshold <- raster_quantile(raster_layer, top_percentile)
  if (!is.finite(threshold)) {
    warning("Could not calculate the top-percentile threshold for ", layer_name, ". Returning an all-NA layer.")
    out <- raster_layer * NA_real_
  } else {
    out <- terra::ifel(
      is.na(raster_layer),
      NA_real_,
      terra::ifel(raster_layer >= threshold, 1, 0)
    )
  }
  names(out) <- layer_name
  out
}

build_ror_raster <- function(summary_raster, top_percentile = 0.99, epsilon = 1e-6) {
  value_layers <- names(summary_raster)
  out_layers <- list()
  for (layer in value_layers) {
    odds <- probability_to_odds(summary_raster[[layer]], epsilon = epsilon)
    baseline_odds <- terra::global(odds, "mean", na.rm = TRUE)[1, 1]
    ror <- odds / baseline_odds
    names(ror) <- sub("^pred_", "ROR_", layer)
    top <- top_percentile_binary_raster(ror, paste0(names(ror), "_top1pct"), top_percentile)
    out_layers <- c(out_layers, list(ror, top))
  }
  do.call(c, out_layers)
}

build_ratio_change_raster <- function(current_raster, previous_raster, prefix, top_percentile = 0.99) {
  common <- intersect(names(current_raster), names(previous_raster))
  common <- common[!grepl("_top1pct$", common)]
  out_layers <- list()
  for (layer in common) {
    ratio <- current_raster[[layer]] / previous_raster[[layer]]
    names(ratio) <- paste0(prefix, "_", sub("^(ROR_|pred_)", "", layer))
    top <- top_percentile_binary_raster(ratio, paste0(names(ratio), "_top1pct"), top_percentile)
    out_layers <- c(out_layers, list(ratio, top))
  }
  do.call(c, out_layers)
}
