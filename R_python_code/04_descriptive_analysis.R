#### 04 Descriptive Analysis ####

# Purpose:
#   Check covariate completeness, summarize unadjusted relationships between
#   presences and pseudo-absences, create basic boxplots, and export Table 1
#   style summaries. This script does not fit prediction models.


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
    "Could not locate the KSPH Code directory. Run this with source('R_python_code/04_descriptive_analysis.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/04_descriptive_analysis.R') from the parent folder.",
    call. = FALSE
  )
}

CODE_DIR <- find_code_dir()
HELPER_FILE <- file.path(CODE_DIR, "R_python_code", "modeling_helpers.R")
source(HELPER_FILE)

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

# Leave blank for all event types. Set to "Z", for example, to make
# descriptive outputs for only type Z events plus all pseudo-absence rows.
SUBANALYSIS_NAME <- ""
TRAINING_TYPE_FILTER <- ""

ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
ACTIVE_SUBANALYSIS_NAME <- derive_subanalysis_name(SUBANALYSIS_NAME, TRAINING_TYPE_FILTER)
ACTIVE_TRAINING_TYPE_FILTER <- trim_nonempty_values(TRAINING_TYPE_FILTER)

ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
DATA_DIR <- file.path(ANALYSIS_DIR, "data")
OUTPUT_DIR <- file.path(ANALYSIS_DIR, "outputs", "descriptive", ACTIVE_SUBANALYSIS_NAME)
TABLE_DIR <- file.path(OUTPUT_DIR, "tables")
PLOT_DIR <- file.path(OUTPUT_DIR, "plots")

TRAINING_CSV <- file.path(DATA_DIR, "dataset2.csv")
PREDICTOR_LIST_CSV <- file.path(CODE_DIR, "config", "predictor_list.csv")

TRAINING_BASE_COLUMNS <- c("id", "year", "latitude", "longitude", "outcome", "type", "country")
OUTCOME_COLUMN <- "outcome"
EVENT_VALUE <- 1L
CONTROL_VALUE <- 0L

BOXPLOT_MAX_VARIABLES <- 40
TABLE_DIGITS <- 2

COMPLETENESS_CSV <- file.path(TABLE_DIR, "covariate_completeness.csv")
UNADJUSTED_CSV <- file.path(TABLE_DIR, "unadjusted_standardized_logistic_relationships.csv")
TABLE1_OVERALL_CSV <- file.path(TABLE_DIR, "table1_presence_vs_absence.csv")
TABLE1_BY_TYPE_CSV <- file.path(TABLE_DIR, "table1_by_event_type.csv")
BOXPLOT_OVERALL_PDF <- file.path(PLOT_DIR, "boxplots_presence_vs_absence.pdf")
BOXPLOT_BY_TYPE_PDF <- file.path(PLOT_DIR, "boxplots_by_event_type.pdf")


#### Helpers ####

format_summary <- function(x, digits = TABLE_DIGITS) {
  x <- suppressWarnings(as.numeric(x))
  finite <- x[is.finite(x)]
  if (length(finite) == 0) {
    return("NA")
  }
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
    stats::median(finite, na.rm = TRUE),
    stats::quantile(finite, 0.25, na.rm = TRUE, names = FALSE),
    stats::quantile(finite, 0.75, na.rm = TRUE, names = FALSE)
  )
}

summarize_by_group <- function(df, predictor, group_column, groups) {
  stats <- vapply(groups, function(group) {
    format_summary(df[[predictor]][df[[group_column]] == group])
  }, character(1))
  names(stats) <- groups
  stats
}

group_p_value <- function(df, predictor, group_column) {
  keep <- is.finite(df[[predictor]]) & !is.na(df[[group_column]])
  if (sum(keep) == 0 || length(unique(df[[group_column]][keep])) < 2) {
    return(NA_real_)
  }
  if (length(unique(df[[group_column]][keep])) == 2) {
    return(tryCatch(
      stats::wilcox.test(df[[predictor]][keep] ~ df[[group_column]][keep])$p.value,
      error = function(e) NA_real_
    ))
  }
  tryCatch(
    stats::kruskal.test(df[[predictor]][keep] ~ df[[group_column]][keep])$p.value,
    error = function(e) NA_real_
  )
}

make_table1 <- function(df, predictor_names, group_column, output_csv) {
  groups <- unique(as.character(df[[group_column]]))
  groups <- groups[!is.na(groups)]
  rows <- lapply(predictor_names, function(predictor) {
    group_stats <- summarize_by_group(df, predictor, group_column, groups)
    data.frame(
      predictor = predictor,
      label = humanize_predictor_name(predictor),
      p_value = group_p_value(df, predictor, group_column),
      as.list(group_stats),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  make_parent_dir(output_csv)
  utils::write.csv(out, output_csv, row.names = FALSE)
  out
}

read_training_predictor_names <- function(predictor_list_csv, training_df, training_base_columns) {
  training_predictors <- setdiff(names(training_df), training_base_columns)

  if (file.exists(predictor_list_csv)) {
    predictor_list <- utils::read.csv(predictor_list_csv, stringsAsFactors = FALSE)
    if ("include_in_model" %in% names(predictor_list)) {
      include <- tolower(as.character(predictor_list$include_in_model)) %in% c("true", "1", "yes", "y")
      predictor_list <- predictor_list[include, , drop = FALSE]
    }
    if ("predictor" %in% names(predictor_list)) {
      ordered <- predictor_list$predictor[predictor_list$predictor %in% training_predictors]
      extra <- setdiff(training_predictors, ordered)
      return(c(ordered, extra))
    }
  }

  training_predictors
}

unadjusted_logistic_summary <- function(df, predictor_names) {
  rows <- lapply(predictor_names, function(predictor) {
    x <- suppressWarnings(as.numeric(df[[predictor]]))
    y <- as.integer(df[[OUTCOME_COLUMN]])
    keep <- is.finite(x) & !is.na(y)
    x_sd <- stats::sd(x[keep])
    if (sum(keep) < 10 || length(unique(y[keep])) < 2 || is.na(x_sd) || x_sd == 0) {
      return(data.frame(
        predictor = predictor,
        label = humanize_predictor_name(predictor),
        n = sum(keep),
        odds_ratio_per_sd = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_value = NA_real_
      ))
    }
    x_scaled <- as.numeric(scale(x[keep]))
    fit <- tryCatch(
      suppressWarnings(stats::glm(y[keep] ~ x_scaled, family = stats::binomial())),
      error = function(e) NULL
    )
    if (is.null(fit) || length(stats::coef(fit)) < 2) {
      return(data.frame(
        predictor = predictor,
        label = humanize_predictor_name(predictor),
        n = sum(keep),
        odds_ratio_per_sd = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_value = NA_real_
      ))
    }
    estimate <- stats::coef(fit)[2]
    se <- summary(fit)$coefficients[2, 2]
    data.frame(
      predictor = predictor,
      label = humanize_predictor_name(predictor),
      n = sum(keep),
      odds_ratio_per_sd = exp(estimate),
      ci_low = exp(estimate - 1.96 * se),
      ci_high = exp(estimate + 1.96 * se),
      p_value = summary(fit)$coefficients[2, 4]
    )
  })
  do.call(rbind, rows)
}

write_completeness <- function(training_df, predictor_names, output_csv) {
  rows <- lapply(predictor_names, function(predictor) {
    training_missing <- sum(is.na(training_df[[predictor]]))
    data.frame(
      predictor = predictor,
      label = humanize_predictor_name(predictor),
      training_rows = nrow(training_df),
      training_missing_count = training_missing,
      training_missing_prop = training_missing / nrow(training_df),
      training_finite_count = sum(is.finite(training_df[[predictor]])),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  make_parent_dir(output_csv)
  utils::write.csv(out, output_csv, row.names = FALSE)
  out
}

plot_boxplot_pdf <- function(df, predictor_names, group_column, output_pdf, title_prefix) {
  require_package("ggplot2")
  make_parent_dir(output_pdf)
  grDevices::pdf(output_pdf, width = 9, height = 5.5)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (predictor in predictor_names) {
    is_population_density <- startsWith(predictor, "pop_density")
    y_label <- humanize_predictor_name(predictor)
    if (is_population_density) {
      y_label <- paste0("log1p(", y_label, ")")
    }
    plot_df <- data.frame(
      group = factor(df[[group_column]]),
      value = suppressWarnings(as.numeric(df[[predictor]]))
    )
    if (is_population_density) {
      plot_df$value <- log1p(plot_df$value)
    }
    plot_df <- plot_df[is.finite(plot_df$value) & !is.na(plot_df$group), , drop = FALSE]
    if (nrow(plot_df) == 0 || length(unique(plot_df$group)) < 2) {
      next
    }
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = group, y = value, fill = group)) +
      ggplot2::geom_boxplot(outlier.shape = NA, linewidth = 0.25) +
      ggplot2::theme_bw(base_size = 12) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::labs(
        title = paste(title_prefix, humanize_predictor_name(predictor)),
        x = "",
        y = y_label
      )
    print(p)
  }
}


#### 1. Read Completed Training Table ####

require_package("ggplot2")
make_dir(TABLE_DIR)
make_dir(PLOT_DIR)

if (!file.exists(TRAINING_CSV)) {
  stop("Could not find training dataset: ", TRAINING_CSV, call. = FALSE)
}

dataset2 <- utils::read.csv(TRAINING_CSV, stringsAsFactors = FALSE)

dataset2[[OUTCOME_COLUMN]] <- as.integer(dataset2[[OUTCOME_COLUMN]])
dataset2 <- filter_training_dataset_by_type(dataset2, TRAINING_TYPE_FILTER, OUTCOME_COLUMN, EVENT_VALUE)

predictor_names <- read_training_predictor_names(
  PREDICTOR_LIST_CSV,
  dataset2,
  TRAINING_BASE_COLUMNS
)
dataset2 <- as_numeric_predictors(dataset2, predictor_names)

dataset2$outcome_group <- ifelse(dataset2[[OUTCOME_COLUMN]] == EVENT_VALUE, "Presence", "Absence")
dataset2$type_group <- ifelse(
  dataset2[[OUTCOME_COLUMN]] == EVENT_VALUE,
  paste0("Presence: ", ifelse(is.na(dataset2$type) | !nzchar(dataset2$type), "Unknown", dataset2$type)),
  "Absence"
)

message("Study-area analysis: ", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
message("Sub-analysis: ", ACTIVE_SUBANALYSIS_NAME)
message("Training rows: ", format(nrow(dataset2), big.mark = ","))
message("Presences: ", sum(dataset2[[OUTCOME_COLUMN]] == EVENT_VALUE))
message("Absences: ", sum(dataset2[[OUTCOME_COLUMN]] == CONTROL_VALUE))
message("Predictors summarized: ", length(predictor_names))
message("Output directory: ", OUTPUT_DIR)


#### 2. Completeness By Variable ####

completeness <- write_completeness(dataset2, predictor_names, COMPLETENESS_CSV)
print(utils::head(completeness[order(-completeness$training_missing_prop), ], 20))
message("Saved completeness table: ", COMPLETENESS_CSV)


#### 3. Table 1 Exports ####

table1_overall <- make_table1(dataset2, predictor_names, "outcome_group", TABLE1_OVERALL_CSV)
table1_by_type <- make_table1(dataset2, predictor_names, "type_group", TABLE1_BY_TYPE_CSV)
message("Saved Table 1 presence/absence export: ", TABLE1_OVERALL_CSV)
message("Saved Table 1 by type export: ", TABLE1_BY_TYPE_CSV)


#### 4. Unadjusted Relationships ####

unadjusted <- unadjusted_logistic_summary(dataset2, predictor_names)
utils::write.csv(unadjusted, UNADJUSTED_CSV, row.names = FALSE)
print(utils::head(unadjusted[order(unadjusted$p_value), ], 20))
message("Saved unadjusted relationship table: ", UNADJUSTED_CSV)


#### 5. Basic Boxplots ####

ranked_predictors <- completeness$predictor[order(
  completeness$training_missing_prop,
  -abs(match(completeness$predictor, predictor_names) - length(predictor_names) / 2)
)]
plot_predictors <- utils::head(ranked_predictors, BOXPLOT_MAX_VARIABLES)

plot_boxplot_pdf(
  dataset2,
  plot_predictors,
  "outcome_group",
  BOXPLOT_OVERALL_PDF,
  "Presence vs absence:"
)
plot_boxplot_pdf(
  dataset2,
  plot_predictors,
  "type_group",
  BOXPLOT_BY_TYPE_PDF,
  "By event type:"
)
message("Saved presence/absence boxplots: ", BOXPLOT_OVERALL_PDF)
message("Saved by-type boxplots: ", BOXPLOT_BY_TYPE_PDF)

message("Done.")
