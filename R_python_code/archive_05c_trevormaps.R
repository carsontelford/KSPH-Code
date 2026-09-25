#### 05c Manuscript-Style Temporal Forward-Validation Maps ####

# This script reads the annual temporal-forward-validation prediction rasters
# that were already produced by 04b_temporal_forward_validation_brt.R and writes
# PowerPoint/manuscript-ready static maps.
#
# Each output PNG has two vertically stacked panels:
#   1. the selected ROR or 1-year ROR-change value layer
#   2. the top 1% cells for that same value layer
#
# Outputs are written to:
#   analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/tfv/<TEMPORAL_ANALYSIS_GROUP>/TrevorPowerpoint


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
    getwd(),
    file.path(getwd(), "KSPH Code")
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
    "Could not locate the KSPH Code repo root. Run this with source('R_python_code/05c_trevormaps.R') ",
    "from the KSPH Code repo root, or source('KSPH Code/R_python_code/05c_trevormaps.R') from the parent folder.",
    call. = FALSE
  )
}

CODE_DIR <- find_code_dir()

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

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(
      sprintf("Package '%s' is required. Install it before running this script.", package),
      call. = FALSE
    )
  }
}

require_package("terra")
require_package("sf")
require_package("ggplot2")

sanitize_path_component <- function(value) {
  value <- trimws(as.character(value))
  value <- gsub("[^A-Za-z0-9]+", "_", value)
  value <- gsub("^_+|_+$", "", value)
  if (!nzchar(value)) {
    stop("Analysis folder name cannot be blank after sanitizing.", call. = FALSE)
  }
  value
}

existing_dir_or_first <- function(paths) {
  existing <- paths[dir.exists(paths)]
  if (length(existing) > 0) {
    return(existing[1])
  }
  paths[1]
}

STUDY_AREA_ANALYSIS_NAME <- "equatorial_africa"
TEMPORAL_ANALYSIS_ROOT <- "tfv"
TEMPORAL_ANALYSIS_GROUP <- "all_types"
TEMPORAL_MAP_SET <- "maps_2022_2025"
TEMPORAL_START_YEAR <- 2001L
ALLOW_LEGACY_PATH_FALLBACK <- FALSE

# Requested display years. The current temporal-forward-validation outputs on
# this machine are 2022-2025; if 2021 rasters are unavailable, the script will
# skip 2021 and record that in the manifest instead of failing.
ANNUAL_ROR_YEARS <- 2021:2025
CHANGE_ROR_YEARS <- 2022:2025
STOP_ON_MISSING_INPUTS <- FALSE

# Use "mean" for a single manuscript/PPT map. Other valid choices are "median",
# "min", and "max" if those layers exist in the rasters.
SUMMARY_MEASURE <- "mean"

ACTIVE_STUDY_AREA_ANALYSIS_NAME <- sanitize_path_component(STUDY_AREA_ANALYSIS_NAME)
TEMPORAL_ANALYSIS_GROUP <- sanitize_path_component(TEMPORAL_ANALYSIS_GROUP)
ANALYSIS_DIR <- file.path(CODE_DIR, "analyses", ACTIVE_STUDY_AREA_ANALYSIS_NAME)
LEGACY_PATH_FALLBACKS <- isTRUE(ALLOW_LEGACY_PATH_FALLBACK) && identical(ACTIVE_STUDY_AREA_ANALYSIS_NAME, "equatorial_africa")
ANALYSIS_OUTPUT_DIR <- existing_dir_or_first(c(
  file.path(ANALYSIS_DIR, "outputs"),
  if (LEGACY_PATH_FALLBACKS) file.path(CODE_DIR, "outputs") else character(0)
))

AFRICA_COUNTRY_BORDER_FILE <- file.path(CODE_DIR, "config", "africacountries_nolakes.shp")
TEMPORAL_OUTPUT_BASE_DIR <- file.path(ANALYSIS_OUTPUT_DIR, TEMPORAL_ANALYSIS_ROOT, TEMPORAL_ANALYSIS_GROUP)
TEMPORAL_MAP_OUTPUT_DIR <- file.path(TEMPORAL_OUTPUT_BASE_DIR, TEMPORAL_MAP_SET)
OUTPUT_DIR <- file.path(TEMPORAL_OUTPUT_BASE_DIR, "TrevorPowerpoint")
OUTPUT_MANIFEST_CSV <- file.path(OUTPUT_DIR, "trevorpowerpoint_map_manifest.csv")

RASTER_CRS <- "EPSG:4326"
MAP_LATITUDE_LIMITS <- if (identical(ACTIVE_STUDY_AREA_ANALYSIS_NAME, "equatorial_africa")) c(-10, 10) else NULL
TOP_PERCENTILE <- 0.99
ODDS_EPSILON <- 1e-6

ROR_BREAKPOINTS <- c(0.4, 0.7, 1.5, 5, 15)
CHANGE_RATIO_BREAKPOINTS <- c(0.6, 0.8, 1.2, 1.6, 2)
ROR_VALUE_COLORS <- c("green4", "lightgreen", "white", "red", "darkred", "purple4")
CHANGE_RATIO_VALUE_COLORS <- c("blue3", "lightblue", "white", "orange1", "red2", "red4")
TOP_PERCENTILE_COLORS <- c(No = "lightgray", Yes = "red")
MAP_BORDER_COLOR <- "gray35"
MAP_GRID_COLOR <- "gray88"

PLOT_WIDTH_IN <- 13.33
PLOT_HEIGHT_IN <- 7.5
PLOT_DPI <- 300
PLOT_TITLE_SIZE <- 16
PLOT_BASE_SIZE <- 12
LEGEND_KEY_HEIGHT_CM <- 0.9
LEGEND_KEY_WIDTH_CM <- 0.65
OVERWRITE_FIGURES <- TRUE


#### Helpers ####

make_parent_dir <- function(path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
}

temporal_run_name <- function(year) {
  sprintf("tr%s_%s_p%s", TEMPORAL_START_YEAR, year - 1L, year)
}

existing_path <- function(paths) {
  paths[file.exists(paths)][1]
}

annual_summary_raster_candidates <- function(year) {
  c(
    file.path(
      TEMPORAL_MAP_OUTPUT_DIR,
      "pred",
      "annual_rasters",
      sprintf("event_probability_summary_%s.tif", year)
    ),
    file.path(
      TEMPORAL_OUTPUT_BASE_DIR,
      temporal_run_name(year),
      "pred",
      "annual_rasters",
      sprintf("event_probability_summary_%s.tif", year)
    )
  )
}

ror_raster_candidates <- function(year) {
  c(
    file.path(TEMPORAL_MAP_OUTPUT_DIR, "pred", "ror", sprintf("event_probability_ROR_%s.tif", year)),
    file.path(TEMPORAL_OUTPUT_BASE_DIR, temporal_run_name(year), "pred", "ror", sprintf("event_probability_ROR_%s.tif", year))
  )
}

ror_change_raster_candidates <- function(year) {
  c(
    file.path(TEMPORAL_MAP_OUTPUT_DIR, "pred", "ror_1yr", sprintf("chgROR_1yr_%s_over_%s.tif", year, year - 1L)),
    file.path(TEMPORAL_OUTPUT_BASE_DIR, temporal_run_name(year), "pred", "ror_1yr", sprintf("chgROR_1yr_%s_over_%s.tif", year, year - 1L))
  )
}

summary_measure_name <- function(summary_column) {
  sub("^pred_", "", summary_column)
}

summary_column_for_measure <- function(measure) {
  paste0("pred_", measure)
}

stack_rasters <- function(raster_list) {
  if (length(raster_list) == 0) {
    stop("No rasters were supplied for stacking.", call. = FALSE)
  }

  raster_stack <- raster_list[[1]]
  if (length(raster_list) > 1) {
    for (i in 2:length(raster_list)) {
      raster_stack <- c(raster_stack, raster_list[[i]])
    }
  }
  raster_stack
}

probability_scalar_to_odds <- function(probability) {
  if (!is.finite(probability)) {
    return(NA_real_)
  }

  bounded_probability <- max(min(probability, 1 - ODDS_EPSILON), ODDS_EPSILON)
  bounded_probability / (1 - bounded_probability)
}

probability_to_odds_raster <- function(probability_raster) {
  bounded_probability <- terra::ifel(
    is.na(probability_raster),
    NA_real_,
    terra::ifel(
      probability_raster < ODDS_EPSILON,
      ODDS_EPSILON,
      terra::ifel(probability_raster > 1 - ODDS_EPSILON, 1 - ODDS_EPSILON, probability_raster)
    )
  )
  bounded_probability / (1 - bounded_probability)
}

raster_quantile <- function(raster_layer, probability = TOP_PERCENTILE) {
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

top_percentile_binary_raster <- function(raster_layer, layer_name) {
  threshold <- raster_quantile(raster_layer, TOP_PERCENTILE)
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

make_ror_raster_stack_from_annual_summary <- function(probability_raster, year) {
  summary_column <- summary_column_for_measure(SUMMARY_MEASURE)
  if (!summary_column %in% names(probability_raster)) {
    stop("Annual summary raster for ", year, " is missing layer: ", summary_column, call. = FALSE)
  }

  probability_layer <- probability_raster[[summary_column]]
  area_probability <- as.numeric(terra::global(probability_layer, "mean", na.rm = TRUE)[1, 1])
  area_odds <- probability_scalar_to_odds(area_probability)
  if (!is.finite(area_odds) || area_odds <= 0) {
    stop("Could not calculate study-area odds for ", SUMMARY_MEASURE, " in ", year, ".", call. = FALSE)
  }

  ror_name <- paste0("ROR_", SUMMARY_MEASURE)
  ror_layer <- probability_to_odds_raster(probability_layer) / area_odds
  names(ror_layer) <- ror_name
  top_layer <- top_percentile_binary_raster(ror_layer, paste0(ror_name, "_top1pct"))
  stack_rasters(list(ror_layer, top_layer))
}

read_ror_stack <- function(year) {
  ror_path <- existing_path(ror_raster_candidates(year))
  if (!is.na(ror_path)) {
    return(terra::rast(ror_path))
  }

  annual_summary_path <- existing_path(annual_summary_raster_candidates(year))
  if (!is.na(annual_summary_path)) {
    message("ROR raster was missing for ", year, "; deriving it from annual summary raster in memory.")
    return(make_ror_raster_stack_from_annual_summary(terra::rast(annual_summary_path), year))
  }

  stop(
    "No ROR or annual summary raster found for ",
    year,
    ". Checked:\n",
    paste(c(ror_raster_candidates(year), annual_summary_raster_candidates(year)), collapse = "\n"),
    call. = FALSE
  )
}

make_ror_change_stack <- function(year) {
  current_stack <- read_ror_stack(year)
  previous_stack <- read_ror_stack(year - 1L)
  value_layer <- paste0("ROR_", SUMMARY_MEASURE)
  if (!value_layer %in% names(current_stack) || !value_layer %in% names(previous_stack)) {
    stop("Missing ROR layer needed for change map: ", value_layer, call. = FALSE)
  }

  ratio_name <- paste0("chgROR_1yr_", SUMMARY_MEASURE)
  ratio_layer <- current_stack[[value_layer]] / previous_stack[[value_layer]]
  ratio_layer <- terra::ifel(is.na(ratio_layer), NA_real_, ratio_layer)
  names(ratio_layer) <- ratio_name
  top_layer <- top_percentile_binary_raster(ratio_layer, paste0(ratio_name, "_top1pct"))
  stack_rasters(list(ratio_layer, top_layer))
}

read_ror_change_stack <- function(year) {
  change_path <- existing_path(ror_change_raster_candidates(year))
  if (!is.na(change_path)) {
    return(terra::rast(change_path))
  }

  message("1-year ROR raster was missing for ", year, "/", year - 1L, "; deriving it from annual ROR rasters in memory.")
  make_ror_change_stack(year)
}

select_value_layer <- function(raster_stack, prefix) {
  preferred <- paste0(prefix, SUMMARY_MEASURE)
  if (preferred %in% names(raster_stack)) {
    return(preferred)
  }

  candidate <- names(raster_stack)[grepl(SUMMARY_MEASURE, names(raster_stack)) & !grepl("_top1pct$", names(raster_stack))]
  if (length(candidate) > 0) {
    warning("Preferred layer ", preferred, " was not found. Using ", candidate[1], " instead.")
    return(candidate[1])
  }

  stop("Could not find a ", SUMMARY_MEASURE, " value layer in raster stack. Layers: ", paste(names(raster_stack), collapse = ", "), call. = FALSE)
}

raster_extent_limits <- function(raster_layer) {
  extent <- terra::ext(raster_layer)
  stats::setNames(
    c(terra::xmin(extent), terra::xmax(extent), terra::ymin(extent), terra::ymax(extent)),
    c("xmin", "xmax", "ymin", "ymax")
  )
}

map_plot_extent <- function(raster_layer) {
  limits <- raster_extent_limits(raster_layer)
  if (is.null(MAP_LATITUDE_LIMITS) || length(MAP_LATITUDE_LIMITS) < 2) {
    return(terra::ext(limits[["xmin"]], limits[["xmax"]], limits[["ymin"]], limits[["ymax"]]))
  }

  ymin <- MAP_LATITUDE_LIMITS[1]
  ymax <- MAP_LATITUDE_LIMITS[2]

  if (
    !is.finite(ymin) ||
    !is.finite(ymax) ||
    ymin >= ymax ||
    ymax < limits[["ymin"]] ||
    ymin > limits[["ymax"]]
  ) {
    ymin <- limits[["ymin"]]
    ymax <- limits[["ymax"]]
  }

  terra::ext(limits[["xmin"]], limits[["xmax"]], ymin, ymax)
}

extent_limits <- function(extent) {
  stats::setNames(
    c(terra::xmin(extent), terra::xmax(extent), terra::ymin(extent), terra::ymax(extent)),
    c("xmin", "xmax", "ymin", "ymax")
  )
}

axis_ticks_within <- function(limits, n = 6) {
  ticks <- pretty(limits, n = n)
  ticks[ticks >= min(limits) & ticks <= max(limits)]
}

format_degree_labels <- function(values, positive_suffix, negative_suffix) {
  suffix <- ifelse(values > 0, positive_suffix, ifelse(values < 0, negative_suffix, ""))
  abs_values <- abs(values)
  labels <- ifelse(
    abs(abs_values - round(abs_values)) < 1e-8,
    as.character(round(abs_values)),
    trimws(format(round(abs_values, 2), scientific = FALSE, trim = TRUE))
  )
  paste0(labels, intToUtf8(176), suffix)
}

load_country_borders <- function(plot_extent) {
  if (!file.exists(AFRICA_COUNTRY_BORDER_FILE)) {
    warning("Africa country border shapefile was not found: ", AFRICA_COUNTRY_BORDER_FILE)
    return(NULL)
  }

  old_s2 <- sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)

  borders <- sf::st_read(AFRICA_COUNTRY_BORDER_FILE, quiet = TRUE)
  borders <- sf::st_transform(borders, RASTER_CRS)
  borders <- tryCatch(
    sf::st_make_valid(borders),
    error = function(e) {
      warning("Could not repair country border geometry with st_make_valid(); using original geometry.")
      borders
    }
  )

  limits <- extent_limits(plot_extent)
  bbox <- sf::st_bbox(
    c(
      xmin = limits[["xmin"]],
      ymin = limits[["ymin"]],
      xmax = limits[["xmax"]],
      ymax = limits[["ymax"]]
    ),
    crs = sf::st_crs(4326)
  )
  borders <- suppressWarnings(sf::st_crop(borders, bbox))
  if (nrow(borders) == 0) {
    return(NULL)
  }
  borders
}

raster_layer_to_df <- function(raster_layer, plot_extent) {
  cropped <- tryCatch(
    terra::crop(raster_layer, plot_extent, snap = "out"),
    error = function(e) raster_layer
  )
  raster_df <- terra::as.data.frame(cropped, xy = TRUE, na.rm = FALSE)
  names(raster_df)[1:2] <- c("x", "y")
  value_column <- names(cropped)[1]
  out <- data.frame(
    x = raster_df$x,
    y = raster_df$y,
    value = raster_df[[value_column]],
    stringsAsFactors = FALSE
  )
  out[is.finite(out$x) & is.finite(out$y) & is.finite(out$value), , drop = FALSE]
}

pretty_legend_value <- function(value) {
  formatted <- format(signif(value, 3), scientific = FALSE, trim = TRUE)
  sub("\\.?0+$", "", formatted)
}

discrete_break_labels <- function(breaks) {
  if (length(breaks) < 2) {
    return(character(0))
  }

  labels <- character(length(breaks) - 1)
  for (i in seq_along(labels)) {
    lower <- pretty_legend_value(breaks[i])
    upper <- pretty_legend_value(breaks[i + 1])
    if (i == 1) {
      labels[i] <- paste0("<", upper)
    } else if (i == length(labels)) {
      labels[i] <- paste0(lower, "+")
    } else {
      labels[i] <- paste0(lower, "-", upper)
    }
  }
  labels
}

value_plot_breaks_from_values <- function(values, plot_kind) {
  finite_values <- values[is.finite(values)]
  if (length(finite_values) == 0) {
    return(c(0, 1))
  }

  min_value <- min(finite_values, na.rm = TRUE)
  max_value <- max(finite_values, na.rm = TRUE)
  if (!is.finite(min_value) || !is.finite(max_value)) {
    return(c(0, 1))
  }
  if (min_value == max_value) {
    pad <- max(abs(min_value) * 0.05, 1e-6)
    return(c(min_value - pad, max_value + pad))
  }

  internal_breaks <- switch(
    plot_kind,
    ror = ROR_BREAKPOINTS,
    change = CHANGE_RATIO_BREAKPOINTS,
    pretty(finite_values, n = 6)
  )
  breaks <- sort(unique(c(
    min_value,
    internal_breaks[internal_breaks > min_value & internal_breaks < max_value],
    max_value
  )))

  if (length(breaks) < 2) {
    pad <- max(abs(min_value) * 0.05, 1e-6)
    breaks <- c(min_value - pad, max_value + pad)
  }
  breaks
}

value_plot_palette <- function(plot_kind, n_colors) {
  if (plot_kind == "ror") {
    return(grDevices::colorRampPalette(ROR_VALUE_COLORS)(n_colors))
  }
  if (plot_kind == "change") {
    return(grDevices::colorRampPalette(CHANGE_RATIO_VALUE_COLORS)(n_colors))
  }
  grDevices::hcl.colors(n_colors, "Viridis")
}

map_theme <- function() {
  ggplot2::theme_bw(base_size = PLOT_BASE_SIZE) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "black", size = PLOT_BASE_SIZE - 1),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = PLOT_BASE_SIZE, face = "bold"),
      legend.text = ggplot2::element_text(size = PLOT_BASE_SIZE - 1),
      legend.key.height = grid::unit(LEGEND_KEY_HEIGHT_CM, "cm"),
      legend.key.width = grid::unit(LEGEND_KEY_WIDTH_CM, "cm"),
      panel.grid.major = ggplot2::element_line(color = MAP_GRID_COLOR, linewidth = 0.25),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.border = ggplot2::element_rect(fill = NA, color = MAP_BORDER_COLOR, linewidth = 0.45),
      plot.title = ggplot2::element_text(size = PLOT_BASE_SIZE + 1, face = "bold", hjust = 0.5),
      plot.margin = ggplot2::margin(3, 6, 3, 3)
    )
}

make_map_base <- function(plot_df, plot_extent, panel_title) {
  limits <- extent_limits(plot_extent)
  x_ticks <- axis_ticks_within(c(limits[["xmin"]], limits[["xmax"]]))
  y_ticks <- axis_ticks_within(c(limits[["ymin"]], limits[["ymax"]]))

  ggplot2::ggplot(plot_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::scale_x_continuous(
      breaks = x_ticks,
      labels = format_degree_labels(x_ticks, "E", "W"),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_ticks,
      labels = format_degree_labels(y_ticks, "N", "S"),
      expand = c(0, 0)
    ) +
    ggplot2::coord_sf(
      xlim = c(limits[["xmin"]], limits[["xmax"]]),
      ylim = c(limits[["ymin"]], limits[["ymax"]]),
      expand = FALSE,
      crs = sf::st_crs(4326)
    ) +
    ggplot2::labs(title = panel_title) +
    map_theme()
}

add_country_borders <- function(plot, plot_extent) {
  borders <- load_country_borders(plot_extent)
  if (is.null(borders)) {
    return(plot)
  }

  plot +
    ggplot2::geom_sf(
      data = borders,
      inherit.aes = FALSE,
      fill = NA,
      color = MAP_BORDER_COLOR,
      linewidth = 0.25
    )
}

make_value_plot <- function(raster_stack, value_layer, plot_kind, panel_title) {
  plot_extent <- map_plot_extent(raster_stack[[value_layer]])
  plot_df <- raster_layer_to_df(raster_stack[[value_layer]], plot_extent)
  if (nrow(plot_df) == 0) {
    stop("No finite raster values were found for layer: ", value_layer, call. = FALSE)
  }

  breaks <- value_plot_breaks_from_values(plot_df$value, plot_kind)
  labels <- discrete_break_labels(breaks)
  colors <- value_plot_palette(plot_kind, length(labels))
  plot_df$value_class <- cut(plot_df$value, breaks = breaks, labels = labels, include.lowest = TRUE)
  plot_df <- plot_df[!is.na(plot_df$value_class), , drop = FALSE]

  p <- make_map_base(plot_df, plot_extent, panel_title) +
    ggplot2::geom_raster(ggplot2::aes(fill = value_class)) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(colors, labels),
      breaks = labels,
      drop = FALSE,
      name = ifelse(plot_kind == "ror", "ROR", "Ratio"),
      guide = ggplot2::guide_legend(
        reverse = TRUE,
        keyheight = grid::unit(LEGEND_KEY_HEIGHT_CM, "cm"),
        keywidth = grid::unit(LEGEND_KEY_WIDTH_CM, "cm")
      )
    )
  add_country_borders(p, plot_extent)
}

make_top_plot <- function(raster_stack, value_layer, panel_title) {
  top_layer <- paste0(value_layer, "_top1pct")
  if (top_layer %in% names(raster_stack)) {
    top_raster <- raster_stack[[top_layer]]
  } else {
    top_raster <- top_percentile_binary_raster(raster_stack[[value_layer]], top_layer)
  }

  plot_extent <- map_plot_extent(top_raster)
  plot_df <- raster_layer_to_df(top_raster, plot_extent)
  if (nrow(plot_df) == 0) {
    stop("No finite top-1% raster values were found for layer: ", top_layer, call. = FALSE)
  }

  plot_df$top1pct <- factor(ifelse(plot_df$value >= 1, "Yes", "No"), levels = c("No", "Yes"))

  p <- make_map_base(plot_df, plot_extent, panel_title) +
    ggplot2::geom_raster(ggplot2::aes(fill = top1pct)) +
    ggplot2::scale_fill_manual(
      values = TOP_PERCENTILE_COLORS,
      breaks = c("Yes", "No"),
      drop = FALSE,
      name = "Top 1%",
      guide = ggplot2::guide_legend(
        keyheight = grid::unit(LEGEND_KEY_HEIGHT_CM, "cm"),
        keywidth = grid::unit(LEGEND_KEY_WIDTH_CM, "cm")
      )
    )
  add_country_borders(p, plot_extent)
}

save_two_panel_map <- function(value_plot, top_plot, title, output_png) {
  if (file.exists(output_png) && !OVERWRITE_FIGURES) {
    message("Figure exists; skipping: ", output_png)
    return(invisible(output_png))
  }

  make_parent_dir(output_png)
  grDevices::png(
    filename = output_png,
    width = PLOT_WIDTH_IN,
    height = PLOT_HEIGHT_IN,
    units = "in",
    res = PLOT_DPI
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  grid::grid.newpage()
  layout <- grid::grid.layout(
    nrow = 3,
    ncol = 1,
    heights = grid::unit.c(
      grid::unit(0.35, "in"),
      grid::unit(1, "null"),
      grid::unit(1, "null")
    )
  )
  grid::pushViewport(grid::viewport(layout = layout))
  grid::grid.text(
    title,
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1),
    gp = grid::gpar(fontsize = PLOT_TITLE_SIZE, fontface = "bold")
  )
  print(value_plot, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(top_plot, vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1))
  grid::popViewport()

  message("Wrote: ", output_png)
  invisible(output_png)
}

plot_annual_ror_figure <- function(year) {
  ror_stack <- read_ror_stack(year)
  value_layer <- select_value_layer(ror_stack, "ROR_")
  value_plot <- make_value_plot(
    ror_stack,
    value_layer,
    plot_kind = "ror",
    panel_title = sprintf("Mean relative odds ratio, %s", year)
  )
  top_plot <- make_top_plot(
    ror_stack,
    value_layer,
    panel_title = sprintf("Top 1%% of mean relative odds ratio, %s", year)
  )
  output_png <- file.path(OUTPUT_DIR, sprintf("tfv_ROR_%s_%s.png", SUMMARY_MEASURE, year))
  save_two_panel_map(
    value_plot,
    top_plot,
    title = sprintf("Temporal Forward Validation ROR, %s", year),
    output_png = output_png
  )
}

plot_annual_ror_change_figure <- function(year) {
  change_stack <- read_ror_change_stack(year)
  value_layer <- select_value_layer(change_stack, "chgROR_1yr_")
  value_plot <- make_value_plot(
    change_stack,
    value_layer,
    plot_kind = "change",
    panel_title = sprintf("Mean 1-year ROR ratio, %s/%s", year, year - 1L)
  )
  top_plot <- make_top_plot(
    change_stack,
    value_layer,
    panel_title = sprintf("Top 1%% of mean 1-year ROR ratio, %s/%s", year, year - 1L)
  )
  output_png <- file.path(OUTPUT_DIR, sprintf("tfv_chgROR_%s_%s_over_%s.png", SUMMARY_MEASURE, year, year - 1L))
  save_two_panel_map(
    value_plot,
    top_plot,
    title = sprintf("Temporal Forward Validation 1-Year ROR Change, %s/%s", year, year - 1L),
    output_png = output_png
  )
}

safe_make_figure <- function(kind, year) {
  output <- tryCatch(
    {
      path <- switch(
        kind,
        annual_ror = plot_annual_ror_figure(year),
        ror_change = plot_annual_ror_change_figure(year),
        stop("Unknown figure kind: ", kind, call. = FALSE)
      )
      data.frame(kind = kind, year = year, status = "written", path = path, message = "", stringsAsFactors = FALSE)
    },
    error = function(e) {
      if (isTRUE(STOP_ON_MISSING_INPUTS)) {
        stop(e)
      }
      msg <- conditionMessage(e)
      warning("Skipping ", kind, " figure for ", year, ": ", msg)
      data.frame(kind = kind, year = year, status = "skipped", path = NA_character_, message = msg, stringsAsFactors = FALSE)
    }
  )
  output
}


#### Run Map Exports ####

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
message("TrevorPowerpoint output directory: ", OUTPUT_DIR)

annual_results <- do.call(
  rbind,
  lapply(ANNUAL_ROR_YEARS, function(year) safe_make_figure("annual_ror", year))
)

change_results <- do.call(
  rbind,
  lapply(CHANGE_ROR_YEARS, function(year) safe_make_figure("ror_change", year))
)

manifest <- rbind(annual_results, change_results)
utils::write.csv(manifest, OUTPUT_MANIFEST_CSV, row.names = FALSE)
message("Wrote map manifest: ", OUTPUT_MANIFEST_CSV)
message("Figures written: ", sum(manifest$status == "written"))
message("Figures skipped: ", sum(manifest$status == "skipped"))

if (any(manifest$status == "skipped")) {
  message("Skipped figures:")
  print(manifest[manifest$status == "skipped", c("kind", "year", "message")], row.names = FALSE)
}
