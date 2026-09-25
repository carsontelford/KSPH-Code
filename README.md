# Event Ecological Niche Pipeline

This repository is a handoff-ready pipeline for predicting where and when an event might occur. The structure follows the reference code conceptually, but separates sampling, covariate extraction, model training, and prediction into reproducible steps.

The current toy study area is mainland Africa within approximately 10 degrees north/south of the equator. The canonical study-area input is `config/africacountries_nolakes.shp`, clipped to the equatorial band inside the scripts. Climate covariates use global products available in Earth Engine, including ERA5-Land aggregates for precipitation, temperature, and PET, and MODIS for NDVI.

Prediction grid cells and pseudo-absence points are created inside the core study area. Covariate extraction uses the same land shapefile with a small latitude margin so outer ring buffers near the study-area edge can still summarize nearby land context.

## Folder Structure

- `R_python_code/`: R scripts plus Python notebooks/helpers that orchestrate Google Earth Engine exports.
- `config/`: Africa no-lakes study-area shapefile, predictor list, and optional external raster config.
- `Static Covariates/`: local static raster covariates supplied by the analyst. The folder is tracked, but raster files inside it are ignored by Git.
- `analyses/`: generated study-area analysis folders, created locally and ignored by Git.

The GitHub repo is intentionally set up to track only `R_python_code/`,
`config/`, `Static Covariates/.gitkeep`, `analyses/.gitkeep`, `README.md`, and
`.gitignore`. Generated covariate exports, fitted models, rasters, HTML
reports, and local session files should be created on each analyst's machine
and are ignored by `.gitignore`.

Generated analyses use two levels of organization:

- Study-area analysis: products that require covariate extraction, such as `equatorial_africa` or `drc`.
- Modeling sub-analysis: products that only require refitting/filtering, such as `all_types` or `type_Z`.

The resulting local folder structure is:

```text
analyses/
  equatorial_africa/
    data/
    models/
      superlearner/
        all_types/
        type_Z/
      stepwise_superlearner/
        all_types/
        type_Z/
    outputs/
      descriptive/
        all_types/
        type_Z/
      extraction/
      superlearner/
        all_types/
        type_Z/
      stepwise_superlearner/
        all_types/
        type_Z/

  drc/
    data/
    models/
      superlearner/
        all_types/
        type_Z/
      stepwise_superlearner/
        all_types/
        type_Z/
    outputs/
      descriptive/
        all_types/
        type_Z/
      extraction/
      superlearner/
        all_types/
        type_Z/
      stepwise_superlearner/
        all_types/
        type_Z/
```

To switch study areas, edit `STUDY_AREA_ANALYSIS_NAME`, `STUDY_AREA_FILE`, and
`STUDY_AREA_BBOX` near the top of scripts `01` and `02`, then use the same
`STUDY_AREA_ANALYSIS_NAME` in scripts `03` through `08`. To run a model
sub-analysis without re-extracting covariates, edit `SUBANALYSIS_NAME` and/or
`TRAINING_TYPE_FILTER` near the top of scripts `04` through `07`, then point
script `08` to the matching sub-analysis folder. Legacy root-level path
fallbacks are off by default; only enable `ALLOW_LEGACY_PATH_FALLBACK` for
one-time migration checks.

## Pipeline

Run scripts from the `KSPH Code` repo root.

Install R package dependencies before running the R scripts:

```r
install.packages(c(
  "sf", "terra", "ggplot2", "caret", "rmarkdown", "knitr",
  "leaflet", "htmltools", "htmlwidgets", "SuperLearner", "rpart", "ranger"
))
```

Archived legacy BRT scripts may require additional packages such as `gbm`,
`dismo`, and `treeshap`, but those are not part of the current production
SuperLearner workflow.

### 1. Create Dataset1

```r
source("R_python_code/01_make_dataset1.R")
```

Output:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset1.csv`

This table contains:

- `id`
- `year`
- `latitude`
- `longitude`
- `outcome`
- `type`
- `country`
- `point_type`
- `source`
- `sampling_version`
- `created_date`

The script reads presence records from
`analyses/<STUDY_AREA_ANALYSIS_NAME>/data/outcomes_csv.csv` if present, then
falls back to `config/outcomes_csv.csv`. It preserves the `type` and `country`
columns. Because generated analysis folders are ignored by Git, each analyst
can place study-area-specific input files in the relevant local analysis data
folder before running the full pipeline.

The pseudo-absence sampling area is controlled here. For the default
equatorial Africa analysis, the Africa no-lakes shapefile is clipped to
`STUDY_AREA_BBOX`. For a DRC-only analysis, set `STUDY_AREA_ANALYSIS_NAME <-
"drc"`, point `STUDY_AREA_FILE` to a DRC shapefile, and set
`STUDY_AREA_BBOX <- NULL`.

### 2. Extract Training And Prediction-Grid Covariates

Install Python requirements if needed:

```bash
python -m pip install -r R_python_code/requirements.txt
```

Recommended notebook workflow:

- `R_python_code/02_predGrid_trainSet_extraction.ipynb`

That notebook has two linked phases:

1. Create/read the prediction grid and extract prediction-grid covariates.
2. Create/read the training point buffers and extract matching training covariates.

The notebook exports large covariate tables to Google Drive because Earth
Engine can write large tables there server-side. Google Drive for Desktop then
syncs those CSVs locally for the merge cells.

Key outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_5km.csv`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2021_2025.csv`

The active study-area controls live near the top of the notebook:

- `STUDY_AREA_NAME`
- `STUDY_AREA_ANALYSIS_NAME`
- `STUDY_AREA_FILE`
- `STUDY_AREA_BBOX`
- `STUDY_AREA_CONTEXT_BUFFER_DEGREES`

Set `STUDY_AREA_BBOX = None` to use the full extent/polygon of a
country-specific `STUDY_AREA_FILE`.

### 3. Add Post-Extraction Covariates

Copy any local static `.tif`/`.tiff` covariate rasters into:

- `Static Covariates/`

This folder is intentionally empty in Git except for `.gitkeep`. Each analyst
must place the required local static rasters there before running script `03`.

Then run:

```r
source("R_python_code/03_add_covars.R")
```

This samples each raster at the training and prediction-grid point locations,
caches one extraction CSV per raster in the active analysis data folder, appends
matching covariate columns to
`analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv` and
`analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2021_2025.csv`,
creates forest/log-population and forest-edge/log-population interaction
covariates at each spatial scale, and updates `config/predictor_list.csv`.

### 4. Descriptive Analysis

```r
source("R_python_code/04_descriptive_analysis.R")
```

Outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/descriptive/<SUBANALYSIS_NAME>/tables/`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/descriptive/<SUBANALYSIS_NAME>/plots/`

This script checks covariate completeness, exports Table 1-style summaries for
presences versus absences and by event type, runs simple unadjusted standardized
logistic summaries, and writes basic boxplot PDFs.

### 5. Tune SuperLearner With Cross-Validation

```r
source("R_python_code/05_tune_superlearner_cv.R")
```

Outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/models/superlearner/<SUBANALYSIS_NAME>/`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/superlearner/<SUBANALYSIS_NAME>/tuning/`

This script runs leave-year-out cross-validation for the production
SuperLearner. Within each outer fold it fits a small sampled ensemble using all
events and 50 sampled controls per event, averages the held-out predictions,
and chooses the threshold that maximizes sensitivity x specificity. It saves
the retained predictor list, the missingness audit, the fold assignments, CV
predictions, caret confusion matrix output, and the tuned hyperparameters/
threshold used by script `06`.

At the start of tuning, predictors are screened for excessive training-data
missingness after the Hansen zero-fill rules are applied. By default, any
predictor with more than 20% missingness in the filtered training data is
excluded from that model run. Script `06` separately verifies that retained
predictors are usable in the full prediction grid before predicting.

### 6. Stepwise SuperLearner Predictions

```r
source("R_python_code/06_stepwise_predict_superlearner.R")
```

Outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/models/stepwise_superlearner/<SUBANALYSIS_NAME>/`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/stepwise_superlearner/<SUBANALYSIS_NAME>/`

This script reads the tuned settings from `05`, trains only on data available
before each target year, then predicts years 2021-2025. Each year is predicted
with a 50-fit sampled SuperLearner ensemble using all events and 50 sampled
controls per event for each ensemble fit. It writes annual prediction tables,
probability rasters, ROR rasters, and 1-year ROR-change rasters.

### 7. Evaluate Stepwise Predictions

```r
source("R_python_code/07_evaluate_stepwise_predictions.R")
```

Outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/stepwise_superlearner/<SUBANALYSIS_NAME>/evaluation/`

This script extracts the stepwise predictions at labeled training-data
locations from 2021-2025 and reports caret confusion matrices, sensitivity,
specificity, PPV, NPV, F1, ROC AUC, and PR AUC. It also evaluates top-1%
decision rules for annual ROR, 1-year ROR increase, and either condition.

### 8. Interactive Email Report

```r
rmarkdown::render("R_python_code/08_interactive_prediction_report_email.Rmd")
```

This self-contained HTML report visualizes the stepwise SuperLearner ROR and
1-year ROR-change rasters with event overlays. Set
`STUDY_AREA_ANALYSIS_NAME` and `STEPWISE_ANALYSIS_GROUP` near the top of the
Rmd to choose the study area and sub-analysis.

## Core Covariates

Spatially scaled covariates are extracted over donut buffers:

- `0_10km`
- `10_25km`
- `25_50km`

These include:

- forest cover proportion
- same-year forest loss
- one-year-prior forest loss
- two-year-prior forest loss
- forest edge proportion
- population density

Non-scaled covariates are extracted over `0_10km` only:

- annual precipitation
- annual precipitation anomaly
- standardized precipitation anomaly
- annual temperature
- annual temperature anomaly
- standardized temperature anomaly
- annual potential evapotranspiration
- annual NDVI
- annual NDVI anomaly
- standardized NDVI anomaly
- elevation

The model predictor names live in:

- `config/predictor_list.csv`

## External Rasters

There are two supported ways to add raster covariates.

For local static rasters, copy `.tif`/`.tiff` files into:

- `Static Covariates/`

Then run `R_python_code/03_add_covars.R`. These files are not
committed to Git.

For rasters that should be extracted inside Earth Engine, upload the raster to
Earth Engine as an asset, then edit:

- `config/external_rasters.csv`

Set `enabled` to `true` and choose `buffer_mode`:

- `nonscaled`: extract only `0_10km`
- `scaled`: extract all donut buffers

## Notes

- LandScan is used with a latest-available-year rule.
- Forest-loss lag variables are structurally unavailable before their lagged Hansen year exists: 1-year lag is unavailable for 2001, and 2-year lag is unavailable for 2001-2002.
- Prediction grid cells are sampled at approximately 5 km resolution.
- Training and grid locations are limited to the configured study area, while covariate extraction uses a buffered land context near the study-area edge.
- `01_make_dataset1.R` requires `sf` for true polygon sampling from `africacountries_nolakes.shp`. A bounding-box fallback exists for quick testing only, but is disabled by default because it can sample ocean points.
- The Python GEE scripts require `geopandas`/`shapely` to read the same shapefile and build the land-only prediction grid.
- The older `01_extract_covariates_gee.js` file is retained as a Code Editor prototype, but the production-oriented pipeline uses the Python scripts.
