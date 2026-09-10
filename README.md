# Event Ecological Niche Pipeline

This repository is a handoff-ready pipeline for predicting where and when an event might occur. The structure follows the reference code conceptually, but separates sampling, covariate extraction, model training, and prediction into reproducible steps.

The current toy study area is mainland Africa within approximately 10 degrees north/south of the equator. The canonical study-area input is `config/africacountries_nolakes.shp`, clipped to the equatorial band inside the scripts. Climate covariates use global products available in Earth Engine, including ERA5-Land aggregates for precipitation, temperature, and PET, and MODIS for NDVI.

Prediction grid cells and pseudo-absence points are created inside the core study area. Covariate extraction uses the same land shapefile with a small latitude margin so 100 km donut buffers near the study-area edge can still summarize nearby land context.

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
      all_types/
      tfv/
        all_types/
    outputs/
      extraction/
      all_types/
      tfv/
        all_types/

  drc/
    data/
    models/
      all_types/
      type_Z/
      tfv/
        all_types/
        type_Z/
    outputs/
      extraction/
      all_types/
      type_Z/
      tfv/
        all_types/
        type_Z/
```

To switch study areas, edit `STUDY_AREA_ANALYSIS_NAME`, `STUDY_AREA_FILE`, and
`STUDY_AREA_BBOX` near the top of scripts `01` and `02`, then use the same
`STUDY_AREA_ANALYSIS_NAME` in scripts `03`, `04`, `04b`, `05`, `05b`, `05c`,
and `06`. To run a model sub-analysis without re-extracting covariates, edit
`SUBANALYSIS_NAME` and/or `TRAINING_TYPE_FILTER` near the top of the modeling
scripts, then point the report scripts to the matching sub-analysis folder.
Legacy root-level path fallbacks are off by default; only enable
`ALLOW_LEGACY_PATH_FALLBACK` for one-time migration checks.

## Pipeline

Run scripts from the `KSPH Code` repo root.

Install R package dependencies before running the R scripts:

```r
install.packages(c(
  "sf", "terra", "gbm", "dismo", "ggplot2", "pROC", "PRROC",
  "caret", "rmarkdown", "knitr", "shiny", "leaflet", "htmltools",
  "htmlwidgets", "SuperLearner", "rpart", "ranger"
))
```

`treeshap` is only needed if `RUN_SHAP_CALCULATIONS <- TRUE` in the BRT
modeling scripts.

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
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_10km.csv`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2020_2025.csv`

The active study-area controls live near the top of the notebook:

- `STUDY_AREA_NAME`
- `STUDY_AREA_ANALYSIS_NAME`
- `STUDY_AREA_FILE`
- `STUDY_AREA_BBOX`
- `STUDY_AREA_CONTEXT_BUFFER_DEGREES`

Set `STUDY_AREA_BBOX = None` to use the full extent/polygon of a
country-specific `STUDY_AREA_FILE`.

### 3. Append Static Local Covariates

Copy any local static `.tif`/`.tiff` covariate rasters into:

- `Static Covariates/`

This folder is intentionally empty in Git except for `.gitkeep`. Each analyst
must place the required local static rasters there before running script `03`.

Then run:

```r
source("R_python_code/03_append_static_covariates.R")
```

This samples each raster at the training and prediction-grid point locations,
caches one extraction CSV per raster in the active analysis data folder, appends
matching covariate columns to
`analyses/<STUDY_AREA_ANALYSIS_NAME>/data/dataset2.csv` and
`analyses/<STUDY_AREA_ANALYSIS_NAME>/data/prediction_grid_covariates_2020_2025.csv`,
and updates `config/predictor_list.csv`.

### 4. Train Full BRT Model And Predict Maps

```r
source("R_python_code/04_train_predict_brt_simple.R")
```

Outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/models/<SUBANALYSIS_NAME>/`
- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/<SUBANALYSIS_NAME>/predictions/`

The script creates sampled control/event training datasets, fits the BRT
ensemble, predicts over the prediction-grid table, rasterizes annual summaries,
and creates derived ROR/change outputs and model diagnostics.

At the start of modeling, predictors are screened for excessive missingness
after the Hansen zero-fill rules are applied. By default, any predictor with
more than 20% missingness in either the filtered training data or selected
prediction-grid rows is excluded from that model run. The audit file is saved
as `models/<SUBANALYSIS_NAME>/predictor_missingness_report.csv`.

### 4b. Temporal Forward Validation

```r
source("R_python_code/04b_temporal_forward_validation_brt.R")
```

Outputs:

- `analyses/<STUDY_AREA_ANALYSIS_NAME>/outputs/tfv/<SUBANALYSIS_NAME>/`

This retrospective workflow trains only on years before each target year, then
predicts the held-forward year.

The same predictor missingness screen is applied once before the forward-year
fits, and its audit file is saved under
`models/tfv/<SUBANALYSIS_NAME>/predictor_missingness_report.csv`.

### 5. Interactive Reports

Run the Shiny report while working interactively:

```r
rmarkdown::run("R_python_code/05_interactive_prediction_report_Shiny.Rmd")
```

Knit the email-friendly HTML report:

```r
rmarkdown::render("R_python_code/05b_interactive_prediction_report_email.Rmd")
```

For reports, set `STUDY_AREA_ANALYSIS_NAME` to choose the extracted study area,
`FULL_MODEL_SUBANALYSIS_NAME` to choose full-model outputs, and
`TEMPORAL_ANALYSIS_GROUP` to choose temporal-forward-validation outputs.

### 6. SuperLearner Prototype

```r
source("R_python_code/06_train_predict_SuperLearner_CV.R")
```

This prototype uses the same predictor missingness screen before median
imputation, so high-missing static covariates are dropped rather than filled
across places where the source raster does not exist.

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

Then run `R_python_code/03_append_static_covariates.R`. These files are not
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
- Prediction grid cells are sampled at approximately 10 km resolution.
- Training and grid locations are limited to the configured study area, while covariate extraction uses a buffered land context near the study-area edge.
- `01_make_dataset1.R` requires `sf` for true polygon sampling from `africacountries_nolakes.shp`. A bounding-box fallback exists for quick testing only, but is disabled by default because it can sample ocean points.
- The Python GEE scripts require `geopandas`/`shapely` to read the same shapefile and build the land-only prediction grid.
- The older `01_extract_covariates_gee.js` file is retained as a Code Editor prototype, but the production-oriented pipeline uses the Python scripts.
