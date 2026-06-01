# HarmonizeR 0.1.0.9000

## Development version

- Added exported demo-data simulation helpers.
- Added lightweight MCMC summary helpers for selected posterior draws.
- Added `repair_light_upload()` for old `*_LIGHT_UPLOAD.rds` files with missing `mcmc_summary`.
- Added unit tests for demo data and MCMC light-summary utilities.
- Kept the full Shiny app intact under `inst/app/app.R`.

# HarmonizeR 0.1.0.9001

* Refactored `inst/app/app.R` to call package-level helpers for demo data and MCMC draw summaries.
* Moved generated-script MCMC light-export helper code into `mcmc_light_export_helper_code()`.
* Kept standalone downloaded MCMC scripts self-contained while avoiding helper duplication in the Shiny app.

- Moved MCMC script generation from `inst/app/app.R` into `R/mcmc_script_code.R` via exported `build_mcmc_script()`, keeping app.R focused on Shiny flow.

## HarmonizeR 0.1.0.9000

- Added cross-validated random forest batch-prediction AUC diagnostics via `batch_auc_cal_cv()`.
- Added pre-harmonization and post-harmonization Shiny panels for global batch detectability within each metric.
- Added optional app dependencies `caret`, `randomForest`, and `pROC` for RF batch AUC diagnostics.
- Improved RF batch-prediction AUC plot colors and table highlighting for easier interpretation.

# HarmonizeR 0.1.0.9000

* Added `install_harmonizer_dependencies()` to help users install the packages
  required by the full Shiny application, with optional support for the MCMC
  backend through `cmdstanr`.
* Added tests and documentation for dependency installation dry-run behavior.
