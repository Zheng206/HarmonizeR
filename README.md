# HarmonizeR

<!-- badges: start -->
<!-- badges: end -->

`HarmonizeR` is an R package that provides an interactive Shiny interface for diagnostic-driven neuroimaging harmonization. It is designed for multi-site imaging studies where technical variation from sites, scanners, acquisition protocols, or other batch variables may obscure biological effects of interest.

The package brings together data upload, batch-effect diagnostics, ComBat-style harmonization, empirical Bayes diagnostics, covariance evaluation, Bayesian MCMC script generation, and post-harmonization assessment in a single workflow. `HarmonizeR` supports both single-metric and multi-metric imaging analyses, with tools for cross-sectional and longitudinal data structures.

## Key features

- **Interactive Shiny workflow** for data setup, model configuration, harmonization, and evaluation.
- **Pre-harmonization diagnostics**, including PCA visualization, residual boxplots, univariate tests, multivariate tests, p-value exploration, and random forest batch-prediction AUC.
- **ComBat-style harmonization** for single-metric and multi-metric data through the `MultiComBat` framework.
- **Flexible model specification**, including linear models, generalized additive models, and longitudinal mixed-effects models.
- **Empirical Bayes diagnostics** for reviewing shrinkage behavior and fitted batch-effect parameters.
- **Covariance diagnostics** for evaluating residual covariance-related batch effects and CovBat-style adjustment.
- **Bayesian MCMC workflow support**, including downloadable standalone Stan scripts and lightweight upload objects for reviewing posterior diagnostics in the app.
- **Post-harmonization evaluation** to compare batch effects before and after harmonization.
- **Demo-data generators** for cross-sectional and longitudinal examples.

## Installation

You can install `HarmonizeR` directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("your-github-username/HarmonizeR")
```

## Installing app dependencies

The full Shiny application uses several plotting, modeling, and diagnostic packages. To keep the package lightweight, many of these packages are listed in `Suggests` rather than imported at package load time.

After installing or loading `HarmonizeR`, install the recommended app dependencies with:

```r
HarmonizeR::install_harmonizer_dependencies()
```

To check which dependencies are missing without installing anything, use:

```r
HarmonizeR::install_harmonizer_dependencies(dry_run = TRUE)
```

For Bayesian MCMC workflows, install the optional MCMC backend as well:

```r
HarmonizeR::install_harmonizer_dependencies(include_mcmc = TRUE)
cmdstanr::install_cmdstan()
```

`HarmonizeR` also depends on the `MultiComBat` framework for the main harmonization routines. By default, `HarmonizeR::install_harmonizer_dependencies()` will check for `MultiComBat` and install it from GitHub if it is missing.

If you already have `MultiComBat` installed from a local or development source, you can skip that step with:

```r
HarmonizeR::install_harmonizer_dependencies(include_multicombat = FALSE)
```

## Launching the app

After installation and dependency setup, launch the Shiny app with:

```r
HarmonizeR::run_harmonizer()
```

You can pass arguments directly to `shiny::runApp()`. For example:

```r
HarmonizeR::run_harmonizer(port = 4004, launch.browser = TRUE)
```

## Basic workflow

The Shiny app is organized around an end-to-end harmonization workflow.

### 1. Data setup

Users provide imaging feature matrices, batch labels, and optional covariates to preserve during harmonization. The app supports both cross-sectional and longitudinal data. Multi-metric analyses require matched observations across imaging metrics.

The data setup module includes:

- data upload or simulated demo-data generation;
- batch-variable and covariate selection;
- model-formula configuration;
- cross-sectional or longitudinal structure specification;
- covariate-feature trend exploration.

### 2. Pre-harmonization diagnostics

Before harmonization, users can inspect whether batch effects are present and how they appear in the data. Diagnostics include:

- PCA visualization by batch;
- additive and multiplicative residual boxplots;
- univariate batch-effect tests;
- p-value summaries and feature-level exploration;
- multivariate tests;
- out-of-sample random forest batch-prediction AUC.

These diagnostics are intended to help users decide whether harmonization is needed and which type of harmonization strategy may be appropriate.

### 3. Harmonization

The harmonization module supports ComBat-style adjustment through the `MultiComBat` framework. Users can configure options such as:

- single-metric or multi-metric harmonization;
- empirical Bayes shrinkage;
- robust estimation;
- reference batch specification;
- CovBat-style covariance adjustment;
- model type, including linear, nonlinear, or longitudinal models.

For Bayesian workflows, the app can generate a standalone MCMC script that can be run outside the Shiny session, for example on a local machine or high-performance computing cluster.

### 4. Post-harmonization evaluation

After harmonization, users can compare pre- and post-harmonization results using:

- PCA comparisons;
- test-statistic and p-value comparisons;
- residual boxplots;
- batch-prediction AUC before and after harmonization.

This step helps evaluate whether residual technical variation remains after adjustment.

### 5. Empirical Bayes, covariance, and Bayesian diagnostics

Additional diagnostic tabs provide more detailed review of fitted harmonization models:

- **EB Diagnostics**: prior-versus-empirical summaries and batch-parameter estimates.
- **Covariance Diagnostics**: covariance and correlation structure summaries before and after adjustment.
- **Bayesian MCMC Diagnostics**: R-hat, effective sample size, trace plots, posterior summaries, and posterior evidence levels from uploaded lightweight MCMC results.

## Demo data

`HarmonizeR` includes helper functions for generating example datasets.

### Cross-sectional demo data

```r
demo <- HarmonizeR::simulate_demo_data(
  n = 100,
  G = 50,
  m = 3,
  n_batches = 3,
  seed = 123
)

str(demo)
```

### Longitudinal demo data

```r
demo_long <- HarmonizeR::simulate_demo_longitudinal_data(
  n_subjects = 50,
  G = 30,
  m = 3,
  n_batches = 3,
  max_visits = 3,
  seed = 123
)

str(demo_long)
```

The returned objects include imaging data matrices, batch labels, covariates, feature names, and metadata used by the app.

## Batch-prediction AUC diagnostic

`HarmonizeR` includes an optional global diagnostic based on out-of-sample random forest prediction of batch labels. This diagnostic can be useful for evaluating whether batch membership remains predictable from the imaging features.

```r
auc_df <- HarmonizeR::batch_auc_cal_cv(
  data_list = demo$data,
  bat_list = demo$bat,
  k = 5,
  ntree = 100,
  seed = 123
)

head(auc_df)
```

A lower post-harmonization AUC suggests weaker out-of-sample batch detectability.

## Bayesian MCMC light-upload workflow

For computationally intensive Bayesian harmonization, `HarmonizeR` follows a script-generation workflow:

1. Configure the model in the Shiny app.
2. Download the generated standalone MCMC script.
3. Run the script outside the app, such as locally or on a computing cluster.
4. Upload the lightweight result object back into the app.
5. Review posterior diagnostics, summaries, and harmonized outputs.

If a lightweight upload object contains posterior draws but is missing a summary, the object can be repaired with:

```r
HarmonizeR::repair_light_upload(
  "results/stan_fits/stan_result_multivariate_LIGHT_UPLOAD.rds",
  outfile = "results/stan_fits/stan_result_multivariate_LIGHT_UPLOAD_FIXED.rds"
)
```

Posterior draws can also be summarized directly:

```r
summary_df <- HarmonizeR::summarize_draws_with_rhat_safe(posterior_draws)
```

## Exported functions

| Function | Purpose |
| --- | --- |
| `run_harmonizer()` | Launch the Shiny app. |
| `install_harmonizer_dependencies()` | Install or check app dependencies. |
| `simulate_demo_data()` | Generate cross-sectional multi-metric demo data. |
| `simulate_demo_longitudinal_data()` | Generate longitudinal multi-metric demo data. |
| `batch_auc_cal_cv()` | Compute cross-validated random forest batch-prediction AUC. |
| `build_mcmc_script()` | Generate a standalone Bayesian MCMC harmonization script. |
| `mcmc_light_export_helper_code()` | Return helper code used in exported MCMC scripts. |
| `safe_downsample_draws_by_chain()` | Downsample posterior draws while preserving chain balance. |
| `summarize_draws_with_rhat_safe()` | Summarize posterior draws with R-hat and effective sample size. |
| `repair_light_upload()` | Repair lightweight MCMC upload objects with missing summaries. |

## Data requirements

For most workflows, users should provide:

- one or more imaging feature matrices;
- one batch-label vector per metric, or one shared batch-label vector;
- optional biological or clinical covariates to preserve;
- optional subject and visit identifiers for longitudinal analyses.

For multi-metric harmonization, feature matrices should generally be aligned so that rows correspond to the same subjects or observations across metrics.

## Citation

If you use `HarmonizeR` in a publication, please cite the associated manuscript when available.

The app builds on ComBat-style neuroimaging harmonization methods, so users should also cite the relevant methodological papers for the specific harmonization options used, such as ComBat, neuroimaging ComBat, ComBat-GAM, Longitudinal ComBat, and CovBat.

## Development status

`HarmonizeR` is under active development. Interfaces, function names, and app modules may change before a stable release. Users are encouraged to verify outputs carefully and document all harmonization choices, diagnostic results, and model settings used in downstream analyses.

## License

This package is released under the MIT license. See `LICENSE` for details.
