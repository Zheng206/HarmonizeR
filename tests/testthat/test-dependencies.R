test_that("install_harmonizer_dependencies dry run returns character vector", {
  deps <- install_harmonizer_dependencies(dry_run = TRUE, quiet = TRUE)
  
  expect_type(deps, "character")
  expect_false(anyNA(deps))
  expect_equal(length(unique(deps)), length(deps))
})

test_that("install_harmonizer_dependencies can exclude optional MCMC and MultiComBat", {
  deps <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_type(deps, "character")
  expect_false("cmdstanr" %in% deps)
  expect_false("MultiComBat" %in% deps)
})

test_that("install_harmonizer_dependencies optional flags preserve base dependency set", {
  deps_base <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  deps_all <- install_harmonizer_dependencies(
    include_mcmc = TRUE,
    include_multicombat = TRUE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_type(deps_all, "character")
  expect_true(all(deps_base %in% deps_all))
})

test_that("install_harmonizer_dependencies dry run is stable across repeated calls", {
  deps_1 <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  deps_2 <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_identical(deps_1, deps_2)
})

test_that("install_harmonizer_dependencies dry run returns only known app dependency names", {
  deps <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expected_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales",
    "RColorBrewer", "plotly", "htmlwidgets", "ggrepel", "openxlsx",
    "posterior", "patchwork", "zip", "caret", "randomForest", "pROC"
  )
  
  expect_true(all(deps %in% expected_pkgs))
})

test_that("install_harmonizer_dependencies dry run returns only known dependency names with optional packages", {
  deps <- install_harmonizer_dependencies(
    include_mcmc = TRUE,
    include_multicombat = TRUE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expected_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales",
    "RColorBrewer", "plotly", "htmlwidgets", "ggrepel", "openxlsx",
    "posterior", "patchwork", "zip", "caret", "randomForest", "pROC",
    "MultiComBat", "cmdstanr"
  )
  
  expect_true(all(deps %in% expected_pkgs))
})

test_that("install_harmonizer_dependencies gives a dry-run message when quiet is FALSE", {
  expect_message(
    install_harmonizer_dependencies(
      include_mcmc = FALSE,
      include_multicombat = FALSE,
      dry_run = TRUE,
      quiet = FALSE
    ),
    regexp = "All requested HarmonizeR dependencies|Packages to install/update"
  )
})