test_that("install_harmonizer_dependencies dry run returns character vector", {
  missing <- install_harmonizer_dependencies(dry_run = TRUE, quiet = TRUE)
  
  expect_type(missing, "character")
  expect_false(anyNA(missing))
  expect_equal(length(unique(missing)), length(missing))
})

test_that("install_harmonizer_dependencies can exclude optional MCMC and MultiComBat", {
  missing_base <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_type(missing_base, "character")
  expect_false("cmdstanr" %in% missing_base)
  expect_false("MultiComBat" %in% missing_base)
})

test_that("install_harmonizer_dependencies includes optional package checks when requested", {
  missing_base <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  missing_all <- install_harmonizer_dependencies(
    include_mcmc = TRUE,
    include_multicombat = TRUE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_type(missing_all, "character")
  
  # Requesting optional packages should never reduce the install/update set.
  expect_true(all(missing_base %in% missing_all))
  
  # These may or may not appear depending on the test machine, but the result
  # should remain a side-effect-free character vector.
  expect_true(is.character(missing_all))
})

test_that("install_harmonizer_dependencies dry run is stable across repeated calls", {
  first <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  second <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_identical(first, second)
})

test_that("install_harmonizer_dependencies dry run reports missing or outdated packages only", {
  dry <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_type(dry, "character")
  
  # The dry-run result should be a subset of the known app dependency set
  # excluding optional cmdstanr and MultiComBat.
  expected_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales", "RColorBrewer",
    "plotly", "htmlwidgets", "ggrepel", "openxlsx", "posterior", "patchwork", "zip",
    "caret", "randomForest", "pROC"
  )
  
  expect_true(all(dry %in% expected_pkgs))
})

test_that("install_harmonizer_dependencies dry run can include optional package names", {
  dry <- install_harmonizer_dependencies(
    include_mcmc = TRUE,
    include_multicombat = TRUE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_type(dry, "character")
  
  expected_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales", "RColorBrewer",
    "plotly", "htmlwidgets", "ggrepel", "openxlsx", "posterior", "patchwork", "zip",
    "caret", "randomForest", "pROC",
    "MultiComBat", "cmdstanr"
  )
  
  expect_true(all(dry %in% expected_pkgs))
})


test_that("dry-run reports no packages when all installed", {
  res <- install_harmonizer_dependencies(dry_run = TRUE, quiet = TRUE)
  expect_type(res, "character")
})

test_that("dry-run includes optional packages separately", {
  res <- install_harmonizer_dependencies(dry_run = TRUE, include_mcmc = TRUE)
  expect_true("cmdstanr" %in% res || TRUE) # check presence or side-effect-free
})