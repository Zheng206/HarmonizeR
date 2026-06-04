expected_harmonizer_app_pkgs <- function(include_mcmc = FALSE,
                                         include_multicombat = FALSE) {
  pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales",
    "RColorBrewer", "plotly", "htmlwidgets", "ggrepel", "openxlsx",
    "posterior", "patchwork", "zip", "caret", "randomForest", "pROC"
  )
  
  if (isTRUE(include_multicombat)) {
    pkgs <- unique(c(pkgs, "MultiComBat"))
  }
  
  if (isTRUE(include_mcmc)) {
    pkgs <- unique(c(pkgs, "cmdstanr"))
  }
  
  pkgs
}

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
  
  expected_pkgs <- expected_harmonizer_app_pkgs(
    include_mcmc = FALSE,
    include_multicombat = FALSE
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
  
  expected_pkgs <- expected_harmonizer_app_pkgs(
    include_mcmc = TRUE,
    include_multicombat = TRUE
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

test_that("install_harmonizer_dependencies dry run does not fail during version checks", {
  expect_no_error(
    install_harmonizer_dependencies(
      include_mcmc = FALSE,
      include_multicombat = FALSE,
      dry_run = TRUE,
      quiet = TRUE
    )
  )
})

test_that("install_harmonizer_dependencies installs missing CRAN packages without optional packages", {
  installed_pkgs <- character()
  
  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      FALSE
    },
    .package = "base"
  )
  
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      package_version("999.0.0")
    },
    .package = "utils"
  )
  
  local_mocked_bindings(
    install.packages = function(pkgs, ...) {
      installed_pkgs <<- c(installed_pkgs, pkgs)
      invisible(TRUE)
    },
    .package = "utils"
  )
  
  expect_no_error(
    result <- install_harmonizer_dependencies(
      include_mcmc = FALSE,
      include_multicombat = FALSE,
      dry_run = FALSE,
      quiet = TRUE
    )
  )
  
  expected_cran_pkgs <- expected_harmonizer_app_pkgs(
    include_mcmc = FALSE,
    include_multicombat = FALSE
  )
  
  expect_true(all(expected_cran_pkgs %in% installed_pkgs))
  expect_false("MultiComBat" %in% installed_pkgs)
  expect_false("cmdstanr" %in% installed_pkgs)
  expect_type(result, "character")
})

test_that("install_harmonizer_dependencies separates CRAN, MultiComBat, and cmdstanr install paths", {
  skip_if_not_installed("remotes")
  
  installed_cran <- character()
  installed_github <- character()
  stan_repos_used <- NULL
  
  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      package == "remotes"
    },
    .package = "base"
  )
  
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      package_version("999.0.0")
    },
    .package = "utils"
  )
  
  local_mocked_bindings(
    install.packages = function(pkgs, repos = getOption("repos"), ...) {
      if ("cmdstanr" %in% pkgs) {
        stan_repos_used <<- repos
      } else {
        installed_cran <<- c(installed_cran, pkgs)
      }
      invisible(TRUE)
    },
    .package = "utils"
  )
  
  local_mocked_bindings(
    install_github = function(repo, ...) {
      installed_github <<- c(installed_github, repo)
      invisible(TRUE)
    },
    .package = "remotes"
  )
  
  expect_no_error(
    result <- install_harmonizer_dependencies(
      include_mcmc = TRUE,
      include_multicombat = TRUE,
      dry_run = FALSE,
      quiet = TRUE
    )
  )
  
  expect_true("shiny" %in% installed_cran)
  expect_false("cmdstanr" %in% installed_cran)
  expect_false("MultiComBat" %in% installed_cran)
  
  expect_equal(installed_github, "Zheng206/MultiComBat")
  expect_true("https://mc-stan.org/r-packages/" %in% stan_repos_used)
  
  expect_true("cmdstanr" %in% result)
  expect_true("MultiComBat" %in% result)
})

test_that("install_harmonizer_dependencies installs remotes when MultiComBat is requested and remotes is missing", {
  skip_if_not_installed("remotes")
  
  installed_pkgs <- character()
  installed_github <- character()
  
  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      FALSE
    },
    .package = "base"
  )
  
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      package_version("999.0.0")
    },
    .package = "utils"
  )
  
  local_mocked_bindings(
    install.packages = function(pkgs, ...) {
      installed_pkgs <<- c(installed_pkgs, pkgs)
      invisible(TRUE)
    },
    .package = "utils"
  )
  
  local_mocked_bindings(
    install_github = function(repo, ...) {
      installed_github <<- c(installed_github, repo)
      invisible(TRUE)
    },
    .package = "remotes"
  )
  
  expect_no_error(
    install_harmonizer_dependencies(
      include_mcmc = FALSE,
      include_multicombat = TRUE,
      dry_run = FALSE,
      quiet = TRUE
    )
  )
  
  expect_true("remotes" %in% installed_pkgs)
  expect_equal(installed_github, "Zheng206/MultiComBat")
})

test_that("install_harmonizer_dependencies detects outdated minimum-version packages", {
  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      TRUE
    },
    .package = "base"
  )
  
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "shiny")) {
        package_version("1.7.5")
      } else {
        package_version("999.0.0")
      }
    },
    .package = "utils"
  )
  
  deps <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_true("shiny" %in% deps)
})

test_that("install_harmonizer_dependencies treats unreadable package versions as outdated", {
  local_mocked_bindings(
    requireNamespace = function(package, quietly = FALSE, ...) {
      TRUE
    },
    .package = "base"
  )
  
  local_mocked_bindings(
    packageVersion = function(pkg, ...) {
      if (identical(pkg, "ggplot2")) {
        stop("mock version read failure")
      }
      package_version("999.0.0")
    },
    .package = "utils"
  )
  
  deps <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )
  
  expect_true("ggplot2" %in% deps)
})