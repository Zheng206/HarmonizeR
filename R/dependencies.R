#' Install HarmonizeR app dependencies
#'
#' Installs packages needed to run the full HarmonizeR Shiny application. The
#' core package keeps many user-interface and modeling packages in `Suggests`
#' so that HarmonizeR remains lightweight and easier to check. This helper gives
#' users a single command for installing the packages required by the app.
#'
#' @param include_mcmc Logical. If `TRUE`, also install `cmdstanr` from the Stan
#'   R package repository. This does not install CmdStan itself; after installing
#'   `cmdstanr`, run `cmdstanr::install_cmdstan()` if CmdStan is not already
#'   installed.
#' @param include_multicombat Logical. If `TRUE`, include `MultiComBat` in the
#'   dependency check and install it from GitHub using
#'   `remotes::install_github("Zheng206/MultiComBat")` if it is missing.
#'   Set to `FALSE` if you have already installed `MultiComBat` from a local or
#'   development source.
#' @param dry_run Logical. If `TRUE`, do not install anything. Instead, return a
#'   character vector of missing packages that would be installed.
#' @param quiet Logical. If `TRUE`, suppress informational messages.
#'
#' @return Invisibly returns a character vector of packages that were missing at
#'   the time the function was called. With `dry_run = TRUE`, the vector is
#'   returned visibly.
#' @export
#'
#' @examples
#' \dontrun{
#' install_harmonizer_dependencies()
#' install_harmonizer_dependencies(include_mcmc = TRUE)
#' }
#'
#' # Check what is missing without installing anything
#' install_harmonizer_dependencies(dry_run = TRUE)
install_harmonizer_dependencies <- function(include_mcmc = FALSE,
                                            include_multicombat = TRUE,
                                            dry_run = FALSE,
                                            quiet = FALSE) {
  cran_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales",
    "plotly", "ggrepel", "openxlsx", "posterior", "patchwork", "zip",
    "caret", "randomForest", "pROC"
  )
  
  if (isTRUE(include_mcmc)) {
    cran_pkgs <- unique(c(cran_pkgs, "cmdstanr"))
  }
  
  github_pkgs <- character(0)
  
  if (isTRUE(include_multicombat)) {
    github_pkgs <- c(github_pkgs, "MultiComBat")
  }
  
  missing_cran <- cran_pkgs[
    !vapply(cran_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  missing_github <- github_pkgs[
    !vapply(github_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  missing <- c(missing_cran, missing_github)
  
  if (isTRUE(dry_run)) {
    if (!quiet) {
      if (length(missing) == 0) {
        message("All requested HarmonizeR dependencies are already installed.")
      } else {
        message("Missing packages: ", paste(missing, collapse = ", "))
      }
    }
    
    return(missing)
  }
  
  if (length(missing) == 0) {
    if (!quiet) {
      message("All requested HarmonizeR dependencies are already installed.")
    }
    
    return(invisible(missing))
  }
  
  stan_pkgs <- intersect(missing_cran, "cmdstanr")
  regular_cran_pkgs <- setdiff(missing_cran, "cmdstanr")
  
  if (length(regular_cran_pkgs) > 0) {
    if (!quiet) {
      message("Installing CRAN packages: ", paste(regular_cran_pkgs, collapse = ", "))
    }
    
    utils::install.packages(regular_cran_pkgs)
  }
  
  if (length(stan_pkgs) > 0) {
    if (!quiet) {
      message("Installing cmdstanr from the Stan R package repository.")
    }
    
    utils::install.packages(
      "cmdstanr",
      repos = c(
        "https://mc-stan.org/r-packages/",
        getOption("repos")
      )
    )
    
    if (!quiet) {
      message(
        "cmdstanr is installed. If CmdStan is not installed, run: ",
        "cmdstanr::install_cmdstan()"
      )
    }
  }
  
  if ("MultiComBat" %in% missing_github) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      if (!quiet) {
        message("Installing remotes because it is needed to install MultiComBat from GitHub.")
      }
      
      utils::install.packages("remotes")
    }
    
    if (!quiet) {
      message("Installing MultiComBat from GitHub: Zheng206/MultiComBat")
    }
    
    remotes::install_github("Zheng206/MultiComBat")
  }
  
  invisible(missing)
}
