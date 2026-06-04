#' Install HarmonizeR app dependencies safely
#'
#' Installs packages needed to run the full HarmonizeR Shiny application.
#'
#' @param include_mcmc Logical. If TRUE, install cmdstanr.
#' @param include_multicombat Logical. If TRUE, install MultiComBat from GitHub.
#' @param dry_run Logical. If TRUE, do not install; just return missing packages.
#' @param quiet Logical. If TRUE, suppress messages.
#'
#' @return Invisibly returns a character vector of packages that were missing or outdated.
#' @export
install_harmonizer_dependencies <- function(include_mcmc = FALSE,
                                            include_multicombat = TRUE,
                                            dry_run = FALSE,
                                            quiet = FALSE) {
  
  # Core app packages
  app_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble",
    "scales", "RColorBrewer", "plotly", "htmlwidgets", "ggrepel",
    "openxlsx", "posterior", "patchwork", "zip",
    "caret", "randomForest", "pROC"
  )
  
  # Minimum version requirements
  min_versions <- c(
    shiny = "1.8.0",
    ggplot2 = "3.5.0",
    scales = "1.3.0",
    plotly = "4.10.0",
    htmlwidgets = "1.6.0",
    RColorBrewer = "1.1.3"
  )
  
  if (isTRUE(include_multicombat)) {
    app_pkgs <- unique(c(app_pkgs, "MultiComBat"))
  }
  if (isTRUE(include_mcmc)) {
    app_pkgs <- unique(c(app_pkgs, "cmdstanr"))
  }
  
  # Identify missing packages
  missing <- app_pkgs[!vapply(app_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  
  # Identify outdated packages safely
  installed_min <- intersect(names(min_versions), app_pkgs)
  outdated <- installed_min[vapply(installed_min, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(TRUE) # treat as outdated
    ver <- tryCatch(as.character(utils::packageVersion(pkg)),
                    error = function(e) NA_character_)
    if (is.na(ver)) return(TRUE) # treat as outdated if version can't be read
    utils::compareVersion(ver, min_versions[[pkg]]) < 0
  }, logical(1))]
  
  # Packages to install
  to_install <- unique(c(missing, outdated))
  
  if (isTRUE(dry_run)) {
    if (!quiet) {
      if (length(to_install) == 0) {
        message("All requested HarmonizeR dependencies are already installed with compatible versions.")
      } else {
        message("Packages to install/update: ", paste(to_install, collapse = ", "))
      }
    }
    return(to_install)
  }
  
  if (length(to_install) == 0) {
    if (!quiet) message("All requested HarmonizeR dependencies are already installed with compatible versions.")
    return(invisible(to_install))
  }
  
  # Separate CRAN, MultiComBat, cmdstanr
  stan_pkgs <- intersect(to_install, "cmdstanr")
  multicombat_pkgs <- intersect(to_install, "MultiComBat")
  cran_pkgs <- setdiff(to_install, c("cmdstanr", "MultiComBat"))
  
  # Install CRAN packages
  if (length(cran_pkgs) > 0) {
    if (!quiet) message("Installing CRAN packages: ", paste(cran_pkgs, collapse = ", "))
    utils::install.packages(cran_pkgs)
  }
  
  # Install MultiComBat from GitHub
  if (length(multicombat_pkgs) > 0) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      utils::install.packages("remotes")
    }
    if (!quiet) message("Installing MultiComBat from GitHub...")
    remotes::install_github("Zheng206/MultiComBat")
  }
  
  # Install cmdstanr
  if (length(stan_pkgs) > 0) {
    if (!quiet) message("Installing cmdstanr from Stan R package repository...")
    utils::install.packages(
      "cmdstanr",
      repos = c("https://mc-stan.org/r-packages/", getOption("repos"))
    )
    if (!quiet) message(
      "cmdstanr installed. If CmdStan itself is not installed, run: cmdstanr::install_cmdstan()"
    )
  }
  
  invisible(to_install)
}
