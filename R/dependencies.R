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
#'   dependency check and installation list. Set to `FALSE` if you have already
#'   installed `MultiComBat` from a local or development source.
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
  app_pkgs <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT",
    "ggplot2", "dplyr", "tidyr", "magrittr", "purrr", "broom",
    "car", "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales", "RColorBrewer",
    "plotly", "htmlwidgets", "ggrepel", "openxlsx", "posterior", "patchwork", "zip",
    "caret", "randomForest", "pROC"
  )

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

  missing <- app_pkgs[!vapply(app_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  installed_min <- intersect(names(min_versions), app_pkgs)
  outdated <- installed_min[vapply(installed_min, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
    utils::compareVersion(as.character(utils::packageVersion(pkg)), min_versions[[pkg]]) < 0
  }, logical(1))]
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

  stan_pkgs <- intersect(to_install, "cmdstanr")
  cran_pkgs <- setdiff(to_install, "cmdstanr")

  if (length(cran_pkgs) > 0) {
    if (!quiet) message("Installing packages: ", paste(cran_pkgs, collapse = ", "))
    utils::install.packages(cran_pkgs)
  }

  if (length(stan_pkgs) > 0) {
    if (!quiet) message("Installing cmdstanr from the Stan R package repository.")
    utils::install.packages(
      "cmdstanr",
      repos = c("https://mc-stan.org/r-packages/", getOption("repos"))
    )
    if (!quiet) {
      message(
        "cmdstanr is installed. If CmdStan is not installed, run: ",
        "cmdstanr::install_cmdstan()"
      )
    }
  }

  invisible(to_install)
}
