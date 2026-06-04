#' Launch the HarmonizeR Shiny App
#'
#' Starts the interactive HarmonizeR Shiny application bundled with this package.
#'
#' @param ... Additional arguments passed to [shiny::runApp()], such as
#'   `port`, `host`, or `launch.browser`.
#'
#' @return Invisibly returns the result of [shiny::runApp()].
#' @export
#'
#' @examples
#' \dontrun{
#' run_harmonizer()
#' run_harmonizer(port = 4004)
#' }
run_harmonizer <- function(...) {
  app_dir <- system.file("app", package = "HarmonizeR")

  if (!nzchar(app_dir)) {
    stop(
      "Could not find the bundled Shiny app. Try reinstalling HarmonizeR.",
      call. = FALSE
    )
  }

  required <- c(
    "shiny", "shinydashboard", "shinyWidgets", "DT", "ggplot2",
    "dplyr", "tidyr", "magrittr", "purrr", "broom", "car",
    "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales", "RColorBrewer",
    "plotly", "htmlwidgets", "ggrepel", "openxlsx", "posterior", "MultiComBat",
    "caret", "randomForest", "pROC"
  )

  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]



  min_versions <- c(
    shiny = "1.8.0",
    ggplot2 = "3.5.0",
    scales = "1.3.0",
    plotly = "4.10.0",
    htmlwidgets = "1.6.0",
    RColorBrewer = "1.1.3"
  )
  outdated <- names(min_versions)[vapply(names(min_versions), function(pkg) {
    requireNamespace(pkg, quietly = TRUE) &&
      utils::compareVersion(as.character(utils::packageVersion(pkg)), min_versions[[pkg]]) < 0
  }, logical(1))]
  if (length(outdated) > 0) {
    stop(
      paste0(
        "The HarmonizeR app requires newer visualization dependencies. Please update: ",
        paste(sprintf("%s (>= %s)", outdated, min_versions[outdated]), collapse = ", "),
        ".
Run HarmonizeR::install_harmonizer_dependencies(), restart R, and try again."
      ),
      call. = FALSE
    )
  }
  if (length(missing) > 0) {
    stop(
      paste0(
        "The HarmonizeR app requires these packages: ",
        paste(missing, collapse = ", "),
        ".
Install them first, or run HarmonizeR::install_harmonizer_dependencies(), then run run_harmonizer() again."
      ),
      call. = FALSE
    )
  }

  shiny::runApp(appDir = app_dir, ...)
}
