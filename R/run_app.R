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
    "mgcv", "lme4", "MASS", "MCMCpack", "tibble", "scales",
    "plotly", "ggrepel", "openxlsx", "posterior", "MultiComBat",
    "caret", "randomForest", "pROC"
  )

  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

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
