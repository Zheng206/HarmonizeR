#' Repair a lightweight HarmonizeR MCMC upload object
#'
#' If a `*_LIGHT_UPLOAD.rds` file has `posterior_draws` but a missing
#' `mcmc_summary`, this function reconstructs the summary using saved draws.
#'
#' @param x Either a path to an RDS file or a loaded light-upload object.
#' @param outfile Optional path to save the repaired object.
#' @param chunk_size Number of parameters per chunk for R-hat/ESS computation.
#'
#' @return The repaired light-upload object, invisibly if `outfile` is supplied.
#' @export
repair_light_upload <- function(x, outfile = NULL, chunk_size = 500) {
  obj <- if (is.character(x) && length(x) == 1L) readRDS(x) else x

  if (is.null(obj$posterior_draws)) {
    stop("The object has no posterior_draws to summarize.", call. = FALSE)
  }

  obj$mcmc_summary <- summarize_draws_with_rhat_safe(
    obj$posterior_draws,
    chunk_size = chunk_size
  )

  obj$note <- paste(
    obj$note %||% "",
    "mcmc_summary was repaired from posterior_draws using HarmonizeR::summarize_draws_with_rhat_safe()."
  )

  if (!is.null(outfile)) {
    saveRDS(obj, outfile, compress = "xz")
    return(invisible(obj))
  }

  obj
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
