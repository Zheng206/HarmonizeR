#' Downsample posterior draws evenly by chain
#'
#' This helper avoids the common problem where global random downsampling leaves
#' unequal numbers of draws per chain, which then breaks R-hat/ESS calculation.
#'
#' @param draws_df A posterior draws data frame, ideally with `.chain` and
#'   `.iteration` columns.
#' @param max_draws Maximum total number of rows to keep.
#' @param seed Random seed used for within-chain sampling.
#'
#' @return A data frame of posterior draws.
#' @export
safe_downsample_draws_by_chain <- function(draws_df, max_draws = 2000, seed = 1) {
  if (is.null(draws_df)) return(NULL)

  draws_df <- as.data.frame(draws_df, check.names = FALSE)

  if (!all(c(".chain", ".iteration") %in% names(draws_df))) {
    warning("No .chain/.iteration columns found; using global downsampling only.")
    if (nrow(draws_df) > max_draws) {
      set.seed(seed)
      keep_rows <- sort(sample(seq_len(nrow(draws_df)), max_draws))
      draws_df <- draws_df[keep_rows, , drop = FALSE]
    }
    return(draws_df)
  }

  draws_df$.chain <- as.integer(draws_df$.chain)
  chains <- sort(unique(draws_df$.chain))
  draws_per_chain <- max(1L, floor(max_draws / length(chains)))

  out <- do.call(
    rbind,
    lapply(chains, function(ch) {
      d <- draws_df[draws_df$.chain == ch, , drop = FALSE]
      d <- d[order(d$.iteration), , drop = FALSE]

      if (nrow(d) > draws_per_chain) {
        set.seed(seed + as.integer(ch))
        keep <- sort(sample(seq_len(nrow(d)), draws_per_chain))
        d <- d[keep, , drop = FALSE]
      }

      d
    })
  )

  rownames(out) <- NULL
  out
}

#' Summarize saved posterior draws with R-hat and ESS
#'
#' Builds a lightweight MCMC summary from selected posterior draws. This is useful
#' when `CmdStanMCMC$summary()` is too memory-intensive for a large model.
#'
#' @param draws_df A posterior draws data frame with parameter columns and,
#'   preferably, `.chain` and `.iteration` metadata columns.
#' @param chunk_size Number of parameters to process per chunk when computing
#'   R-hat and ESS.
#'
#' @return A data frame with `variable`, posterior summaries, `rhat`,
#'   `ess_bulk`, and `ess_tail`.
#' @export
summarize_draws_with_rhat_safe <- function(draws_df, chunk_size = 500) {
  if (is.null(draws_df)) return(NULL)

  draws_df <- as.data.frame(draws_df, check.names = FALSE)

  meta_cols <- intersect(c(".chain", ".iteration", ".draw"), names(draws_df))
  param_cols <- setdiff(names(draws_df), meta_cols)

  if (length(param_cols) == 0) return(NULL)

  has_chain_info <- all(c(".chain", ".iteration") %in% names(draws_df))

  if (has_chain_info) {
    draws_df$.chain <- as.integer(draws_df$.chain)
    chain_counts <- table(draws_df$.chain)
    min_n <- min(chain_counts)

    draws_balanced <- do.call(
      rbind,
      lapply(split(draws_df, draws_df$.chain), function(d) {
        d <- d[order(d$.iteration), , drop = FALSE]
        d[seq_len(min_n), , drop = FALSE]
      })
    )
    rownames(draws_balanced) <- NULL
  } else {
    draws_balanced <- draws_df
  }

  basic_summary <- lapply(param_cols, function(v) {
    x <- draws_balanced[[v]]
    x <- x[is.finite(x)]

    if (length(x) == 0) {
      return(data.frame(
        variable = v,
        mean = NA_real_,
        median = NA_real_,
        sd = NA_real_,
        q5 = NA_real_,
        q95 = NA_real_,
        q025 = NA_real_,
        q975 = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      variable = v,
      mean = mean(x, na.rm = TRUE),
      median = stats::median(x, na.rm = TRUE),
      sd = stats::sd(x, na.rm = TRUE),
      q5 = unname(stats::quantile(x, 0.05, na.rm = TRUE)),
      q95 = unname(stats::quantile(x, 0.95, na.rm = TRUE)),
      q025 = unname(stats::quantile(x, 0.025, na.rm = TRUE)),
      q975 = unname(stats::quantile(x, 0.975, na.rm = TRUE)),
      stringsAsFactors = FALSE
    )
  })

  smry_basic <- do.call(rbind, basic_summary)
  smry_basic$rhat <- NA_real_
  smry_basic$ess_bulk <- NA_real_
  smry_basic$ess_tail <- NA_real_

  if (!has_chain_info || !requireNamespace("posterior", quietly = TRUE)) {
    return(smry_basic)
  }

  chunks <- split(param_cols, ceiling(seq_along(param_cols) / chunk_size))
  diag_list <- vector("list", length(chunks))

  for (k in seq_along(chunks)) {
    vars_k <- chunks[[k]]
    d_k <- draws_balanced[, c(".chain", ".iteration", vars_k), drop = FALSE]
    arr_k <- posterior::as_draws_array(d_k)

    diag_k <- posterior::summarise_draws(
      arr_k,
      rhat = posterior::rhat,
      ess_bulk = posterior::ess_bulk,
      ess_tail = posterior::ess_tail
    )

    diag_k <- as.data.frame(diag_k, check.names = FALSE)

    if (!"variable" %in% names(diag_k)) {
      possible_var_cols <- c("variable", ".variable", "parameter", "term")
      found_var <- intersect(possible_var_cols, names(diag_k))
      if (length(found_var) > 0) {
        names(diag_k)[names(diag_k) == found_var[1]] <- "variable"
      } else {
        diag_k$variable <- vars_k
      }
    }

    needed_cols <- c("variable", "rhat", "ess_bulk", "ess_tail")
    missing_cols <- setdiff(needed_cols, names(diag_k))
    if (length(missing_cols) > 0) {
      for (cc in missing_cols) diag_k[[cc]] <- NA_real_
    }

    diag_list[[k]] <- diag_k[, needed_cols, drop = FALSE]
  }

  smry_diag <- do.call(rbind, diag_list)

  merge(
    smry_basic[, !names(smry_basic) %in% c("rhat", "ess_bulk", "ess_tail"), drop = FALSE],
    smry_diag[, c("variable", "rhat", "ess_bulk", "ess_tail"), drop = FALSE],
    by = "variable",
    all.x = TRUE,
    sort = FALSE
  )
}
