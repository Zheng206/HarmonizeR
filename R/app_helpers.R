#' Internal helper functions for HarmonizeR Shiny app
#' @keywords internal

utils::globalVariables(c(".data", "bat", "measurement"))

`%||%` <- function(a, b) if (!is.null(a)) a else b

safe_positive_int <- function(x, default = 1L, min_val = 1L, max_val = Inf) {
  out <- suppressWarnings(as.integer(x))
  out <- out[1]
  if (length(out) == 0 || is.na(out) || out < min_val) out <- default
  if (is.finite(max_val)) out <- min(out, max_val)
  as.integer(out)
}

valid_positive_int <- function(x, min_val = 1L, max_val = Inf) {
  out <- suppressWarnings(as.integer(x))
  out <- out[1]
  length(out) == 1L && !is.na(out) && out >= min_val &&
    (!is.finite(max_val) || out <= max_val)
}

safe_m_value <- function(m) {
  if (!valid_positive_int(m, min_val = 1L)) return(NULL)
  as.integer(m)
}

clean_batch_for_plot <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL", "null", "NaN", "nan")] <- NA_character_
  factor(x, levels = unique(x[!is.na(x)]))
}

safe_discrete_palette <- function(levels_or_n, palette = "Dark 3") {
  if (length(levels_or_n) == 1L && is.numeric(levels_or_n)) {
    n <- as.integer(levels_or_n)
    lev <- NULL
  } else {
    lev <- as.character(levels_or_n)
    n <- length(lev)
  }

  n <- max(1L, n)

  cols <- tryCatch(
    grDevices::hcl.colors(n, palette = palette),
    error = function(e) grDevices::rainbow(n)
  )

  cols[!nzchar(cols) | is.na(cols)] <- "#666666"

  ok <- tryCatch({
    grDevices::col2rgb(cols)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) cols <- grDevices::rainbow(n)

  if (!is.null(lev)) stats::setNames(cols, lev) else cols
}

pca_equal_axes_default <- function(pca_prep_result, type = "within") {
  !(("F_list" %in% names(pca_prep_result)) && identical(type, "within"))
}

pca_ncol_facets <- function(m) {
  m <- suppressWarnings(as.integer(m))[1]
  if (length(m) == 0L || is.na(m) || m <= 1L) return(1L)
  if (m <= 2L) return(m)
  if (m <= 4L) return(2L)
  3L
}

pca_legend_rows <- function(n_batch) {
  n_batch <- suppressWarnings(as.integer(n_batch))[1]
  if (length(n_batch) == 0L || is.na(n_batch) || n_batch <= 8L) return(1L)
  if (n_batch <= 16L) return(2L)
  3L
}

pca_plot_height_px <- function(pca_prep_result, type = "within", compare_mode = FALSE) {
  base_height <- if (compare_mode) 520L else 560L
  if (is.null(pca_prep_result)) return(base_height)

  if (("F_list" %in% names(pca_prep_result)) && identical(type, "within")) {
    m <- length(pca_prep_result$F_list)
    ncol_facets <- pca_ncol_facets(m)
    nrow_facets <- max(1L, ceiling(m / ncol_facets))
    row_height  <- if (compare_mode) 260L else 240L
    legend_pad  <- 80L
    return(max(base_height, as.integer(nrow_facets * row_height + legend_pad)))
  }

  base_height
}

can_draw_ellipse_by_group <- function(df, group_var = "bat", facet_var = NULL) {
  if (is.null(df) || !nrow(df) || !group_var %in% names(df)) return(FALSE)

  d <- df[!is.na(df[[group_var]]), , drop = FALSE]
  if (!nrow(d)) return(FALSE)

  enough_for_one <- function(x) {
    tab <- table(x)
    length(tab) > 0L && all(tab >= 3L)
  }

  if (!is.null(facet_var) && facet_var %in% names(d)) {
    splits <- split(seq_len(nrow(d)), d[[facet_var]], drop = TRUE)
    if (!length(splits)) return(FALSE)
    all(vapply(splits, function(ix) enough_for_one(d[[group_var]][ix]), logical(1)))
  } else {
    enough_for_one(d[[group_var]])
  }
}

pca_plot_robust <- function(pca_prep_result,
                            type = "within",
                            pc_1 = 1,
                            pc_2 = 2,
                            ellipse = TRUE,
                            equal_axes = NULL) {
  pc_pair <- paste0("PC", c(pc_1, pc_2))
  element_names <- names(pca_prep_result)

  if (is.null(equal_axes)) {
    equal_axes <- pca_equal_axes_default(pca_prep_result, type)
  }

  base_pca_theme <- ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2),
      strip.text = ggplot2::element_text(face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey95", color = NA),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key.size = grid::unit(4, "mm"),
      legend.title = ggplot2::element_text(face = "bold", size = 9),
      legend.text = ggplot2::element_text(size = 8),
      plot.title.position = "plot",
      panel.spacing = grid::unit(6, "pt"),
      axis.title = ggplot2::element_text(face = "bold")
    )

  prep_pca_df <- function(df) {
    shiny::validate(
      shiny::need(all(pc_pair %in% colnames(df)), "Selected PCs are not available."),
      shiny::need(nrow(df) > 0, "No PCA scores are available for plotting.")
    )

    for (nm in pc_pair) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }

    df$bat <- clean_batch_for_plot(df$bat)
    df <- df[
      is.finite(df[[pc_pair[1]]]) &
        is.finite(df[[pc_pair[2]]]) &
        !is.na(df$bat),
      ,
      drop = FALSE
    ]

    shiny::validate(shiny::need(nrow(df) > 0, "No valid PCA points are available after filtering."))

    df$bat <- droplevels(df$bat)

    batch_cols <- safe_discrete_palette(levels(df$bat))
    legend_rows <- pca_legend_rows(nlevels(df$bat))

    list(
      df = df,
      scale = ggplot2::scale_color_manual(values = batch_cols, name = "Batch / Site"),
      guides = ggplot2::guides(color = ggplot2::guide_legend(
        nrow = legend_rows,
        byrow = TRUE,
        override.aes = list(size = 2.5, alpha = 1)
      ))
    )
  }

  add_optional_ellipse <- function(plt, df, ellipse, facet_var = NULL) {
    if (isTRUE(ellipse) && can_draw_ellipse_by_group(df, group_var = "bat", facet_var = facet_var)) {
      plt + ggplot2::stat_ellipse(level = 0.68, linewidth = 0.6, show.legend = FALSE)
    } else {
      plt
    }
  }

  if ("F_list" %in% element_names) {
    if (identical(type, "within")) {
      m <- length(pca_prep_result$F_list)
      ncol_facets <- pca_ncol_facets(m)

      F_df_con <- lapply(seq_len(m), function(i) {
        data.frame(pca_prep_result$F_list[[i]], check.names = FALSE) |>
          dplyr::mutate(
            measurement = factor(paste0("M_", i), levels = paste0("M_", seq_len(m))),
            bat = pca_prep_result$bat[[i]]
          )
      }) |>
        dplyr::bind_rows() |>
        dplyr::select(dplyr::all_of(pc_pair), measurement, bat)

      tmp <- prep_pca_df(F_df_con)
      F_df_con <- tmp$df

      plt <- ggplot2::ggplot(
        F_df_con,
        ggplot2::aes(x = .data[[pc_pair[1]]], y = .data[[pc_pair[2]]], color = bat)
      ) +
        ggplot2::geom_point(alpha = 0.7, size = 1.6, stroke = 0) +
        ggplot2::facet_wrap(~ measurement, ncol = ncol_facets, scales = "fixed") +
        ggplot2::labs(x = pc_pair[1], y = pc_pair[2], color = "Batch / Site") +
        tmp$scale + tmp$guides + base_pca_theme

      if (isTRUE(equal_axes)) plt <- plt + ggplot2::coord_equal()
      add_optional_ellipse(plt, F_df_con, ellipse, facet_var = "measurement")
    } else {
      G_df <- data.frame(pca_prep_result$G, check.names = FALSE) |>
        dplyr::mutate(bat = pca_prep_result$bat[[1]])

      tmp <- prep_pca_df(G_df)
      G_df <- tmp$df

      plt <- ggplot2::ggplot(
        G_df,
        ggplot2::aes(x = .data[[pc_pair[1]]], y = .data[[pc_pair[2]]], color = bat)
      ) +
        ggplot2::geom_point(alpha = 0.7, size = 1.6, stroke = 0) +
        ggplot2::labs(x = pc_pair[1], y = pc_pair[2], color = "Batch / Site") +
        tmp$scale + tmp$guides + base_pca_theme

      if (isTRUE(equal_axes)) plt <- plt + ggplot2::coord_equal()
      add_optional_ellipse(plt, G_df, ellipse)
    }
  } else {
    F_df <- data.frame(pca_prep_result$F_t, check.names = FALSE) |>
      dplyr::mutate(bat = pca_prep_result$bat)

    tmp <- prep_pca_df(F_df)
    F_df <- tmp$df

    plt <- ggplot2::ggplot(
      F_df,
      ggplot2::aes(x = .data[[pc_pair[1]]], y = .data[[pc_pair[2]]], color = bat)
    ) +
      ggplot2::geom_point(alpha = 0.7, size = 1.6, stroke = 0) +
      ggplot2::labs(x = pc_pair[1], y = pc_pair[2], color = "Batch / Site") +
      tmp$scale + tmp$guides + base_pca_theme

    if (isTRUE(equal_axes)) plt <- plt + ggplot2::coord_equal()
    add_optional_ellipse(plt, F_df, ellipse)
  }
}

to_numeric_if_possible <- function(x, min_prop = 0.80) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x) || is.integer(x)) return(as.numeric(x))
  if (!is.character(x)) return(x)

  x_trim <- trimws(x)
  x_clean <- gsub(",", "", x_trim, fixed = TRUE)
  non_empty <- !is.na(x_clean) & nzchar(x_clean)

  if (!any(non_empty)) return(x)

  y <- suppressWarnings(as.numeric(x_clean))
  prop_numeric <- mean(!is.na(y[non_empty]))

  if (isTRUE(prop_numeric >= min_prop)) y else x
}

numeric_like_prop <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x) || is.integer(x)) return(1)
  if (!is.character(x)) return(0)

  x_clean <- gsub(",", "", trimws(x), fixed = TRUE)
  non_empty <- !is.na(x_clean) & nzchar(x_clean)

  if (!any(non_empty)) return(0)

  y <- suppressWarnings(as.numeric(x_clean))
  mean(!is.na(y[non_empty]))
}

clean_uploaded_covar <- function(df, subject_id_cols = character()) {
  if (is.null(df)) return(NULL)

  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))

  subject_id_cols <- intersect(subject_id_cols, names(df))

  for (nm in setdiff(names(df), subject_id_cols)) {
    df[[nm]] <- to_numeric_if_possible(df[[nm]])
  }

  for (nm in subject_id_cols) {
    df[[nm]] <- as.character(df[[nm]])
  }

  df
}

get_param_family <- function(x) {
  sub("\\[.*$", "", as.character(x))
}

extract_stan_indices <- function(x) {
  inside <- sub("^.*\\[", "", as.character(x))
  inside <- sub("\\]$", "", inside)

  if (identical(inside, as.character(x)) || !nzchar(inside)) {
    return(integer(0))
  }

  suppressWarnings(as.integer(strsplit(inside, ",")[[1]]))
}

get_first_index <- function(vars) {
  idx <- lapply(vars, extract_stan_indices)
  vapply(idx, function(z) {
    if (length(z) >= 1 && !is.na(z[1])) z[1] else NA_integer_
  }, integer(1))
}

get_second_index <- function(vars) {
  idx <- lapply(vars, extract_stan_indices)
  vapply(idx, function(z) {
    if (length(z) >= 2 && !is.na(z[2])) z[2] else NA_integer_
  }, integer(1))
}

standardize_mcmc_summary <- function(smry) {
  if (is.null(smry)) return(NULL)

  smry <- as.data.frame(smry, check.names = FALSE)

  if (!"variable" %in% colnames(smry)) {
    rn <- rownames(smry)
    if (!is.null(rn) && length(rn) == nrow(smry) && any(nzchar(rn))) {
      smry$variable <- rn
      rownames(smry) <- NULL
    }
  }

  if (!"variable" %in% colnames(smry)) return(NULL)

  smry
}

detect_mcmc_families <- function(smry) {
  smry <- standardize_mcmc_summary(smry)
  if (is.null(smry) || !"variable" %in% colnames(smry)) return(character(0))
  sort(unique(get_param_family(smry$variable)))
}

match_mcmc_param <- function(smry, semantic = c("gamma", "delta")) {
  semantic <- match.arg(semantic)
  families <- detect_mcmc_families(smry)

  if (length(families) == 0) return(NA_character_)

  if (semantic == "gamma") {
    candidate_patterns <- c("^gamma", "^gamma_star", "^gamma_hat", "^mu_ig", "^mu", "^g_")
  } else {
    candidate_patterns <- c("^delta", "^delta_star", "^delta_hat", "^sigma", "^Sigma_ig", "^Sigma", "^d_")
  }

  for (pat in candidate_patterns) {
    hit <- families[grepl(pat, families, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }

  fallback <- families[!families %in% c("lp__")]
  if (length(fallback) > 0) fallback[1] else families[1]
}

filter_mcmc_summary <- function(smry, family, batch = NULL, modality = NULL) {
  smry <- standardize_mcmc_summary(smry)

  if (is.null(smry) || is.na(family) || !nzchar(family)) {
    return(data.frame())
  }

  df <- smry[grepl(paste0("^", family, "(\\[|$)"), smry$variable), , drop = FALSE]

  if (nrow(df) == 0) return(df)

  df$.batch_index <- get_first_index(df$variable)
  df$.mod_index <- get_second_index(df$variable)

  if (!is.null(batch) && !identical(as.character(batch), "All")) {
    b_int <- suppressWarnings(as.integer(batch))
    if (!is.na(b_int)) df <- df[df$.batch_index == b_int, , drop = FALSE]
  }

  if (!is.null(modality) && !identical(as.character(modality), "All")) {
    m_int <- suppressWarnings(as.integer(modality))
    if (!is.na(m_int)) df <- df[df$.mod_index == m_int, , drop = FALSE]
  }

  df
}

find_nested_component <- function(x, candidate_names, max_depth = 6) {
  if (max_depth < 0 || is.null(x)) return(NULL)

  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm)) {
      hit <- nm[tolower(nm) %in% tolower(candidate_names)]
      if (length(hit) > 0) return(x[[hit[1]]])
    }

    for (z in x) {
      out <- find_nested_component(z, candidate_names, max_depth = max_depth - 1)
      if (!is.null(out)) return(out)
    }
  }

  NULL
}

get_shrink_pair <- function(obj, parm = c("gamma", "delta")) {
  parm <- match.arg(parm)

  if (parm == "gamma") {
    empirical_names <- c("gamma_hat", "gamma.hat", "g_hat", "g.hat", "gamma_empirical")
    shrunken_names <- c("gamma_star", "gamma.star", "g_star", "g.star", "gamma_posterior", "gamma_shrunken")
  } else {
    empirical_names <- c("delta_hat", "delta.hat", "d_hat", "d.hat", "delta_empirical")
    shrunken_names <- c("delta_star", "delta.star", "d_star", "d.star", "delta_posterior", "delta_shrunken")
  }

  list(
    empirical = find_nested_component(obj, empirical_names),
    shrunken = find_nested_component(obj, shrunken_names),
    empirical_names = empirical_names,
    shrunken_names = shrunken_names
  )
}

vectorize_shrink_object <- function(x, batch = NULL) {
  if (is.null(x)) return(numeric(0))
  if (is.data.frame(x)) x <- as.matrix(x)

  if (is.matrix(x)) {
    if (!is.null(batch) && !identical(as.character(batch), "All")) {
      if (!is.null(rownames(x)) && as.character(batch) %in% rownames(x)) {
        return(as.numeric(x[as.character(batch), ]))
      }

      b_int <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(batch))))
      if (!is.na(b_int) && b_int >= 1 && b_int <= nrow(x)) {
        return(as.numeric(x[b_int, ]))
      }
    }

    return(as.numeric(x))
  }

  if (is.array(x)) return(as.numeric(x))

  if (is.list(x)) {
    if (!is.null(batch) && !identical(as.character(batch), "All")) {
      nm <- names(x)
      if (!is.null(nm) && as.character(batch) %in% nm) {
        return(as.numeric(unlist(x[[as.character(batch)]])))
      }

      b_int <- suppressWarnings(as.integer(gsub("[^0-9]", "", as.character(batch))))
      if (!is.na(b_int) && b_int >= 1 && b_int <= length(x)) {
        return(as.numeric(unlist(x[[b_int]])))
      }
    }

    return(as.numeric(unlist(x)))
  }

  as.numeric(x)
}
