test_that("safe integer helpers handle valid and invalid inputs", {
  expect_identical(HarmonizeR:::safe_positive_int("3"), 3L)
  expect_identical(HarmonizeR:::safe_positive_int(NA, default = 2L), 2L)
  expect_identical(HarmonizeR:::safe_positive_int("bad", default = 4L), 4L)
  expect_identical(HarmonizeR:::safe_positive_int(20, max_val = 10L), 10L)
  expect_identical(HarmonizeR:::safe_positive_int(0, default = 1L, min_val = 1L), 1L)

  expect_true(HarmonizeR:::valid_positive_int(1))
  expect_true(HarmonizeR:::valid_positive_int("5"))
  expect_false(HarmonizeR:::valid_positive_int(0))
  expect_false(HarmonizeR:::valid_positive_int(NA))
  expect_false(HarmonizeR:::valid_positive_int("abc"))
  expect_false(HarmonizeR:::valid_positive_int(11, max_val = 10L))

  expect_identical(HarmonizeR:::safe_m_value(3), 3L)
  expect_identical(HarmonizeR:::safe_m_value("2"), 2L)
  expect_null(HarmonizeR:::safe_m_value(0))
  expect_null(HarmonizeR:::safe_m_value(NA))
  expect_null(HarmonizeR:::safe_m_value("bad"))
})

test_that("clean_batch_for_plot trims labels and removes invalid values", {
  x <- c(" A ", "B", "", "NA", "N/A", "NULL", "null", "NaN", "nan", NA, "C")
  out <- HarmonizeR:::clean_batch_for_plot(x)

  expect_s3_class(out, "factor")
  expect_equal(levels(out), c("A", "B", "C"))
  expect_true(is.na(out[3]))
  expect_true(is.na(out[4]))
  expect_true(is.na(out[10]))
})

test_that("safe_discrete_palette returns valid colors for many levels", {
  lev <- paste0("Batch_", seq_len(15))
  cols <- HarmonizeR:::safe_discrete_palette(lev)

  expect_type(cols, "character")
  expect_length(cols, 15)
  expect_equal(names(cols), lev)
  expect_false(anyNA(cols))
  expect_silent(grDevices::col2rgb(cols))

  cols_n <- HarmonizeR:::safe_discrete_palette(12)
  expect_type(cols_n, "character")
  expect_length(cols_n, 12)
  expect_null(names(cols_n))
  expect_silent(grDevices::col2rgb(cols_n))
})

test_that("PCA sizing helpers return expected values", {
  expect_false(HarmonizeR:::pca_equal_axes_default(list(F_list = list(1, 2)), type = "within"))
  expect_true(HarmonizeR:::pca_equal_axes_default(list(F_list = list(1, 2)), type = "shared"))
  expect_true(HarmonizeR:::pca_equal_axes_default(list(G = matrix(1:4, ncol = 2)), type = "within"))

  expect_identical(HarmonizeR:::pca_ncol_facets(1), 1L)
  expect_identical(HarmonizeR:::pca_ncol_facets(2), 2L)
  expect_identical(HarmonizeR:::pca_ncol_facets(3), 2L)
  expect_identical(HarmonizeR:::pca_ncol_facets(4), 2L)
  expect_identical(HarmonizeR:::pca_ncol_facets(5), 3L)
  expect_identical(HarmonizeR:::pca_ncol_facets(NA), 1L)

  expect_identical(HarmonizeR:::pca_legend_rows(8), 1L)
  expect_identical(HarmonizeR:::pca_legend_rows(9), 2L)
  expect_identical(HarmonizeR:::pca_legend_rows(16), 2L)
  expect_identical(HarmonizeR:::pca_legend_rows(17), 3L)

  pca_obj_1 <- list(
    F_list = list(matrix(1:8, ncol = 2)),
    bat = list(c("A", "B", "A", "B"))
  )
  pca_obj_6 <- list(
    F_list = replicate(6, matrix(1:8, ncol = 2), simplify = FALSE),
    bat = replicate(6, c("A", "B", "A", "B"), simplify = FALSE)
  )

  expect_identical(HarmonizeR:::pca_plot_height_px(NULL), 560L)
  expect_gte(
    HarmonizeR:::pca_plot_height_px(pca_obj_6, type = "within"),
    HarmonizeR:::pca_plot_height_px(pca_obj_1, type = "within")
  )
})

test_that("can_draw_ellipse_by_group requires enough points per group", {
  good_df <- data.frame(bat = c(rep("A", 3), rep("B", 3)))
  bad_df <- data.frame(bat = c("A", "A", "B", "B", "B"))

  expect_true(HarmonizeR:::can_draw_ellipse_by_group(good_df, "bat"))
  expect_false(HarmonizeR:::can_draw_ellipse_by_group(bad_df, "bat"))

  faceted_good <- data.frame(
    bat = rep(c(rep("A", 3), rep("B", 3)), 2),
    measurement = rep(c("M1", "M2"), each = 6)
  )
  faceted_bad <- data.frame(
    bat = c(rep("A", 3), rep("B", 3), rep("A", 2), rep("B", 3)),
    measurement = c(rep("M1", 6), rep("M2", 5))
  )

  expect_true(HarmonizeR:::can_draw_ellipse_by_group(faceted_good, "bat", "measurement"))
  expect_false(HarmonizeR:::can_draw_ellipse_by_group(faceted_bad, "bat", "measurement"))
})

test_that("pca_plot_robust returns ggplot objects for within, shared, and single-metric PCA", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")

  F1 <- data.frame(PC1 = rnorm(12), PC2 = rnorm(12), PC3 = rnorm(12))
  F2 <- data.frame(PC1 = rnorm(12), PC2 = rnorm(12), PC3 = rnorm(12))
  bat <- rep(c("A", "B", "C"), each = 4)

  pca_multi <- list(
    F_list = list(F1, F2),
    G = data.frame(PC1 = rnorm(12), PC2 = rnorm(12), PC3 = rnorm(12)),
    bat = list(bat, bat)
  )

  p_within <- HarmonizeR:::pca_plot_robust(pca_multi, type = "within", ellipse = FALSE)
  p_shared <- HarmonizeR:::pca_plot_robust(pca_multi, type = "shared", ellipse = FALSE)

  expect_s3_class(p_within, "ggplot")
  expect_s3_class(p_shared, "ggplot")

  pca_single <- list(
    F_t = data.frame(PC1 = rnorm(12), PC2 = rnorm(12), PC3 = rnorm(12)),
    bat = bat
  )

  p_single <- HarmonizeR:::pca_plot_robust(pca_single, ellipse = FALSE)
  expect_s3_class(p_single, "ggplot")
})

test_that("pca_plot_robust handles many batches without Brewer Set2 limits", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")

  n <- 12
  pca_single <- list(
    F_t = data.frame(PC1 = rnorm(n), PC2 = rnorm(n)),
    bat = paste0("Batch_", seq_len(n))
  )

  p <- HarmonizeR:::pca_plot_robust(pca_single, ellipse = FALSE)
  built <- ggplot2::ggplot_build(p)

  expect_s3_class(p, "ggplot")
  expect_true(length(unique(built$data[[1]]$colour)) >= 10)
})

test_that("to_numeric_if_possible handles numeric-like columns", {
  expect_equal(HarmonizeR:::to_numeric_if_possible(1:3), c(1, 2, 3))
  expect_equal(HarmonizeR:::to_numeric_if_possible(c("1", "2", "3")), c(1, 2, 3))
  expect_equal(HarmonizeR:::to_numeric_if_possible(c("1,000", "2,500")), c(1000, 2500))
  expect_equal(HarmonizeR:::to_numeric_if_possible(factor(c("1", "2"))), c(1, 2))
  expect_equal(HarmonizeR:::to_numeric_if_possible(c("a", "b", "c")), c("a", "b", "c"))

  mixed <- HarmonizeR:::to_numeric_if_possible(c("1", "x", "3"), min_prop = 0.60)
  expect_equal(mixed, c(1, NA, 3))
})

test_that("numeric_like_prop estimates numeric-like proportion", {
  expect_equal(HarmonizeR:::numeric_like_prop(1:3), 1)
  expect_equal(HarmonizeR:::numeric_like_prop(c("1", "2", "x")), 2 / 3)
  expect_equal(HarmonizeR:::numeric_like_prop(c("", NA, " ")), 0)
  expect_equal(HarmonizeR:::numeric_like_prop(list(1, 2)), 0)
})

test_that("clean_uploaded_covar converts numeric-like columns and preserves subject IDs", {
  df <- data.frame(
    subid = c(101, 102, 103),
    age = c("30", "40", "50"),
    site_note = c("A", "B", "C"),
    check.names = FALSE
  )

  out <- HarmonizeR:::clean_uploaded_covar(df, subject_id_cols = "subid")

  expect_type(out$subid, "character")
  expect_type(out$age, "double")
  expect_type(out$site_note, "character")
  expect_equal(names(out), c("subid", "age", "site_note"))
  expect_null(HarmonizeR:::clean_uploaded_covar(NULL))
})

test_that("Stan parameter parsing helpers work", {
  vars <- c("gamma[1,2]", "delta[3]", "lp__", "theta[4,5,6]")

  expect_equal(HarmonizeR:::get_param_family(vars), c("gamma", "delta", "lp__", "theta"))
  expect_equal(HarmonizeR:::extract_stan_indices("gamma[1,2]"), c(1L, 2L))
  expect_equal(HarmonizeR:::extract_stan_indices("lp__"), integer(0))
  expect_equal(HarmonizeR:::get_first_index(vars), c(1L, 3L, NA_integer_, 4L))
  expect_equal(HarmonizeR:::get_second_index(vars), c(2L, NA_integer_, NA_integer_, 5L))
})

test_that("MCMC summary helpers standardize, detect, match, and filter parameters", {
  smry <- data.frame(
    mean = c(0.1, 0.2, 0.3, 0.4),
    row.names = c("gamma[1,1]", "gamma[2,1]", "delta[1,1]", "lp__")
  )

  std <- HarmonizeR:::standardize_mcmc_summary(smry)

  expect_true("variable" %in% names(std))
  expect_equal(HarmonizeR:::detect_mcmc_families(std), c("delta", "gamma", "lp__"))
  expect_equal(HarmonizeR:::match_mcmc_param(std, "gamma"), "gamma")
  expect_equal(HarmonizeR:::match_mcmc_param(std, "delta"), "delta")

  filt <- HarmonizeR:::filter_mcmc_summary(std, family = "gamma", batch = 2, modality = 1)

  expect_equal(nrow(filt), 1)
  expect_equal(filt$variable, "gamma[2,1]")
  expect_equal(filt$.batch_index, 2L)
  expect_equal(filt$.mod_index, 1L)

  expect_equal(nrow(HarmonizeR:::filter_mcmc_summary(std, family = "notfound")), 0)
  expect_null(HarmonizeR:::standardize_mcmc_summary(data.frame(mean = 1, row.names = "")))
})

test_that("nested component and shrinkage helpers find and vectorize objects", {
  obj <- list(
    a = list(
      b = list(
        gamma_hat = matrix(1:6, nrow = 2, dimnames = list(c("A", "B"), NULL)),
        gamma_star = matrix(7:12, nrow = 2, dimnames = list(c("A", "B"), NULL))
      )
    )
  )

  pair <- HarmonizeR:::get_shrink_pair(obj, "gamma")

  expect_equal(pair$empirical, obj$a$b$gamma_hat)
  expect_equal(pair$shrunken, obj$a$b$gamma_star)
  expect_equal(
    HarmonizeR:::vectorize_shrink_object(obj$a$b$gamma_hat, batch = "B"),
    as.numeric(obj$a$b$gamma_hat["B", ])
  )
  expect_equal(
    HarmonizeR:::vectorize_shrink_object(list(A = c(1, 2), B = c(3, 4)), batch = "B"),
    c(3, 4)
  )
  expect_equal(HarmonizeR:::vectorize_shrink_object(NULL), numeric(0))
})
