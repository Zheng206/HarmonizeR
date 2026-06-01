test_that("batch_auc_cal_cv returns fold-level AUC rows", {
  testthat::skip_if_not_installed("caret")
  testthat::skip_if_not_installed("randomForest")
  testthat::skip_if_not_installed("pROC")

  set.seed(1)
  batch <- factor(rep(c("A", "B"), each = 12))
  x1 <- data.frame(
    f1 = rnorm(24, mean = as.numeric(batch == "B")),
    f2 = rnorm(24)
  )
  x2 <- data.frame(
    f1 = rnorm(24, mean = as.numeric(batch == "B")),
    f2 = rnorm(24)
  )

  out <- batch_auc_cal_cv(
    data_list = list(x1, x2),
    bat_list = list(batch, batch),
    k = 3,
    ntree = 20,
    seed = 1
  )

  expect_true(all(c("measurement", "fold", "auc") %in% names(out)))
  expect_equal(length(unique(out$measurement)), 2)
  expect_true(all(is.na(out$auc) | (out$auc >= 0 & out$auc <= 1)))
})
