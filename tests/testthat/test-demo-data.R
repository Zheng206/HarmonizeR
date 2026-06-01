test_that("simulate_demo_data returns expected structure", {
  x <- simulate_demo_data(n = 20, G = 5, m = 2, n_batches = 2, seed = 1)
  expect_equal(length(x$data), 2)
  expect_equal(nrow(x$data[[1]]), 20)
  expect_equal(ncol(x$data[[1]]), 5)
  expect_false(x$is_longitudinal)
  expect_equal(length(x$bat), 2)
})

test_that("simulate_demo_longitudinal_data returns longitudinal structure", {
  x <- simulate_demo_longitudinal_data(n_subjects = 10, G = 4, m = 2, n_batches = 2, max_visits = 3, seed = 1)
  expect_equal(length(x$data), 2)
  expect_equal(ncol(x$data[[1]]), 4)
  expect_true(x$is_longitudinal)
  expect_true("subid" %in% names(x$covar[[1]]))
  expect_true("visit" %in% names(x$covar[[1]]))
})
