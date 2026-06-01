test_that("repair_light_upload errors when posterior_draws is missing", {
  obj <- list(
    note = "Existing note."
  )
  
  expect_error(
    HarmonizeR::repair_light_upload(obj),
    "no posterior_draws"
  )
})

test_that("repair_light_upload repairs loaded object", {
  fake_summary <- data.frame(
    variable = c("alpha", "beta"),
    mean = c(0.1, 0.2),
    rhat = c(1.01, 1.02)
  )
  
  testthat::local_mocked_bindings(
    summarize_draws_with_rhat_safe = function(posterior_draws, chunk_size = 500) {
      expect_equal(posterior_draws, data.frame(alpha = 1:3, beta = 4:6))
      expect_equal(chunk_size, 100)
      fake_summary
    },
    .package = "HarmonizeR"
  )
  
  obj <- list(
    posterior_draws = data.frame(alpha = 1:3, beta = 4:6),
    note = "Original note."
  )
  
  repaired <- HarmonizeR::repair_light_upload(obj, chunk_size = 100)
  
  expect_true(is.list(repaired))
  expect_equal(repaired$mcmc_summary, fake_summary)
  expect_true(grepl("Original note.", repaired$note, fixed = TRUE))
  expect_true(grepl("mcmc_summary was repaired from posterior_draws", repaired$note, fixed = TRUE))
})

test_that("repair_light_upload works when note is missing", {
  fake_summary <- data.frame(
    variable = "alpha",
    mean = 0.1,
    rhat = 1.01
  )
  
  testthat::local_mocked_bindings(
    summarize_draws_with_rhat_safe = function(posterior_draws, chunk_size = 500) {
      fake_summary
    },
    .package = "HarmonizeR"
  )
  
  obj <- list(
    posterior_draws = data.frame(alpha = 1:3)
  )
  
  repaired <- HarmonizeR::repair_light_upload(obj)
  
  expect_equal(repaired$mcmc_summary, fake_summary)
  expect_true(grepl("mcmc_summary was repaired from posterior_draws", repaired$note, fixed = TRUE))
})

test_that("repair_light_upload accepts an RDS file path", {
  fake_summary <- data.frame(
    variable = "alpha",
    mean = 0.1,
    rhat = 1.01
  )
  
  testthat::local_mocked_bindings(
    summarize_draws_with_rhat_safe = function(posterior_draws, chunk_size = 500) {
      fake_summary
    },
    .package = "HarmonizeR"
  )
  
  obj <- list(
    posterior_draws = data.frame(alpha = 1:3),
    note = "Loaded from file."
  )
  
  infile <- tempfile(fileext = ".rds")
  saveRDS(obj, infile)
  
  repaired <- HarmonizeR::repair_light_upload(infile)
  
  expect_equal(repaired$mcmc_summary, fake_summary)
  expect_true(grepl("Loaded from file.", repaired$note, fixed = TRUE))
  expect_true(grepl("mcmc_summary was repaired", repaired$note, fixed = TRUE))
})

test_that("repair_light_upload saves repaired object when outfile is supplied", {
  fake_summary <- data.frame(
    variable = "alpha",
    mean = 0.1,
    rhat = 1.01
  )
  
  testthat::local_mocked_bindings(
    summarize_draws_with_rhat_safe = function(posterior_draws, chunk_size = 500) {
      fake_summary
    },
    .package = "HarmonizeR"
  )
  
  obj <- list(
    posterior_draws = data.frame(alpha = 1:3),
    note = "Original note."
  )
  
  outfile <- tempfile(fileext = ".rds")
  
  expect_invisible(
    HarmonizeR::repair_light_upload(obj, outfile = outfile)
  )
  
  expect_true(file.exists(outfile))
  
  repaired_from_file <- readRDS(outfile)
  
  expect_equal(repaired_from_file$mcmc_summary, fake_summary)
  expect_true(grepl("Original note.", repaired_from_file$note, fixed = TRUE))
  expect_true(grepl("mcmc_summary was repaired", repaired_from_file$note, fixed = TRUE))
})
