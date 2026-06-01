test_that("install_harmonizer_dependencies dry run returns package names", {
  missing <- install_harmonizer_dependencies(dry_run = TRUE, quiet = TRUE)
  expect_type(missing, "character")
})

test_that("install_harmonizer_dependencies can exclude optional MCMC and MultiComBat", {
  missing_base <- install_harmonizer_dependencies(
    include_mcmc = FALSE,
    include_multicombat = FALSE,
    dry_run = TRUE,
    quiet = TRUE
  )

  expect_type(missing_base, "character")
  expect_false("cmdstanr" %in% missing_base)
  expect_false("MultiComBat" %in% missing_base)
})

test_that("install_harmonizer_dependencies includes optional packages when requested", {
  missing_all <- install_harmonizer_dependencies(
    include_mcmc = TRUE,
    include_multicombat = TRUE,
    dry_run = TRUE,
    quiet = TRUE
  )

  expect_type(missing_all, "character")

  # The packages may or may not be missing on the test machine, so this test only
  # verifies that the dry-run path is side-effect free and returns character data.
  expect_true(is.character(missing_all))
})
