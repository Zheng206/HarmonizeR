test_that("bundled app directory exists", {
  app_dir <- system.file("app", package = "HarmonizeR")
  expect_true(nzchar(app_dir))
})
