test_that("run_harmonizer is exported and callable", {
  expect_true(is.function(HarmonizeR::run_harmonizer))
})

test_that("bundled Shiny app directory exists", {
  app_dir <- system.file("app", package = "HarmonizeR")
  
  expect_true(nzchar(app_dir))
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})
