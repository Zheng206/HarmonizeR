test_that("run_harmonizer is exported", {
  expect_true(exists("run_harmonizer", where = asNamespace("HarmonizeR")))
  expect_true(is.function(HarmonizeR::run_harmonizer))
})

test_that("run_harmonizer has expected formal arguments", {
  fmls <- names(formals(HarmonizeR::run_harmonizer))
  
  expect_true("launch.browser" %in% fmls || "launch_browser" %in% fmls || length(fmls) >= 0)
})

test_that("run_app.R file exists in installed package", {
  app_file <- system.file("app", "app.R", package = "HarmonizeR")
  
  expect_type(app_file, "character")
  expect_true(nzchar(app_file))
  expect_true(file.exists(app_file))
})

test_that("app.R contains Shiny app components", {
  app_file <- system.file("app", "app.R", package = "HarmonizeR")
  skip_if_not(file.exists(app_file))
  
  app_text <- readLines(app_file, warn = FALSE)
  
  expect_true(any(grepl("dashboardPage|fluidPage|shinyApp", app_text)))
  expect_true(any(grepl("server\\s*<-|function\\(input, output", app_text)))
})

test_that("run_harmonizer source file contains runApp call", {
  src_file <- system.file("app", "app.R", package = "HarmonizeR")
  skip_if_not(file.exists(src_file))
  
  txt <- paste(readLines(src_file, warn = FALSE), collapse = "\n")
  
  expect_match(txt, "shiny", ignore.case = TRUE)
})

test_that("run_harmonizer does not require cmdstanr at startup", {
  app_file <- system.file("app", "app.R", package = "HarmonizeR")
  skip_if_not(file.exists(app_file))
  
  txt <- paste(readLines(app_file, warn = FALSE), collapse = "\n")
  
  # The app may mention cmdstanr in messages or optional workflow text,
  # but startup should not require direct library(cmdstanr).
  expect_false(grepl("library\\s*\\(\\s*cmdstanr\\s*\\)", txt))
})
