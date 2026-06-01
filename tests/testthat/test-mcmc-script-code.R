test_that("MCMC script helper code is generated", {
  txt <- mcmc_light_export_helper_code()
  expect_type(txt, "character")
  expect_length(txt, 1)
  expect_match(txt, "safe_downsample_draws_by_chain", fixed = TRUE)
  expect_match(txt, "summarize_draws_with_rhat_safe", fixed = TRUE)
  expect_match(txt, "make_light_mcmc_export", fixed = TRUE)
})


test_that("build_mcmc_script is generated from package helper", {
  script <- build_mcmc_script(
    m = 2, G = 3, n = 10,
    batch_levels = c("A", "B"),
    feat_names = paste0("feat_", 1:3),
    harm_mode = "multi",
    formula_txt = "y ~ age",
    ref_batch = NULL,
    is_longitudinal = FALSE,
    random_var = NULL,
    visit_col = NULL,
    model_type = "lm",
    harm_eb = TRUE,
    harm_robust = FALSE,
    harm_cov = FALSE,
    harm_vt = 0.95,
    harm_minr = 1,
    harm_maxr = 50,
    harm_robcov = FALSE,
    chains = 2,
    parallel_chains = 2,
    adapt_delta = 0.95
  )
  expect_true(grepl("make_light_mcmc_export", script))
  expect_true(grepl("safe_downsample_draws_by_chain", script))
  expect_true(grepl("LIGHT_UPLOAD", script))
  expect_true(grepl("posterior_draws-based lightweight summary", script))
})
