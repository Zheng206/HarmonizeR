test_that("safe_downsample_draws_by_chain keeps equal rows per chain", {
  draws <- data.frame(
    .chain = rep(1:4, times = c(30, 25, 28, 22)),
    .iteration = unlist(lapply(c(30, 25, 28, 22), seq_len)),
    theta = rnorm(105),
    check.names = FALSE
  )
  out <- safe_downsample_draws_by_chain(draws, max_draws = 40)
  expect_equal(length(unique(table(out$.chain))), 1)
})

test_that("summarize_draws_with_rhat_safe returns summary columns", {
  draws <- data.frame(
    .chain = rep(1:4, each = 20),
    .iteration = rep(seq_len(20), times = 4),
    theta = rnorm(80),
    beta = rnorm(80),
    check.names = FALSE
  )
  smry <- summarize_draws_with_rhat_safe(draws, chunk_size = 1)
  expect_true(all(c("variable", "mean", "rhat", "ess_bulk", "ess_tail") %in% names(smry)))
  expect_equal(nrow(smry), 2)
})
