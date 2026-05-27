test_that("names are Q<round(q*100)> and structure is value/var", {
  set.seed(1)
  res <- ch.quantileStats(rnorm(500), quantiles = c(0.1, 0.5, 0.9))
  expect_equal(names(res), c("Q10", "Q50", "Q90"))
  expect_named(res$Q50, c("value", "var"))
})

test_that("Q50 value converges to the median and var is positive (bootstrap)", {
  set.seed(1)
  x <- rnorm(5000)
  res <- ch.quantileStats(x, quantiles = 0.5, n_boot = 500)
  expect_equal(res$Q50$value, stats::median(x), tolerance = 1e-8)
  expect_true(is.finite(res$Q50$var) && res$Q50$var > 0)
})

test_that("bootstrap variance shrinks as n grows", {
  set.seed(1)
  v_small <- ch.quantileStats(rnorm(200),  0.5, n_boot = 1000)$Q50$var
  v_large <- ch.quantileStats(rnorm(4000), 0.5, n_boot = 1000)$Q50$var
  expect_lt(v_large, v_small)
})

test_that("Maritz-Jarrett method returns finite positive variance", {
  set.seed(2)
  res <- ch.quantileStats(rnorm(500), quantiles = c(0.25, 0.5, 0.75), method = "mj")
  vars <- vapply(res, function(e) e$var, numeric(1))
  expect_true(all(is.finite(vars) & vars > 0))
})

test_that("fewer than 2 observations yields NA variance, value still returned", {
  res <- ch.quantileStats(c(3.2), quantiles = 0.5)
  expect_equal(res$Q50$value, 3.2)
  expect_true(is.na(res$Q50$var))
})

test_that("value matches type-7 sample quantile", {
  set.seed(3)
  x <- rnorm(1000)
  res <- ch.quantileStats(x, quantiles = c(0.1, 0.9), n_boot = 100)
  expect_equal(res$Q10$value, unname(stats::quantile(x, 0.1, type = 7)))
  expect_equal(res$Q90$value, unname(stats::quantile(x, 0.9, type = 7)))
})
