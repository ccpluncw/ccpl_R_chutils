#predict the way the function has to: newdata carries only those of
#shift/scale that the fit made data columns
pred_at <- function(f, xx) {
  nd <- data.frame(x = xx)
  for (cl in c("shift", "scale")) {
    if (!is.null(f$data[[cl]])) nd[[cl]] <- f$data[[cl]][1]
  }
  as.numeric(stats::predict(f$fit, nd))
}

prob_fit <- function(seed = 3) {
  set.seed(seed)
  x <- rep(seq(0, 1, 0.05), 12)
  y <- 1 / (1 + exp(5 - 10 * x)) + rnorm(length(x), 0, 0.02)
  ch.logistic(x, y)
}

rt_fit <- function(seed = 3) {
  set.seed(seed)
  x <- rep(seq(0, 1, 0.05), 12)
  y <- 200 + 600 / (1 + exp(5 - 10 * x)) + rnorm(length(x), 0, 15)
  ch.logistic(x, y, parameters = c(bottom = 200, top = 800, slope = 5))
}

test_that("it inverts a probability-scale fit with scale left free", {
  #scale is free under the defaults, so there is no scale column
  f <- prob_fit()
  expect_false("scale" %in% names(f$data))
  got <- ch.logisticXfromY(f, 0.5)
  expect_false(is.na(got))
  expect_equal(pred_at(f, got), 0.5, tolerance = 1e-3)
})

test_that("it inverts a fit whose scale was fixed", {
  set.seed(3)
  x <- rep(seq(0, 1, 0.05), 12)
  y <- 1 / (1 + exp(5 - 10 * x)) + rnorm(length(x), 0, 0.02)
  f <- ch.logistic(x, y, fixedMaxX = 1)
  expect_true("scale" %in% names(f$data))
  got <- ch.logisticXfromY(f, 0.5)
  expect_equal(pred_at(f, got), 0.5, tolerance = 1e-3)
})

test_that("a target outside 0 and 1 works when the curve runs there", {
  f <- rt_fit()
  got <- ch.logisticXfromY(f, 500)
  expect_false(is.na(got))
  expect_equal(pred_at(f, got), 500, tolerance = 1e-4)
})

test_that("a target outside the fitted range is NA, with a message", {
  f <- rt_fit()
  expect_message(r <- ch.logisticXfromY(f, 5000), "outside the fitted range")
  expect_true(is.na(r))
  expect_message(r2 <- ch.logisticXfromY(f, 0.5), "outside the fitted range")
  expect_true(is.na(r2))
})

test_that("the old name still works and warns", {
  f <- prob_fit()
  expect_warning(old <- ch.getChlogisticXfromProbability(f, 0.5), "deprecated")
  expect_equal(old, ch.logisticXfromY(f, 0.5))
})
