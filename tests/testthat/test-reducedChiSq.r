# Reference: the legacy ch.reducedChiSq arithmetic, frozen here so the refactored
# wrapper can be checked against it. Masks the count on y only (the historical
# behavior); valid only when yFit/yVar have no NAs that y does not also have.
legacy_rcs <- function(y, yFit, yVar, k) {
  y.n <- length(y[!is.na(y)])
  df  <- y.n - k
  chi <- sum((y - yFit)^2 / yVar, na.rm = TRUE)
  list(n = y.n, df = df, chiSq = chi,
       pValueChiSq = pchisq(chi, df, lower.tail = FALSE),
       reducedChiSq = chi / df)
}

test_that("Case 3: no NAs -- wrapper reproduces legacy arithmetic exactly", {
  set.seed(1)
  y <- rnorm(100); yFit <- y + rnorm(100, 0, 0.3); yVar <- runif(100, 0.5, 1.5)
  expect_equal(ch.reducedChiSq(y, yFit, yVar, 5), legacy_rcs(y, yFit, yVar, 5))
})

test_that("Case 1: NAs in y only -- wrapper still matches legacy (back-compat)", {
  set.seed(1)
  y <- rnorm(100); yFit <- y + rnorm(100, 0, 0.3); yVar <- runif(100, 0.5, 1.5)
  y[sample(100, 10)] <- NA
  expect_equal(ch.reducedChiSq(y, yFit, yVar, 5), legacy_rcs(y, yFit, yVar, 5))
})

test_that("Case 2: NA in yVar (not y) -- df counts only terms that enter the sum", {
  set.seed(1)
  y <- rnorm(100); yFit <- y + rnorm(100, 0, 0.3); yVar <- runif(100, 0.5, 1.5)
  yVar[sample(100, 8)] <- NA
  res <- ch.reducedChiSq(y, yFit, yVar, 5)
  expect_equal(res$n, 92)                                  # 100 - 8 dropped terms
  expect_equal(res$df, 92 - 5)
  # legacy would have over-counted n at 100 -- confirm we diverge here on purpose
  expect_false(isTRUE(all.equal(res$n, legacy_rcs(y, yFit, yVar, 5)$n)))
})

test_that("ch.reducedChiSqMulti with one group equals ch.reducedChiSq", {
  set.seed(2)
  y <- rnorm(80); yFit <- y + rnorm(80, 0, 0.4); yVar <- runif(80, 0.5, 2)
  multi <- ch.reducedChiSqMulti(list(y), list(yFit), list(yVar), 4)
  multi$perGroup <- NULL
  expect_equal(multi, ch.reducedChiSq(y, yFit, yVar, 4))
})

test_that("ch.reducedChiSqMulti combines groups additively at unit weight", {
  set.seed(3)
  y1 <- rnorm(50); f1 <- y1 + rnorm(50, 0, 0.3); v1 <- runif(50, 0.5, 1.5)
  y2 <- rnorm(30); f2 <- y2 + rnorm(30, 0, 0.3); v2 <- runif(30, 0.5, 1.5)
  res <- ch.reducedChiSqMulti(list(pHit = y1, RT_Q50 = y2),
                              list(pHit = f1, RT_Q50 = f2),
                              list(pHit = v1, RT_Q50 = v2), 6)
  chi1 <- sum((y1 - f1)^2 / v1); chi2 <- sum((y2 - f2)^2 / v2)
  expect_equal(res$n, 80)
  expect_equal(res$chiSq, chi1 + chi2)
  expect_equal(res$df, 80 - 6)
  expect_equal(nrow(res$perGroup), 2)
  expect_equal(res$perGroup$chiSq, c(chi1, chi2))
})

test_that("groupWeights rescale chi and n contributions", {
  set.seed(4)
  y1 <- rnorm(40); f1 <- y1 + rnorm(40, 0, 0.3); v1 <- runif(40, 0.5, 1.5)
  y2 <- rnorm(40); f2 <- y2 + rnorm(40, 0, 0.3); v2 <- runif(40, 0.5, 1.5)
  w  <- c(pHit = 2, RT = 0.5)
  res <- ch.reducedChiSqMulti(list(pHit = y1, RT = y2), list(pHit = f1, RT = f2),
                              list(pHit = v1, RT = v2), 5, groupWeights = w)
  chi1 <- sum((y1 - f1)^2 / v1); chi2 <- sum((y2 - f2)^2 / v2)
  expect_equal(res$chiSq, 2 * chi1 + 0.5 * chi2)
  expect_equal(res$n, 2 * 40 + 0.5 * 40)
})
