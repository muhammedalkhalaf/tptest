# Fieller confidence sets checked against brute-force inversion of the
# defining inequality (num - rho*den)^2 <= T^2 Var(num - rho*den)

brute_quad <- function(b, V, Tc, grid) {
  num <- (b[1] + 2 * b[2] * grid)^2
  v <- V[1, 1] + 4 * grid^2 * V[2, 2] + 4 * grid * V[1, 2]
  grid[num <= Tc^2 * v]
}
brute_inv <- function(b, V, Tc, gx) {
  num <- (b[1] - b[2] / gx^2)^2
  v <- V[1, 1] + V[2, 2] / gx^4 - 2 * V[1, 2] / gx^2
  gx[num <= Tc^2 * v]
}

test_that("quadratic Fieller interval matches closed form and brute force, t critical value", {
  set.seed(1)
  n <- 150; x <- runif(n, 1, 10); y <- 20 - 6 * x + 0.55 * x^2 + rnorm(n, sd = 4)
  d <- data.frame(y, x, x_sq = x^2); fit <- lm(y ~ x + x_sq, d)
  r <- tptest(fit, vars = c("x", "x_sq"), fieller = TRUE)
  b <- coef(fit)[2:3]; V <- vcov(fit)[2:3, 2:3]; df <- fit$df.residual
  Tc <- qt(0.975, df)
  a <- b[2]^2 - Tc^2 * V[2, 2]
  bq <- b[1] * b[2] - Tc^2 * V[1, 2]
  dd <- (V[1, 2]^2 - V[1, 1] * V[2, 2]) * Tc^2 + b[2]^2 * V[1, 1] + b[1]^2 * V[2, 2] - 2 * b[1] * b[2] * V[1, 2]
  rho <- (bq + c(-1, 1) * Tc * sqrt(dd)) / a
  expect_equal(r$fieller$type, "bounded")
  expect_equal(unname(c(r$fieller$lo, r$fieller$hi)), unname(rev(-0.5 * rho)), tolerance = 1e-10)
  # brute force on a fine grid
  A <- brute_quad(b, V, Tc, seq(4, 7, by = 1e-4))
  expect_equal(unname(c(r$fieller$lo, r$fieller$hi)), range(A), tolerance = 1e-4)
  # a normal critical value gives a different (narrower) interval
  fz <- fieller_ci(b[1], b[2], V[1, 1], V[1, 2], V[2, 2])
  expect_gt(fz$lo, r$fieller$lo)
  expect_lt(fz$hi, r$fieller$hi)
})

test_that("inverse-form Fieller interval matches brute-force inversion (both U and inverse U)", {
  set.seed(4)
  n <- 200; x <- runif(n, 0.5, 6)
  y <- 3 + 2 * x + 8 / x + rnorm(n, sd = 1.5)
  d <- data.frame(y, x, xi = 1 / x); fit <- lm(y ~ x + xi, d)
  r <- tptest(fit, vars = c("x", "xi"), form = "inverse", fieller = TRUE)
  b <- coef(fit)[2:3]; V <- vcov(fit)[2:3, 2:3]; Tc <- qt(0.975, fit$df.residual)
  # closed form for theta = b2/b1
  a <- b[1]^2 - Tc^2 * V[1, 1]; bb <- b[1] * b[2] - Tc^2 * V[1, 2]
  dd <- (V[1, 2]^2 - V[1, 1] * V[2, 2]) * Tc^2 + b[1]^2 * V[2, 2] + b[2]^2 * V[1, 1] - 2 * b[1] * b[2] * V[1, 2]
  closed <- sqrt((bb + c(-1, 1) * Tc * sqrt(dd)) / a)
  expect_equal(unname(c(r$fieller$lo, r$fieller$hi)), unname(closed), tolerance = 1e-10)
  A <- brute_inv(b, V, Tc, seq(1.5, 2.5, by = 1e-4))
  expect_equal(unname(c(r$fieller$lo, r$fieller$hi)), range(A), tolerance = 1e-4)

  # inverse U: both coefficients negative
  y2 <- 3 - 2 * x - 8 / x + rnorm(n, sd = 1.5)
  d2 <- data.frame(y = y2, x, xi = 1 / x); f2 <- lm(y ~ x + xi, d2)
  r2 <- tptest(f2, vars = c("x", "xi"), form = "inverse", fieller = TRUE)
  b <- coef(f2)[2:3]; V <- vcov(f2)[2:3, 2:3]; Tc <- qt(0.975, f2$df.residual)
  A <- brute_inv(b, V, Tc, seq(1.5, 2.5, by = 1e-4))
  expect_equal(r2$fieller$type, "bounded")
  expect_equal(unname(c(r2$fieller$lo, r2$fieller$hi)), range(A), tolerance = 1e-4)
  expect_equal(r2$shape, "Inverse U shape")
})

test_that("inverse-form Fieller set with theta_l <= 0 is (0, sqrt(theta_h)]", {
  fi <- fieller_ci(b1 = 1, b2 = 0.5, s11 = 0.04, s12 = 0, s22 = 0.16, form = "inverse")
  expect_equal(fi$type, "bounded")
  expect_equal(fi$lo, 0)
  A <- brute_inv(c(1, 0.5), diag(c(0.04, 0.16)), qnorm(0.975), seq(1e-3, 5, by = 1e-4))
  expect_equal(min(A), 1e-3)
  expect_equal(fi$hi, max(A), tolerance = 1e-4)
})

test_that("quadratic Fieller set is the union of two rays when b2 is not significant", {
  b1 <- -2; b2 <- 0.15; s11 <- 0.5; s22 <- 0.01; s12 <- -0.05   # t(b2) = 1.5
  f <- fieller_ci(b1, b2, s11, s12, s22, 0.95, "quadratic")
  expect_equal(f$type, "two_rays")
  g <- seq(-200, 200, by = 0.001)
  A <- brute_quad(c(b1, b2), matrix(c(s11, s12, s12, s22), 2), qnorm(0.975), g)
  gap <- which(diff(A) > 0.01)
  expect_length(gap, 1)
  expect_equal(f$lo, A[gap], tolerance = 1e-3)
  expect_equal(f$hi, A[gap + 1], tolerance = 1e-3)
  expect_lt(f$lo, f$hi)
  # brute force: points just inside the gap are excluded, just outside included
  inset <- function(xs) {
    (b1 + 2 * b2 * xs)^2 <= qnorm(0.975)^2 * (s11 + 4 * xs^2 * s22 + 4 * xs * s12)
  }
  expect_true(inset(f$lo - 1e-3))
  expect_false(inset(f$lo + 1e-3))
  expect_false(inset(f$hi - 1e-3))
  expect_true(inset(f$hi + 1e-3))
  # whole line when the discriminant is negative
  f2 <- fieller_ci(-0.5, 0.05, 1, 0, 0.01, 0.95, "quadratic")
  expect_equal(f2$type, "unbounded")
  expect_equal(c(f2$lo, f2$hi), c(-Inf, Inf))
})

# Inputs of Table 1 of Lind and Mehlum (2010) (Chambers 2007 Kuznets
# regression). Only the coefficients, their SEs and the SEs of the slopes at
# the bounds are printed, so s12 and the bounds are backed out from the
# rounded SEs; the package results are therefore only approximately
# comparable with the printed values.
table1 <- local({
  b1 <- 32.27; b2 <- -1.88; s11 <- 11.63^2; s22 <- 0.65^2
  xl <- (b1 - 8.15) / (-2 * b2); xh <- (b1 + 4.33) / (-2 * b2)
  s12 <- mean(c((3.65^2 - s11 - 4 * xl^2 * s22) / (4 * xl),
                (2.34^2 - s11 - 4 * xh^2 * s22) / (4 * xh)))
  V <- matrix(c(s11, s12, s12, s22), 2, dimnames = list(c("lx", "lx2"), c("lx", "lx2")))
  tptest(coefs = c(lx = b1, lx2 = b2), vcov_mat = V, vars = c("lx", "lx2"),
         min = xl, max = xh, level = 0.90, fieller = TRUE)
})

test_that("coefs path snapshot on Table-1-like inputs (unchanged from version 1.0.3)", {
  r <- table1
  expect_null(r$df)  # coefs path: normal distribution
  expect_equal(r$shape, "Inverse U shape")
  expect_equal(r$tp, 8.582447, tolerance = 1e-6)
  expect_equal(unname(c(r$fieller$lo, r$fieller$hi)), c(7.419347, 9.517129), tolerance = 1e-6)
  expect_equal(unname(r$tp_ci), c(7.724816, 9.440077), tolerance = 1e-6)
  expect_equal(r$sasabuchi$t_overall, 1.907153, tolerance = 1e-6)
  expect_equal(r$sasabuchi$p_overall, 0.02825038, tolerance = 1e-6)
})

test_that("approximate agreement with the printed values of Table 1 of Lind and Mehlum (2010)", {
  r <- table1
  # printed: extremum 8.60, Fieller 90% [7.44, 9.58], delta 90% [7.73, 9.47],
  # Sasabuchi statistic 1.85 with p-value 0.033
  expect_equal(r$tp, 8.60, tolerance = 0.06)
  expect_equal(unname(c(r$fieller$lo, r$fieller$hi)), c(7.44, 9.58), tolerance = 0.06)
  expect_equal(unname(r$tp_ci), c(7.73, 9.47), tolerance = 0.06)
  expect_equal(r$sasabuchi$t_overall, 1.85, tolerance = 0.06)
  expect_equal(r$sasabuchi$p_overall, 0.033, tolerance = 0.2)
})
