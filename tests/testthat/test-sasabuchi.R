tstat <- function(Fx, b, V) sum(Fx * b) / sqrt(drop(t(Fx) %*% V %*% Fx))

test_that("quadratic form: endpoint t, Sasabuchi statistic, delta SE and CI match hand computation", {
  set.seed(1)
  n <- 150; x <- runif(n, 1, 10); y <- 20 - 6 * x + 0.55 * x^2 + rnorm(n, sd = 4)
  d <- data.frame(y, x, x_sq = x^2); fit <- lm(y ~ x + x_sq, d)
  r <- tptest(fit, vars = c("x", "x_sq"))
  b <- coef(fit)[2:3]; V <- vcov(fit)[2:3, 2:3]; df <- fit$df.residual
  tl <- tstat(c(1, 2 * min(x)), b, V); th <- tstat(c(1, 2 * max(x)), b, V)
  S <- min(-tl, th)
  expect_equal(unname(r$bounds), range(x))
  expect_equal(r$df, df)
  expect_equal(c(r$sasabuchi$t_min, r$sasabuchi$t_max), c(tl, th), tolerance = 1e-10)
  expect_equal(r$sasabuchi$t_overall, S, tolerance = 1e-10)
  expect_equal(r$sasabuchi$p_overall, pt(S, df, lower.tail = FALSE), tolerance = 1e-10)
  expect_equal(r$sasabuchi$p_min, pt(tl, df), tolerance = 1e-10)
  expect_equal(r$sasabuchi$p_max, pt(th, df, lower.tail = FALSE), tolerance = 1e-10)
  tp <- -b[1] / (2 * b[2]); g <- c(-1 / (2 * b[2]), b[1] / (2 * b[2]^2)); se <- sqrt(drop(t(g) %*% V %*% g))
  expect_equal(unname(r$tp), unname(tp), tolerance = 1e-10)
  expect_equal(unname(r$tp_se), se, tolerance = 1e-10)
  expect_equal(unname(r$tp_ci), unname(tp + c(-1, 1) * qt(0.975, df) * se), tolerance = 1e-10)
  expect_equal(r$shape, "U shape")
  expect_false(r$sasabuchi$outside)
  # confint at another level uses the same t distribution
  expect_equal(as.numeric(confint(r, level = 0.9)), unname(tp + c(-1, 1) * qt(0.95, df) * se), tolerance = 1e-10)
})

test_that("extremum outside the interval gives a negative statistic and a monotone label", {
  set.seed(2)
  n <- 150; x <- runif(n, 1, 10); y <- 5 + x + 0.2 * x^2 + rnorm(n, sd = 3)
  d <- data.frame(y, x, x_sq = x^2); fit <- lm(y ~ x + x_sq, d)
  r <- tptest(fit, vars = c("x", "x_sq"))
  b <- coef(fit)[2:3]; V <- vcov(fit)[2:3, 2:3]; df <- fit$df.residual
  tl <- tstat(c(1, 2 * min(x)), b, V); th <- tstat(c(1, 2 * max(x)), b, V)
  expect_true(r$sasabuchi$outside)
  expect_equal(r$alternative, "U shape")
  expect_match(r$shape, "Monotone increasing")
  expect_equal(r$sasabuchi$t_overall, min(-tl, th), tolerance = 1e-10)
  expect_lt(r$sasabuchi$t_overall, 0)
  expect_equal(r$sasabuchi$p_overall, pt(min(-tl, th), df, lower.tail = FALSE), tolerance = 1e-10)
  expect_gt(r$sasabuchi$p_overall, 0.5)
  expect_output(print(r), "cannot be rejected")
})

test_that("inverse form: delta-method gradient is valid for both signs of b1", {
  set.seed(4)
  n <- 200; x <- runif(n, 0.5, 6)
  for (sgn in c(1, -1)) {
    y <- 3 + sgn * (2 * x + 8 / x) + rnorm(n, sd = 1.5)
    d <- data.frame(y, x, xi = 1 / x); fit <- lm(y ~ x + xi, d)
    r <- tptest(fit, vars = c("x", "xi"), form = "inverse")
    b <- coef(fit)[2:3]; V <- vcov(fit)[2:3, 2:3]; df <- fit$df.residual
    th <- b[2] / b[1]
    g <- c(-0.5 * sqrt(th) / b[1], 0.5 / (b[1] * sqrt(th)))
    expect_equal(unname(r$tp), unname(sqrt(th)), tolerance = 1e-10)
    expect_equal(unname(r$tp_se), sqrt(drop(t(g) %*% V %*% g)), tolerance = 1e-10)
    expect_false(is.na(r$tp_se))
    tl <- tstat(c(1, -1 / min(x)^2), b, V); tr <- tstat(c(1, -1 / max(x)^2), b, V)
    expect_equal(c(r$sasabuchi$t_min, r$sasabuchi$t_max), c(tl, tr), tolerance = 1e-10)
    S <- if (sgn > 0) min(-tl, tr) else min(tl, -tr)
    expect_equal(r$sasabuchi$t_overall, S, tolerance = 1e-10)
    expect_equal(r$sasabuchi$p_overall, pt(S, df, lower.tail = FALSE), tolerance = 1e-10)
    expect_equal(r$shape, if (sgn > 0) "U shape" else "Inverse U shape")
  }
})

test_that("log-quadratic form equals the quadratic form in ln x, reported in levels", {
  set.seed(5)
  n <- 200; X <- exp(runif(n, 1, 4)); lx <- log(X)
  y <- 1 + 3 * lx - 0.6 * lx^2 + rnorm(n, sd = 0.3)
  d <- data.frame(y, lx, lx2 = lx^2); fit <- lm(y ~ lx + lx2, d)
  r <- tptest(fit, vars = c("lx", "lx2"), form = "logquadratic", fieller = TRUE)
  q <- tptest(fit, vars = c("lx", "lx2"), form = "quadratic", fieller = TRUE)
  # automatic range is the range of the ln x column itself
  expect_equal(unname(r$bounds), range(lx))
  expect_equal(unname(r$bounds_levels), range(X))
  expect_equal(c(r$sasabuchi$t_min, r$sasabuchi$t_max), c(q$sasabuchi$t_min, q$sasabuchi$t_max))
  expect_equal(r$sasabuchi$p_overall, q$sasabuchi$p_overall)
  expect_equal(r$tp, exp(q$tp))
  expect_equal(r$tp_log, q$tp)
  expect_equal(r$tp_ci, exp(q$tp_ci))
  expect_equal(r$tp_se, exp(q$tp) * q$tp_se)
  expect_equal(c(r$fieller$lo, r$fieller$hi), exp(c(q$fieller$lo, q$fieller$hi)))
  expect_equal(as.numeric(confint(r, level = 0.9)), exp(as.numeric(confint(q, level = 0.9))))
  # bounds may be given in levels of x
  r2 <- tptest(fit, vars = c("lx", "lx2"), form = "logquadratic",
               min = min(X), max = max(X), bounds_scale = "levels")
  expect_equal(r2$bounds, r$bounds)
  expect_equal(r2$sasabuchi$t_overall, r$sasabuchi$t_overall)
  expect_warning(tptest(fit, vars = c("lx", "lx2"), form = "quadratic",
                        min = 1, max = 4, bounds_scale = "levels"), "ignored")
})

# Internal-consistency check of the distribution choice: the package uses the
# normal distribution for glm and the coefs path (the paper only states that
# the test is asymptotically valid for GLMs) and t(df.residual) for lm.
test_that("distribution choice is applied consistently: normal for glm and coefs, t for lm, df can be forced", {
  set.seed(9)
  xg <- runif(300, -3, 3); pr <- plogis(0.5 - 0.3 * xg - 0.4 * xg^2); yg <- rbinom(300, 1, pr)
  dg <- data.frame(yg, x = xg, x_sq = xg^2); fg <- glm(yg ~ x + x_sq, binomial, dg)
  r <- tptest(fg, vars = c("x", "x_sq"))
  expect_null(r$df)
  expect_equal(r$sasabuchi$p_overall, pnorm(r$sasabuchi$t_overall, lower.tail = FALSE))
  expect_equal(unname(r$tp_ci), unname(r$tp + c(-1, 1) * qnorm(0.975) * r$tp_se))
  rc <- tptest(coefs = coef(fg), vcov_mat = vcov(fg), vars = c("x", "x_sq"), min = min(xg), max = max(xg))
  expect_equal(rc$sasabuchi$p_overall, r$sasabuchi$p_overall)
  rt <- tptest(fg, vars = c("x", "x_sq"), df = 50)
  expect_equal(rt$sasabuchi$p_overall, pt(rt$sasabuchi$t_overall, 50, lower.tail = FALSE))
  set.seed(1)
  x <- runif(30, 1, 10); y <- 20 - 6 * x + 0.55 * x^2 + rnorm(30, sd = 4)
  fit <- lm(y ~ x + x_sq, data.frame(y, x, x_sq = x^2))
  expect_equal(tptest(fit, vars = c("x", "x_sq"))$df, 27)
  expect_null(tptest(fit, vars = c("x", "x_sq"), df = Inf)$df)
})

test_that("automatic range uses the estimation sample, not all rows of data", {
  set.seed(1)
  x <- runif(30, 1, 10); y <- 20 - 6 * x + 0.55 * x^2 + rnorm(30, sd = 4)
  d <- data.frame(y, x, x_sq = x^2)
  d$y[which.max(d$x)] <- NA
  fit <- lm(y ~ x + x_sq, d)
  used <- d$x[!is.na(d$y)]
  expect_equal(unname(tptest(fit, vars = c("x", "x_sq"))$bounds), range(used))
  expect_equal(unname(tptest(fit, vars = c("x", "x_sq"), data = d)$bounds), range(used))
  # coefs path needs explicit bounds
  expect_error(tptest(coefs = coef(fit), vcov_mat = vcov(fit), vars = c("x", "x_sq")), "data range")
})

test_that("cubic form applies the test on each monotone sub-interval of the slope", {
  set.seed(6)
  n <- 300; x <- runif(n, -2, 3); y <- 1 + 0.5 * x - 1.5 * x^2 + 0.4 * x^3 + rnorm(n, sd = 0.5)
  d <- data.frame(y, x, x2 = x^2, x3 = x^3); fit <- lm(y ~ x + x2 + x3, d)
  r <- tptest(fit, vars = c("x", "x2", "x3"))
  b <- coef(fit)[2:4]; V <- vcov(fit)[2:4, 2:4]; df <- fit$df.residual
  tt <- function(x0) tstat(c(1, 2 * x0, 3 * x0^2), b, V)
  ip <- unname(-b[2] / (3 * b[3]))
  sg <- r$sasabuchi$segments
  expect_equal(nrow(sg), 2)
  expect_equal(sg$upper[1], ip)
  expect_equal(c(sg$t_lower, sg$t_upper), c(tt(min(x)), tt(ip), tt(ip), tt(max(x))), tolerance = 1e-10)
  expect_equal(sg$alternative, c("Inverse U shape", "U shape"))
  expect_equal(sg$statistic[1], min(tt(min(x)), -tt(ip)), tolerance = 1e-10)
  expect_equal(sg$statistic[2], min(-tt(ip), tt(max(x))), tolerance = 1e-10)
  expect_equal(sg$p_value, pt(sg$statistic, df, lower.tail = FALSE), tolerance = 1e-10)
  expect_true(is.na(r$sasabuchi$t_overall))
  expect_match(r$shape, "N shape")
  expect_output(print(r), "package extension")

  # inflection point outside the interval: single segment equals the
  # two-endpoint test with H = 3 and is reported as the overall test
  d2 <- d[d$x > ip + 0.2, ]
  f2 <- lm(y ~ x + x2 + x3, d2)
  r2 <- tptest(f2, vars = c("x", "x2", "x3"))
  b <- coef(f2)[2:4]; V <- vcov(f2)[2:4, 2:4]; df <- f2$df.residual
  tt2 <- function(x0) tstat(c(1, 2 * x0, 3 * x0^2), b, V)
  expect_equal(nrow(r2$sasabuchi$segments), 1)
  expect_equal(r2$sasabuchi$t_overall, min(-tt2(min(d2$x)), tt2(max(d2$x))), tolerance = 1e-10)
  expect_equal(r2$sasabuchi$p_overall, pt(r2$sasabuchi$t_overall, df, lower.tail = FALSE), tolerance = 1e-10)
  expect_equal(r2$alternative, "U shape")
  expect_output(print(r2), "Overall test")
  expect_false(any(grepl("not monotone", capture.output(print(r2)))))
})

test_that("inverse form requires a positive interval", {
  set.seed(4)
  x <- runif(50, 0.5, 6); y <- 3 + 2 * x + 8 / x + rnorm(50)
  fit <- lm(y ~ x + xi, data.frame(y, x, xi = 1 / x))
  expect_error(tptest(fit, vars = c("x", "xi"), form = "inverse", min = 0), "positive interval")
  expect_error(tptest(fit, vars = c("x", "xi"), form = "inverse", min = -1, max = 6), "positive interval")
})
