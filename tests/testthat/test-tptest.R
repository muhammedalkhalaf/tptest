test_that("tptest returns correct class", {
  set.seed(1)
  n  <- 100
  x  <- runif(n, -3, 3)
  y  <- -0.5 * x + 0.3 * x^2 + rnorm(n, sd = 0.5)
  df <- data.frame(y = y, x = x, x2 = x^2)
  fit <- lm(y ~ x + x2, data = df)
  res <- tptest(fit, var1 = "x", var2 = "x2", data = df, model = "quad")
  expect_s3_class(res, "tptest")
})

test_that("quadratic turning point is near b1/(2*b2)", {
  set.seed(42)
  n  <- 200
  x  <- runif(n, 0, 10)
  y  <- 2 * x - 0.2 * x^2 + rnorm(n, sd = 1)
  df <- data.frame(y = y, x = x, x2 = x^2)
  fit <- lm(y ~ x + x2, data = df)
  res <- tptest(fit, var1 = "x", var2 = "x2", data = df)
  # turning point should be near 5 (= 2 / (2 * 0.2))
  expect_true(abs(res$turning_point - 5) < 1.5)
})

test_that("tptest errors on missing variable", {
  set.seed(1)
  n  <- 50
  x  <- runif(n)
  y  <- x + rnorm(n)
  fit <- lm(y ~ x)
  expect_error(tptest(fit, var1 = "x", var2 = "MISSING",
                      x_min = 0, x_max = 1),
               "not found in model")
})

test_that("tptest errors when no data and no range", {
  set.seed(1)
  n  <- 50
  x  <- runif(n, -1, 1)
  y  <- -x + 0.5 * x^2 + rnorm(n)
  df <- data.frame(y = y, x = x, x2 = x^2)
  fit <- lm(y ~ x + x2, data = df)
  expect_error(tptest(fit, var1 = "x", var2 = "x2"),
               "Supply 'data'")
})

test_that("inverse U shape is detected", {
  set.seed(7)
  n  <- 200
  x  <- runif(n, 0, 10)
  y  <- 3 * x - 0.3 * x^2 + rnorm(n)
  df <- data.frame(y = y, x = x, x2 = x^2)
  fit <- lm(y ~ x + x2, data = df)
  res <- tptest(fit, var1 = "x", var2 = "x2", data = df)
  expect_equal(res$shape, "Inverse U")
})

test_that("print.tptest uses message not cat", {
  set.seed(1)
  n  <- 100
  x  <- runif(n, -3, 3)
  y  <- -0.5 * x + 0.3 * x^2 + rnorm(n)
  df <- data.frame(y = y, x = x, x2 = x^2)
  fit <- lm(y ~ x + x2, data = df)
  res <- tptest(fit, var1 = "x", var2 = "x2", data = df)
  expect_message(print(res))
  out <- capture.output(print(res))
  expect_equal(length(out), 0L)
})
