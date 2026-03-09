#' Universal Turning Point and Inflection Point Test
#'
#' @description
#' Tests for U-shaped or inverse U-shaped relationships using the
#' Sasabuchi (1980) test as extended by Lind and Mehlum (2010).
#' Supports quadratic, cubic, log-quadratic, and inverse functional forms.
#'
#' @param model A fitted model object (e.g., from \code{lm}, \code{glm}).
#'   Alternatively, coefficients can be provided directly via \code{coefs}.
#' @param vars Character vector of length 2 or 3 specifying the variable names:
#'   \code{c("x", "x_sq")} for quadratic, \code{c("x", "x_sq", "x_cu")} for cubic.
#' @param coefs Named numeric vector of coefficients. If provided, \code{model}
#'   is not required. Must include names matching \code{vars}.
#' @param vcov_mat Variance-covariance matrix for the coefficients. Required
#'   when \code{coefs} is provided.
#' @param min Lower bound of the data interval. If \code{NULL}, extracted from
#'   the model data.
#' @param max Upper bound of the data interval. If \code{NULL}, extracted from
#'   the model data.
#' @param form Functional form: \code{"auto"} (default), \code{"quadratic"},
#'   \code{"cubic"}, \code{"inverse"}, or \code{"logquadratic"}.
#' @param level Confidence level for intervals (default 0.95).
#' @param delta Logical; compute delta-method SE and CI (default \code{TRUE}).
#' @param fieller Logical; compute Fieller confidence interval (default \code{FALSE}).
#' @param twolines Logical; perform Simonsohn (2018) two-lines test (default \code{FALSE}).
#' @param bootstrap Logical; compute parametric bootstrap CI (default \code{FALSE}).
#' @param breps Number of bootstrap replications (default 1000).
#' @param data Optional data frame for two-lines test.
#' @param depvar Name of dependent variable for two-lines test.
#'
#' @return An object of class \code{"tptest"} containing:
#' \describe{
#'   \item{tp}{Turning point estimate}
#'   \item{tp_se}{Delta-method standard error}
#'   \item{tp_ci}{Confidence interval for turning point}
#'   \item{shape}{Detected shape ("U shape" or "Inverse U shape")}
#'   \item{model_form}{Functional form used}
#'   \item{sasabuchi}{List with Sasabuchi test results}
#'   \item{fieller}{Fieller interval (if requested)}
#'   \item{twolines}{Two-lines test results (if requested)}
#'   \item{bootstrap}{Bootstrap results (if requested)}
#'   \item{coefficients}{Named vector of relevant coefficients}
#'   \item{vcov}{Variance-covariance matrix}
#'   \item{bounds}{Data interval bounds}
#' }
#'
#' @details
#' The function implements several approaches for testing non-monotonic relationships:
#'
#' \strong{Sasabuchi (1980) / Lind-Mehlum (2010) Test:}
#' Tests whether the relationship is U-shaped (or inverse U-shaped) by examining
#' slopes at the interval boundaries. The null hypothesis is monotonicity or
#' opposite U-shape.
#'
#' \strong{Functional Forms:}
#' \itemize{
#'   \item \strong{Quadratic:} \eqn{y = \beta_1 x + \beta_2 x^2}; turning point at \eqn{x^* = -\beta_1 / (2\beta_2)}
#'   \item \strong{Cubic:} \eqn{y = \beta_1 x + \beta_2 x^2 + \beta_3 x^3}; up to two turning points
#'   \item \strong{Inverse:} \eqn{y = \beta_1 x + \beta_2 / x}; turning point at \eqn{x^* = \sqrt{\beta_2 / \beta_1}}
#'   \item \strong{Log-quadratic:} \eqn{\ln(y) = \beta_1 \ln(x) + \beta_2 [\ln(x)]^2}
#' }
#'
#' @references
#' Lind, J. T., & Mehlum, H. (2010). With or without U? The appropriate test
#' for a U-shaped relationship. \emph{Oxford Bulletin of Economics and Statistics},
#' 72(1), 109-118. \doi{10.1111/j.1468-0084.2009.00569.x}
#'
#' Sasabuchi, S. (1980). A test of a multivariate normal mean with composite
#' hypotheses determined by linear inequalities. \emph{Biometrika}, 67(2), 429-439.
#'
#' Fieller, E. C. (1954). Some problems in interval estimation.
#' \emph{Journal of the Royal Statistical Society: Series B}, 16(2), 175-185.
#' \doi{10.1111/j.2517-6161.1954.tb00159.x}
#'
#' Simonsohn, U. (2018). Two lines: A valid alternative to the invalid testing
#' of U-shaped relationships with quadratic regressions.
#' \emph{Advances in Methods and Practices in Psychological Science}, 1(4), 538-555.
#'
#' @examples
#' # Simulate data with U-shaped relationship
#' set.seed(42)
#' n <- 200
#' x <- runif(n, 1, 10)
#' y <- 50 - 8*x + 0.5*x^2 + rnorm(n, sd = 5)
#' dat <- data.frame(y = y, x = x, x_sq = x^2)
#'
#' # Fit quadratic model
#' fit <- lm(y ~ x + x_sq, data = dat)
#'
#' # Test for U-shape
#' result <- tptest(fit, vars = c("x", "x_sq"), data = dat)
#' print(result)
#'
#' \donttest{
#' # With Fieller interval and two-lines test
#' result2 <- tptest(fit, vars = c("x", "x_sq"),
#'                   fieller = TRUE, twolines = TRUE,
#'                   data = dat, depvar = "y")
#' summary(result2)
#' }
#'
#' @export
tptest <- function(model = NULL,
                   vars,
                   coefs = NULL,
                   vcov_mat = NULL,
                   min = NULL,
                   max = NULL,
                   form = c("auto", "quadratic", "cubic", "inverse", "logquadratic"),
                   level = 0.95,
                   delta = TRUE,
                   fieller = FALSE,
                   twolines = FALSE,
                   bootstrap = FALSE,
                   breps = 1000,
                   data = NULL,
                   depvar = NULL) {

  form <- match.arg(form)

  # Validate inputs
  if (is.null(model) && is.null(coefs)) {
    stop("Either 'model' or 'coefs' must be provided")
  }

  if (!is.null(coefs) && is.null(vcov_mat)) {
    stop("'vcov_mat' must be provided when using 'coefs'")
  }

  nvar <- length(vars)
  if (!nvar %in% c(2, 3)) {
    stop("'vars' must have length 2 (quadratic/inverse) or 3 (cubic)")
  }

  # Extract coefficients and variance-covariance matrix
  if (!is.null(model)) {
    all_coefs <- stats::coef(model)
    all_vcov <- stats::vcov(model)

    # Check if vars exist in model
    missing_vars <- vars[!vars %in% names(all_coefs)]
    if (length(missing_vars) > 0) {
      stop(paste("Variables not found in model:", paste(missing_vars, collapse = ", ")))
    }

    b <- all_coefs[vars]
    V <- all_vcov[vars, vars]

    # Get degrees of freedom
    df <- if (!is.null(model$df.residual)) model$df.residual else NULL
  } else {
    # Use provided coefficients
    missing_vars <- vars[!vars %in% names(coefs)]
    if (length(missing_vars) > 0) {
      stop(paste("Variables not found in coefs:", paste(missing_vars, collapse = ", ")))
    }

    b <- coefs[vars]
    V <- vcov_mat[vars, vars]
    df <- NULL
  }

  names(b) <- c("b1", "b2", if (nvar == 3) "b3" else NULL)
  b1 <- b["b1"]
  b2 <- b["b2"]
  b3 <- if (nvar == 3) b["b3"] else NULL

  # Extract variance components
  s11 <- V[1, 1]
  s12 <- V[1, 2]
  s22 <- V[2, 2]
  if (nvar == 3) {
    s13 <- V[1, 3]
    s23 <- V[2, 3]
    s33 <- V[3, 3]
  }

  # Determine data range
  if (is.null(min) || is.null(max)) {
    if (!is.null(model) && !is.null(data)) {
      x_var <- vars[1]
      if (x_var %in% names(data)) {
        x_data <- data[[x_var]]
        if (is.null(min)) min <- base::min(x_data, na.rm = TRUE)
        if (is.null(max)) max <- base::max(x_data, na.rm = TRUE)
      }
    }

    if (is.null(min) || is.null(max)) {
      stop("Could not determine data range. Please provide 'min' and 'max'.")
    }
  }

  # Auto-detect or validate functional form
  if (form == "auto") {
    if (nvar == 3) {
      form <- "cubic"
    } else {
      # Default to quadratic for 2 variables
      form <- "quadratic"
    }
  }

  if (form == "cubic" && nvar != 3) {
    stop("Cubic form requires 3 variables: x, x^2, x^3")
  }

  # Compute turning point and related statistics based on form
  result <- switch(form,
    "quadratic" = .compute_quadratic(b1, b2, s11, s12, s22, min, max, df, level),
    "cubic" = .compute_cubic(b1, b2, b3, s11, s12, s13, s22, s23, s33, min, max, df, level),
    "inverse" = .compute_inverse(b1, b2, s11, s12, s22, min, max, df, level),
    "logquadratic" = .compute_logquadratic(b1, b2, s11, s12, s22, min, max, df, level)
  )

  # Add Fieller interval if requested
  if (fieller && form %in% c("quadratic", "inverse")) {
    result$fieller <- fieller_ci(b1, b2, s11, s12, s22, level, form)
  }

  # Add two-lines test if requested
  if (twolines && !is.na(result$tp)) {
    if (is.null(data) || is.null(depvar)) {
      message("Note: 'data' and 'depvar' required for two-lines test.")
    } else {
      result$twolines <- twolines_test(data, vars[1], depvar, result$tp, form)
    }
  }

  # Add bootstrap CI if requested
  if (bootstrap && !is.na(result$tp) && form %in% c("quadratic", "inverse", "logquadratic")) {
    result$bootstrap <- .bootstrap_ci(b, V, form, breps, level)
  }

  # Build output object
  out <- structure(
    list(
      tp = result$tp,
      tp_se = result$tp_se,
      tp_ci = if (delta && !is.null(result$tp_ci)) result$tp_ci else NULL,
      shape = result$shape,
      model_form = form,
      sasabuchi = list(
        t_min = result$t_min,
        t_max = result$t_max,
        p_min = result$p_min,
        p_max = result$p_max,
        t_overall = result$t_overall,
        p_overall = result$p_overall,
        slope_min = result$sl_min,
        slope_max = result$sl_max
      ),
      fieller = if (fieller) result$fieller else NULL,
      twolines = if (twolines && !is.null(result$twolines)) result$twolines else NULL,
      bootstrap = if (bootstrap && !is.null(result$bootstrap)) result$bootstrap else NULL,
      coefficients = b,
      vcov = V,
      bounds = c(min = min, max = max),
      level = level,
      df = df,
      call = match.call()
    ),
    class = "tptest"
  )

  # Add cubic-specific results
  if (form == "cubic") {
    out$tp2 <- result$tp2
    out$inflection <- result$ip
    out$inflection_se <- result$ip_se
    out$inflection_ci <- result$ip_ci
  }

  out
}


#' @noRd
.compute_quadratic <- function(b1, b2, s11, s12, s22, x_min, x_max, df, level) {
  # Turning point: x* = -b1 / (2*b2)
  tp <- -b1 / (2 * b2)

  # Slopes at interval bounds
  sl_min <- b1 + 2 * b2 * x_min
  sl_max <- b1 + 2 * b2 * x_max

  # Variance of slope at x0: Var(b1 + 2*b2*x0) = s11 + 4*x0^2*s22 + 4*x0*s12
  var_sl_min <- s11 + 4 * x_min^2 * s22 + 4 * x_min * s12
  var_sl_max <- s11 + 4 * x_max^2 * s22 + 4 * x_max * s12

  # t-statistics at bounds
  t_min <- sl_min / sqrt(var_sl_min)
  t_max <- sl_max / sqrt(var_sl_max)

  # Delta-method SE for turning point
  # G = (dx*/db1, dx*/db2) = (-1/(2*b2), b1/(2*b2^2))
  g1 <- -1 / (2 * b2)
  g2 <- b1 / (2 * b2^2)
  tp_var <- g1^2 * s11 + 2 * g1 * g2 * s12 + g2^2 * s22
  tp_se <- sqrt(tp_var)

  # Compute p-values and overall test
  result <- .sasabuchi_test(t_min, t_max, df, level)

  # CI for turning point
  crit <- .get_critical(level, df)
  tp_ci <- c(tp - crit * tp_se, tp + crit * tp_se)

  c(list(
    tp = tp,
    tp_se = tp_se,
    tp_ci = tp_ci,
    sl_min = sl_min,
    sl_max = sl_max
  ), result)
}


#' @noRd
.compute_cubic <- function(b1, b2, b3, s11, s12, s13, s22, s23, s33,
                           x_min, x_max, df, level) {
  # Turning points: dy/dx = b1 + 2*b2*x + 3*b3*x^2 = 0
  discrim <- 4 * b2^2 - 12 * b1 * b3

  if (discrim < 0) {
    tp <- NA
    tp2 <- NA
    tp_se <- NA
  } else {
    tp <- (-2 * b2 + sqrt(discrim)) / (6 * b3)
    tp2 <- (-2 * b2 - sqrt(discrim)) / (6 * b3)

    # Keep the one closer to midpoint as primary
    xmid <- (x_min + x_max) / 2
    if (abs(tp - xmid) > abs(tp2 - xmid)) {
      tmp <- tp
      tp <- tp2
      tp2 <- tmp
    }

    # Delta-method for turning point
    denom <- 2 * b2 + 6 * b3 * tp
    if (abs(denom) > 1e-10) {
      g1 <- -1 / denom
      g2 <- -2 * tp / denom
      g3 <- -3 * tp^2 / denom
      tp_var <- g1^2 * s11 + g2^2 * s22 + g3^2 * s33 +
                2 * g1 * g2 * s12 + 2 * g1 * g3 * s13 + 2 * g2 * g3 * s23
      tp_se <- sqrt(tp_var)
    } else {
      tp_se <- NA
    }
  }

  # Inflection point: d²y/dx² = 2*b2 + 6*b3*x = 0 => x_ip = -b2/(3*b3)
  ip <- -b2 / (3 * b3)
  ip_g2 <- -1 / (3 * b3)
  ip_g3 <- b2 / (3 * b3^2)
  ip_var <- ip_g2^2 * s22 + 2 * ip_g2 * ip_g3 * s23 + ip_g3^2 * s33
  ip_se <- sqrt(ip_var)

  # Slopes at bounds
  sl_min <- b1 + 2 * b2 * x_min + 3 * b3 * x_min^2
  sl_max <- b1 + 2 * b2 * x_max + 3 * b3 * x_max^2

  # Variance of slope for cubic
  var_sl_min <- s11 + 4 * x_min^2 * s22 + 9 * x_min^4 * s33 +
                4 * x_min * s12 + 6 * x_min^2 * s13 + 12 * x_min^3 * s23
  var_sl_max <- s11 + 4 * x_max^2 * s22 + 9 * x_max^4 * s33 +
                4 * x_max * s12 + 6 * x_max^2 * s13 + 12 * x_max^3 * s23

  t_min <- sl_min / sqrt(var_sl_min)
  t_max <- sl_max / sqrt(var_sl_max)

  result <- .sasabuchi_test(t_min, t_max, df, level)

  # CI for turning point and inflection
  crit <- .get_critical(level, df)
  tp_ci <- if (!is.na(tp_se)) c(tp - crit * tp_se, tp + crit * tp_se) else c(NA, NA)
  ip_ci <- c(ip - crit * ip_se, ip + crit * ip_se)

  c(list(
    tp = tp,
    tp2 = tp2,
    tp_se = tp_se,
    tp_ci = tp_ci,
    ip = ip,
    ip_se = ip_se,
    ip_ci = ip_ci,
    sl_min = sl_min,
    sl_max = sl_max
  ), result)
}


#' @noRd
.compute_inverse <- function(b1, b2, s11, s12, s22, x_min, x_max, df, level) {
  # Turning point: x* = sqrt(b2/b1)
  if (b2 / b1 < 0) {
    tp <- NA
    tp_se <- NA
  } else {
    tp <- sqrt(b2 / b1)

    # Delta-method: dx*/db1 = -0.5*sqrt(b2)/b1^(3/2), dx*/db2 = 0.5/sqrt(b1*b2)
    g1 <- -0.5 * sqrt(b2) / (b1^(3/2))
    g2 <- 0.5 / sqrt(b1 * b2)
    tp_var <- g1^2 * s11 + 2 * g1 * g2 * s12 + g2^2 * s22
    tp_se <- sqrt(tp_var)
  }

  # Slopes at bounds: dy/dx = b1 - b2/x^2
  sl_min <- b1 - b2 / (x_min^2)
  sl_max <- b1 - b2 / (x_max^2)

  var_sl_min <- s11 + s22 / (x_min^4) - 2 * s12 / (x_min^2)
  var_sl_max <- s11 + s22 / (x_max^4) - 2 * s12 / (x_max^2)

  t_min <- sl_min / sqrt(var_sl_min)
  t_max <- sl_max / sqrt(var_sl_max)

  result <- .sasabuchi_test(t_min, t_max, df, level)

  crit <- .get_critical(level, df)
  tp_ci <- if (!is.na(tp_se)) c(tp - crit * tp_se, tp + crit * tp_se) else c(NA, NA)

  c(list(
    tp = tp,
    tp_se = tp_se,
    tp_ci = tp_ci,
    sl_min = sl_min,
    sl_max = sl_max
  ), result)
}


#' @noRd
.compute_logquadratic <- function(b1, b2, s11, s12, s22, x_min, x_max, df, level) {
  # Turning point in log-space, then exponentiate
  tp_log <- -b1 / (2 * b2)
  tp <- exp(tp_log)

  # Slopes in log-space
  lx_min <- log(x_min)
  lx_max <- log(x_max)
  sl_min <- b1 + 2 * b2 * lx_min
  sl_max <- b1 + 2 * b2 * lx_max

  var_sl_min <- s11 + 4 * lx_min^2 * s22 + 4 * lx_min * s12
  var_sl_max <- s11 + 4 * lx_max^2 * s22 + 4 * lx_max * s12

  t_min <- sl_min / sqrt(var_sl_min)
  t_max <- sl_max / sqrt(var_sl_max)

  # Delta-method for exp(-b1/(2*b2))
  g1 <- tp * (-1 / (2 * b2))
  g2 <- tp * (b1 / (2 * b2^2))
  tp_var <- g1^2 * s11 + 2 * g1 * g2 * s12 + g2^2 * s22
  tp_se <- sqrt(tp_var)

  result <- .sasabuchi_test(t_min, t_max, df, level)

  crit <- .get_critical(level, df)
  tp_ci <- c(tp - crit * tp_se, tp + crit * tp_se)

  c(list(
    tp = tp,
    tp_se = tp_se,
    tp_ci = tp_ci,
    sl_min = sl_min,
    sl_max = sl_max
  ), result)
}


#' @noRd
.sasabuchi_test <- function(t_min, t_max, df, level) {
  # Determine shape based on t-statistics
  shape <- if (t_min > t_max) "Inverse U shape" else "U shape"

  # Overall Sasabuchi test statistic
  t_overall <- min(abs(t_min), abs(t_max))

  # Compute p-values
  if (!is.null(df) && !is.na(df)) {
    p_min <- stats::pt(abs(t_min), df, lower.tail = FALSE)
    p_max <- stats::pt(abs(t_max), df, lower.tail = FALSE)
    p_overall <- stats::pt(t_overall, df, lower.tail = FALSE)
  } else {
    p_min <- stats::pnorm(abs(t_min), lower.tail = FALSE)
    p_max <- stats::pnorm(abs(t_max), lower.tail = FALSE)
    p_overall <- stats::pnorm(t_overall, lower.tail = FALSE)
  }

  # Check if extremum is outside interval
  outside <- (t_min * t_max > 0)
  if (outside) {
    p_overall <- NA
    t_overall <- NA
  }

  list(
    shape = shape,
    t_min = t_min,
    t_max = t_max,
    p_min = p_min,
    p_max = p_max,
    t_overall = t_overall,
    p_overall = p_overall,
    outside = outside
  )
}


#' @noRd
.get_critical <- function(level, df) {
  alpha <- 1 - level
  if (!is.null(df) && !is.na(df)) {
    stats::qt(1 - alpha / 2, df)
  } else {
    stats::qnorm(1 - alpha / 2)
  }
}


#' @noRd
.bootstrap_ci <- function(b, V, form, breps, level) {
  # Parametric bootstrap
  n_params <- length(b)

  # Cholesky decomposition
  L <- tryCatch(chol(V), error = function(e) NULL)
  if (is.null(L)) {
    return(list(se = NA, bias = NA, ci = c(NA, NA), reps = breps))
  }
  L <- t(L)  # Lower triangular

  tp_draws <- numeric(breps)

  for (r in seq_len(breps)) {
    z <- stats::rnorm(n_params)
    b_star <- b + L %*% z

    if (form == "quadratic") {
      tp_draws[r] <- -b_star[1] / (2 * b_star[2])
    } else if (form == "logquadratic") {
      tp_draws[r] <- exp(-b_star[1] / (2 * b_star[2]))
    } else if (form == "inverse") {
      if (b_star[2] / b_star[1] >= 0) {
        tp_draws[r] <- sqrt(b_star[2] / b_star[1])
      } else {
        tp_draws[r] <- NA
      }
    }
  }

  # Remove NAs
  tp_draws <- tp_draws[!is.na(tp_draws)]

  if (length(tp_draws) == 0) {
    return(list(se = NA, bias = NA, ci = c(NA, NA), reps = breps))
  }

  # Original turning point
  if (form == "quadratic") {
    tp_orig <- -b[1] / (2 * b[2])
  } else if (form == "logquadratic") {
    tp_orig <- exp(-b[1] / (2 * b[2]))
  } else if (form == "inverse") {
    tp_orig <- sqrt(b[2] / b[1])
  }

  bs_se <- stats::sd(tp_draws)
  bs_bias <- mean(tp_draws) - tp_orig

  alpha <- 1 - level
  bs_ci <- stats::quantile(tp_draws, c(alpha / 2, 1 - alpha / 2))

  list(
    se = bs_se,
    bias = bs_bias,
    ci = as.numeric(bs_ci),
    reps = breps
  )
}


#' Fieller Confidence Interval for Ratio of Coefficients
#'
#' @description
#' Computes the Fieller (1954) confidence interval for the turning point,
#' which is more appropriate when the denominator coefficient has high uncertainty.
#'
#' @param b1 First coefficient
#' @param b2 Second coefficient
#' @param s11 Variance of b1
#' @param s12 Covariance of b1 and b2
#' @param s22 Variance of b2
#' @param level Confidence level
#' @param form Functional form ("quadratic" or "inverse")
#'
#' @return A list with elements \code{lo}, \code{hi}, and \code{type}.
#'
#' @references
#' Fieller, E. C. (1954). Some problems in interval estimation.
#' \emph{Journal of the Royal Statistical Society: Series B}, 16(2), 175-185.
#' \doi{10.1111/j.2517-6161.1954.tb00159.x}
#'
#' @export
fieller_ci <- function(b1, b2, s11, s12, s22, level = 0.95, form = "quadratic") {
  alpha <- 1 - level
  T_fi <- stats::qnorm(1 - alpha / 2)

  if (form == "quadratic") {
    # Fieller for ratio -b1/(2*b2)
    d_fi <- s12^2 - s11 * s22
    d_fi <- d_fi * T_fi^2 + b2^2 * s11 + b1^2 * s22 - 2 * b1 * b2 * s12

    if (d_fi > 0 && (b2^2 - s22 * T_fi^2) > 0) {
      theta_l <- (-s12 * T_fi^2 + b1 * b2 - T_fi * sqrt(d_fi)) / (b2^2 - s22 * T_fi^2)
      theta_h <- (-s12 * T_fi^2 + b1 * b2 + T_fi * sqrt(d_fi)) / (b2^2 - s22 * T_fi^2)
      lo <- -0.5 * theta_h
      hi <- -0.5 * theta_l
      type <- "bounded"
    } else if (d_fi > 0 && (b2^2 - s22 * T_fi^2) < 0) {
      lo <- -Inf
      hi <- Inf
      type <- "unbounded"
    } else {
      lo <- -Inf
      hi <- Inf
      type <- "entire_real_line"
    }
  } else if (form == "inverse") {
    # Fieller for ratio b2/b1 (then sqrt)
    d_fi <- s12^2 - s11 * s22
    d_fi <- d_fi * T_fi^2 + b1^2 * s22 + b2^2 * s11 - 2 * b1 * b2 * s12

    if (d_fi > 0 && (b1^2 - s11 * T_fi^2) > 0) {
      theta_l <- (s12 * T_fi^2 + b1 * b2 - T_fi * sqrt(d_fi)) / (b1^2 - s11 * T_fi^2)
      theta_h <- (s12 * T_fi^2 + b1 * b2 + T_fi * sqrt(d_fi)) / (b1^2 - s11 * T_fi^2)
      if (theta_l * theta_h > 0 && theta_l > 0) {
        lo <- sqrt(theta_l)
        hi <- sqrt(theta_h)
        type <- "bounded"
      } else {
        lo <- NA
        hi <- NA
        type <- "invalid_negative_ratio"
      }
    } else {
      lo <- -Inf
      hi <- Inf
      type <- "unbounded"
    }
  } else {
    lo <- NA
    hi <- NA
    type <- "not_applicable"
  }

  list(lo = lo, hi = hi, type = type)
}


#' Simonsohn (2018) Two-Lines Test
#'
#' @description
#' Performs the two-lines test proposed by Simonsohn (2018) as an alternative
#' to quadratic regression for testing U-shaped relationships.
#'
#' @param data Data frame containing the variables
#' @param x_var Name of the x variable (character)
#' @param y_var Name of the y variable (character)
#' @param split_point Point at which to split the data (usually the turning point)
#' @param form Functional form (for log-quadratic, split is in log-space)
#'
#' @return A list with test results including slopes, t-values, p-values,
#'   and whether the test confirms the U-shape.
#'
#' @references
#' Simonsohn, U. (2018). Two lines: A valid alternative to the invalid testing
#' of U-shaped relationships with quadratic regressions.
#' \emph{Advances in Methods and Practices in Psychological Science}, 1(4), 538-555.
#'
#' @export
twolines_test <- function(data, x_var, y_var, split_point, form = "quadratic") {
  x <- data[[x_var]]
  y <- data[[y_var]]

  # Split point adjustment for log-quadratic
  if (form == "logquadratic") {
    split_val <- log(split_point)
  } else {
    split_val <- split_point
  }

  # Left segment: x <= split
  left_idx <- x <= split_val
  if (sum(left_idx) < 3) {
    return(list(
      slope_l = NA, t_l = NA, p_l = NA, n_l = sum(left_idx),
      slope_r = NA, t_r = NA, p_r = NA, n_r = sum(!left_idx),
      p_joint = NA, confirms = FALSE
    ))
  }

  fit_l <- stats::lm(y ~ x, data = data.frame(x = x[left_idx], y = y[left_idx]))
  slope_l <- stats::coef(fit_l)["x"]
  se_l <- sqrt(stats::vcov(fit_l)["x", "x"])
  t_l <- slope_l / se_l
  df_l <- fit_l$df.residual
  p_l <- 2 * stats::pt(abs(t_l), df_l, lower.tail = FALSE)

  # Right segment: x > split
  right_idx <- x > split_val
  if (sum(right_idx) < 3) {
    return(list(
      slope_l = slope_l, t_l = t_l, p_l = p_l, n_l = sum(left_idx),
      slope_r = NA, t_r = NA, p_r = NA, n_r = sum(right_idx),
      p_joint = NA, confirms = FALSE
    ))
  }

  fit_r <- stats::lm(y ~ x, data = data.frame(x = x[right_idx], y = y[right_idx]))
  slope_r <- stats::coef(fit_r)["x"]
  se_r <- sqrt(stats::vcov(fit_r)["x", "x"])
  t_r <- slope_r / se_r
  df_r <- fit_r$df.residual
  p_r <- 2 * stats::pt(abs(t_r), df_r, lower.tail = FALSE)

  # Test: for U-shape, left slope < 0 AND right slope > 0
  #       for inv-U, left slope > 0 AND right slope < 0
  correct_signs <- (slope_l < 0 && slope_r > 0) || (slope_l > 0 && slope_r < 0)

  if (correct_signs) {
    p1 <- stats::pt(abs(t_l), df_l, lower.tail = FALSE)
    p2 <- stats::pt(abs(t_r), df_r, lower.tail = FALSE)
    p_joint <- max(p1, p2)
    confirms <- p_joint < 0.05
  } else {
    p_joint <- 1
    confirms <- FALSE
  }

  list(
    slope_l = as.numeric(slope_l),
    t_l = as.numeric(t_l),
    p_l = as.numeric(p_l),
    n_l = sum(left_idx),
    slope_r = as.numeric(slope_r),
    t_r = as.numeric(t_r),
    p_r = as.numeric(p_r),
    n_r = sum(right_idx),
    p_joint = p_joint,
    confirms = confirms
  )
}
