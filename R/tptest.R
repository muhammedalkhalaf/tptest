#' Universal Turning Point and Inflection Point Test
#'
#' @description
#' Tests for U-shaped or inverse U-shaped relationships using the
#' Sasabuchi (1980) test as extended by Lind and Mehlum (2010).
#' Supports quadratic, inverse and log-quadratic functional forms, and a
#' cubic form with a segment-wise extension of the test.
#'
#' @param model A fitted model object (e.g., from \code{lm}, \code{glm},
#'   \code{plm::plm}). Alternatively, coefficients can be provided directly
#'   via \code{coefs}.
#' @param vars Character vector of length 2 or 3 giving the names of the
#'   coefficients of the regressors that carry the curvature, in this order:
#'   \code{c("x", "x_sq")} for the quadratic form, \code{c("x", "x_inv")}
#'   (the regressors \eqn{x} and \eqn{1/x}) for the inverse form,
#'   \code{c("lnx", "lnx_sq")} (the regressors \eqn{\ln x} and
#'   \eqn{(\ln x)^2}) for the log-quadratic form, and
#'   \code{c("x", "x_sq", "x_cu")} for the cubic form. The first name is
#'   also used to look up the regressor column when the data range is
#'   determined automatically.
#' @param coefs Named numeric vector of coefficients. If provided, \code{model}
#'   is not required. Must include names matching \code{vars}.
#' @param vcov_mat Variance-covariance matrix for the coefficients. Required
#'   when \code{coefs} is provided.
#' @param min Lower bound of the interval \eqn{[x_l, x_h]} on which the test
#'   is carried out. If \code{NULL}, the minimum of the regressor named in
#'   \code{vars[1]} over the estimation sample is used. See
#'   \code{bounds_scale} for the scale of this argument.
#' @param max Upper bound of the interval; see \code{min}.
#' @param form Functional form: \code{"auto"} (default), \code{"quadratic"},
#'   \code{"cubic"}, \code{"inverse"}, or \code{"logquadratic"}. With
#'   \code{"auto"}, two names in \code{vars} select the quadratic form and
#'   three names select the cubic form.
#' @param level Confidence level for intervals (default 0.95). The
#'   Sasabuchi test itself does not depend on \code{level}.
#' @param delta Logical; compute delta-method SE and CI (default \code{TRUE}).
#' @param fieller Logical; compute Fieller confidence set (default \code{FALSE}).
#'   Available for the quadratic, inverse and log-quadratic forms.
#' @param twolines Logical; perform Simonsohn (2018) two-lines test (default \code{FALSE}).
#' @param bootstrap Logical; compute parametric bootstrap CI (default \code{FALSE}).
#' @param breps Number of bootstrap replications (default 1000).
#' @param data Optional data frame: the data used to fit \code{model}. It is
#'   required for the two-lines test and is used as a fallback to determine the
#'   data range when the regressor cannot be recovered from the model frame
#'   (rows dropped by the model's \code{na.action} are then excluded).
#' @param depvar Name of dependent variable for two-lines test.
#' @param bounds_scale Scale on which user-supplied \code{min} and \code{max}
#'   are given. \code{"regressor"} (default): the scale of the regressor named
#'   in \code{vars[1]}, that is \eqn{x} for the quadratic, inverse and cubic
#'   forms and \eqn{\ln x} for the log-quadratic form. \code{"levels"}: only
#'   meaningful for the log-quadratic form, \code{min} and \code{max} are
#'   given in levels of \eqn{x} and are log-transformed internally. The
#'   automatic range is always taken on the regressor scale.
#' @param df Degrees of freedom for the t distribution used in the one-sided
#'   tests and in the critical values of the delta-method and Fieller
#'   intervals. \code{NULL} (default) selects automatically: the residual
#'   degrees of freedom of the model for \code{lm}-type models (including
#'   \code{plm}), and the normal distribution for \code{glm} objects, for
#'   models without \code{df.residual}, and for the \code{coefs} path.
#'   \code{Inf} forces the normal distribution; a finite number forces
#'   \eqn{t(df)}.
#'
#' @return An object of class \code{"tptest"} containing:
#' \describe{
#'   \item{tp}{Turning point estimate. For the log-quadratic form it is
#'     reported in levels of \eqn{x}, \eqn{\exp(-b_1/(2 b_2))}.}
#'   \item{tp_se}{Delta-method standard error of \code{tp}}
#'   \item{tp_ci}{Delta-method confidence interval for the turning point. For
#'     the log-quadratic form the interval is computed on the \eqn{\ln x}
#'     scale and exponentiated.}
#'   \item{tp_log, tp_log_se}{Log-quadratic form only: turning point and its
#'     delta-method SE on the \eqn{\ln x} scale.}
#'   \item{shape}{Description of the fitted curve on the interval: "U shape",
#'     "Inverse U shape", or a monotone label when the extremum lies outside
#'     the interval (cubic form: see Details)}
#'   \item{alternative}{The alternative hypothesis tested by the Sasabuchi
#'     statistic ("U shape" or "Inverse U shape")}
#'   \item{model_form}{Functional form used}
#'   \item{sasabuchi}{List with the Sasabuchi test results: slopes and
#'     t-statistics at the bounds, one-sided p-values, the overall statistic
#'     and its p-value, and a logical \code{outside} (extremum outside the
#'     interval). For the cubic form it also contains \code{segments}.}
#'   \item{fieller}{Fieller confidence set (if requested); see
#'     \code{\link{fieller_ci}}}
#'   \item{twolines}{Two-lines test results (if requested)}
#'   \item{bootstrap}{Bootstrap results (if requested)}
#'   \item{coefficients}{Named vector of relevant coefficients}
#'   \item{vcov}{Variance-covariance matrix}
#'   \item{bounds}{Interval bounds on the regressor scale}
#'   \item{bounds_levels}{Log-quadratic form only: the bounds in levels of \eqn{x}}
#'   \item{df}{Degrees of freedom used (\code{NULL} when the normal
#'     distribution is used)}
#' }
#'
#' @details
#' \strong{Sasabuchi (1980) / Lind and Mehlum (2010) test.}
#' With \eqn{y = \beta x + \gamma f(x)} and \eqn{f'} monotone on
#' \eqn{[x_l, x_h]}, a U shape is implied by
#' \eqn{\beta + \gamma f'(x_l) < 0 < \beta + \gamma f'(x_h)}. The null
#' hypothesis (monotone or inverse U) is rejected at level \eqn{\alpha} when
#' both one-sided t-tests reject at level \eqn{\alpha}; the overall statistic
#' is \eqn{\min(-t_l, t_h)} for a U shape and \eqn{\min(t_l, -t_h)} for an
#' inverse U shape, and its p-value is the upper tail probability of that
#' minimum. The alternative (U or inverse U) is chosen from the sign of the
#' change of the slope across the interval. When the fitted extremum lies
#' outside the interval the statistic is negative and the null hypothesis
#' cannot be rejected; the statistic and its p-value are still reported.
#' The reported \code{p_min} and \code{p_max} are the one-sided p-values of
#' the two component tests under the tested alternative.
#'
#' \strong{Distribution.} The t distribution with the model's residual
#' degrees of freedom is used for \code{lm}-type models. For generalized
#' linear models the test is only asymptotically valid (Lind and Mehlum 2010,
#' Section 2), so the normal distribution is used; the same applies to the
#' \code{coefs} path. The same distribution is used for the delta-method and
#' Fieller critical values; see argument \code{df}.
#'
#' \strong{Functional forms:}
#' \itemize{
#'   \item \strong{Quadratic:} \eqn{y = \beta_1 x + \beta_2 x^2}; turning point at \eqn{x^* = -\beta_1 / (2\beta_2)}
#'   \item \strong{Inverse:} \eqn{y = \beta_1 x + \beta_2 / x}; turning point at \eqn{x^* = \sqrt{\beta_2 / \beta_1}}
#'     (requires \eqn{\beta_2/\beta_1 > 0}; \eqn{\beta_1 > 0} gives a U shape,
#'     \eqn{\beta_1 < 0} an inverse U shape)
#'   \item \strong{Log-quadratic:} \eqn{y = \beta_1 \ln x + \beta_2 (\ln x)^2}.
#'     The regressors named in \code{vars} are \eqn{\ln x} and \eqn{(\ln x)^2}
#'     and all computations are those of the quadratic form in \eqn{\ln x}.
#'     The turning point is reported in levels, \eqn{x^* = \exp(-\beta_1/(2\beta_2))},
#'     with intervals transformed by \eqn{\exp}. The bounds are on the
#'     \eqn{\ln x} scale unless \code{bounds_scale = "levels"}.
#'   \item \strong{Cubic:} \eqn{y = \beta_1 x + \beta_2 x^2 + \beta_3 x^3}.
#'     Here \eqn{f'} is not monotone on an interval containing the inflection
#'     point, so the two-endpoint test of Lind and Mehlum (2010) does not
#'     apply to the whole interval (their footnote 3). As a package extension,
#'     the interval is split at the inflection point \eqn{-\beta_2/(3\beta_3)}
#'     when it lies inside, and the two-endpoint test is applied on each
#'     sub-interval, on which the slope is monotone. The results are returned
#'     in \code{sasabuchi$segments}. The split point is the estimated
#'     inflection point, treated as fixed, so the sub-interval tests do not
#'     have the exact size of the test in the paper; this extension is not
#'     part of Lind and Mehlum (2010). When the inflection point lies outside
#'     the interval the slope is monotone on the whole interval, the single
#'     segment is the test of equation (9) of Lind and Mehlum (2010) with
#'     \eqn{H = 3}, and \code{t_overall} and \code{p_overall} are reported;
#'     otherwise they are \code{NA}.
#' }
#'
#' @references
#' Lind, J. T. and Mehlum, H. (2010). With or without U? The appropriate test
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
#' # Test for U-shape (data range taken from the estimation sample)
#' result <- tptest(fit, vars = c("x", "x_sq"))
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
                   depvar = NULL,
                   bounds_scale = c("regressor", "levels"),
                   df = NULL) {

  form <- match.arg(form)
  bounds_scale <- match.arg(bounds_scale)

  # Validate inputs
  if (is.null(model) && is.null(coefs)) {
    stop("Either 'model' or 'coefs' must be provided")
  }

  if (!is.null(coefs) && is.null(vcov_mat)) {
    stop("'vcov_mat' must be provided when using 'coefs'")
  }

  nvar <- length(vars)
  if (!nvar %in% c(2, 3)) {
    stop("'vars' must have length 2 (quadratic/inverse/logquadratic) or 3 (cubic)")
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

    # Degrees of freedom: t(df) for lm-type models, normal for glm
    # (Lind and Mehlum 2010: only asymptotically valid for GLMs)
    if (is.null(df)) {
      df <- if (inherits(model, "glm")) NULL else model[["df.residual"]]
    }
  } else {
    # Use provided coefficients
    missing_vars <- vars[!vars %in% names(coefs)]
    if (length(missing_vars) > 0) {
      stop(paste("Variables not found in coefs:", paste(missing_vars, collapse = ", ")))
    }

    b <- coefs[vars]
    V <- vcov_mat[vars, vars]
  }
  if (!is.null(df) && (length(df) != 1 || is.na(df) || !is.finite(df))) df <- NULL
  if (!is.null(df) && df <= 0) stop("'df' must be positive")

  names(b) <- c("b1", "b2", if (nvar == 3) "b3" else NULL)
  b1 <- unname(b["b1"])
  b2 <- unname(b["b2"])
  b3 <- if (nvar == 3) unname(b["b3"]) else NULL

  # Extract variance components
  s11 <- V[1, 1]
  s12 <- V[1, 2]
  s22 <- V[2, 2]
  if (nvar == 3) {
    s13 <- V[1, 3]
    s23 <- V[2, 3]
    s33 <- V[3, 3]
  }

  # Auto-detect or validate functional form
  if (form == "auto") {
    form <- if (nvar == 3) "cubic" else "quadratic"
  }

  if (form == "cubic" && nvar != 3) {
    stop("Cubic form requires 3 variables: x, x^2, x^3")
  }
  if (form != "cubic" && nvar != 2) {
    stop("The ", form, " form requires 2 variables")
  }

  if (bounds_scale == "levels" && form != "logquadratic") {
    warning("'bounds_scale = \"levels\"' is only meaningful for the log-quadratic form and is ignored")
    bounds_scale <- "regressor"
  }

  # Determine the interval [x_l, x_h] on the regressor scale
  user_min <- !is.null(min)
  user_max <- !is.null(max)
  if (bounds_scale == "levels") {
    if (user_min) {
      if (min <= 0) stop("'min' must be positive when 'bounds_scale = \"levels\"'")
      min <- log(min)
    }
    if (user_max) {
      if (max <= 0) stop("'max' must be positive when 'bounds_scale = \"levels\"'")
      max <- log(max)
    }
  }
  if (!user_min || !user_max) {
    x_data <- .regressor_values(model, vars[1], data)
    if (is.null(x_data)) {
      stop("Could not determine the data range for '", vars[1],
           "'. Please provide 'min' and 'max' (or 'data').")
    }
    if (!user_min) min <- base::min(x_data, na.rm = TRUE)
    if (!user_max) max <- base::max(x_data, na.rm = TRUE)
  }
  if (!is.finite(min) || !is.finite(max) || min >= max) {
    stop("'min' must be smaller than 'max' and both must be finite")
  }

  # Compute turning point and related statistics based on form
  result <- switch(form,
    "quadratic" = .compute_quadratic(b1, b2, s11, s12, s22, min, max, df, level),
    "cubic" = .compute_cubic(b1, b2, b3, s11, s12, s13, s22, s23, s33, min, max, df, level),
    "inverse" = .compute_inverse(b1, b2, s11, s12, s22, min, max, df, level),
    "logquadratic" = .compute_logquadratic(b1, b2, s11, s12, s22, min, max, df, level)
  )

  # Add Fieller interval if requested
  if (fieller && form %in% c("quadratic", "inverse", "logquadratic")) {
    result$fieller <- fieller_ci(b1, b2, s11, s12, s22, level, form, df = df)
  } else if (fieller) {
    message("Note: the Fieller interval is not available for the cubic form.")
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
      alternative = result$alternative,
      model_form = form,
      sasabuchi = list(
        t_min = result$t_min,
        t_max = result$t_max,
        p_min = result$p_min,
        p_max = result$p_max,
        t_overall = result$t_overall,
        p_overall = result$p_overall,
        slope_min = result$sl_min,
        slope_max = result$sl_max,
        outside = result$outside
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

  # Add form-specific results
  if (form == "cubic") {
    out$tp2 <- result$tp2
    out$inflection <- result$ip
    out$inflection_se <- result$ip_se
    out$inflection_ci <- result$ip_ci
    out$sasabuchi$segments <- result$segments
  }
  if (form == "logquadratic") {
    out$tp_log <- result$tp_log
    out$tp_log_se <- result$tp_log_se
    out$bounds_levels <- exp(out$bounds)
  }

  out
}


#' @noRd
# Values of the regressor named `var` over the estimation sample.
# First choice: the model frame (rows actually used by the fit). Fallback:
# the column of `data`, with rows dropped by the model's na.action removed.
.regressor_values <- function(model, var, data) {
  if (!is.null(model)) {
    mf <- model[["model"]]
    if (is.null(mf) || !is.data.frame(mf)) {
      mf <- tryCatch(stats::model.frame(model), error = function(e) NULL)
    }
    if (!is.null(mf) && is.data.frame(mf) && var %in% names(mf)) {
      x <- mf[[var]]
      if (is.numeric(x)) return(as.numeric(x))
    }
  }
  if (!is.null(data) && var %in% names(data)) {
    x <- as.numeric(data[[var]])
    na <- if (!is.null(model)) stats::na.action(model) else NULL
    if (!is.null(na) && inherits(na, c("omit", "exclude")) && length(na) > 0 &&
        all(na >= 1 & na <= length(x))) {
      x <- x[-na]
    }
    return(x)
  }
  NULL
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
  result <- .sasabuchi_test(t_min, t_max, sl_min, sl_max, df)

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
  V <- matrix(c(s11, s12, s13, s12, s22, s23, s13, s23, s33), 3, 3)
  bvec <- c(b1, b2, b3)

  # Turning points: dy/dx = b1 + 2*b2*x + 3*b3*x^2 = 0
  discrim <- 4 * b2^2 - 12 * b1 * b3

  if (discrim < 0) {
    tp <- NA
    tp2 <- NA
    tp_se <- NA
    roots <- numeric(0)
  } else {
    tp <- (-2 * b2 + sqrt(discrim)) / (6 * b3)
    tp2 <- (-2 * b2 - sqrt(discrim)) / (6 * b3)
    roots <- c(tp, tp2)

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

  # Inflection point: d2y/dx2 = 2*b2 + 6*b3*x = 0 => x_ip = -b2/(3*b3)
  ip <- -b2 / (3 * b3)
  ip_g2 <- -1 / (3 * b3)
  ip_g3 <- b2 / (3 * b3^2)
  ip_var <- ip_g2^2 * s22 + 2 * ip_g2 * ip_g3 * s23 + ip_g3^2 * s33
  ip_se <- sqrt(ip_var)

  # Slope and its t-statistic at a point x0: F(x0) = (1, 2 x0, 3 x0^2)
  slope_t <- function(x0) {
    Fx <- c(1, 2 * x0, 3 * x0^2)
    sl <- sum(Fx * bvec)
    c(slope = sl, t = sl / sqrt(drop(t(Fx) %*% V %*% Fx)))
  }
  st_min <- slope_t(x_min)
  st_max <- slope_t(x_max)
  sl_min <- unname(st_min["slope"])
  sl_max <- unname(st_max["slope"])
  t_min <- unname(st_min["t"])
  t_max <- unname(st_max["t"])

  # Package extension: f' is monotone on each side of the inflection point,
  # so the two-endpoint test is applied on each such sub-interval.
  breaks <- c(x_min, if (ip > x_min && ip < x_max) ip, x_max)
  nseg <- length(breaks) - 1
  segments <- data.frame(
    lower = breaks[-length(breaks)], upper = breaks[-1],
    slope_lower = NA_real_, slope_upper = NA_real_,
    t_lower = NA_real_, t_upper = NA_real_,
    alternative = NA_character_, statistic = NA_real_, p_value = NA_real_,
    stringsAsFactors = FALSE
  )
  seg_res <- vector("list", nseg)
  for (k in seq_len(nseg)) {
    a <- slope_t(breaks[k])
    z <- slope_t(breaks[k + 1])
    r <- .sasabuchi_test(unname(a["t"]), unname(z["t"]),
                         unname(a["slope"]), unname(z["slope"]), df)
    seg_res[[k]] <- r
    segments$slope_lower[k] <- unname(a["slope"])
    segments$slope_upper[k] <- unname(z["slope"])
    segments$t_lower[k] <- unname(a["t"])
    segments$t_upper[k] <- unname(z["t"])
    segments$alternative[k] <- r$alternative
    segments$statistic[k] <- r$t_overall
    segments$p_value[k] <- r$p_overall
  }

  # Shape on the interval, from the roots of f' that lie inside it
  inside <- roots[roots > x_min & roots < x_max]
  if (length(inside) == 0) {
    shape <- if (sl_min > 0) "Monotone increasing on the interval" else
      "Monotone decreasing on the interval"
    alternative <- NA_character_
  } else if (length(inside) == 1) {
    curv <- 2 * b2 + 6 * b3 * inside
    shape <- if (curv > 0) "U shape" else "Inverse U shape"
    alternative <- shape
  } else {
    shape <- if (b3 > 0) "N shape (inverse U then U)" else "Inverse N shape (U then inverse U)"
    alternative <- NA_character_
  }

  # With the inflection point outside the interval, f' is monotone on the
  # whole interval and the single segment is the test of equation (9) of
  # Lind and Mehlum (2010) with H = 3: report it as the overall test.
  if (nseg == 1) {
    r1 <- seg_res[[1]]
    p_min <- r1$p_min; p_max <- r1$p_max
    t_overall <- r1$t_overall; p_overall <- r1$p_overall
    alternative <- r1$alternative
    if (length(inside) == 0) shape <- r1$shape
  } else {
    p_min <- NA_real_; p_max <- NA_real_
    t_overall <- NA_real_; p_overall <- NA_real_
  }

  # CI for turning point and inflection
  crit <- .get_critical(level, df)
  tp_ci <- if (!is.na(tp_se)) c(tp - crit * tp_se, tp + crit * tp_se) else c(NA, NA)
  ip_ci <- c(ip - crit * ip_se, ip + crit * ip_se)

  list(
    tp = tp,
    tp2 = tp2,
    tp_se = tp_se,
    tp_ci = tp_ci,
    ip = ip,
    ip_se = ip_se,
    ip_ci = ip_ci,
    sl_min = sl_min,
    sl_max = sl_max,
    t_min = t_min,
    t_max = t_max,
    p_min = p_min,
    p_max = p_max,
    t_overall = t_overall,
    p_overall = p_overall,
    outside = length(inside) == 0,
    shape = shape,
    alternative = alternative,
    segments = segments
  )
}


#' @noRd
.compute_inverse <- function(b1, b2, s11, s12, s22, x_min, x_max, df, level) {
  # Turning point: x* = sqrt(b2/b1), defined when b2/b1 > 0
  if (x_min <= 0) {
    stop("The inverse form requires a positive interval: the slope -1/x^2 is not defined at 0 ",
         "and must be monotone on [min, max] (Lind and Mehlum 2010, Section 2)")
  }
  theta <- b2 / b1
  if (!is.finite(theta) || theta <= 0) {
    tp <- NA
    tp_se <- NA
  } else {
    tp <- sqrt(theta)

    # Delta-method: dx*/db1 = -0.5*sqrt(theta)/b1, dx*/db2 = 0.5/(b1*sqrt(theta))
    # (valid for b1 > 0 and for b1 < 0)
    g1 <- -0.5 * tp / b1
    g2 <- 0.5 / (b1 * tp)
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

  result <- .sasabuchi_test(t_min, t_max, sl_min, sl_max, df)

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
# Log-quadratic form: y = b1*lnx + b2*lnx^2. The regressor is ln x, the
# bounds are on the ln x scale, and everything is the quadratic form in ln x.
# The turning point is reported in levels, exp(-b1/(2 b2)).
.compute_logquadratic <- function(b1, b2, s11, s12, s22, lx_min, lx_max, df, level) {
  q <- .compute_quadratic(b1, b2, s11, s12, s22, lx_min, lx_max, df, level)

  tp_log <- q$tp
  tp_log_se <- q$tp_se
  tp <- exp(tp_log)

  q$tp <- tp
  q$tp_se <- tp * tp_log_se          # delta method for exp(tp_log)
  q$tp_ci <- exp(q$tp_ci)            # interval on the ln x scale, exponentiated
  q$tp_log <- tp_log
  q$tp_log_se <- tp_log_se
  q
}


#' @noRd
# One-sided tests at the two endpoints and the Sasabuchi (intersection-union)
# statistic. The alternative is a U shape when the slope increases across
# the interval and an inverse U shape otherwise.
.sasabuchi_test <- function(t_min, t_max, sl_min, sl_max, df) {
  alternative <- if (sl_min < sl_max) "U shape" else "Inverse U shape"
  outside <- (sl_min * sl_max > 0)

  ptail <- function(q, lower) {
    if (!is.null(df)) stats::pt(q, df, lower.tail = lower) else stats::pnorm(q, lower.tail = lower)
  }

  if (alternative == "U shape") {
    # H1L: slope(x_l) < 0 ; H1H: slope(x_h) > 0
    p_min <- ptail(t_min, TRUE)
    p_max <- ptail(t_max, FALSE)
    t_overall <- min(-t_min, t_max)
  } else {
    # H1L: slope(x_l) > 0 ; H1H: slope(x_h) < 0
    p_min <- ptail(t_min, FALSE)
    p_max <- ptail(t_max, TRUE)
    t_overall <- min(t_min, -t_max)
  }
  p_overall <- ptail(t_overall, FALSE)

  shape <- if (!outside) {
    alternative
  } else if (sl_min > 0) {
    "Monotone increasing on the interval (extremum outside)"
  } else {
    "Monotone decreasing on the interval (extremum outside)"
  }

  list(
    shape = shape,
    alternative = alternative,
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
  if (!is.null(df) && !is.na(df) && is.finite(df)) {
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


#' Fieller Confidence Set for the Turning Point
#'
#' @description
#' Computes the Fieller (1954) confidence set for the turning point, which is
#' exact for linear models and remains informative when the denominator
#' coefficient is imprecisely estimated (Lind and Mehlum 2010, equation 8).
#'
#' @param b1 First coefficient (\eqn{\beta_1})
#' @param b2 Second coefficient (\eqn{\beta_2})
#' @param s11 Variance of \code{b1}
#' @param s12 Covariance of \code{b1} and \code{b2}
#' @param s22 Variance of \code{b2}
#' @param level Confidence level (two-sided). Lind and Mehlum (2010) note
#'   that the test of a U shape at level \eqn{\alpha} corresponds to checking
#'   whether the \eqn{1 - 2\alpha} interval lies inside the data range.
#' @param form Functional form: \code{"quadratic"} (\eqn{x^* = -b_1/(2 b_2)}),
#'   \code{"inverse"} (\eqn{x^* = \sqrt{b_2/b_1}}), or \code{"logquadratic"}
#'   (\eqn{x^* = \exp(-b_1/(2 b_2))}).
#' @param df Degrees of freedom for the critical value: \code{NULL} (default)
#'   uses the normal distribution, a finite number uses \eqn{t(df)}.
#'
#' @return A list with elements \code{lo}, \code{hi} and \code{type}:
#' \describe{
#'   \item{\code{"bounded"}}{the set is the interval \code{[lo, hi]}. For the
#'     inverse and log-quadratic forms \code{lo = 0} means the set is
#'     \code{(0, hi]}.}
#'   \item{\code{"two_rays"}}{the set is the union of two rays,
#'     \code{(-Inf, lo]} and \code{[hi, Inf)} (\code{(0, lo]} and
#'     \code{[hi, Inf)} for the inverse and log-quadratic forms). This
#'     happens when the denominator coefficient is not significant at
#'     \code{level} but the discriminant is positive.}
#'   \item{\code{"ray"}}{the set is a half-line; one of \code{lo}, \code{hi}
#'     is infinite (or \code{lo = 0} for the positive forms).}
#'   \item{\code{"unbounded"}}{the set is the whole real line (whole positive
#'     line for the inverse and log-quadratic forms).}
#'   \item{\code{"empty"}}{inverse form only: no positive turning point is
#'     compatible with the data at \code{level}.}
#'   \item{\code{"not_applicable"}}{\code{form} is not supported.}
#' }
#'
#' @details
#' The set for the ratio \eqn{\rho = n/d} of two coefficients is
#' \eqn{\{\rho : (n - \rho d)^2 \le T^2 (s_{nn} - 2 \rho s_{nd} + \rho^2 s_{dd})\}},
#' whose boundary points are
#' \eqn{(n d - T^2 s_{nd} \pm T \sqrt{D}) / (d^2 - T^2 s_{dd})} with
#' \eqn{D = (s_{nd}^2 - s_{nn} s_{dd}) T^2 + d^2 s_{nn} + n^2 s_{dd} - 2 n d s_{nd}}.
#' For the quadratic form \eqn{\rho = b_1/b_2} and \eqn{x^* = -\rho/2}; for the
#' inverse form \eqn{\rho = b_2/b_1} and \eqn{x^* = \sqrt{\rho}}, restricted to
#' \eqn{\rho > 0}.
#'
#' @references
#' Fieller, E. C. (1954). Some problems in interval estimation.
#' \emph{Journal of the Royal Statistical Society: Series B}, 16(2), 175-185.
#' \doi{10.1111/j.2517-6161.1954.tb00159.x}
#'
#' Lind, J. T. and Mehlum, H. (2010). With or without U? The appropriate test
#' for a U-shaped relationship. \emph{Oxford Bulletin of Economics and Statistics},
#' 72(1), 109-118. \doi{10.1111/j.1468-0084.2009.00569.x}
#'
#' @examples
#' # Quadratic: b1 = -6, b2 = 0.55 with a precise b2 gives a bounded interval
#' fieller_ci(-6, 0.55, s11 = 0.04, s12 = -0.003, s22 = 0.0004, level = 0.95)
#'
#' # Same with t(120) critical value
#' fieller_ci(-6, 0.55, s11 = 0.04, s12 = -0.003, s22 = 0.0004, df = 120)
#'
#' # b2 not significant at 5 percent: the set is the union of two rays
#' fieller_ci(-2, 0.15, s11 = 0.5, s12 = -0.05, s22 = 0.01)
#'
#' @export
fieller_ci <- function(b1, b2, s11, s12, s22, level = 0.95, form = "quadratic", df = NULL) {
  T_fi <- .get_critical(level, df)
  b1 <- unname(b1); b2 <- unname(b2)
  s11 <- unname(s11); s12 <- unname(s12); s22 <- unname(s22)

  if (form %in% c("quadratic", "logquadratic")) {
    # Set for rho = b1/b2, then x* = -rho/2 (decreasing map)
    r <- .fieller_ratio(b1, b2, s11, s12, s22, T_fi)
    out <- switch(r$type,
      "bounded"   = list(lo = -0.5 * r$hi, hi = -0.5 * r$lo, type = "bounded"),
      "two_rays"  = list(lo = -0.5 * r$hi, hi = -0.5 * r$lo, type = "two_rays"),
      "ray"       = list(lo = -0.5 * r$hi, hi = -0.5 * r$lo, type = "ray"),
      "unbounded" = list(lo = -Inf, hi = Inf, type = "unbounded"),
      "empty"     = list(lo = NA_real_, hi = NA_real_, type = "empty")
    )
    if (form == "logquadratic") {
      out$lo <- exp(out$lo)
      out$hi <- exp(out$hi)
    }
  } else if (form == "inverse") {
    # Set for theta = b2/b1, then x* = sqrt(theta) on theta > 0
    r <- .fieller_ratio(b2, b1, s22, s12, s11, T_fi)
    out <- .fieller_positive_sqrt(r)
  } else {
    out <- list(lo = NA_real_, hi = NA_real_, type = "not_applicable")
  }

  out
}


#' @noRd
# Fieller set for the ratio rho = num/den with Var(num) = snn, Cov = snd,
# Var(den) = sdd and critical value Tc. Returns lo, hi and type, where for
# "two_rays" the set is (-Inf, lo] U [hi, Inf).
.fieller_ratio <- function(num, den, snn, snd, sdd, Tc) {
  a  <- den^2 - Tc^2 * sdd
  bq <- num * den - Tc^2 * snd
  cq <- num^2 - Tc^2 * snn
  disc <- bq^2 - a * cq     # equals Tc^2 * D in the documented formula

  if (a == 0) {
    # Linear boundary: -2 bq rho + cq <= 0
    if (bq == 0) {
      return(if (cq <= 0) list(lo = -Inf, hi = Inf, type = "unbounded")
             else list(lo = NA_real_, hi = NA_real_, type = "empty"))
    }
    r0 <- cq / (2 * bq)
    return(if (bq > 0) list(lo = r0, hi = Inf, type = "ray")
           else list(lo = -Inf, hi = r0, type = "ray"))
  }
  if (disc < 0) {
    # No real roots: the quadratic keeps the sign of a
    return(if (a < 0) list(lo = -Inf, hi = Inf, type = "unbounded")
           else list(lo = NA_real_, hi = NA_real_, type = "empty"))
  }
  roots <- sort(c((bq - sqrt(disc)) / a, (bq + sqrt(disc)) / a))
  if (a > 0) {
    list(lo = roots[1], hi = roots[2], type = "bounded")
  } else {
    list(lo = roots[1], hi = roots[2], type = "two_rays")
  }
}


#' @noRd
# Map a Fieller set for theta to the set for x* = sqrt(theta), x* > 0.
.fieller_positive_sqrt <- function(r) {
  sq <- function(v) if (v <= 0) 0 else sqrt(v)
  switch(r$type,
    "bounded" = {
      if (r$hi <= 0) list(lo = NA_real_, hi = NA_real_, type = "empty")
      else list(lo = sq(r$lo), hi = sqrt(r$hi), type = "bounded")
    },
    "two_rays" = {
      if (r$hi <= 0) list(lo = 0, hi = Inf, type = "unbounded")
      else if (r$lo <= 0) list(lo = sqrt(r$hi), hi = Inf, type = "ray")
      else list(lo = sqrt(r$lo), hi = sqrt(r$hi), type = "two_rays")
    },
    "ray" = {
      if (is.infinite(r$hi)) {
        if (r$lo <= 0) list(lo = 0, hi = Inf, type = "unbounded")
        else list(lo = sqrt(r$lo), hi = Inf, type = "ray")
      } else {
        if (r$hi <= 0) list(lo = NA_real_, hi = NA_real_, type = "empty")
        else list(lo = 0, hi = sqrt(r$hi), type = "bounded")
      }
    },
    "unbounded" = list(lo = 0, hi = Inf, type = "unbounded"),
    "empty" = list(lo = NA_real_, hi = NA_real_, type = "empty")
  )
}


#' Simonsohn (2018) Two-Lines Test
#'
#' @description
#' Performs the two-lines test proposed by Simonsohn (2018) as an alternative
#' to quadratic regression for testing U-shaped relationships.
#'
#' @param data Data frame containing the variables
#' @param x_var Name of the x variable (character). For the log-quadratic
#'   form this is the \eqn{\ln x} column, the regressor used in the model.
#' @param y_var Name of the y variable (character)
#' @param split_point Point at which to split the data (usually the turning
#'   point). For the log-quadratic form it is given in levels of \eqn{x}, as
#'   returned by \code{\link{tptest}}, and the split is made at
#'   \code{log(split_point)} on the \eqn{\ln x} column.
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
