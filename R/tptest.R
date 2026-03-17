#' Universal Turning Point and Inflection Point Test
#'
#' Tests for U-shaped or inverse U-shaped (hump-shaped) nonlinear
#' relationships. Extends Lind and Mehlum (2010) to quadratic, cubic,
#' log-quadratic, and inverse functional forms using the Sasabuchi (1980)
#' joint slope test.  Works as a post-estimation command: after fitting a
#' model with \code{\link[stats]{lm}} (or any model that exposes \code{coef}
#' and \code{vcov}), supply the names of the linear and squared terms to
#' \code{tptest}.
#'
#' @param object A fitted model object with \code{coef()} and \code{vcov()}
#'   methods (e.g., output of \code{lm}).
#' @param var1 Character string. Name of the linear term (e.g., \code{"x"}).
#' @param var2 Character string. Name of the squared (or inverse, or
#'   log-squared) term (e.g., \code{"I(x^2)"}).
#' @param var3 Character string or \code{NULL}. Name of the cubic term for
#'   cubic specifications. Default \code{NULL}.
#' @param x_min Numeric or \code{NULL}. Lower bound of the evaluation
#'   interval. Defaults to the minimum of the predictor in \code{data}.
#' @param x_max Numeric or \code{NULL}. Upper bound of the evaluation
#'   interval. Defaults to the maximum of the predictor in \code{data}.
#' @param data Optional data frame used to determine the range of \code{var1}
#'   when \code{x_min} / \code{x_max} are not supplied.
#' @param model Character string specifying the functional form. One of
#'   \code{"quad"} (quadratic, default when \code{var3 = NULL}),
#'   \code{"cubic"} (requires \code{var3}), \code{"inv"} (inverse),
#'   or \code{"logquad"} (log-quadratic).
#' @param level Numeric (0, 100). Confidence level for delta-method and
#'   Fieller intervals. Default \code{95}.
#' @param fieller Logical. If \code{TRUE}, compute a Fieller confidence
#'   interval for the turning point (quadratic and inverse models only).
#'   Default \code{FALSE}.
#' @param twolines Logical. If \code{TRUE}, perform the Simonsohn (2018)
#'   two-lines robustness test using the data supplied in \code{data}.
#'   Default \code{FALSE}.
#'
#' @return An object of class \code{"tptest"} with components:
#'   \describe{
#'     \item{turning_point}{Estimated turning/extreme point \eqn{x^*}.}
#'     \item{tp_se}{Delta-method standard error of \eqn{x^*}.}
#'     \item{tp_ci}{Named vector: \code{lower} and \code{upper} bounds of the
#'       \code{level}\% delta-method CI.}
#'     \item{slope_lower}{Slope at the lower bound of the interval.}
#'     \item{slope_upper}{Slope at the upper bound of the interval.}
#'     \item{t_lower}{t-statistic for the slope at the lower bound.}
#'     \item{t_upper}{t-statistic for the slope at the upper bound.}
#'     \item{p_lower}{One-sided p-value for the lower slope.}
#'     \item{p_upper}{One-sided p-value for the upper slope.}
#'     \item{t_sasabuchi}{Sasabuchi joint test statistic.}
#'     \item{p_sasabuchi}{p-value of the Sasabuchi test.}
#'     \item{shape}{Detected shape: \code{"U"} or \code{"Inverse U"}.}
#'     \item{model}{Functional form used.}
#'     \item{x_min}{Lower bound used.}
#'     \item{x_max}{Upper bound used.}
#'     \item{level}{Confidence level.}
#'     \item{fieller_ci}{Fieller CI bounds (if \code{fieller = TRUE}).}
#'     \item{df}{Residual degrees of freedom (or \code{NA}).}
#'     \item{b1}{Estimated coefficient on \code{var1}.}
#'     \item{b2}{Estimated coefficient on \code{var2}.}
#'     \item{b3}{Estimated coefficient on \code{var3} (cubic only).}
#'     \item{second_tp}{Second turning point (cubic only, or \code{NA}).}
#'     \item{inflection_pt}{Inflection point (cubic only, or \code{NA}).}
#'   }
#'
#' @references
#' Lind, J. T. and Mehlum, H. (2010). With or Without U? The Appropriate
#' Test for a U-Shaped Relationship. \emph{Oxford Bulletin of Economics and
#' Statistics}, 72(1), 109--118. \doi{10.1111/j.1468-0084.2009.00569.x}
#'
#' Sasabuchi, S. (1980). A Test of a Multivariate Normal Mean with Composite
#' Hypotheses Determined by Linear Inequalities. \emph{Biometrika}, 67(2),
#' 429--439. \doi{10.1093/biomet/67.2.429}
#'
#' Fieller, E. C. (1954). Some Problems in Interval Estimation. \emph{Journal
#' of the Royal Statistical Society. Series B}, 16(2), 175--185.
#'
#' Simonsohn, U. (2018). Two Lines: A Valid Alternative to the Invalid
#' Testing of U-Shaped Relationships with Quadratic Regressions.
#' \emph{Advances in Methods and Practices in Psychological Science},
#' 1(4), 538--555. \doi{10.1177/2515245918805755}
#'
#' @examples
#' set.seed(1)
#' n  <- 100
#' x  <- runif(n, -3, 3)
#' y  <- -0.5 * x + 0.3 * x^2 + rnorm(n, sd = 0.5)
#' df <- data.frame(y = y, x = x, x2 = x^2)
#' fit <- lm(y ~ x + x2, data = df)
#' res <- tptest(fit, var1 = "x", var2 = "x2", data = df)
#' print(res)
#'
#' @importFrom stats coef vcov pt pnorm qt qnorm
#' @export
tptest <- function(object, var1, var2, var3 = NULL,
                   x_min = NULL, x_max = NULL, data = NULL,
                   model = NULL, level = 95,
                   fieller = FALSE, twolines = FALSE) {

  ## --- Extract coefficients and VCov ---
  b_all <- tryCatch(coef(object), error = function(e) {
    stop("'object' must have a coef() method.", call. = FALSE)
  })
  V_all <- tryCatch(vcov(object), error = function(e) {
    stop("'object' must have a vcov() method.", call. = FALSE)
  })

  if (!var1 %in% names(b_all)) {
    stop(sprintf("Coefficient '%s' not found in model.", var1), call. = FALSE)
  }
  if (!var2 %in% names(b_all)) {
    stop(sprintf("Coefficient '%s' not found in model.", var2), call. = FALSE)
  }

  b1 <- b_all[[var1]]
  b2 <- b_all[[var2]]
  s11 <- V_all[var1, var1]
  s12 <- V_all[var1, var2]
  s22 <- V_all[var2, var2]

  b3 <- NA_real_
  s13 <- NA_real_; s23 <- NA_real_; s33 <- NA_real_
  if (!is.null(var3)) {
    if (!var3 %in% names(b_all)) {
      stop(sprintf("Coefficient '%s' not found in model.", var3), call. = FALSE)
    }
    b3  <- b_all[[var3]]
    s13 <- V_all[var1, var3]
    s23 <- V_all[var2, var3]
    s33 <- V_all[var3, var3]
  }

  ## --- Degrees of freedom ---
  df_r <- tryCatch(as.numeric(object$df.residual),
                   error = function(e) NA_real_)
  use_normal <- is.na(df_r)

  ## --- Determine model ---
  if (is.null(model)) {
    model <- if (!is.null(var3)) "cubic" else "quad"
  }
  model <- tolower(model)
  if (!model %in% c("quad", "cubic", "inv", "logquad")) {
    stop("'model' must be one of \"quad\", \"cubic\", \"inv\", \"logquad\".",
         call. = FALSE)
  }
  if (model == "cubic" && is.null(var3)) {
    stop("model = \"cubic\" requires var3.", call. = FALSE)
  }

  ## --- Data range ---
  if (!is.null(data) && var1 %in% names(data)) {
    x_vals <- data[[var1]]
    if (is.null(x_min)) x_min <- min(x_vals, na.rm = TRUE)
    if (is.null(x_max)) x_max <- max(x_vals, na.rm = TRUE)
  } else {
    if (is.null(x_min) || is.null(x_max)) {
      stop("Supply 'data' or explicit 'x_min'/'x_max'.", call. = FALSE)
    }
  }

  ## --- Compute turning point and slopes by model ---
  tp <- NA_real_; tp_se <- NA_real_; second_tp <- NA_real_
  inflection_pt <- NA_real_; inflection_se <- NA_real_

  if (model == "quad") {
    tp  <- -b1 / (2 * b2)
    g1  <- -1 / (2 * b2)
    g2  <- b1 / (2 * b2^2)
    tp_var <- g1^2 * s11 + 2 * g1 * g2 * s12 + g2^2 * s22
    tp_se  <- sqrt(tp_var)

    sl_min <- b1 + 2 * b2 * x_min
    sl_max <- b1 + 2 * b2 * x_max
    var_sl_min <- s11 + 4 * x_min^2 * s22 + 4 * x_min * s12
    var_sl_max <- s11 + 4 * x_max^2 * s22 + 4 * x_max * s12

  } else if (model == "cubic") {
    discrim <- 4 * b2^2 - 12 * b1 * b3
    if (discrim < 0) {
      warning("No real turning points (discriminant < 0).")
      tp <- NA_real_
    } else {
      tp1 <- (-2 * b2 + sqrt(discrim)) / (6 * b3)
      tp2 <- (-2 * b2 - sqrt(discrim)) / (6 * b3)
      xmid <- (x_min + x_max) / 2
      if (abs(tp1 - xmid) <= abs(tp2 - xmid)) {
        tp <- tp1; second_tp <- tp2
      } else {
        tp <- tp2; second_tp <- tp1
      }
    }
    inflection_pt <- -b2 / (3 * b3)
    ip_g2 <- -1 / (3 * b3)
    ip_g3 <- b2 / (3 * b3^2)
    ip_var <- ip_g2^2 * s22 + 2 * ip_g2 * ip_g3 * s23 + ip_g3^2 * s33
    inflection_se <- sqrt(ip_var)

    if (!is.na(tp)) {
      denom <- 2 * b2 + 6 * b3 * tp
      if (abs(denom) > 1e-10) {
        g1 <- -1 / denom; g2 <- -2 * tp / denom; g3 <- -3 * tp^2 / denom
        tp_var <- g1^2 * s11 + g2^2 * s22 + g3^2 * s33 +
                  2 * g1 * g2 * s12 + 2 * g1 * g3 * s13 + 2 * g2 * g3 * s23
        tp_se <- sqrt(tp_var)
      }
    }

    sl_min <- b1 + 2 * b2 * x_min + 3 * b3 * x_min^2
    sl_max <- b1 + 2 * b2 * x_max + 3 * b3 * x_max^2
    var_sl_min <- s11 + 4 * x_min^2 * s22 + 9 * x_min^4 * s33 +
                  4 * x_min * s12 + 6 * x_min^2 * s13 + 12 * x_min^3 * s23
    var_sl_max <- s11 + 4 * x_max^2 * s22 + 9 * x_max^4 * s33 +
                  4 * x_max * s12 + 6 * x_max^2 * s13 + 12 * x_max^3 * s23

  } else if (model == "inv") {
    ratio <- b2 / b1
    if (!is.finite(ratio) || ratio < 0) {
      warning("No real turning point for inverse model (b2/b1 < 0 or infinite).")
      tp <- NA_real_
    } else {
      tp <- sqrt(ratio)
      g1 <- -0.5 * sqrt(b2) / (b1^1.5)
      g2 <-  0.5 / sqrt(b1 * b2)
      tp_var <- g1^2 * s11 + 2 * g1 * g2 * s12 + g2^2 * s22
      tp_se  <- sqrt(tp_var)
    }
    sl_min <- b1 - b2 / x_min^2
    sl_max <- b1 - b2 / x_max^2
    var_sl_min <- s11 + s22 / x_min^4 - 2 * s12 / x_min^2
    var_sl_max <- s11 + s22 / x_max^4 - 2 * s12 / x_max^2

  } else {  # logquad
    tp_log <- -b1 / (2 * b2)
    tp     <- exp(tp_log)
    g1 <- tp * (-1 / (2 * b2))
    g2 <- tp * (b1 / (2 * b2^2))
    tp_var <- g1^2 * s11 + 2 * g1 * g2 * s12 + g2^2 * s22
    tp_se  <- sqrt(tp_var)

    lx_min <- log(x_min); lx_max <- log(x_max)
    sl_min <- b1 + 2 * b2 * lx_min
    sl_max <- b1 + 2 * b2 * lx_max
    var_sl_min <- s11 + 4 * lx_min^2 * s22 + 4 * lx_min * s12
    var_sl_max <- s11 + 4 * lx_max^2 * s22 + 4 * lx_max * s12
  }

  ## --- t-statistics at bounds ---
  t_min <- sl_min / sqrt(pmax(var_sl_min, .Machine$double.eps))
  t_max <- sl_max / sqrt(pmax(var_sl_max, .Machine$double.eps))

  ## --- p-values ---
  if (use_normal) {
    p_min <- pnorm(-abs(t_min))
    p_max <- pnorm(-abs(t_max))
  } else {
    p_min <- pt(-abs(t_min), df = df_r)
    p_max <- pt(-abs(t_max), df = df_r)
  }

  ## Shape detection: Inverse U has positive slope at lower bound (t_min > 0)
  ## and negative at upper bound (t_max < 0), so t_min > t_max
  shape <- if (t_min > t_max) "Inverse U" else "U"

  ## Sasabuchi joint statistic
  t_sac <- min(abs(t_min), abs(t_max))
  if (use_normal) {
    p_sac <- pnorm(-t_sac)
  } else {
    p_sac <- pt(-t_sac, df = df_r)
  }

  ## --- Delta-method CI ---
  alpha <- (100 - level) / 100
  if (use_normal) {
    crit <- qnorm(1 - alpha / 2)
  } else {
    crit <- qt(1 - alpha / 2, df = df_r)
  }
  tp_ci <- if (!is.na(tp) && !is.na(tp_se)) {
    c(lower = tp - crit * tp_se, upper = tp + crit * tp_se)
  } else {
    c(lower = NA_real_, upper = NA_real_)
  }

  ## --- Fieller CI ---
  fieller_ci <- c(lower = NA_real_, upper = NA_real_)
  if (fieller && model %in% c("quad", "inv")) {
    T_fi <- if (use_normal) qnorm(1 - alpha / 2) else qt(1 - alpha / 2, df_r)
    if (model == "quad") {
      d_fi  <- (s12^2 - s11 * s22) * T_fi^2 +
               b2^2 * s11 + b1^2 * s22 - 2 * b1 * b2 * s12
      denom_fi <- b2^2 - s22 * T_fi^2
      if (d_fi > 0 && denom_fi > 0) {
        theta_l <- (-s12 * T_fi^2 + b1 * b2 - T_fi * sqrt(d_fi)) / denom_fi
        theta_h <- (-s12 * T_fi^2 + b1 * b2 + T_fi * sqrt(d_fi)) / denom_fi
        fieller_ci <- c(lower = -0.5 * theta_h, upper = -0.5 * theta_l)
      }
    }
  }

  ## --- Two-lines test ---
  tl_results <- NULL
  if (twolines && !is.null(data) && !is.na(tp)) {
    tl_results <- .tptest_twolines(
      object = object, var1 = var1, tp = tp,
      data = data, df_r = df_r, use_normal = use_normal
    )
  }

  ## --- Return ---
  result <- list(
    turning_point  = tp,
    tp_se          = tp_se,
    tp_ci          = tp_ci,
    slope_lower    = sl_min,
    slope_upper    = sl_max,
    t_lower        = t_min,
    t_upper        = t_max,
    p_lower        = p_min,
    p_upper        = p_max,
    t_sasabuchi    = t_sac,
    p_sasabuchi    = p_sac,
    shape          = shape,
    model          = model,
    x_min          = x_min,
    x_max          = x_max,
    level          = level,
    fieller_ci     = fieller_ci,
    df             = df_r,
    b1             = b1,
    b2             = b2,
    b3             = b3,
    second_tp      = second_tp,
    inflection_pt  = inflection_pt,
    inflection_se  = inflection_se,
    twolines       = tl_results
  )
  class(result) <- "tptest"
  result
}


## ===========================================================================
## INTERNAL HELPERS
## ===========================================================================

#' Two-lines robustness test (Simonsohn 2018)
#'
#' @param object Fitted model object.
#' @param var1 Name of the linear predictor.
#' @param tp Turning point.
#' @param data Data frame.
#' @param df_r Residual df.
#' @param use_normal Logical.
#' @return List with left/right slope, t, p, and joint p.
#' @keywords internal
.tptest_twolines <- function(object, var1, tp, data, df_r, use_normal) {
  depvar <- as.character(formula(object)[[2L]])
  x_vec  <- data[[var1]]

  ## Left segment: x <= tp
  left_data  <- data[x_vec <= tp, , drop = FALSE]
  right_data <- data[x_vec > tp,  , drop = FALSE]

  if (nrow(left_data) < 5L || nrow(right_data) < 5L) {
    return(NULL)
  }

  fit_l <- tryCatch(
    lm(reformulate(var1, depvar), data = left_data),
    error = function(e) NULL
  )
  fit_r <- tryCatch(
    lm(reformulate(var1, depvar), data = right_data),
    error = function(e) NULL
  )
  if (is.null(fit_l) || is.null(fit_r)) return(NULL)

  slope_l <- coef(fit_l)[[var1]]
  slope_r <- coef(fit_r)[[var1]]
  se_l    <- sqrt(vcov(fit_l)[var1, var1])
  se_r    <- sqrt(vcov(fit_r)[var1, var1])
  t_l     <- slope_l / se_l
  t_r     <- slope_r / se_r
  df_l    <- fit_l$df.residual
  df_r2   <- fit_r$df.residual

  p_l <- 2 * pt(-abs(t_l), df = df_l)
  p_r <- 2 * pt(-abs(t_r), df = df_r2)

  ## Joint test: correct signs?
  correct_signs <- (slope_l < 0 && slope_r > 0) ||
                   (slope_l > 0 && slope_r < 0)
  if (correct_signs) {
    p1 <- pt(-abs(t_l), df = df_l)
    p2 <- pt(-abs(t_r), df = df_r2)
    p_joint <- max(p1, p2)
  } else {
    p_joint <- 1
  }

  list(
    slope_l  = slope_l, slope_r = slope_r,
    t_l      = t_l,     t_r     = t_r,
    p_l      = p_l,     p_r     = p_r,
    p_joint  = p_joint,
    n_l      = nrow(left_data),
    n_r      = nrow(right_data)
  )
}
