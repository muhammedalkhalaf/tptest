#' Print method for tptest objects
#'
#' @param x An object of class \code{"tptest"}.
#' @param digits Integer. Significant digits for numeric output.
#' @param ... Further arguments (ignored).
#' @return Invisibly returns \code{x}.
#' @export
print.tptest <- function(x, digits = 4L, ...) {
  sig <- .tptest_stars(x$p_sasabuchi)
  shape_label <- x$shape

  message(strrep("-", 62))
  message("tptest: Turning Point and Inflection Point Test")
  message("Lind & Mehlum (2010) with Extensions")
  message(strrep("-", 62))
  message(sprintf("Functional form : %s", x$model))
  message(sprintf("Interval        : [%.*f, %.*f]",
                  digits, x$x_min, digits, x$x_max))
  message(sprintf("Detected shape  : %s", shape_label))
  if (!is.na(x$turning_point)) {
    message(sprintf("Turning point   : %.*f", digits, x$turning_point))
    message(sprintf("Delta SE        : %.*f", digits, x$tp_se))
    message(sprintf("%d%% CI (delta)  : [%.*f, %.*f]",
                    x$level, digits, x$tp_ci["lower"], digits, x$tp_ci["upper"]))
  }
  if (!is.na(x$inflection_pt)) {
    message(sprintf("Inflection pt   : %.*f", digits, x$inflection_pt))
  }
  message(strrep("-", 62))
  message(sprintf("Sasabuchi Test for %s:", shape_label))
  message(sprintf("  Slope at lower bound : %.*f  (t = %.*f, p = %.*f)",
                  digits, x$slope_lower, digits, x$t_lower, digits, x$p_lower))
  message(sprintf("  Slope at upper bound : %.*f  (t = %.*f, p = %.*f)",
                  digits, x$slope_upper, digits, x$t_upper, digits, x$p_upper))
  message(sprintf("  t_sac = %.*f  %s   p = %.*f",
                  digits, x$t_sasabuchi, sig, digits, x$p_sasabuchi))
  message(strrep("-", 62))
  if (!is.null(x$twolines)) {
    tl <- x$twolines
    message("Two-Lines Test (Simonsohn 2018):")
    message(sprintf("  Left  slope = %.*f (t = %.*f, p = %.*f, n = %d)",
                    digits, tl$slope_l, digits, tl$t_l,
                    digits, tl$p_l, tl$n_l))
    message(sprintf("  Right slope = %.*f (t = %.*f, p = %.*f, n = %d)",
                    digits, tl$slope_r, digits, tl$t_r,
                    digits, tl$p_r, tl$n_r))
    message(sprintf("  Joint p = %.*f  %s",
                    digits, tl$p_joint, .tptest_stars(tl$p_joint)))
    message(strrep("-", 62))
  }
  message("*** p<0.01, ** p<0.05, * p<0.10")
  invisible(x)
}


#' Summary method for tptest objects
#'
#' @param object An object of class \code{"tptest"}.
#' @param ... Further arguments passed to \code{print.tptest}.
#' @return Invisibly returns \code{object}.
#' @export
summary.tptest <- function(object, ...) {
  print(object, ...)
}


#' @keywords internal
.tptest_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}
