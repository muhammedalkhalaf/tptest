#' @title Methods for tptest Objects
#' @name tptest-methods
#' @description Print, summary, plot, and accessor methods for \code{tptest} objects.
NULL


#' @rdname tptest-methods
#' @param x A \code{tptest} object
#' @param ... Additional arguments (currently ignored)
#' @export
print.tptest <- function(x, ...) {
  cat("\n")
  cat("==========================================\n")
  cat("  Turning Point Test (Lind & Mehlum 2010)\n")
  cat("==========================================\n\n")

  # Model info
  cat("Model form:", .form_label(x$model_form), "\n")
  cat("Data interval: [", format(x$bounds["min"], digits = 4), ", ",
      format(x$bounds["max"], digits = 4), "]\n", sep = "")
  cat("\n")

  # Shape and turning point
  cat("Detected shape:", x$shape, "\n")
  if (!is.na(x$tp)) {
    cat("Turning point (x*): ", format(x$tp, digits = 6), "\n", sep = "")
    if (!is.null(x$tp_se) && !is.na(x$tp_se)) {
      cat("  Delta-method SE:  ", format(x$tp_se, digits = 6), "\n", sep = "")
    }
    if (!is.null(x$tp_ci)) {
      cat("  ", round(x$level * 100), "% CI:         [",
          format(x$tp_ci[1], digits = 6), ", ",
          format(x$tp_ci[2], digits = 6), "]\n", sep = "")
    }
  } else {
    cat("Turning point: not found\n")
  }

  # Cubic extras
  if (x$model_form == "cubic") {
    if (!is.null(x$tp2) && !is.na(x$tp2)) {
      cat("Second turning pt:  ", format(x$tp2, digits = 6), "\n", sep = "")
    }
    if (!is.null(x$inflection)) {
      cat("Inflection point:   ", format(x$inflection, digits = 6), "\n", sep = "")
    }
  }
  cat("\n")

  # Sasabuchi test
  cat("------------------------------------------\n")
  cat("Sasabuchi (1980) Test\n")
  cat("------------------------------------------\n")
  cat(sprintf("                Lower bound    Upper bound\n"))
  cat(sprintf("Interval        %11.4f    %11.4f\n", x$bounds["min"], x$bounds["max"]))
  cat(sprintf("Slope           %11.4f    %11.4f\n",
              x$sasabuchi$slope_min, x$sasabuchi$slope_max))

  if (!is.na(x$sasabuchi$t_overall)) {
    cat(sprintf("t-value         %11.4f    %11.4f\n",
                x$sasabuchi$t_min, x$sasabuchi$t_max))
    cat(sprintf("P>|t|           %11.4f    %11.4f\n",
                x$sasabuchi$p_min, x$sasabuchi$p_max))
    cat("\n")

    stars <- .get_stars(x$sasabuchi$p_overall)
    cat(sprintf("Overall test: t = %.4f, p = %.6f %s\n",
                x$sasabuchi$t_overall, x$sasabuchi$p_overall, stars))

    cat(.interpret_pvalue(x$sasabuchi$p_overall, x$shape), "\n")
  } else {
    cat("\nExtremum outside interval - trivial failure to reject H0\n")
  }

  cat("------------------------------------------\n")
  cat("*** p<0.01, ** p<0.05, * p<0.10\n\n")

  invisible(x)
}


#' @rdname tptest-methods
#' @param object A \code{tptest} object
#' @export
summary.tptest <- function(object, ...) {
  # Print basic results
  print.tptest(object)

  # Fieller interval
  if (!is.null(object$fieller)) {
    cat("Fieller Confidence Interval\n")
    cat("------------------------------------------\n")
    if (object$fieller$type == "bounded") {
      cat(sprintf("%d%% CI: [%.6f, %.6f]\n",
                  round(object$level * 100), object$fieller$lo, object$fieller$hi))
    } else if (object$fieller$type == "unbounded") {
      cat("Interval: (-Inf, +Inf) - unbounded\n")
    } else if (object$fieller$type == "entire_real_line") {
      cat("Interval: (-Inf, +Inf) - entire real line\n")
    } else {
      cat("Fieller interval: cannot be computed\n")
    }
    cat("\n")
  }

  # Two-lines test
  if (!is.null(object$twolines)) {
    tl <- object$twolines
    cat("Simonsohn (2018) Two-Lines Test\n")
    cat("------------------------------------------\n")
    cat(sprintf("Split at x* = %.4f\n\n", object$tp))
    cat(sprintf("           Left (x <= x*)    Right (x > x*)\n"))
    cat(sprintf("N          %14d    %15d\n", tl$n_l, tl$n_r))
    cat(sprintf("Slope      %14.4f    %15.4f\n", tl$slope_l, tl$slope_r))
    cat(sprintf("t-value    %14.4f    %15.4f\n", tl$t_l, tl$t_r))
    cat(sprintf("P>|t|      %14.4f    %15.4f\n", tl$p_l, tl$p_r))
    cat("\n")
    stars <- .get_stars(tl$p_joint)
    cat(sprintf("Joint p-value: %.6f %s\n", tl$p_joint, stars))
    if (tl$confirms) {
      cat("-> Two-lines test confirms", object$shape, "\n")
    } else {
      cat("-> Two-lines test does not confirm", object$shape, "\n")
    }
    cat("\n")
  }

  # Bootstrap
  if (!is.null(object$bootstrap)) {
    bs <- object$bootstrap
    cat("Parametric Bootstrap\n")
    cat("------------------------------------------\n")
    cat(sprintf("Replications: %d\n", bs$reps))
    cat(sprintf("Bootstrap SE:    %.6f\n", bs$se))
    cat(sprintf("Bootstrap bias:  %.6f\n", bs$bias))
    cat(sprintf("%d%% CI (percentile): [%.6f, %.6f]\n",
                round(object$level * 100), bs$ci[1], bs$ci[2]))
    cat("\n")
  }

  invisible(object)
}


#' @rdname tptest-methods
#' @param main Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @param col.line Color for the fitted curve
#' @param col.tp Color for the turning point marker
#' @param col.ci Color for confidence band
#' @param lwd Line width
#' @param n Number of points for plotting curve
#' @export
plot.tptest <- function(x, main = NULL, xlab = "x", ylab = "Marginal Effect",
                        col.line = "steelblue", col.tp = "red",
                        col.ci = grDevices::rgb(0.2, 0.4, 0.8, 0.2),
                        lwd = 2, n = 200, ...) {
  if (is.na(x$tp)) {
    message("No turning point to plot.")
    return(invisible(x))
  }

  # Get coefficients
  b <- x$coefficients
  b1 <- b["b1"]
  b2 <- b["b2"]
  b3 <- if (length(b) == 3) b["b3"] else 0

  # Generate x values
  x_seq <- seq(x$bounds["min"], x$bounds["max"], length.out = n)

  # Compute marginal effect (derivative)
  if (x$model_form == "quadratic") {
    me <- b1 + 2 * b2 * x_seq
  } else if (x$model_form == "cubic") {
    me <- b1 + 2 * b2 * x_seq + 3 * b3 * x_seq^2
  } else if (x$model_form == "inverse") {
    me <- b1 - b2 / (x_seq^2)
  } else if (x$model_form == "logquadratic") {
    me <- b1 + 2 * b2 * log(x_seq)
  }

  if (is.null(main)) {
    main <- paste("Turning Point Test:", x$shape)
  }

  # Plot
  graphics::plot(x_seq, me, type = "n", main = main, xlab = xlab, ylab = ylab,
                 ylim = range(c(me, 0)) * c(1.1, 1.1), ...)

  # Add zero line
  graphics::abline(h = 0, col = "gray60", lty = 2)

  # Add the curve
  graphics::lines(x_seq, me, col = col.line, lwd = lwd)

  # Mark turning point
  if (!is.na(x$tp)) {
    tp_x <- x$tp
    tp_y <- 0  # At the turning point, marginal effect = 0

    graphics::points(tp_x, tp_y, pch = 19, col = col.tp, cex = 1.5)

    # Add CI if available
    if (!is.null(x$tp_ci) && !any(is.na(x$tp_ci))) {
      graphics::arrows(x$tp_ci[1], tp_y, x$tp_ci[2], tp_y,
                       angle = 90, code = 3, length = 0.1, col = col.tp, lwd = 2)
    }
  }

  # Add legend
  legend_text <- c(
    "Marginal Effect",
    paste0("Turning Point = ", format(x$tp, digits = 4))
  )
  if (!is.null(x$sasabuchi$p_overall) && !is.na(x$sasabuchi$p_overall)) {
    legend_text <- c(legend_text,
                     paste0("p-value = ", format(x$sasabuchi$p_overall, digits = 4)))
  }

  graphics::legend("topright", legend = legend_text,
                   col = c(col.line, col.tp, NA),
                   lty = c(1, NA, NA), pch = c(NA, 19, NA),
                   lwd = c(lwd, NA, NA), bty = "n", cex = 0.9)

  invisible(x)
}


#' @rdname tptest-methods
#' @export
coef.tptest <- function(object, ...) {
  c(turning_point = object$tp,
    tp_se = object$tp_se)
}


#' @rdname tptest-methods
#' @param parm Parameter specification (currently ignored)
#' @param level Confidence level (default uses level from tptest object)
#' @export
confint.tptest <- function(object, parm = NULL, level = NULL, ...) {
  if (is.null(level)) level <- object$level

  # If different level requested, recompute
  if (!is.null(object$tp_ci) && level == object$level) {
    ci <- object$tp_ci
  } else if (!is.na(object$tp_se)) {
    crit <- stats::qnorm(1 - (1 - level) / 2)
    ci <- c(object$tp - crit * object$tp_se, object$tp + crit * object$tp_se)
  } else {
    ci <- c(NA, NA)
  }

  ci_mat <- matrix(ci, nrow = 1)
  rownames(ci_mat) <- "turning_point"
  colnames(ci_mat) <- c(paste0((1 - level) / 2 * 100, "%"),
                        paste0((1 - (1 - level) / 2) * 100, "%"))
  ci_mat
}


#' @noRd
.form_label <- function(form) {
  switch(form,
    "quadratic" = "Quadratic: y = b1*x + b2*x^2",
    "cubic" = "Cubic: y = b1*x + b2*x^2 + b3*x^3",
    "inverse" = "Inverse: y = b1*x + b2/x",
    "logquadratic" = "Log-Quadratic: ln(y) = b1*ln(x) + b2*[ln(x)]^2",
    form
  )
}


#' @noRd
.get_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}


#' @noRd
.interpret_pvalue <- function(p, shape) {
  if (is.na(p)) return("")
  if (p < 0.01) {
    paste("-> Strong evidence of", shape, "(p < 0.01)")
  } else if (p < 0.05) {
    paste("-> Evidence of", shape, "(p < 0.05)")
  } else if (p < 0.10) {
    paste("-> Weak evidence of", shape, "(p < 0.10)")
  } else {
    paste("-> Cannot reject H0: Monotone or opposite shape")
  }
}
