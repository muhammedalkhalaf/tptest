\name{tptest}
\alias{tptest}
\title{Universal Turning Point and Inflection Point Test}
\usage{
tptest(object, var1, var2, var3 = NULL,
       x_min = NULL, x_max = NULL, data = NULL,
       model = NULL, level = 95,
       fieller = FALSE, twolines = FALSE)
}
\arguments{
\item{object}{A fitted model object with \code{coef()} and \code{vcov()}
methods (e.g., output of \code{lm}).}

\item{var1}{Character string. Name of the linear term (e.g., \code{"x"}).}

\item{var2}{Character string. Name of the squared (or inverse, or
log-squared) term (e.g., \code{"I(x^2)"}).}

\item{var3}{Character string or \code{NULL}. Name of the cubic term for
cubic specifications. Default \code{NULL}.}

\item{x_min}{Numeric or \code{NULL}. Lower bound of the evaluation
interval. Defaults to the minimum of the predictor in \code{data}.}

\item{x_max}{Numeric or \code{NULL}. Upper bound of the evaluation
interval. Defaults to the maximum of the predictor in \code{data}.}

\item{data}{Optional data frame used to determine the range of
\code{var1} when \code{x_min}/\code{x_max} are not supplied.}

\item{model}{Character string specifying the functional form: one of
\code{"quad"} (quadratic, default), \code{"cubic"} (requires
\code{var3}), \code{"inv"} (inverse), or \code{"logquad"}
(log-quadratic).}

\item{level}{Numeric (0, 100). Confidence level. Default \code{95}.}

\item{fieller}{Logical. If \code{TRUE}, compute a Fieller confidence
interval. Default \code{FALSE}.}

\item{twolines}{Logical. If \code{TRUE}, perform the Simonsohn (2018)
two-lines test. Requires \code{data}. Default \code{FALSE}.}
}
\value{
An object of class \code{"tptest"} with components:
\describe{
  \item{turning_point}{Estimated turning/extreme point.}
  \item{tp_se}{Delta-method standard error.}
  \item{tp_ci}{Named vector: \code{lower} and \code{upper} delta-method CI.}
  \item{slope_lower}{Slope at the lower bound.}
  \item{slope_upper}{Slope at the upper bound.}
  \item{t_lower}{t-statistic at the lower bound.}
  \item{t_upper}{t-statistic at the upper bound.}
  \item{p_lower}{One-sided p-value at the lower bound.}
  \item{p_upper}{One-sided p-value at the upper bound.}
  \item{t_sasabuchi}{Sasabuchi joint test statistic.}
  \item{p_sasabuchi}{p-value of the Sasabuchi test.}
  \item{shape}{Detected shape: \code{"U"} or \code{"Inverse U"}.}
  \item{model}{Functional form used.}
  \item{x_min}{Lower bound used.}
  \item{x_max}{Upper bound used.}
  \item{level}{Confidence level.}
  \item{fieller_ci}{Fieller CI bounds (if requested).}
  \item{df}{Residual degrees of freedom.}
  \item{b1}{Coefficient on \code{var1}.}
  \item{b2}{Coefficient on \code{var2}.}
  \item{b3}{Coefficient on \code{var3} (cubic only).}
  \item{second_tp}{Second turning point (cubic only).}
  \item{inflection_pt}{Inflection point (cubic only).}
  \item{twolines}{Two-lines test results (if requested).}
}
}
\description{
Tests for U-shaped or inverse U-shaped (hump-shaped) nonlinear
relationships using the Sasabuchi (1980) joint slope test. Extends
Lind and Mehlum (2010) to quadratic, cubic, log-quadratic, and inverse
functional forms.
}
\examples{
set.seed(1)
n  <- 100
x  <- runif(n, -3, 3)
y  <- -0.5 * x + 0.3 * x^2 + rnorm(n, sd = 0.5)
df <- data.frame(y = y, x = x, x2 = x^2)
fit <- lm(y ~ x + x2, data = df)
res <- tptest(fit, var1 = "x", var2 = "x2", data = df)
print(res)
}
\references{
Lind, J. T. and Mehlum, H. (2010). With or Without U? The Appropriate
Test for a U-Shaped Relationship. \emph{Oxford Bulletin of Economics
and Statistics}, 72(1), 109--118.
\doi{10.1111/j.1468-0084.2009.00569.x}

Sasabuchi, S. (1980). A Test of a Multivariate Normal Mean with
Composite Hypotheses Determined by Linear Inequalities.
\emph{Biometrika}, 67(2), 429--439.
\doi{10.1093/biomet/67.2.429}

Simonsohn, U. (2018). Two Lines: A Valid Alternative to the Invalid
Testing of U-Shaped Relationships with Quadratic Regressions.
\emph{Advances in Methods and Practices in Psychological Science},
1(4), 538--555. \doi{10.1177/2515245918805755}
}
