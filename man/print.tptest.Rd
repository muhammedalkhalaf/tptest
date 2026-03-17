\name{print.tptest}
\alias{print.tptest}
\alias{summary.tptest}
\title{Print and Summary Methods for tptest Objects}
\usage{
\method{print}{tptest}(x, digits = 4L, ...)

\method{summary}{tptest}(object, ...)
}
\arguments{
\item{x}{An object of class \code{"tptest"}.}

\item{digits}{Integer. Significant digits for numeric output.}

\item{...}{Further arguments (ignored).}

\item{object}{An object of class \code{"tptest"}.}
}
\value{
Invisibly returns the input object.
}
\description{
Print and summary methods for objects of class \code{"tptest"} returned
by \code{\link{tptest}}.
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
summary(res)
}
