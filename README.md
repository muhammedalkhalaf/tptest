# tptest

**Universal Turning Point and Inflection Point Test** for R

## Overview

`tptest` tests for U-shaped and inverse U-shaped (hump-shaped) nonlinear
relationships using the Sasabuchi (1980) joint slope test, extending
Lind and Mehlum (2010) to multiple functional forms. Works as a
post-estimation command after any model that exposes `coef()` and `vcov()`.

## Supported Functional Forms

| `model`     | Specification                                     |
|-------------|---------------------------------------------------|
| `"quad"`    | y = β₁·x + β₂·x² (default)                       |
| `"cubic"`   | y = β₁·x + β₂·x² + β₃·x³                        |
| `"inv"`     | y = β₁·x + β₂·(1/x)                              |
| `"logquad"` | ln(y) = β₁·ln(x) + β₂·[ln(x)]²                  |

## Installation

```r
install.packages("tptest")
```

## Usage

```r
library(tptest)

set.seed(1)
n  <- 200
x  <- runif(n, 0, 10)
y  <- 2 * x - 0.2 * x^2 + rnorm(n)
df <- data.frame(y = y, x = x, x2 = x^2)

fit <- lm(y ~ x + x2, data = df)
res <- tptest(fit, var1 = "x", var2 = "x2", data = df,
              fieller = TRUE, twolines = TRUE)
print(res)
```

## References

Lind, J. T. and Mehlum, H. (2010). With or Without U? The Appropriate Test
for a U-Shaped Relationship. *Oxford Bulletin of Economics and Statistics*,
72(1), 109–118.

Sasabuchi, S. (1980). A Test of a Multivariate Normal Mean with Composite
Hypotheses Determined by Linear Inequalities. *Biometrika*, 67(2), 429–439.

Simonsohn, U. (2018). Two Lines: A Valid Alternative to the Invalid Testing
of U-Shaped Relationships. *Advances in Methods and Practices in
Psychological Science*, 1(4), 538–555.

## License

GPL-3
