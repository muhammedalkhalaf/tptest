# tptest: Universal Turning Point and Inflection Point Tests

## Overview

The `tptest` package implements tests for U-shaped and inverse U-shaped relationships in regression analysis. It provides a comprehensive framework for detecting turning points and inflection points in time series and panel data.

## Key Features

- **Sasabuchi (1980) / Lind-Mehlum (2010) Test**: Rigorous test for U-shape or inverse U-shape
- **Multiple Functional Forms**: Quadratic, cubic, inverse, and log-quadratic specifications
- **Delta-Method Standard Errors**: Proper inference for turning point estimates
- **Fieller Confidence Intervals**: Robust intervals when denominator is uncertain
- **Simonsohn (2018) Two-Lines Test**: Alternative U-shape validation
- **Parametric Bootstrap**: Bootstrap confidence intervals

## Installation

```r
# Install from CRAN (when available)
install.packages("tptest")

# Install development version from GitHub
devtools::install_github("muhammedalkhalaf/tptest")
```

## Usage

```r
library(tptest)

# Simulate data with U-shaped relationship
set.seed(42)
n <- 200
x <- runif(n, 1, 10)
y <- 50 - 8*x + 0.5*x^2 + rnorm(n, sd = 5)
dat <- data.frame(y = y, x = x, x_sq = x^2)

# Fit quadratic model
fit <- lm(y ~ x + x_sq, data = dat)

# Test for U-shape
result <- tptest(fit, vars = c("x", "x_sq"), data = dat)
print(result)
```

## Output

```
==========================================
  Turning Point Test (Lind & Mehlum 2010)
==========================================

Model form: Quadratic: y = b1*x + b2*x^2
Data interval: [1.023, 9.987]

Detected shape: U shape
Turning point (x*): 8.234
  Delta-method SE:  0.5123
  95% CI:         [7.230, 9.238]

------------------------------------------
Sasabuchi (1980) Test
------------------------------------------
                Lower bound    Upper bound
Interval            1.0230         9.9870
Slope              -6.2456         1.7532
t-value           -12.4532         3.4521
P>|t|               0.0000         0.0003

Overall test: t = 3.4521, p = 0.000312 ***
-> Strong evidence of U shape (p < 0.01)
------------------------------------------
*** p<0.01, ** p<0.05, * p<0.10
```

## Environmental Kuznets Curve Example

```r
# Load example data
data(ekc)

# Fit model
fit <- lm(emissions ~ gdp + gdp_sq, data = ekc)

# Test for inverse U-shape
result <- tptest(fit, vars = c("gdp", "gdp_sq"), 
                 fieller = TRUE, data = ekc)
summary(result)
plot(result)
```

## References

- Lind, J. T., & Mehlum, H. (2010). With or without U? The appropriate test for a U-shaped relationship. *Oxford Bulletin of Economics and Statistics*, 72(1), 109-118. https://doi.org/10.1111/j.1468-0084.2009.00569.x

- Sasabuchi, S. (1980). A test of a multivariate normal mean with composite hypotheses determined by linear inequalities. *Biometrika*, 67(2), 429-439.

- Fieller, E. C. (1954). Some problems in interval estimation. *Journal of the Royal Statistical Society: Series B*, 16(2), 175-185. https://doi.org/10.1111/j.2517-6161.1954.tb00159.x

- Simonsohn, U. (2018). Two lines: A valid alternative to the invalid testing of U-shaped relationships with quadratic regressions. *Advances in Methods and Practices in Psychological Science*, 1(4), 538-555.

## License

GPL-3

## Author

