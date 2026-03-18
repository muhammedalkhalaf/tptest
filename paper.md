---
title: 'R Packages for GARCH-Based Unit Root Testing, Residual-Based FM-VAR Estimation, and Turning Point Analysis'
tags:
  - R
  - econometrics
  - GARCH
  - unit root
  - VAR
  - turning point
  - nonlinear relationships
authors:
  - name: Muhammad Abdullah Alkhalaf
    orcid: 0009-0002-2677-9246
    corresponding: true
    email: muhammedalkhalaf@gmail.com
    affiliation: 1
affiliations:
  - name: Rufyq Elngeh for Academic and Business Services, Riyadh, Saudi Arabia
    index: 1
date: 18 March 2026
bibliography: paper.bib
---

# Summary

We present three R packages that address distinct but important gaps in the econometric toolkit: `garchur` for unit root testing under GARCH errors with endogenous structural breaks, `rbfmvar` for Residual-Based Fully Modified VAR estimation accommodating mixed integration orders, and `tptest` for testing U-shaped and inverse U-shaped relationships in regression models. These packages implement methodologies that are widely cited in applied research but previously lacked dedicated R implementations.

# Statement of Need

Applied econometric research frequently encounters three challenges that standard tools handle inadequately:

1. **Conditional heteroskedasticity in unit root testing**: Financial and energy time series often exhibit GARCH-type volatility clustering. Standard ADF tests assume homoskedastic errors, leading to size distortions and power losses. The @NarayanLiu2015 trend-GARCH unit root test with endogenous structural breaks addresses both issues simultaneously, but no R package implements it.

2. **VAR estimation with unknown integration orders**: When a VAR system contains an unknown mixture of I(0), I(1), and I(2) variables, standard OLS-based VAR estimation produces biased inference. The Residual-Based Fully Modified VAR (RBFM-VAR) of @Chang2000, extending the @Phillips1995 FM-VAR, applies nonparametric corrections that yield valid inference regardless of integration orders. This method is particularly valuable when pre-testing for unit roots is inconclusive.

3. **Testing for nonlinear (U-shaped) relationships**: The Environmental Kuznets Curve, the Laffer curve, and numerous other economic theories predict quadratic or other nonlinear functional forms with turning points. While ad hoc tests of quadratic coefficient significance are common, the formal @LindMehlum2010 test provides proper inference on whether a true extreme point exists within the data range. No comprehensive R package implements this test with extensions to cubic, log-quadratic, and inverse forms.

# Packages

## garchur

Implements the trend-GARCH(1,1) unit root test with endogenous structural breaks proposed by @NarayanLiu2015. The procedure estimates a mean equation with a linear trend and level-shift dummies at endogenously determined break dates while allowing for GARCH(1,1) conditional heteroskedasticity. Supports up to three breaks with critical values interpolated from Monte Carlo simulation tables.

```r
library(garchur)
result <- garchur(y, max_breaks = 2, trim = 0.15)
summary(result)
# Output includes:
# - GARCH(1,1) parameter estimates (omega, alpha, beta)
# - Unit root t-statistic
# - Endogenous break dates
# - Critical values at 1%, 5%, 10%
```

## rbfmvar

Implements the Residual-Based Fully Modified VAR (RBFM-VAR) estimator of @Chang2000 for VAR models containing an unknown mixture of I(0), I(1), and I(2) components. The procedure applies a nonparametric long-run variance correction to OLS residuals, enabling valid inference without prior knowledge of integration orders. Supports lag selection by AIC, BIC, and Hannan-Quinn criteria, modified Wald Granger non-causality tests, and impulse response functions.

```r
library(rbfmvar)
# Estimate RBFM-VAR without specifying integration orders
result <- rbfmvar(data = macro_data, vars = c("gdp", "inflation",
                  "interest_rate"), lag_select = "bic")
summary(result)

# Modified Wald Granger causality test
granger <- rbfm_granger(result, cause = "inflation",
                        effect = "gdp")

# Impulse response functions
irf <- rbfm_irf(result, impulse = "interest_rate",
                response = "gdp", horizon = 20)
plot(irf)
```

## tptest

Tests for U-shaped and inverse U-shaped (hump-shaped) nonlinear relationships in regression models. Extends @LindMehlum2010 to quadratic, cubic, log-quadratic, and inverse functional forms. Implements the @Sasabuchi1980 joint slope test, delta-method standard errors for the turning point, Fieller confidence intervals, and the @Simonsohn2018 two-lines robustness test. Works as a post-estimation command after any model returning coefficient vectors and variance-covariance matrices.

```r
library(tptest)
# Fit a quadratic model
model <- lm(y ~ x + I(x^2), data = ekc_data)

# Test for U-shape or inverse U-shape
result <- tptest(model, var = "x", form = "quadratic")
summary(result)
# Output includes:
# - Sasabuchi joint slope test (H0: monotone or inverse U)
# - Estimated turning point with delta-method SE
# - Fieller 95% confidence interval
# - Whether turning point is within data range

# Two-lines robustness test
robust <- tptest(model, var = "x", form = "quadratic",
                 two_lines = TRUE)
```

# References
