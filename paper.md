---
title: 'tptest, garchur, and rbfmvar: R Packages for Turning Point Tests, GARCH Unit Roots, and Fully Modified VAR'
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

Three R packages address gaps in the applied econometrics toolkit. `tptest` tests for U-shaped and inverse U-shaped relationships using the @Sasabuchi1980 likelihood-ratio-like procedure formalized by @LindMehlum2010, with extensions to cubic, log-quadratic, and inverse forms, Fieller confidence intervals, and the @Simonsohn2018 two-lines test. `garchur` implements the @NarayanLiu2015 trend-GARCH(1,1) unit root test with endogenous structural breaks. `rbfmvar` estimates the Residual-Based Fully Modified VAR of @Chang2000, extending @Phillips1995, for systems with unknown mixtures of I(0), I(1), and I(2) variables. All packages are open-source under GPL-3.

# Statement of Need

**Turning point tests.** The Environmental Kuznets Curve, the Laffer curve, and other economic theories predict nonlinear relationships with an extreme point. A significant quadratic coefficient does not establish a U-shape or inverse U-shape over the relevant data range; the @LindMehlum2010 test provides proper joint inference on slopes at both interval endpoints. No existing R package implements this test with extensions to cubic, log-quadratic, and inverse functional forms.

**GARCH unit root testing.** Financial and energy time series exhibit volatility clustering. Standard ADF tests assume homoskedastic errors, producing size distortions under GARCH effects. The @NarayanLiu2015 test jointly models GARCH(1,1) errors and endogenous structural breaks, but no R implementation exists.

**VAR with mixed integration orders.** When a VAR system contains an unknown mixture of I(0), I(1), and I(2) variables, standard OLS produces biased inference. The RBFM-VAR of @Chang2000 applies nonparametric long-run variance corrections yielding mixed-normal asymptotics regardless of integration orders, but no R package implements it.

# Packages

## tptest

Tests for U-shaped and inverse U-shaped relationships as a post-estimation command. Extends @LindMehlum2010 to quadratic, cubic, log-quadratic, and inverse forms. Implements the @Sasabuchi1980 joint slope test, delta-method and Fieller confidence intervals, and the @Simonsohn2018 two-lines robustness test.

```r
library(tptest)
fit <- lm(y ~ x + I(x^2), data = ekc_data)

result <- tptest(fit, var1 = "x", var2 = "I(x^2)",
                 model = "quad", fieller = TRUE,
                 twolines = TRUE, data = ekc_data)
print(result)
```

## garchur

Implements the trend-GARCH(1,1) unit root test of @NarayanLiu2015 with endogenous structural breaks. Supports constant-only (`"c"`) and constant-plus-trend (`"ct"`) specifications with up to three breaks. Critical values are interpolated from Monte Carlo simulation tables.

```r
library(garchur)
result <- garchur(y, breaks = 2, model = "ct", trim = 0.15)
print(result)
```

## rbfmvar

Estimates the Residual-Based Fully Modified VAR of @Chang2000 for systems with unknown integration orders. Supports lag selection by AIC/BIC/HQ, Bartlett/Parzen/quadratic-spectral kernels for long-run variance estimation, modified Wald Granger non-causality tests, and Cholesky impulse response functions.

```r
library(rbfmvar)
result <- rbfmvar(macro_data, lags = 2, kernel = "bartlett")
print(result)
```

# References
