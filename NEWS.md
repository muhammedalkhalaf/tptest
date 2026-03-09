# tptest 1.0.0

## Initial Release

This is the first release of the `tptest` package, a port of the Stata `tptest` command for R.

### Features

* `tptest()`: Main function for turning point and inflection point tests
  - Sasabuchi (1980) / Lind-Mehlum (2010) test for U-shape detection
  - Support for quadratic, cubic, inverse, and log-quadratic forms
  - Delta-method standard errors and confidence intervals
  - Works with `lm`, `glm`, and other standard model objects

* `fieller_ci()`: Fieller (1954) confidence intervals for turning points
  - More robust when denominator coefficient has high uncertainty
  - Handles bounded, unbounded, and degenerate cases

* `twolines_test()`: Simonsohn (2018) two-lines test
  - Alternative validation for U-shaped relationships
  - Splits data at turning point and tests slope signs

* Parametric bootstrap confidence intervals

* S3 methods: `print()`, `summary()`, `plot()`, `coef()`, `confint()`

### Data

* `ekc`: Environmental Kuznets Curve example dataset
  - Simulated panel data for 50 countries over 10 years
  - Demonstrates inverse U-shaped relationship

### Documentation

* Complete roxygen2 documentation with examples
* References to key papers with DOIs
* README with usage examples
