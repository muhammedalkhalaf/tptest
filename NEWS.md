# tptest 1.1.0

This release corrects several computations. Results from version 1.0.3 for
the inverse and log-quadratic forms, for Fieller intervals, for generalized
linear models and for the cubic form should be recomputed.

## Corrected computations

* Fieller interval for the inverse form (`y = b1*x + b2/x`): the covariance
  term in the boundary points of the set for `b2/b1` had the wrong sign. The
  boundary points are now `(b1*b2 - s12*T^2 +/- T*sqrt(D)) / (b1^2 - s11*T^2)`,
  which matches a brute-force inversion of the defining inequality (in the
  audit example the package gave [1.9391, 2.0923] where the correct interval
  is [1.9142, 2.0693]; grid inversion gives [1.91425, 2.06926]).
* Log-quadratic form: the regressor named in `vars[1]` is `ln(x)`, so the
  automatic data range is the minimum and maximum of that column. Version
  1.0.3 took the logarithm of that column a second time and evaluated the
  slopes at `log(log(x))`. User-supplied `min` and `max` are now on the
  `ln(x)` scale by default; the new argument `bounds_scale = "levels"` accepts
  them in levels of `x`. The turning point is reported in levels,
  `exp(-b1/(2*b2))`.
* Fieller critical value: `t(df)` with the residual degrees of freedom of the
  model is used for `lm`-type models (normal for `glm` and for the `coefs`
  path), consistent with the Sasabuchi p-value and the delta-method interval
  in the same object. `fieller_ci()` gains a `df` argument. `confint()` at a
  level other than the one used in the call now uses the same distribution
  instead of always using the normal.
* Fieller edge cases: when the denominator coefficient is not significant at
  the chosen level but the discriminant is positive, the confidence set is the
  union of two rays and is reported as `type = "two_rays"` with both boundary
  points (version 1.0.3 reported the whole real line). For the inverse form,
  when the set for `b2/b1` contains zero the set for `x* = sqrt(b2/b1)` is
  reported as `(0, hi]` (`lo = 0`) instead of `NA`. The types `"ray"` and
  `"empty"` are added; `"entire_real_line"` is replaced by `"unbounded"`,
  which is now used only when the set is the whole (positive) line.
* Inverse form, delta-method standard error: the gradient of
  `x* = sqrt(b2/b1)` was written with `sqrt(b1)` and `sqrt(b2)` separately
  and returned `NaN` for an inverse U shape (`b1 < 0`, `b2 < 0`). It is now
  `(-0.5*sqrt(b2/b1)/b1, 0.5/(b1*sqrt(b2/b1)))`.
* Degrees of freedom: `t(df.residual)` is used for `lm` and `plm` models.
  For `glm` objects and for the `coefs` path the normal distribution is used;
  this is the package's choice, Lind and Mehlum (2010, Section 2) only state
  that the test is asymptotically valid for generalized linear models. The
  new argument `df` overrides this choice (`Inf` forces the normal
  distribution).
* Cubic form: the two-endpoint test requires the slope to be monotone on the
  interval (Lind and Mehlum 2010, footnote 3), which does not hold for a cubic
  across its inflection point. The interval is now split at the inflection
  point (the estimate, treated as fixed, so the sub-interval tests do not have
  exact size) and the test is applied on each monotone sub-interval; the
  results are in `sasabuchi$segments` and are labelled as a package
  extension. `t_overall` and `p_overall` are then `NA`. When the inflection
  point lies outside the interval the single segment is the test of equation
  (9) of the paper with H = 3 and is reported as the overall test.
* Inverse form: `min` must be positive (the slope `-1/x^2` is not defined at
  0); an error is raised otherwise.
* Automatic data range: the minimum and maximum are taken over the estimation
  sample (the rows used by the model, from its model frame), not over all rows
  of `data`. `data` is no longer required for the range when the regressor can
  be recovered from the model.

## Changed

* Log-quadratic form: the delta-method confidence interval for the turning
  point is computed on the `ln(x)` scale and exponentiated (package choice;
  version 1.0.3 applied the delta method to `exp(-b1/(2*b2))` directly).
  `tp_log`, `tp_log_se` and `bounds_levels` are returned in addition, and the
  Fieller set is now available for this form (the exponentiated quadratic set).

## Output changes

* When the fitted extremum lies outside the interval the Sasabuchi statistic
  `min(-t_l, t_h)` (or `min(t_l, -t_h)`) and its p-value are reported (both
  are negative and above 0.5, respectively) together with the message that H0
  cannot be rejected, instead of `NA` and the message "trivial failure".
* `shape` describes the fitted curve on the interval ("U shape", "Inverse U
  shape", or a monotone label when the extremum is outside); the new element
  `alternative` gives the alternative hypothesis tested.
* `p_min` and `p_max` are the one-sided p-values of the two component tests
  under the tested alternative and are labelled "P (one-sided)".
* `sasabuchi$outside` and `df` are returned; the print method shows the
  distribution used.
* `plot()` draws the log-quadratic form on the `ln(x)` scale.

## Tests

* A testthat suite pins the endpoint t-statistics, the Sasabuchi statistic,
  the delta-method standard errors and the Fieller sets for the quadratic,
  inverse and log-quadratic forms to hand calculations from `lm()`
  coefficients and covariance matrices and to brute-force inversion of the
  Fieller inequality, and compares the coefs-path results approximately with
  Table 1 of Lind and Mehlum (2010) (the covariance of the coefficients is
  backed out from the rounded standard errors printed there).

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
