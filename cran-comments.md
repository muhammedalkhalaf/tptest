## tptest 1.1.0

This release corrects the computations below; the version on CRAN is 1.0.3.

* Fieller interval for the inverse form: the covariance term in the boundary points had the wrong sign; corrected and checked against brute-force inversion.
* Log-quadratic form: the automatic data range took the logarithm of the ln(x) regressor a second time; the range is now the range of that column, with a new `bounds_scale` argument for bounds given in levels, and the turning point interval is computed on the ln(x) scale and exponentiated.
* Fieller critical value and `confint()` at a non-default level now use t(df) for lm-type models and the normal distribution for glm and the `coefs` path, consistent with the Sasabuchi p-value and the delta-method interval.
* Fieller edge cases: the union of two rays is reported as such with both boundary points; the inverse-form set (0, hi] is reported instead of NA.
* Inverse form: delta-method gradient corrected (returned NaN for b1 < 0, b2 < 0).
* Normal distribution for glm objects and the `coefs` path (package choice; the paper only states asymptotic validity for GLMs), t(df.residual) for lm and plm; new `df` argument.
* Inverse form: `min` must be positive; an error is raised otherwise.
* Cubic form: the two-endpoint test is applied on each monotone sub-interval of the slope (split at the estimated inflection point, labelled as a package extension); with the inflection point outside the interval the single-segment test is reported as the overall test.
* Log-quadratic form: the delta-method interval is computed on the ln(x) scale and exponentiated (package choice); Fieller set now available for this form.
* Automatic data range uses the estimation sample, not all rows of `data`.
* Output: the Sasabuchi statistic and p-value are reported when the extremum is outside the interval; one-sided p-values are labelled as such; shape labels for monotone fits.
* testthat suite added.

## Test environments

* Ubuntu 24.04, R 4.3.3 and R-devel, R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 0 notes
