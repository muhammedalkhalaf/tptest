#' Environmental Kuznets Curve Data
#'
#' Simulated data representing the Environmental Kuznets Curve (EKC) hypothesis,
#' which posits an inverse U-shaped relationship between environmental degradation
#' and economic development.
#'
#' @format A data frame with 500 observations and 6 variables:
#' \describe{
#'   \item{country}{Country identifier (1-50)}
#'   \item{year}{Year (2000-2009)}
#'   \item{gdp}{GDP per capita (thousands of dollars)}
#'   \item{gdp_sq}{Squared GDP per capita}
#'   \item{emissions}{CO2 emissions (tons per capita)}
#'   \item{emissions_log}{Log of CO2 emissions}
#' }
#'
#' @details
#' This is simulated panel data designed to demonstrate the tptest package.
#' The true data generating process follows an inverse U-shape with:
#' \itemize{
#'   \item Turning point at GDP ~ 25,000 USD per capita
#'   \item Country-specific fixed effects
#'   \item Year-specific trends
#' }
#'
#' @examples
#' data(ekc)
#' head(ekc)
#'
#' # Fit quadratic model
#' fit <- lm(emissions ~ gdp + gdp_sq, data = ekc)
#' summary(fit)
#'
#' # Test for inverse U-shape
#' result <- tptest(fit, vars = c("gdp", "gdp_sq"), data = ekc)
#' print(result)
#'
#' @source Simulated data for demonstration purposes.
#' @references
#' Grossman, G. M., & Krueger, A. B. (1995). Economic growth and the environment.
#' \emph{Quarterly Journal of Economics}, 110(2), 353-377.
"ekc"
