pkgname <- "tptest"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "tptest-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('tptest')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("ekc")
### * ekc

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ekc
### Title: Environmental Kuznets Curve Data
### Aliases: ekc
### Keywords: datasets

### ** Examples

data(ekc)
head(ekc)

# Fit quadratic model
fit <- lm(emissions ~ gdp + gdp_sq, data = ekc)
summary(fit)

# Test for inverse U-shape
result <- tptest(fit, vars = c("gdp", "gdp_sq"), data = ekc)
print(result)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ekc", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tptest")
### * tptest

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tptest
### Title: Universal Turning Point and Inflection Point Test
### Aliases: tptest

### ** Examples

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

## No test: 
# With Fieller interval and two-lines test
result2 <- tptest(fit, vars = c("x", "x_sq"),
                  fieller = TRUE, twolines = TRUE,
                  data = dat, depvar = "y")
summary(result2)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tptest", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
