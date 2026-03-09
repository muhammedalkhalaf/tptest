.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "tptest: Universal Turning Point and Inflection Point Tests\n",
    "Version ", utils::packageVersion("tptest"), "\n",
    "Based on Lind & Mehlum (2010) <doi:10.1111/j.1468-0084.2009.00569.x>"
  )
}
