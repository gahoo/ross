.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Ross is a OSS API Wrapper for R.")
  op <- options()
  op.ross <- list(
    ross.location = "beijing",
    ross.internal = FALSE,
    ross.vpc = FALSE,
    ross.debug = FALSE,
    ross.aria2c = is.installed('aria2c')
  )
  toset <- !(names(op.ross) %in% names(op))
  if(any(toset)) options(op.ross[toset])

  invisible()
}
