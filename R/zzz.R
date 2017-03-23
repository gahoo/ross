.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Ross is a OSS API Wrapper for R.")
  op <- options()
  op.ross <- list(
    ross.zone = "beijing"
  )
  toset <- !(names(op.ross) %in% names(op))
  if(any(toset)) options(op.ross[toset])

  AccessKeyId=Sys.getenv("AccessKeyId")
  AccessKeySecret=Sys.getenv("AccessKeySecret")

  invisible()
}
