.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Ross is a OSS API Wrapper for R.")
  op <- options()
  op.ross <- list(
    ross.location = "beijing",
    ross.internal = FALSE
  )
  toset <- !(names(op.ross) %in% names(op))
  if(any(toset)) options(op.ross[toset])

  AccessKeyId=Sys.getenv("AccessKeyId")
  AccessKeySecret=Sys.getenv("AccessKeySecret")

  invisible()
}

#' Build Endpoint host
#'
#' @param location bucket location
#' @param internal whether access from aliyun ecs
#'
#' @return
#' @export
#'
#' @examples
.build.endpoint <- function(location="oss-cn-beijing", internal=TRUE){
  if(internal){
    domain <- "%s-internal.aliyuncs.com"
  }else{
    domain <- "%s.aliyuncs.com"
  }
  sprintf(domain, location)
}

#' Build Bucket host
#'
#' @param name bucket name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.build.bucket.host <- function(name, ...){
  endpoint <- .build.endpoint(...)
  sprintf('http://%s.%s', name, endpoint)
}
