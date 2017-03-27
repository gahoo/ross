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

.except.http_error <- function(response){
  doc <- content(response, encoding = 'UTF-8')
  Code <- xml_text(xml_find_all(doc, '/Error/Code'))
  Message <- xml_text(xml_find_all(doc, '/Error/Message'))
  RequestId <- xml_text(xml_find_all(doc, '/Error/RequestId'))
  warning(sprintf("%s:<%s %s> %s", RequestId, response$status_code, Code, Message))
}

.check.http_error <- function(response){
  is_error <- http_error(response)
  if(is_error){
    .except.http_error(response)
  }
  is_error
}

