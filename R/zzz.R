.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Ross is a OSS API Wrapper for R.")
  op <- options()
  op.ross <- list(
    ross.location = "beijing",
    ross.internal = FALSE
  )
  toset <- !(names(op.ross) %in% names(op))
  if(any(toset)) options(op.ross[toset])

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
.build.endpoint <- function(Location="oss-cn-beijing", internal=TRUE){
  if(internal){
    domain <- "%s-internal.aliyuncs.com"
  }else{
    domain <- "%s.aliyuncs.com"
  }
  sprintf(domain, Location)
}

#' Build host
#'
#' @param bucketname bucket name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.build.host <- function(bucketname=NULL, Location=NULL, ...){
  if(is.null(bucketname)){
    "http://oss.aliyuncs.com"
  }else{
    if(is.null(Location)){
      Location <- .get.cache.bucket.location(bucketname)
    }
    endpoint <- .build.endpoint(Location, ...)
    sprintf('http://%s.%s', bucketname, endpoint)
  }
}

.build.header <- function(x) {
  if(is.null(x)){
    character()
  }else{
    unlist(x)
  }
}

#' @export
.build.ossheader <- function(x) {
  if(is.null(x)){
    NULL
  }else{
    paste0(sprintf("%s:%s", names(x), x), collapse = '\n')
  }
}

#' @import xml2
.except.http_error <- function(response){
  doc <- content(response, encoding = 'UTF-8')
  Code <- xml_text(xml_find_all(doc, '/Error/Code'))
  Message <- xml_text(xml_find_all(doc, '/Error/Message'))
  RequestId <- xml_text(xml_find_all(doc, '/Error/RequestId'))
  warning(sprintf("%s:<%s %s> %s", RequestId, response$status_code, Code, Message))
}

#' @import httr
.check.http_error <- function(response){
  is_error <- http_error(response)
  if(is_error){
    .except.http_error(response)
  }
  is_error
}


.get.cache.bucket.location <- function(bucketname) {
  location <- .state$location[[bucketname]]
  if(is.null(location)){
    .get.bucket.location(bucketname)
  }else{
    location
  }
}

#' @import httr
#' @import xml2
.get.bucket.location <- function(bucketname){
  r <- GetBucketLocation(bucketname)
  if(r$status_code == 200){
    location <- unlist(xml2::as_list(httr::content(r, encoding = 'UTF-8')))
    .state$location[[bucketname]] <- location
  }else{
    stop("No Such Bucket.")
  }
  location
}
