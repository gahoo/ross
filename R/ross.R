#' ross: ross is an aliyun OSS API Wrapper for R.
#'
#' The ross package provides the basic OSS API. includes:
#' Bucket, Object, STS
#'
#' @section Foo functions:
#' The foo functions ...
#'
#' @docType package
#' @name ross
NULL

.state <- new.env(parent=emptyenv())
.state$location <- list()

.api.request <- function(sign.func, method, ossresource,
                                bucketname=NULL, Location=NULL, ...,
                                header=NULL, path=NULL) {
  host <- .build.host(bucketname, Location=Location, internal=getOption('ross.internal'), vpc=getOption('ross.vpc'))
  .headers <- .build.header(header)
  ossheader <- .build.ossheader(header)
  if(is.null(path)){
    url <- host
  }else{
    url <- httr::modify_url(host, path=path)
  }

  sign.func(method, url, ossresource,
            .headers=.headers,
            ossheader=ossheader,
            ...)
}

.api.header.request <- function(...) {
  .api.request(.sign.header, ...)
}

.api.put.header.request <- function(ossresource, ...){
  .api.header.request(method = 'PUT', ossresource, ...)
}

.api.post.header.request <- function(ossresource, ...){
  .api.header.request(method = 'POST', ossresource, ...)
}

.api.get.header.request <- function(ossresource, ...){
  .api.header.request(method = 'GET', ossresource, ...)
}

.api.head.header.request <- function(ossresource, ...){
  .api.header.request(method = 'HEAD', ossresource, ...)
}

.api.delete.header.request <- function(ossresource, ...){
  .api.header.request(method = 'DELETE', ossresource, ...)
}

.api.url.request <- function(...) {
  .api.request(.sign.url, ...)
}

.api.get.url.request <- function(ossresource, ...){
  .api.url.request(method = 'GET', ossresource, ...)
}
