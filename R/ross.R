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

.api.header.request <- function(method, ossresource,
                                bucketname=NULL, Location=NULL, ...,
                                header=NULL) {
  host <- .build.host(bucketname, Location=Location, internal=getOption('ross.internal'))
  .headers <- .build.header(header)
  ossheader <- .build.ossheader(header)
  response <- .sign.header(method, host, ossresource,
                           .headers=.headers,
                           ossheader=ossheader,
                           ...)
  .check.http_error(response)
  response
}

.api.put.header.request <- function(ossresource, ...){
  .api.header.request(method = 'PUT', ossresource, ...)
}

.api.get.header.request <- function(ossresource, ...){
  .api.header.request(method = 'GET', ossresource, ...)
}

.api.delete.header.request <- function(ossresource, ...){
  .api.header.request(method = 'DELETE', ossresource, ...)
}
