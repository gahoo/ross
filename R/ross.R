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
                                name=NULL, Location=NULL, ...,
                                header=NULL, body=NULL) {
  host <- .build.host(name, Location=Location, internal=getOption('ross.internal'))
  .headers <- .build.header(header)
  ossheader <- .build.ossheader(header)
  response <- .sign.header(method, host, ossresource,
                           .headers=.headers,
                           ossheader=ossheader,
                           body=body,
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
