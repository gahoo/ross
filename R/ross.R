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

.api.request <- function(sign.func, method, ossresource=NULL,
                                bucketname=NULL, Location=NULL, ...,
                                header=NULL, path=NULL, query=NULL) {
  host <- .build.host(bucketname, Location=Location, internal=getOption('ross.internal'), vpc=getOption('ross.vpc'))
  .headers <- .build.header(header)
  ossheader <- .build.ossheader(header)
  if(is.null(path)){
    url <- host
  }else{
    url <- httr::modify_url(host, path=path)
  }

  if(is.null(ossresource)){
    ossresource <- .build.ossresource(bucketname, path, query)
  }

  sign.func(method, url, ossresource,
            .headers=.headers,
            ossheader=ossheader,
            query=query,
            ...)
}

.api.header.request <- function(...) {
  .api.request(.sign.header, ...)
}

.api.put.header.request <- function(...){
  .api.header.request(method = 'PUT', ...)
}

.api.post.header.request <- function(...){
  .api.header.request(method = 'POST', ...)
}

.api.get.header.request <- function(...){
  .api.header.request(method = 'GET', ...)
}

.api.head.header.request <- function(...){
  .api.header.request(method = 'HEAD', ...)
}

.api.delete.header.request <- function(...){
  .api.header.request(method = 'DELETE', ...)
}

.api.url.request <- function(...) {
  .api.request(.sign.url, ...)
}

.api.get.url.request <- function(...){
  .api.url.request(method = 'GET', ...)
}
