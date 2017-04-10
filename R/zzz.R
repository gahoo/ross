.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Ross is a OSS API Wrapper for R.")
  op <- options()
  op.ross <- list(
    ross.location = "beijing",
    ross.internal = FALSE,
    ross.vpc = FALSE,
    ross.debug = FALSE
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
.build.endpoint <- function(Location="oss-cn-beijing", internal=TRUE, vpc=FALSE){
  if(internal){
    domain <- "%s-internal.aliyuncs.com"
  }else{
    domain <- "%s.aliyuncs.com"
  }
  if(vpc && !internal){
    domain <- "vpc100-%s.aliyuncs.com"
  }else if(vpc && internal){
    stop('Endpoint can not be both internal and vpc')
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
  x <- .rm.null(x)
  if(is.null(x)){
    character()
  }else{
    unlist(x)
  }
}

#' @export
.build.ossheader <- function(x) {
  oss_idx <- grep('^x-oss', names(x))
  x <- x[oss_idx]
  x <- .rm.null(x)
  if(is.null(x) || length(x) == 0){
    NULL
  }else{
    order_idx <- order(names(x))
    x <- x[order_idx]
    paste0(paste(names(x), x, sep=":", collapse = '\n'), '\n')
  }
}

.rm.null <- function(x){
  null_idx <- sapply(x, is.null)
  x[!null_idx]
}

.extract.header <- function(x, .headers) {
  ifelse(x %in% names(.headers), .headers[[x]], '')
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
    stop(sprintf("No Bucket Named %s.", bucketname))
  }
  location
}


#' xpath2list
#'
#' Turn xml nodes into list, xpath is supported
#'
#' @param xpath
#' @param doc
#'
#' @return
#' @import xml2
#' @export
#'
#' @examples
#' r <- GetService()
#' xpath2list(r)
#' xpath2list(r, '/ListAllMyBucketsResult/Owner')
#' xpath2list(r, '/ListAllMyBucketsResult/Buckets/Bucket')
xpath2list <- function(doc, xpath=NULL, smart=TRUE){

  extractNodes <- function(doc, xpath){
    nodes <- xml_find_all(doc, xpath)
    lst <- lapply(nodes, function(x){
      as.list(unlist(as_list(x)))
    })
    if(length(lst) == 1 && smart){
      lst[[1]]
    }else{
      lst
    }
  }

  if('xml_document' %in% class(doc)){
    if(is.null(xpath)){
      xml2::as_list(doc)
    }else{
      extractNodes(doc, xpath)
    }
  }else{
    list()
  }

}

#' Title
#'
#' @param x
#' @import digest
#' @import base64enc
#'
#' @return
#' @export
#'
#' @examples
md5 <- function(x) {
  if(class(x) == "form_file"){
    digested_md5 <- digest::digest(file=x$path, serialize=F, algo = 'md5', raw=T)
  }else if(is.character(x)){
    digested_md5 <- digest::digest(x, serialize=F, algo = 'md5', raw=T)
  }else{
    return(NULL)
  }
  base64enc::base64encode(digested_md5)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
body_type <- function(x){
  if(class(x) == 'form_file'){
    x$type
  }else if(is.character(x)){
    'text/plain'
  }else{
    NULL
  }
}
