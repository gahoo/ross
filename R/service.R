#' Title
#'
#' @param prefix
#' @param marker
#' @param max_keys
#'
#' @return
#' @export
#'
#' @examples
GetService <- function(prefix=NULL, marker=NULL, max_keys=100){
  response <- .sign.header('GET', "http://oss.aliyuncs.com", "/",
                           query=list(prefix=prefix,
                                      marker=marker,
                                      "max-keys"=max_keys))
  if(http_error(response)){
    stop(content(response))
  }
  response
}


#' BucketList
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @export
#' @name BucketList
#'
BucketList <- R6::R6Class("BucketList",
  public = list(
    response = NULL,
    content = NULL,
    owner = NULL,
    buckets = list(),
    initialize = function(prefix=NULL, marker=NULL, max_keys=100) {
      self$response <- GetService(prefix, marker, max_keys)
      self$content <- httr::content(self$response)
      self$owner <- private$parseOwner(self$content)
      self$buckets <- private$parseBuckets(self$content)
    },
    list = function(val) {
      self$buckets <- val
    }
  ),
  private = list(
    parseOwner = function(doc) {
      ID <- xml2::xml_text(xml2::xml_find_all(doc, "/ListAllMyBucketsResult/Owner/ID"))
      DisplayName <- xml2::xml_text(xml2::xml_find_all(doc, "/ListAllMyBucketsResult/Owner/DisplayName"))
      list(ID=ID, DisplayName=DisplayName)
    },
    parseBuckets = function(doc) {
      buckets <- xml2::xml_find_all(doc, '/ListAllMyBucketsResult/Buckets/Bucket', ns=xml2::xml_ns(doc))
      lapply(buckets, function(x){
        x<-as.list(unlist(as_list(x)))
        Bucket$new(x$Name, x$CreationDate, x$Location, x$ExtranetEndpoint, x$StorageClass)
        })
    }
  )
)
