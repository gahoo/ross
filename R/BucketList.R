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
#' @example
#' b <- BucketList$new()
#' b <- BucketList$new(max_keys=2)
#' b$max_keys <- 1
#' b$refresh()
#'
#' b$buckets
#'
BucketList <- R6::R6Class("BucketList",
  public = list(
    prefix = NULL,
    marker = NULL,
    max_keys = 100,
    response = NULL,
    content = NULL,
    owner = NULL,
    buckets = list(),
    initialize = function(response=NULL, prefix=NULL, marker=NULL, max_keys=NULL) {
      if(is.null(response)){
        self$prefix = prefix
        self$marker = marker
        self$max_keys = max_keys
        self$refresh()
      }else{
        self$parseResponse(response)
      }
    },
    parseResponse = function(response) {
      self$response <- response
      self$content <- httr::content(self$response, encoding="UTF-8")
      self$owner <- private$parseOwner(self$content)
      self$buckets <- private$parseBuckets(self$content)
    },
    refresh = function() {
      response <- GetService(self$prefix, self$marker, self$max_keys)
      self$parseResponse(response)
    },
    list = function(val) {
      self$buckets <- val
    }
  ),
  private = list(
    parseOwner = function(doc) {
      ID <- xml_text(xml_find_all(doc, "/ListAllMyBucketsResult/Owner/ID"))
      DisplayName <- xml_text(xml_find_all(doc, "/ListAllMyBucketsResult/Owner/DisplayName"))
      list(ID=ID, DisplayName=DisplayName)
    },
    parseBuckets = function(doc) {
      buckets <- xml_find_all(doc, '/ListAllMyBucketsResult/Buckets/Bucket')
      lapply(buckets, function(x){
        x <- as.list(unlist(as_list(x)))
        Bucket$new(x$Name, x$CreationDate, x$Location, x$StorageClass)
        })
    }
  )
)
