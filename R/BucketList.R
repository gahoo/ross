#' BucketList
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @import tibble
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
      self$owner <- xpath2list(self$content)
      self$buckets <- private$parseBuckets(self$content)
    },
    refresh = function() {
      response <- GetService(self$prefix, self$marker, self$max_keys)
      self$parseResponse(response)
    },
    list = function(){
      tibble::tibble(
        Name = private$getBucketSlot('Name'),
        CreationDate = private$getBucketSlot('CreationDate'),
        Location = private$getBucketSlot('Location'),
        StorageClass = private$getBucketSlot('StorageClass'),
        Owner = private$getBucketSlotOwner(),
        Bucket = self$buckets)
    },
    print = function(){
      print(self$list())
    }
  ),
  private = list(
    parseBuckets = function(doc) {
      buckets <- xpath2list(doc, '/ListAllMyBucketsResult/Buckets/Bucket')
      buckets <- lapply(buckets, function(x){
        Bucket$new(x$Name, x$CreationDate, x$Location, x$StorageClass, autoCreate=F)
        })
      names(buckets) <- sapply(buckets, function(x) x$Name)
      buckets
    },
    getBucketSlot = function(name){
      sapply(self$buckets, function(x) x[[name]])
    },
    getBucketSlotOwner = function(){
      sapply(self$buckets, function(x) x[['Owner']]$DisplayName)
    }
  )
)
