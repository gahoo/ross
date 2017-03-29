#' Bucket
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @export
#' @name Bucket
#'

Bucket <- R6::R6Class("Bucket",
  public = list(
    Name = NULL,
    CreationDate = NULL,
    Location = NULL,
    ExtranetEndpoint = NULL,
    IntranetEndpoint = NULL,
    StorageClass = NULL,
    Owner = list(),
    initialize = function(Name, Location='oss-cn-beijing', StorageClass="Standard", acl="private", autoCreate=FALSE) {
      self$Name = Name
      self$Location = Location
      self$StorageClass = StorageClass

      tryCatch(
        suppressWarnings(self$refresh()),
        error = function(e) {
          if(autoCreate){
            self$create(Location, acl, StorageClass)
          }else{
            warning(e)
          }
        }
      )
    },
    create = function(Location, acl='private', StorageClass) {
      if(missing(Location)){
        Location <- self$Location
      }

      if(missing(StorageClass)){
        StorageClass <- self$StorageClass
      }

      r <- PutBucket(self$Name, Location, acl, StorageClass)
      if(r$status_code == 200){
        message(sprintf("New Bucket %s with %s access and %s storage is created on %s.", self$Name, acl, StorageClass, Location))
        .state$location[[self$Name]] <- Location
        self$refresh()
      }
    },
    refresh = function() {
      r <- GetBucketInfo(self$Name)
      doc <- httr::content(r, encoding = 'UTF-8')
      self$Owner <- xpath2list(doc, '/BucketInfo/Bucket/Owner')
      info <- xpath2list(doc, '/BucketInfo/Bucket')
      private$setInfo(info)
    },
    rm = function() {
      r <- DeleteBucket(self$Name)
      .state$location[[self$Name]] <- NULL
    },

#' @method list
#' @inheritParams GetBucket
#' @param .output output format
#'
#' @return
#'
#' @examples
#' b$list()
    list = function(prefix=NULL, marker=NULL, delimiter=NULL, max_keys=NULL, .output="data.frame") {
      list2df <- function(x) do.call(rbind, (lapply(x, as.data.frame)))

      r <- GetBucket(self$Name, prefix, marker, delimiter, max_keys)
      doc <- httr::content(r, encoding = 'UTF-8')
      contents <- xpath2list(doc, '/ListBucketResult/Contents')

      if(.output == "data.frame"){
        list2df(contents)
      }else if(.output == 'list'){
        contents
      }else if(.output == 'oss-obj'){

      }
    },
    print = function(...) {
      bucket_text <- sprintf(paste(
          "<Bucket>",
          "Name: %s",
          "CreationDate: %s",
          "Location: %s",
          "Endpoint: %s",
          "StorageClass: %s",
          sep = "\n"),
          self$Name, self$CreationDate, self$Location, self$ExtranetEndpoint, self$StorageClass)
      cat(bucket_text)
    }
  ),
  private = list(
    setInfo = function(bucket_info) {
      self$CreationDate = bucket_info$CreationDate
      self$Location = bucket_info$Location
      self$ExtranetEndpoint = .build.endpoint(bucket_info$Location, internal=FALSE)
      self$IntranetEndpoint = .build.endpoint(bucket_info$Location, internal=TRUE)
      self$StorageClass = bucket_info$StorageClass
    }
  ),
  active = list(
    acl = function(acl){
      if(missing(acl)){
        r <- GetBucketAcl(self$Name)
        doc <- httr::content(r, encoding = 'UTF-8')
        unlist(xpath2list(doc, '/AccessControlPolicy/AccessControlList/Grant'))
      }else{
        PutBucket(self$Name, acl = acl)
      }
    },
    logging = function(){},
    website = function(){},
    referer = function(){},
    lifecycle = function(){}
  )
)
