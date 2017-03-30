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
      if(r$status == 204){
        .state$location[[self$Name]] <- NULL
      }
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
    read = function() {},
    download = function() {},
    upload = function() {},
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
#'
#' ## acl
#' b$acl
#' b$acl <- "private"
#' b$acl <- "public-read"
    acl = function(acl){
      if(missing(acl)){
        r <- GetBucketAcl(self$Name)
        doc <- httr::content(r, encoding = 'UTF-8')
        unlist(xpath2list(doc, '/AccessControlPolicy/AccessControlList/Grant'))
      }else{
        PutBucket(self$Name, acl = acl)
      }
    },
#' @examples
#'
#' ## logging
#' b$logging
#' b$logging <- list(TargetBucket='ross-test', TargetPrefix='log-')
#' b$logging <- list(TargetBucket='ross-test')
#' b$logging <- list(TargetPrefix='log-')
#' b$logging <- list()
#' b$logging <- NULL
    logging = function(conf){
      if(missing(conf)){
        r <- GetBucketLogging(self$Name)
        doc <- httr::content(r, encoding = 'UTF-8')
        xpath2list(doc, '/BucketLoggingStatus/LoggingEnabled')
      }else if(is.null(conf) || identical(conf, list())){
        r <- DeleteBucketLogging(self$Name)
      }else{
        if(is.null(conf$TargetBucket)){
          conf$TargetBucket <- self$Name
        }
        r <- PutBucketLogging(self$Name, conf$TargetPrefix, conf$TargetBucket)
      }

    },
#' @examples
#'
#' ## website
#' b$website
#' b$website <- list(Suffix='index.html', Key='404.html')
#' b$website <- list(Suffix='index.html')
#' b$website <- list(Key='404.html')
#' b$website <- list()
#' b$website <- NULL
    website = function(conf){
      if(missing(conf)){
        suppressWarnings(r <- GetBucketWebsite(self$Name))
        doc <- httr::content(r, encoding = 'UTF-8')
        conf <- xpath2list(doc, '/WebsiteConfiguration')
        names(conf) <- gsub('.*\\.','',names(conf))
        conf
      }else if(is.null(conf) || identical(conf, list())){
        r <- DeleteBucketWebsite(self$Name)
      }else{
        if(is.null(conf$Suffix)){
          message("Suffix is missing index.html will be used.")
          conf$Suffix <- 'index.html'
        }
        if(is.null(conf$Key)){
          message("Key is missing 404.html will be used.")
          conf$Key <- '404.html'
        }
        r <- PutBucketWebsite(self$Name, conf$Suffix, conf$Key)
      }
    },
#' @examples
#'
#' ## referer
#' b$referer
#' b$referer <- list(AllowEmptyReferer=T, RefererList=c('*.igenecode.com', 'aliyun.com'))
#' b$referer <- list(AllowEmptyReferer=T)
#' b$referer <- list(RefererList=c('*.igenecode.com', 'aliyun.com'))
#' b$referer <- NULL
#' b$referer <- list()
    referer = function(conf){
      if(missing(conf)){
        suppressWarnings(r <- GetBucketReferer(self$Name))
        doc <- httr::content(r, encoding = 'UTF-8')
        list(
          AllowEmptyReferer = unlist(xpath2list(doc, '/RefererConfiguration/AllowEmptyReferer')),
          RefererList = unlist(xpath2list(doc, '/RefererConfiguration/RefererList'))
        )
      }else if(is.null(conf) || identical(conf, list())){
        r <- DeleteBucketWebsite(self$Name)
      }else{
        if(is.null(conf$AllowEmptyReferer)){
          message("AllowEmptyReferer is missing TRUE will be used.")
          conf$AllowEmptyReferer <- TRUE
        }
        r <- PutBucketReferer(self$Name, conf$AllowEmptyReferer, conf$RefererList)
      }
    },
    lifecycle = function(conf){
      if(missing(conf)){
        suppressWarnings(r <- GetBucketLifecycle(self$Name))
        doc <- httr::content(r, encoding = 'UTF-8')
        xpath2list(doc, '/LifecycleConfiguration/Rule')
      }else if(is.null(conf) || identical(conf, list())){
        r <- DeleteBucketLifecycle(self$Name)
      }else{
        conf$name <- self$Name
        r <- do.call(PutBucketLifecycle, conf)
      }
    }
  )
)

BucketLifecycle <- R6::R6Class("BucketLifecycle",
  public = list(
    add = function(){},
    delete = function(){},
    print = function(){}
  )
)
