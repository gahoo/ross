#' Object
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @export
#' @name Object
#'
Object <- R6::R6Class("Object",
  public = list(
    bucket = NULL,
    key = NULL,
    size = NULL,
    type = NULL,
    etag = NULL,
    creation_date = NULL,
    modified_date = NULL,
    initialize = function(bucketname, key){
      self$bucket <- bucketname
      self$key <- key
      if(self$exists()) self$refresh()
    },
    write = function(content, ...){
      r <- PutObject(self$bucket, self$key, content, ...)
      self$refresh()
      invisible(r)
    },
    read = function(..., encoding = 'UTF-8'){
      r <- GetObject(self$bucket, self$key, ...)
      httr::content(r, encoding = encoding)
    },
    url = function(expires = 1200){
      GetObject(self$bucket, self$key, expires = expires, .url = TRUE)
    },
    refresh = function(){
      info <- getObjectInfo(self$bucket, self$key, print=FALSE)
      self$setInfo(info)
    },
    setInfo = function(info){
      self$size <- as.numeric(info$`content-length`)
      self$type <- info$`x-oss-object-type`
      self$etag <- info$etag
      self$creation_date <- info$date
      self$modified_date <- info$`last-modified`
      private$append_position <- self$size
    },
    exists = function(){
      isObjectExist(self$bucket, self$key)
    },
    append = function(content, ...){
      r <- AppendObject(self$bucket, self$key, body = content, position = private$append_position, ...)
      self$refresh()
      invisible(r)
    },
    save = function(..., envir = parent.frame(), opts=NULL){
      saveObject(self$bucket, self$key, ..., envir = envir, opts=opts)
    },
    load = function(..., envir = parent.frame(), quiet = T){
      loadObject(self$bucket, self$key, ..., envir = envir, quiet = quiet)
    },
    saveRDS = function(object, ..., opts=NULL){
      saveRDSObject(self$bucket, self$key, object, ..., opts=NULL)
    },
    readRDS = function(..., refhook=NULL, quiet = T){
      readRDSObject(self$bucket, self$key, ..., refhook=refhook, quiet = quiet)
    },
    upload = function(){},
    download = function(){},
    delete = function(){
      r <- DeleteObject(self$bucket, self$key)
      private$init()
      invisible(r)
    },
    copyTo = function(bucket, key, ..., .meta = NULL){
      source <- sprintf('/%s/%s', self$bucket, self$key)
      r <- CopyObject(source, bucket, key, ..., .meta = .meta)
      invisible(r)
    },
    copyFrom = function(bucket, key, ..., .meta = NULL){
      source <- sprintf('/%s/%s', bucket, key)
      r <- CopyObject(source, self$bucket, self$key, ..., .meta = .meta)
      invisible(r)
    },
    restore = function(){}
  ),
  private = list(
    append_position = 0,
    init = function(){
      size = NULL
      type = NULL
      etag = NULL
      creation_date = NULL
      modified_date = NULL
      private$append_position <- 0
    }
  ),
  active = list(
    acl = function(){},
    link = function(target){
      if(self$type == 'Symlink' || is.null(self$type)){
        linkObject(self$bucket, self$key, target)
      }else{
        stop(sprintf('%s type has no target.', self$type))
      }
    },
    meta = function(){}
  )
)
