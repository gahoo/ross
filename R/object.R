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
    read = function(n=-1L, ..., encoding = 'UTF-8'){
      makeRange <- function(n){
        start <- self$seek()
        end <- start + n
        end <- ifelse(end > self$size, self$size, end)
        self$seek(end)
        Range <- sprintf("%s-%s", start, end - 1)
        Range
      }

      if(n <= 0){
        Range <- NULL
      }else{
        if(self$seek() == self$size) return(raw(0))
        Range <- makeRange(n)
      }

      r <- GetObject(self$bucket, self$key, Range=Range, ...)
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
      if(self$type == 'Appendable'){
        private$position <- self$size
      }else{
        private$position <- 0
      }
    },
    exists = function(){
      isObjectExist(self$bucket, self$key)
    },
    append = function(content, ...){
      r <- AppendObject(self$bucket, self$key, body = content, position = private$position, ...)
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
    upload = function(src, ...){
      uploadObject(self$bucket, src, dest = self$key, ...)
    },
    download = function(dest=NULL, ...){
      downloadObject(self$bucket, self$key, dest, ...)
    },
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
    seek = function(where, origin='start'){
      if(missing(where)){
        private$position
      }else{
        if(origin == 'start'){
          private$position <- where
        }else if(origin == 'current'){
          private$position <- private$position + where
        }else if(origin == 'end'){
          private$position <- self$size - where
        }

        if(private$position > self$size){
          private$position <- self$size
        }
      }
    },
    restore = function(){
      r <- RestoreObject(self$bucket, self$key)
      invisible(r)
    }
  ),
  private = list(
    position = 0,
    init = function(){
      size = NULL
      type = NULL
      etag = NULL
      creation_date = NULL
      modified_date = NULL
      private$position <- 0
    }
  ),
  active = list(
    acl = function(acl){
      aclObject(self$bucket, self$key, acl)
    },
    link = function(target){
      if(self$type == 'Symlink' || is.null(self$type)){
        linkObject(self$bucket, self$key, target)
      }else{
        stop(sprintf('%s type has no target.', self$type))
      }
    },
    meta = function(meta){
      metaObject(self$bucket, self$key, meta)
    }
  )
)
