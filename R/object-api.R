#' PutObject
#'
#' @param bucketname The bucket name
#' @param key The file path of object on oss.
#' @param body The content of object, character or upload_file(file_path).
#' @param encryption server side encryption algorithm AES256
#' @param acl Access control: private, public-read, public-read-write
#' @param .overwrite overwrite or not. default TRUE
#' @param ... Headers. Cache-Control, Content-Disposition, Content-Encoding, Expires
#' @param .meta Other meta info set to the object, < 8k.
#'
#' @import digest
#' @import mime
#' @import base64enc
#'
#' @return
#' @export
#'
#' @examples
PutObject <- function(bucketname, key, body='', encryption=NULL, acl='private', ..., .md5=TRUE, .meta=NULL, .overwrite=TRUE) {
  build.header <- function(){
    header <- list(
      'x-oss-object-acl' = acl,
      'x-oss-server-side-encryption' = encryption)
    header <- c(header, list(...))

    if(is.list(.meta)){
      message('meta')
      names(.meta) <- paste0('x-oss-meta-', names(.meta))
      header <- c(header, .meta)
    }

    if(.md5){
      header[['Content-MD5']] <- md5(body)
    }
    header[['Content-Type']] <- body_type(body)
    header
  }

  if(!acl %in% c('private', 'public-read-write', 'public-read')){
    stop('Invalid acl, choose from public-read-write, public-read, private')
  }

  if(missing(key) && class(body) == 'form_file'){
    key = basename(body$path)
  }

  if(!.overwrite){
    HeadObject
  }

  header <- build.header()
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.put.header.request(ossresource, bucketname=bucketname, header=header, path=key, body=body)
}

#' CopyObject
#'
#' @inheritParams PutObject
#' @param source The source object. /bucketname/objectname
#' @param bucketname Destinate bucket
#' @param key Destinate path on bucket
#' @param meta.directive COPY or REPLACE meta info of object. All source meta will be ignore when this value set to REPLACE.
#' @param ETag ETag of object.
#' @param ETag.match Copy when ETag matches or copy when Etag not match.
#' @param since POSIXct time.
#' @param modified.since Copy when object was modified since a time, or copy when object was not modified since a time.
#' @param ... Set header directly.
#' @param .meta Other meta info about the object.
#'
#' @return
#' @export
#'
#' @examples
#' CopyObject('/ross-test/test2.txt', 'ross-test', 'test2.txt')
#' CopyObject('/ross-test/test2.txt', 'ross-test', 'test2.txt', encryption = 'AES256')
#' CopyObject('/ross-test/test2.txt', 'ross-test', 'test2.txt', acl = 'public-read')
#' CopyObject('/ross-test/test2.txt', 'ross-test', 'test2.txt', meta.directive = 'REPLACE', .meta = list(owner='igenecode.com'))
#' CopyObject('/ross-test/test2.txt', 'ross-test', 'test2.txt', ETag = 'AAAA', ETag.match = F)
#' CopyObject('/ross-test/test2.txt', 'ross-test', 'test2.txt', since = Sys.time(), modified.since = F)
CopyObject <- function(source, bucketname, key, encryption=NULL,
                       acl='private', meta.directive = 'COPY',
                       ETag = NULL, ETag.match=TRUE,
                       since=NULL, modified.since=TRUE, ..., .meta=NULL) {
  build.header <- function(){
    header <- list(
      'x-oss-copy-source' = source,
      'x-oss-object-acl' = acl,
      'x-oss-server-side-encryption' = encryption,
      'x-oss-metadata-directive' = meta.directive)
    header <- c(header, list(...))

    if(!is.null(since)){
      if(!identical(class(since), c("POSIXct", "POSIXt"))){
        stop('The class of since is not POSIXct or POSIXt')
      }
      if(modified.since){
        header_title <- 'x-oss-copy-source-if-modified-since'
      }else{
        header_title <- 'x-oss-copy-source-if-unmodified-since'
      }
      header[[header_title]] <- httr::http_date(since)
    }

    if(!is.null(ETag)){
      if(ETag.match){
        header_title <- 'x-oss-copy-source-if-match'
      }else{
        header_title <- 'x-oss-copy-source-if-none-match'
      }
      header[[header_title]] <- ETag
    }

    if(is.list(.meta)){
      message('meta')
      names(.meta) <- paste0('x-oss-meta-', names(.meta))
      header <- c(header, .meta)
    }

    header
  }

  header <- build.header()
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.put.header.request(ossresource, bucketname=bucketname, header=header, path=key)
}


GetObject <- function(bucketname, key, Range=NULL,
                      ETag = NULL, ETag.match=TRUE,
                      since=NULL, modified.since=TRUE,
                      ...) {
  build.header <- function(){
    header <- list('Range' = sprintf("bytes=%s", Range))
    header <- c(header, list(...))

    if(!is.null(since)){
      if(!identical(class(since), c("POSIXct", "POSIXt"))){
        stop('The class of since is not POSIXct or POSIXt')
      }
      if(modified.since){
        header_title <- 'If-Modified-Since'
      }else{
        header_title <- 'If-Unmodified-Since'
      }
      header[[header_title]] <- httr::http_date(since)
    }

    if(!is.null(ETag)){
      if(ETag.match){
        header_title <- 'If-Match'
      }else{
        header_title <- 'If-None-Match'
      }
      header[[header_title]] <- ETag
    }

    header
  }

  header <- build.header()
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.get.header.request(ossresource, bucketname=bucketname, header=header, path=key)
}



#' PutObjectACL
#'
#' @param bucketname
#' @param key
#' @param acl
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt', acl = 'public-read')
PutObjectACL <- function(bucketname, key, acl='private') {

  if(!acl %in% c('private', 'public-read-write', 'public-read')){
    stop('Invalid acl, choose from public-read-write, public-read, private')
  }

  header <- list('x-oss-object-acl' = acl)
  ossresource <- sprintf("/%s/%s?acl", bucketname, key)
  .api.put.header.request(ossresource, bucketname=bucketname, header=header, path=key, query="acl")
}
