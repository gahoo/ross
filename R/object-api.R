#' Title
#'
#' @param bucketname
#' @param key
#' @param body
#' @param encryption server side encryption algorithm AES256
#' @param acl
#' @param .overwrite
#' @param ... Headers. Cache-Control, Content-Disposition, Content-Encoding, Expires
#' @param .meta
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


