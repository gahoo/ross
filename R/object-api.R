#' PutObject
#'
#' @inheritParams .build.object.header
#' @param bucketname The bucket name
#' @param key The file path of object on oss.
#' @param .overwrite overwrite or not. default TRUE
#'
#' @import digest
#' @import mime
#' @import base64enc
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt', 'test')
#' PutObject('ross-test', 'test.txt', 'test', encryption = 'AES256')
#' PutObject('ross-test', 'test.txt', 'test', acl = 'public-read')
#' PutObject('ross-test', 'test.txt', 'test', .md5 = F)
#' PutObject('ross-test', 'test.txt', 'test', .meta = list(location='beijing', owner='igenecode.com'))
#' PutObject('ross-test', 'test.txt', 'test', .overwrite = F)
PutObject <- function(bucketname, key, body='', encryption=NULL, acl='private', ..., .md5=TRUE, .meta=NULL, .overwrite=TRUE) {
  .check.acl(acl)

  if(missing(key) && class(body) == 'form_file'){
    key = basename(body$path)
  }

  if(!.overwrite){
    r <- HeadObject(bucketname, key)
    if(r$status_code != 404){
      stop(sprintf(".overwrite=FALSE and </%s/%s> exists.", bucketname, key))
    }
  }

  header <- .build.object.header(acl=acl, encryption=encryption, .md5=.md5, .meta=.meta, body=body, ...)
  .api.put.header.request(bucketname=bucketname, header=header, path=key, body=body)
}

#' CopyObject
#'
#' @inheritParams .build.object.header
#' @param bucketname Destinate bucket
#' @param key Destinate path on bucket
#'
#' @return
#' @export
#'
#' @examples
#' CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt')
#' CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', encryption = 'AES256')
#' CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', acl = 'public-read')
#' CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', meta.directive = 'REPLACE', .meta = list(owner='igenecode.com'))
#' CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', ETag = 'AAAA', ETag.match = F)
#' CopyObject('/ross-test/test.txt', 'ross-test', 'test2.txt', since = Sys.time(), modified.since = F)
CopyObject <- function(source, bucketname, key, encryption=NULL,
                       acl='private', meta.directive = 'COPY',
                       ETag = NULL, ETag.match=TRUE,
                       since=NULL, modified.since=TRUE, ..., .meta=NULL) {

  header <- .build.object.header(acl=acl, encryption=encryption,
                                 source = source,
                                 meta.directive = meta.directive,
                                 ETag = ETag, ETag.match = ETag.match,
                                 since = since, modified.since = modified.since,
                                 .meta=.meta, ...)
  .api.put.header.request(bucketname=bucketname, header=header, path=key)
}


#' GetObject
#'
#' @inheritParams CopyObject
#' @inheritParams .sign.url
#' @param Range The object content range, '0-99' means the first 100 bytes.
#' @param ... avaliable headers: response-content-type, response-content-language, response-expires, response-cache-control, response-content-disposition, response-content-encoding
#'
#' @return
#' @export
#'
#' @examples
#' GetObject('ross-test', 'test.txt')
#' GetObject('ross-test', 'test.txt', Range = '0-1')
#' GetObject('ross-test', 'test.txt', ETag = 'AAAA', ETag.match = F)
#' GetObject('ross-test', 'test.txt', since = Sys.time(), modified.since = T)
#' GetObject('ross-test', 'test.txt', "response-cache-control"='no-cache')
#' GetObject('ross-test', 'test.txt', .url=T)
#' GetObject('ross-test', 'test.txt', expires=3600, .url=T)
GetObject <- function(bucketname, key, Range=NULL,
                      ETag = NULL, ETag.match=TRUE,
                      since=NULL, modified.since=TRUE,
                      ..., expires=60, .url=FALSE) {
  header <- .build.object.header(Range = Range, ETag = ETag, ETag.match = ETag.match,
                                 since = since, modified.since = modified.since, ...)
  if(.url){
    .api.get.url.request(bucketname=bucketname, header=header, path=key, expires=expires, .url=.url)
  }else{
    .api.get.header.request(bucketname=bucketname, header=header, path=key)
  }
}

#' AppendObject
#'
#' @inherit .build.object.header
#' @inherit PutObject
#' @param position The position of of body to append. Should be the same as existing file length.
#'
#' @return
#' @export
#'
#' @examples
#' AppendObject('ross-test', 'test-append.txt', body='1', position = 0)
#' AppendObject('ross-test', 'test-append.txt', body='2', position = 1, acl = 'public-read', encryption = 'AES256')
#' AppendObject('ross-test', 'test-append.txt', body='3', position = 2, .md5 = F)
#' AppendObject('ross-test', 'test-append.txt', body='4', position = 3, .meta = list(location='beijing'))
#' AppendObject('ross-test', 'test-append.txt', body='5', position = 4, "Content-Encoding"='UTF-8')
AppendObject <- function(bucketname, key, body='', position=0, encryption=NULL, acl='private', ..., .md5=TRUE, .meta=NULL){

  if(missing(key) && class(body) == 'form_file'){
    key = basename(body$path)
  }

  header <- .build.object.header(acl = acl, encryption = encryption, body = body, .md5 = .md5, .meta = .meta, ...)
  query <- list(append='', position=position)
  .api.post.header.request(bucketname=bucketname, header=header, path=key, body=body, query=query)
}

#' DeleteObject
#'
#' @inherit PutObject
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt', 'test')
#' DeleteObject('ross-test', 'test.txt')
DeleteObject <- function(bucketname, key){
  .api.delete.header.request(bucketname=bucketname, path=key)
}

#' DeleteMultipleObjects
#'
#' @param bucketname The bucketname
#' @param keys Object keys to be deleted. Should less than 1000 once.
#' @param quiet Return deleted keys or not. When TRUE, only return keys with errors.
#'
#' @return
#' @export
#'
#' @examples
#' r<-GetBucket('ross-test')
#' keys<-unlist(xpath2list(httr::content(r), '/ListBucketResult/Contents/Key'))
#' DeleteMultipleObjects('ross-test', keys, FALSE)
DeleteMultipleObjects <- function(bucketname, keys, quiet=TRUE){
  if(is.null(keys)){
    stop("Keys should not be NULL.")
  }
  body <- .build.xml_body.DeleteMultipleObjects(keys, quiet)
  header <- .build.object.header(body = body)
  .api.post.header.request(bucketname=bucketname, header=header, body=body, query='delete')
}

.build.xml_body.DeleteMultipleObjects <- function(keys, quiet){
  keys_element <- lapply(keys, function(x) list(Object=list(Key=list(x))))
  quiet <- ifelse(quiet, 'true', 'false')
  doc <- list(
    Delete=c(list(list(Quiet=list(quiet))),
             keys_element)
  )
  doc <- xml2::as_xml_document(doc)
  as.character(doc)
}


#' HeadObject
#'
#' @inherit .build.object.header
#' @inherit PutObject
#'
#' @return
#' @export
#'
#' @examples
#' r <- PutObject('ross-test', 'test.txt')
#' HeadObject('ross-test', 'test.txt')
#' etag <- gsub('"', '', r$headers$etag)
#' HeadObject('ross-test', 'test.txt', ETag=etag, ETag.match=T)
#' HeadObject('ross-test', 'test.txt', ETag=etag, ETag.match=F)
HeadObject <- function(bucketname, key,
                       ETag = NULL, ETag.match=TRUE,
                       since=NULL, modified.since=TRUE){
  header <- .build.object.header(ETag = ETag, ETag.match = ETag.match,
                                 since = since, modified.since = modified.since)
  .api.head.header.request(bucketname=bucketname, header=header, path=key)
}


#' GetObjectMeta
#'
#' @inheritParams PutObject
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt')
#' GetObjectMeta('ross-test', 'test.txt')
GetObjectMeta <- function(bucketname, key){
  .api.get.header.request(bucketname=bucketname, path=key, query='objectMeta')
}


#' PutObjectACL
#'
#' @inheritParams PutObject
#'
#' @return
#' @export
#'
#' @examples
#' PutObjectACL('ross-test', 'test.txt', acl = 'public-read')
PutObjectACL <- function(bucketname, key, acl='private') {

  .check.acl(acl)

  header <- .build.object.header(acl = acl)
  .api.put.header.request(bucketname=bucketname, header=header, path=key, query="acl")
}

#' GetObjectACL
#'
#' @inheritParams PutObject
#'
#' @return
#' @export
#'
#' @examples
#' GetObjectACL('ross-test', 'test.txt')
GetObjectACL <- function(bucketname, key){
  .api.get.header.request(bucketname=bucketname, path=key, query='acl')
}

#' PutSymlink
#'
#' @param bucketname
#' @param key
#' @param target
#' @param .meta
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt')
#' PutSymlink('ross-test', 'test-linked.txt', 'test.txt')
PutSymlink <- function(bucketname, key, target, .meta=NULL){
  header <- .build.object.header(target = target, .meta = .meta)
  .api.put.header.request(bucketname=bucketname, header=header, path=key, query='symlink')
}

#' GetSymlink
#'
#' @param bucketname
#' @param key
#'
#' @return
#' @export
#'
#' @examples
#' GetSymlink('ross-test', 'test-linked.txt')
#' GetSymlink('ross-test', 'test.txt')
GetSymlink <- function(bucketname, key){
  .api.get.header.request(bucketname=bucketname, path=key, query='symlink')
}
