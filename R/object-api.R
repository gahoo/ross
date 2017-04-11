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
    HeadObject
  }

  header <- .build.object.header(acl=acl, encryption=encryption, .md5=.md5, .meta=.meta, body=body, ...)
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.put.header.request(ossresource, bucketname=bucketname, header=header, path=key, body=body)
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

  header <- .build.object.header(acl=acl, encryption=encryption,
                                 source = source,
                                 meta.directive = meta.directive,
                                 ETag = ETag, ETag.match = ETag.match,
                                 since = since, modified.since = modified.since,
                                 .meta=.meta, ...)
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.put.header.request(ossresource, bucketname=bucketname, header=header, path=key)
}


#' GetObject
#'
#' @inheritParams CopyObject
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
GetObject <- function(bucketname, key, Range=NULL,
                      ETag = NULL, ETag.match=TRUE,
                      since=NULL, modified.since=TRUE,
                      ...) {
  header <- .build.object.header(Range = Range, ETag = ETag, ETag.match = ETag.match,
                                 since = since, modified.since = modified.since, ...)
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.get.header.request(ossresource, bucketname=bucketname, header=header, path=key)
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
  ossresource <- sprintf("/%s/%s?append&position=%s", bucketname, key, position)
  query <- list(append='', position=position)
  .api.post.header.request(ossresource, bucketname=bucketname, header=header, path=key, body=body, query=query)
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
  ossresource <- sprintf("/%s/%s", bucketname, key)
  .api.delete.header.request(ossresource, bucketname=bucketname, path=key)
}

DeleteMultipleObjects <- function(bucketname, keys, quiet=TRUE){
  body <- .build.xml_body.DeleteMultipleObjects(keys, quiet)
  header <- .build.object.header(body = body)
  ossresource <- sprintf("/%s/?delete", bucketname)
  .api.post.header.request(ossresource, bucketname=bucketname, header=header, body=body, query=c('delete'))
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

#' PutObjectACL
#'
#' @inheritParams PutObject
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt', acl = 'public-read')
PutObjectACL <- function(bucketname, key, acl='private') {

  .check.acl(acl)

  header <- list('x-oss-object-acl' = acl)
  ossresource <- sprintf("/%s/%s?acl", bucketname, key)
  .api.put.header.request(ossresource, bucketname=bucketname, header=header, path=key, query="acl")
}
