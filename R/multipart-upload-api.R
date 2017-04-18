#' InitiateMultipartUpload
#'
#' @param bucketname
#' @param key
#' @inheritParams .build.object.header
#'
#' @return
#' @export
#'
#' @examples
#' InitiateMultipartUpload('ross-test', 'multi-test.txt')
InitiateMultipartUpload <- function(bucketname, key, encryption=NULL, ...){
  header <- .build.object.header(encryption=encryption, ...)
  ossresource <- sprintf("/%s/%s?uploads", bucketname, key)
  .api.post.header.request(ossresource, bucketname=bucketname, path=key, header=header, query='uploads')
}

#' UploadPart
#'
#' @param bucketname
#' @param key
#' @param uploadId Multipart upload event ID. InitiateMultipartUpload will return this in the response.
#' @param partNumber Part number indicating which part uploading. Coresponding to relative position in the file. Data will be overwrite when the part already exists.
#' @inheritParams .build.object.header
#'
#' @return
#' @export
#'
#' @examples
#' InitiateMultipartUpload('ross-test', 'multi-test.txt')
#' UploadPart('ross-test', 'multi-test.txt', 'F7376AAB033344C1922A588D15CE56A2', 1, 'test')
UploadPart <- function(bucketname, key, uploadId, partNumber, body=NULL, ..., .md5=TRUE){
  if(partNumber < 1 || partNumber > 10000){
    stop('Invalid partNumber.')
  }
  header <- .build.object.header(.md5=.md5, body=body, ...)
  ossresource <- sprintf("/%s/%s?partNumber=%s&uploadId=%s", bucketname, key, partNumber, uploadId)
  query <- list(partNumber=partNumber, uploadId=uploadId)
  .api.put.header.request(ossresource, bucketname=bucketname, path=key, header=header, query=query, body=body)
}

#' UploadPartCopy
#'
#' @param bucketname Destinate bucket
#' @param key Destinate path on bucket
#' @inheritParams .build.object.header
#' @inheritParams UploadPart
#'
#' @return
#' @export
#'
#' @examples
#' PutObject('ross-test', 'test.txt', 'test')
#' UploadPartCopy('/ross-test/test.txt', 'ross-test', 'multi-test.txt', '300C834B2037432E871D62B848087139', 2, Range = '0-1')
UploadPartCopy <- function(source, bucketname, key, uploadId, partNumber, Range=NULL,
                           ETag = NULL, ETag.match=TRUE, since=NULL, modified.since=TRUE){
  UploadPart(bucketname, key, uploadId, partNumber,
             source=source, Range=Range,
             ETag = ETag, ETag.match = ETag.match,
             since = since, modified.since = modified.since)
}

#' CompleteMultipartUpload
#'
#' @inheritParams UploadPart
#' @param Etags PartNumber named character vector, which contains ETags of all parts.
#'
#' @return
#' @export
#'
#' @examples
#' CompleteMultipartUpload('ross-test', 'multi-test.txt', '300C834B2037432E871D62B848087139')
CompleteMultipartUpload <- function(bucketname, key, uploadId, Etags=NULL){
  ossresource <- sprintf("/%s/%s?uploadId=%s", bucketname, key, uploadId)
  query <- list(uploadId=uploadId)
  if(is.null(Etags)){
    Etags <- getAllPartETags(bucketname, key, uploadId)
  }
  body <- .build.xml_body.CompleteMultipartUpload(Etags)
  .api.post.header.request(ossresource, bucketname=bucketname, path=key, query=query, body=body)
}

.build.xml_body.CompleteMultipartUpload <- function(Etags){
  part_numbers <- names(Etags)
  if(is.null(part_numbers)){
    part_numbers <- 1:length(Etags)
  }
  parts <- lapply(part_numbers,
                  function(x) list(PartNumber=list(x), ETag=list(Etags[[x]])))
  names(parts) <- rep("Part", length(Etags))
  doc <- list(CompleteMultipartUpload=parts)
  doc <- xml2::as_xml_document(doc)
  as.character(doc)
}

getAllPartETags <- function(bucketname, key, uploadId){
  Etags <- c()
  marker <- 0
  repeat {
    r <- ListParts(bucketname, key, uploadId, marker = marker)
    doc <- httr::content(r, encoding = 'UTF-8')
    isTruncated <- unlist(xpath2list(doc, '/ListPartsResult/IsTruncated'))
    marker <- unlist(xpath2list(doc, '/ListPartsResult/Part/NumberMarker'))
    etags <- unlist(xpath2list(doc, '/ListPartsResult/Part/ETag'))
    names(etags) <- unlist(xpath2list(doc, '/ListPartsResult/Part/PartNumber'))
    Etags <- c(Etags, etags)
    if(isTruncated != 'true'){
      break
    }
  }
  Etags
}

#' AbortMultipartUpload
#'
#' @inheritParams UploadPart
#'
#' @return
#' @export
#'
#' @examples
#' AbortMultipartUpload('ross-test', 'multi-test.txt', 'F7376AAB033344C1922A588D15CE56A2')
AbortMultipartUpload <- function(bucketname, key, uploadId){
  ossresource <- sprintf("/%s/%s?uploadId=%s", bucketname, key, uploadId)
  query <- list(uploadId=uploadId)
  .api.delete.header.request(ossresource, bucketname=bucketname, path=key, query=query)
}

#' ListMultipartUploads
#'
#' @inheritParams GetBucket
#' @param max Max number of Multipart Upload events.
#' @param id_marker Should be use with marker parameter. Multipart Upload event which UploadId grater than id_marker will be return.
#'
#' @return
#' @export
#'
#' @examples
#' ListMultipartUploads('ross-test')
#' ListMultipartUploads('ross-test', max=1, marker='multi-test.txt')
#' ListMultipartUploads('ross-test', max=1, marker='multi-test.txt', id_marker='3B2D3355E0CD4718B6421AEAD272D068')
#' ListMultipartUploads('ross-test', delimiter = '/', max=1, id_marker='3B2D3355E0CD4718B6421AEAD272D068')
ListMultipartUploads <- function(bucketname, prefix=NULL, delimiter=NULL, max=1000,
                                 marker=NULL, id_marker=NULL, encoding_type=NULL){
  ossresource <- sprintf("/%s/?uploads", bucketname)
  query <- list(uploads='', prefix=prefix, 'key-marker'=marker, 'upload-id-marker'=id_marker,
                delimiter=delimiter, "max-uploads"=max, "encoding-type"=encoding_type)
  .api.get.header.request(ossresource, bucketname=bucketname, query=query)
}

#' ListParts
#'
#' @inheritParams UploadPart
#' @param max Max number of parts.
#' @param marker Which index to start with.
#'
#' @return
#' @export
#'
#' @examples
#' r <- InitiateMultipartUpload('ross-test', 'multi-test.txt')
#' uploadId <- unlist(xpath2list(content(r), '//UploadId'))
#' UploadPart('ross-test', 'multi-test.txt', uploadId, 1, 'test1')
#' UploadPart('ross-test', 'multi-test.txt', uploadId, 2, 'test2')
#' ListParts('ross-test', 'multi-test.txt', uploadId)
#' ListParts('ross-test', 'multi-test.txt', uploadId, max=1)
#' ListParts('ross-test', 'multi-test.txt', uploadId, marker = 1)
#' AbortMultipartUpload('ross-test', 'multi-test.txt', uploadId)
ListParts <- function(bucketname, key, uploadId, max=1000, marker=NULL){
  ossresource <- sprintf("/%s/%s?uploadId=%s", bucketname, key, uploadId)
  query <- list(uploadId=uploadId, 'max-parts'=max, 'part-number-marker'=marker)
  .api.get.header.request(ossresource, bucketname=bucketname, path=key, query=query)
}
