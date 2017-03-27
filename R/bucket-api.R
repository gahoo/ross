#' PutBucket
#'
#' Create new Bucket. Modify authority control.
#'
#' @param name The bucket name
#' @param acl Authority control of bucket. choose from public-read-write, public-read, private.
#' @param StorageClass Storage type, Standard or IA.
#' @param Location The region of bucket.
#'
#' @return
#' @export
#'
#' @examples
#' PutBucket('ross-test', 'public-read-write')
PutBucket <- function(name, Location="oss-cn-beijing", acl = "private", StorageClass="Standard") {
  if(!acl %in% c('private', 'public-read-write', 'public-read')){
    stop('Invalid acl, choose from public-read-write, public-read, private')
  }

  if(!StorageClass %in% c("Standard", "IA")){
    stop('Invalid StorageClass, choose from Standard, .')
  }

  host <- .build.bucket.host(name, Location, internal=getOption('ross.internal'))
  ossheader <- paste0('x-oss-acl:', acl, '\n')
  ossresource <- sprintf("/%s/", name)
  body <- .build.xml_body.PutBucket(StorageClass)
  response <- .sign.header("PUT", host, ossresource,
                           .header=c("x-oss-acl"=acl),
                           ossheader=ossheader,
                           body=body)
  .check.http_error(response)
  response
}

.build.xml_body.PutBucket <- function(StorageClass="Standard"){
  doc <- list(
    CreateBucketConfiguration=list(
      StorageClass=list(StorageClass)
    )
  )
  doc <- as_xml_document(doc)
  as.character(doc)
}


#' DeleteBucket
#'
#' Remove bucket.
#'
#' @inheritParams PutBucket
#'
#' @return
#' @export
#'
#' @examples
#' DeleteBucket('ross-test')
DeleteBucket <- function(name, Location="oss-cn-beijing") {
  host <- .build.bucket.host(name, Location, internal=getOption('ross.internal'))
  response <- .sign.header("DELETE", host, sprintf("/%s/", name))
  .check.http_error(response)
  response
}


#' PutBucketLogging
#'
#' Turn on/off bucket logging.
#'
#' @inheritParams PutBucket
#' @param prefix The prefix of log object.
#' @param target The target bucket saving the log object.
#'
#' @return
#' @export
#'
#' @examples
#' r <- PutBucketLogging('ross-test', 'log-')
PutBucketLogging <- function(name, prefix, target=name, Location="oss-cn-beijing", on=TRUE) {
  host <- .build.bucket.host(name, Location, internal=getOption('ross.internal'))
  body <- .build.xml_body.PutBucketLogging(name, prefix, target, on)
  ossresource <- sprintf("/%s/?logging", name)
  response <- .sign.header("PUT", host, ossresource,
                           query = c('logging'),
                           body=body)
  .check.http_error(response)
  response
}

.build.xml_body.PutBucketLogging <- function(name, prefix, target=name, on=TRUE){
  doc <- xml_new_document()
  if(on){
    doc<-list(
      BucketLoggingStatus=list(
        LoggingEnabled=list(
          TargetBucket=list(target),
          TargetPrefix=list(prefix)
        )
      )
    )
    doc <- as.character(as_xml_document(doc))
  }else{
    doc <- "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<BucketLoggingStatus>\n</BucketLoggingStatus>\n"
  }

  doc
}


#' PutBucketWebsite
#'
#' @inheritParams PutBucket
#' @param suffix Index file name.
#' @param key 404 error file name.
#'
#' @return
#' @export
#'
#' @examples
PutBucketWebsite <- function(name, Location="oss-cn-beijing", suffix='index.html', key='404.html'){
  host <- .build.bucket.host(name, Location, internal=getOption('ross.internal'))
  body <- .build.xml_body.PutBucketWebsite(suffix, key)
  ossresource <- sprintf("/%s/?website", name)
  response <- .sign.header("PUT", host, ossresource,
                           query = c('website'),
                           body=body)
  .check.http_error(response)
  response
}

.build.xml_body.PutBucketWebsite <- function(suffix='index.html', key='404.html'){
  doc <- list(
    WebsiteConfiguration=list(
      IndexDocument=list(
        Suffix=list(suffix)
      ),
      ErrorDocument=list(
        Key=list(key)
      )
    )
  )
  doc <- as_xml_document(doc)
  as.character(doc)
}


#' PutBucketReferer
#'
#' Configure referer white list.
#'
#' @inheritParams PutBucket
#' @param AllowEmptyReferer Allow empty referer or not. boolean
#' @param RefererList Referer white list.
#'
#' @return
#' @export
#'
#' @examples
#' PutBucketReferer('ross-test', AllowEmptyReferer=FALSE, RefererList='http://*.aliyun.com')
PutBucketReferer <- function(name, Location="oss-cn-beijing", AllowEmptyReferer=TRUE, RefererList=c()){
  host <- .build.bucket.host(name, Location, internal=getOption('ross.internal'))
  body <- .build.xml_body.PutBucketReferer(AllowEmptyReferer, RefererList)
  ossresource <- sprintf("/%s/?referer", name)
  response <- .sign.header("PUT", host, ossresource,
                           query = c('referer'),
                           body=body)
  .check.http_error(response)
  response
}

.build.xml_body.PutBucketReferer <- function(AllowEmptyReferer=TRUE, RefererList=c()) {
  cnt <- length(RefererList)
  if(cnt == 0){
    RefererList <- list()
  }else{
    names(RefererList) <- rep('Referer', cnt)
    RefererList <- lapply(RefererList, list)
  }

  doc <- list(
    RefererConfiguration=list(
      AllowEmptyReferer=list(AllowEmptyReferer),
      RefererList=RefererList
    )
  )
  doc <- as_xml_document(doc)
  as.character(doc)
}
