#### PUT

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
PutBucket <- function(bucketname, Location="oss-cn-beijing", acl = "private", StorageClass="Standard") {
  if(!acl %in% c('private', 'public-read-write', 'public-read')){
    stop('Invalid acl, choose from public-read-write, public-read, private')
  }

  if(!StorageClass %in% c("Standard", "IA")){
    stop('Invalid StorageClass, choose from Standard, .')
  }

  body <- .build.xml_body.PutBucket(StorageClass)
  header <- list('x-oss-acl' = acl)
  ossresource <- sprintf("/%s/", bucketname)
  .api.put.header.request(ossresource, bucketname=bucketname, Location=Location, header=header, body=body)
}

.build.xml_body.PutBucket <- function(StorageClass="Standard"){
  doc <- list(
    CreateBucketConfiguration=list(
      StorageClass=list(StorageClass)
    )
  )
  doc <- xml2::as_xml_document(doc)
  as.character(doc)
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
PutBucketLogging <- function(name, prefix, target=name, on=TRUE) {
  body <- .build.xml_body.PutBucketLogging(name, prefix, target, on)
  ossresource <- sprintf("/%s/?logging", name)
  .api.put.header.request(ossresource, bucketname=name, query = c('logging'), body=body)
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
    doc <- as.character(xml2::as_xml_document(doc))
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
#' PutBucketWebsite('ross-test')
#' PutBucketWebsite('ross-test', key='error.html', suffix='index.html')
PutBucketWebsite <- function(name, suffix='index.html', key='404.html'){
  body <- .build.xml_body.PutBucketWebsite(suffix, key)
  ossresource <- sprintf("/%s/?website", name)
  .api.put.header.request(ossresource, bucketname=name, query = c('website'), body=body)
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
  doc <- xml2::as_xml_document(doc)
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
PutBucketReferer <- function(name, AllowEmptyReferer=TRUE, RefererList=c()){
  body <- .build.xml_body.PutBucketReferer(AllowEmptyReferer, RefererList)
  ossresource <- sprintf("/%s/?referer", name)
  .api.put.header.request(ossresource, bucketname=name, query = c('referer'), body=body)
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
  doc <- xml2::as_xml_document(doc)
  as.character(doc)
}

#' PutBucketLifecycle
#'
#' Config bucket object life cycle rules.
#'
#' @inheritParams PutBucket
#' @param Prefix Object prefix applying the rule.
#' @param RuleID Uid of rule
#' @param Status Enable or Disable the rule.
#' @param Object.CreatedBeforeDate Expires date for object.
#' @param Object.Days Expris days for object.
#' @param Multpart.CreatedBeforeDate Expris date for multipart upload.
#' @param Multpart.Days Expris day for multipart upload.
#'
#' @return
#' @export
#'
#'
#' @examples
#' PutBucketLifecycle('ross-test', Prefix = 'upload_', Object.Days = 30)
#' PutBucketLifecycle('ross-test', Prefix = 'upload_', Object.CreatedBeforeDate = Sys.Date()+7)
#' PutBucketLifecycle('ross-test', Prefix = 'upload_', Multpart.Days = 5)
#' PutBucketLifecycle('ross-test', Prefix = 'upload_', Object.Days = 30, Multpart.Days = 5)
PutBucketLifecycle <- function(name, Location="oss-cn-beijing",
                               Prefix, RuleID=NULL, Status='Enabled',
                               Object.CreatedBeforeDate=NULL, Object.Days=NULL,
                               Multpart.CreatedBeforeDate=NULL, Multpart.Days=NULL){

  body <- .build.xml_body.PutBucketLifecycle(Prefix, RuleID, Status,
                                             Object.CreatedBeforeDate, Object.Days,
                                             Multpart.CreatedBeforeDate, Multpart.Days)

  ossresource <- sprintf("/%s/?lifecycle", name)
  .api.put.header.request(ossresource, bucketname=name, query = c('lifecycle'), body = body)
}


.build.xml_body.PutBucketLifecycle <- function(Prefix, RuleID=NULL, Status='Enabled',
                                               Object.CreatedBeforeDate=NULL, Object.Days=NULL,
                                               Multpart.CreatedBeforeDate=NULL, Multpart.Days=NULL) {

  build.expir <- function(CreatedBeforeDate=NULL, Days=NULL){
    if(is.null(CreatedBeforeDate) && !is.null(Days)){
      list(Days=list(Days))
    }else if(!is.null(CreatedBeforeDate) && is.null(Days)){
      list(CreatedBeforeDate=list(paste0(CreatedBeforeDate, "T00:00:00.000Z")))
    }else if(!is.null(CreatedBeforeDate) && !is.null(Days)){
      stop('Conflict! Either Days or CreatedBeforeDate should be used.')
    }else{
      NULL
    }
  }

  # TODO: add multiple rule support.

  doc <- list(
    LifecycleConfiguration=list(
      Rule=list(
        ID = list(RuleID),
        Prefix = list(Prefix),
        Status = list(Status),
        Expiration = build.expir(Object.CreatedBeforeDate, Object.Days),
        AbortMultipartUpload = build.expir(Multpart.CreatedBeforeDate, Multpart.Days)
      )
    )
  )

  null_idx<-sapply(doc$LifecycleConfiguration$Rule, function(x) is.null(x[[1]]) )
  doc$LifecycleConfiguration$Rule <- doc$LifecycleConfiguration$Rule[!null_idx]

  doc <- xml2::as_xml_document(doc)
  as.character(doc)
}

######## GET
#' GetBucket
#'
#' Equals to ListObject
#'
#' @param prefix The prefix of buckets to filter.
#' @param marker Which index to start with.
#' @param max_keys Max number of buckets.
#' @param delimiter Single character grouping the object name.
#'
#' @return
#' @export
#'
#' @examples
#'
# TODO: an R6 Class to hadle ListBucketResult is needed
GetBucket <- function(bucketname, prefix=NULL, marker=NULL, delimiter=NULL, max_keys=NULL){
  ossresource <- sprintf("/%s/", bucketname)
  query <- list(prefix=prefix, marker=marker, delimiter=delimiter, "max-keys"=max_keys)
  .api.get.header.request(ossresource, bucketname=bucketname, query = query)
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
ListObject <- function(...) {
  GetBucket(...)
}

#' GetBucketAcl
#'
#' @inheritParams PutBucket
#'
#' @return
#' @export
#'
#' @examples
GetBucketAcl <- function(name){
  ossresource <- sprintf("/%s/?acl", name)
  .api.get.header.request(ossresource, bucketname=name, query = c('acl'))
}


#' GetBucketLocation
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
GetBucketLocation <- function(name) {
  ossresource <- sprintf("/%s/?location", name)
  .api.get.header.request(ossresource, bucketname=name,
                          Location='oss-cn-hangzhou',
                          query = c('location'))
}


######## DELETE

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
  host <- .build.host(name, Location, internal=getOption('ross.internal'))
  response <- .sign.header("DELETE", host, sprintf("/%s/", name))
  .check.http_error(response)
  response
}

