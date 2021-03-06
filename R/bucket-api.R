#### PUT

#' PutBucket
#'
#' Create new Bucket. Modify authority control.
#'
#' @param name The bucket name
#' @param acl Authority control of bucket. choose from public-read-write, public-read, private.
#' @param StorageClass Storage type, Standard, IA or Archive.
#' @param Location The region of bucket.
#'
#' @return
#' @export
#'
#' @examples
#' PutBucket('ross-test', acl='public-read-write')
PutBucket <- function(bucketname, Location="oss-cn-beijing", acl = "private", StorageClass="Standard") {
  .check.acl(acl)

  if(!StorageClass %in% c("Standard", "IA", "Archive")){
    stop('Invalid StorageClass, choose from Standard, .')
  }

  body <- .build.xml_body.PutBucket(StorageClass)
  header <- list('x-oss-acl' = acl)
  .api.put.header.request(bucketname=bucketname, Location=Location, header=header, body=body)
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
  .api.put.header.request(bucketname=name, query = 'logging', body=body)
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
  .api.put.header.request(bucketname=name, query = 'website', body=body)
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
  .api.put.header.request(bucketname=name, query = 'referer', body=body)
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
#' @param rules list Rules created by `.build.xml_body.PutBucketLifecycle.Rules`. Will be ignore when body parameter is specified.
#' @param body charactor XML content of rules.
#'
#' @return
#' @export
#'
#'
#' @examples
#'
#' rules <- list()
#'
#' rules[[1]] <- .build.xml_body.PutBucketLifecycle.Rules(Prefix = 'upload_', Object.CreatedBeforeDate = Sys.Date()+7)
#' rules[[2]] <- .build.xml_body.PutBucketLifecycle.Rules(Prefix = 'upload_', Multpart.Days = 5)
#' rules[[3]] <- .build.xml_body.PutBucketLifecycle.Rules(Prefix = 'upload_', Object.Days = 30, Multpart.Days = 5)
#' rules[[4]] <- .build.xml_body.PutBucketLifecycle.Rules(Prefix = 'upload_', Object.Days = 30)
#'
#' PutBucketLifecycle('ross-test', rules)
PutBucketLifecycle <- function(name, rules, body=NULL){

  if(is.null(body)) {
    body <- .build.xml_body.PutBucketLifecycle(rules)
  }
  .api.put.header.request(bucketname=name, query = 'lifecycle', body = body)
}

.build.xml_body.PutBucketLifecycle <- function(rules) {
  doc <- list(
    LifecycleConfiguration=list(
      Rule=rules
    )
  )
  null_idx<-sapply(doc$LifecycleConfiguration$Rule, function(x) is.null(x[[1]]) )
  doc$LifecycleConfiguration$Rule <- doc$LifecycleConfiguration$Rule[!null_idx]

  doc <- xml2::as_xml_document(doc)
  as.character(doc)
}

#' .build.xml_body.PutBucketLifecycle.Rules
#' @param Prefix Object prefix applying the rule.
#' @param RuleID Uid of rule
#' @param Status Enable or Disable the rule.
#' @param Object.CreatedBeforeDate Expires date for object.
#' @param Object.Days Expris days for object.
#' @param Multpart.CreatedBeforeDate Expris date for multipart upload.
#' @param Multpart.Days Expris day for multipart upload.
.build.xml_body.PutBucketLifecycle.Rules <- function(Prefix, RuleID=NULL, Status='Enabled',
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

  expries <- c(Object.CreatedBeforeDate, Object.Days, Multpart.CreatedBeforeDate, Multpart.Days)
  if(all(is.null(expries))){
    stop('At least one expries parameter must be specified: Object.CreatedBeforeDate, Object.Days, Multpart.CreatedBeforeDate, Multpart.Days')
  }

  rule <- list(
    ID = list(RuleID),
    Prefix = list(Prefix),
    Status = list(Status),
    Expiration = build.expir(Object.CreatedBeforeDate, Object.Days),
    AbortMultipartUpload = build.expir(Multpart.CreatedBeforeDate, Multpart.Days)
  )
  null_idx<-sapply(rule, function(x) is.null(x[[1]]) )
  rule[!null_idx]
}

#' PutBucketcors
#'
#' @inheritParams PutBucket
#' @param rules list Rules created by `.build.xml_body.PutBucketLifecycle.Rules`. Will be ignore when body parameter is specified.
#' @param body charactor XML content of rules.
#'
#' @return
#' @export
#'
#' @examples
#' rules <- list()
#' rules[[1]] <- .build.xml_body.PutBucketcors.Rules(AllowedOrigin="*", AllowedMethod='GET')
#' rules[[2]] <- .build.xml_body.PutBucketcors.Rules(AllowedOrigin="igenecode.com", AllowedMethod=c('GET', 'PUT'))
#' rules[[3]] <- .build.xml_body.PutBucketcors.Rules(AllowedOrigin="www.igenecode.com", AllowedMethod='GET',
#'  AllowedHeader='Authorization', ExposeHeader=c('x-oss-meta1', 'x-oss-meta2'), MaxAgeSeconds=100)
#' PutBucketcors('ross-test', rules)
PutBucketcors <- function(name, rules, body=NULL){
  if(is.null(body)) {
    body <- .build.xml_body.PutBucketcors(rules)
  }
  .api.put.header.request(bucketname=name, query = 'cors', body = body)
}

.build.xml_body.PutBucketcors <- function(rules){
  names(rules) <- rep('CORSRule', length(rules))
  doc <- list(
    CORSConfiguration=rules
  )
  null_idx<-sapply(doc$CORSConfiguration$Rule, function(x) is.null(x[[1]]) )
  doc$CORSConfiguration$Rule <- doc$CORSConfiguration$Rule[!null_idx]

  doc <- xml2::as_xml_document(doc)
  as.character(doc)
}

#' .build.xml_body.PutBucketcors.Rules
#'
#' @param AllowedOrigin The source of CORS. charactor vector is acceptable for multiple elements. Only one "*" is permitted. "*" means accept all CORS.
#' @param AllowedMethod Methods for CORS. GET,PUT,DELETE,POST,HEAD
#' @param AllowedHeader Allowed headers in OPTIONS preflight Access-Control-Request-Headers
#' @param ExposeHeader Headers that users could access.
#' @param MaxAgeSeconds OPTIONS preflight cache time. in seconds.
#'
#' @return
#' @export
#'
#' @examples
#' rules <- list()
#' rules[[1]] <- .build.xml_body.PutBucketcors.Rules(AllowedOrigin="*", AllowedMethod='GET')
#' rules[[2]] <- .build.xml_body.PutBucketcors.Rules(AllowedOrigin="igenecode.com", AllowedMethod=c('GET', 'PUT'))
#' rules[[3]] <- .build.xml_body.PutBucketcors.Rules(AllowedOrigin="www.igenecode.com", AllowedMethod='GET',
#'  AllowedHeader='Authorization', ExposeHeader=c('x-oss-meta1', 'x-oss-meta2'), , MaxAgeSeconds=100)
.build.xml_body.PutBucketcors.Rules <- function(AllowedOrigin, AllowedMethod,
                                                AllowedHeader=NULL, ExposeHeader=NULL,
                                                MaxAgeSeconds=NULL){
  toList <- function(name, values){
    values <- lapply(values, list)
    names(values) <- rep(name, length(values))
    values
  }

  rule <- c(
    toList('AllowedOrigin', AllowedOrigin),
    toList('AllowedMethod', AllowedMethod),
    toList('AllowedHeader', AllowedHeader),
    toList('ExposeHeader', ExposeHeader),
    list(MaxAgeSeconds=list(MaxAgeSeconds))
  )
  null_idx<-sapply(rule, function(x) is.null(x[[1]]) )
  rule[!null_idx]
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
#' @param encoding_type For those control characters not supported in xml 1.0 should set this to `url`.
#'
#' @return
#' @export
#'
#' @examples
#' GetBucket('ross-test')
#' GetBucket('ross-test', 'upload')
#' GetBucket('ross-test', 'upload', 'upload/file1', '/', '10')
#'
# TODO: an R6 Class to hadle ListBucketResult is needed
GetBucket <- function(name, prefix=NULL, marker=NULL, delimiter=NULL, max_keys=NULL, encoding_type=NULL){
  ossresource <- sprintf("/%s/", name)
  query <- list(prefix=prefix, marker=marker, delimiter=delimiter, "max-keys"=max_keys, "encoding-type"=encoding_type)
  .api.get.header.request(ossresource=ossresource, bucketname=name, query = query)
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
#' GetBucketAcl('ross-test')
GetBucketAcl <- function(name){
  .api.get.header.request(bucketname=name, query = 'acl')
}


#' GetBucketLocation
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketLocation('ross-test')
GetBucketLocation <- function(name) {
  .api.get.header.request(bucketname=name, Location='oss-cn-hangzhou', query = 'location')
}


#' GetBucketInfo
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketInfo('ross-test')
GetBucketInfo <- function(name){
  .api.get.header.request(bucketname=name, query = 'bucketInfo')
}

#' GetBucketLogging
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketLogging('ross-test')
GetBucketLogging <- function(name){
  .api.get.header.request(bucketname=name, query = 'logging')
}

#' GetBucketWebsite
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketWebsite('ross-test')
GetBucketWebsite <- function(name){
  .api.get.header.request(bucketname=name, query = 'website')
}

#' GetBucketReferer
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketReferer('ross-test')
GetBucketReferer <- function(name){
  .api.get.header.request(bucketname=name, query = 'referer')
}

#' GetBucketLifecycle
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketLifecycle('ross-test')
GetBucketLifecycle <- function(name){
  .api.get.header.request(bucketname=name, query = 'lifecycle')
}

#' GetBucketcors
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' GetBucketcors('ross-test')
GetBucketcors <- function(name){
  .api.get.header.request(bucketname=name, query = 'cors')
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
DeleteBucket <- function(name) {
  .api.delete.header.request(bucketname=name)
}

#' DeleteBucketLogging
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' DeleteBucketLogging('ross-test')
DeleteBucketLogging <- function(name){
  .api.delete.header.request(bucketname=name, query = 'logging')
}

#' DeleteBucketWebsite
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' DeleteBucketWebsite('ross-test')
DeleteBucketWebsite <- function(name){
  .api.delete.header.request(bucketname=name, query = 'website')
}

#' DeleteBucketLifecycle
#'
#' Delete all lifecycle rules for a bucket.
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' DeleteBucketLifecycle('ross-test')
DeleteBucketLifecycle <- function(name){
  .api.delete.header.request(bucketname=name, query = 'lifecycle')
}

#' DeleteBucketcors
#'
#' @param name
#'
#' @return
#' @export
#'
#' @examples
#' DeleteBucketcors('ross-test')
DeleteBucketcors <- function(name){
  .api.delete.header.request(bucketname=name, query = 'cors')
}
