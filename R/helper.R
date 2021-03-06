#' Build Endpoint host
#'
#' @param location bucket location
#' @param internal whether access from aliyun ecs
#'
#' @return
#' @export
#'
#' @examples
.build.endpoint <- function(Location="oss-cn-beijing", internal=TRUE, vpc=FALSE){
  if(internal){
    domain <- "%s-internal.aliyuncs.com"
  }else{
    domain <- "%s.aliyuncs.com"
  }
  if(vpc && !internal){
    domain <- "vpc100-%s.aliyuncs.com"
  }else if(vpc && internal){
    stop('Endpoint can not be both internal and vpc')
  }
  sprintf(domain, Location)
}

#' Build host
#'
#' @param bucketname bucket name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
.build.host <- function(bucketname=NULL, Location=NULL, ...){
  if(is.null(bucketname)){
    "http://oss.aliyuncs.com"
  }else{
    custom_domain <- cdnState(bucketname)
    if(!is.null(custom_domain)) {
      if(!grepl("^http", custom_domain)){
        custom_domain <- paste0("http://", custom_domain)
        }
      return(custom_domain)
    }

    if(is.null(Location)){
      Location <- .get.cache.bucket.location(bucketname)
    }
    endpoint <- .build.endpoint(Location, ...)
    sprintf('http://%s.%s', bucketname, endpoint)
  }
}

.build.header <- function(x) {
  x <- .rm.null(x)
  if(is.null(x)){
    character()
  }else{
    unlist(x)
  }
}

#' @export
.build.ossheader <- function(x) {
  oss_idx <- grep('^x-oss', names(x))
  x <- x[oss_idx]
  x <- .rm.null(x)
  if(is.null(x) || length(x) == 0){
    NULL
  }else{
    order_idx <- order(names(x))
    x <- x[order_idx]
    paste0(paste(names(x), x, sep=":", collapse = '\n'), '\n')
  }
}

.rm.null <- function(x){
  null_idx <- sapply(x, is.null)
  x[!null_idx]
}

.build.ossresource <- function(bucketname, key=NULL, query=NULL){
  if(is.null(key)){
    path <- sprintf("/%s/", bucketname)
  }else{
    path <- sprintf("/%s/%s", bucketname, key)
  }
  is_empty_query <- all(sapply(query, is.null))
  if(!is_empty_query){
    path <- paste0(path, .build.query(query))
  }
  path
}

.build.query <- function(query){
  q <- list(query=query)
  class(q) <- 'url'
  q <- gsub(':///', '', httr::build_url(q))
  gsub('=&', '&', q)
}

#' .build.object.header
#'
#' @param encryption server side encryption algorithm AES256
#' @param acl Access control: private, public-read, public-read-write
#' @param source The source object. /bucketname/objectname
#' @param meta.directive COPY or REPLACE meta info of object. All source meta will be ignore when this value set to REPLACE.
#' @param Range The object content range, '0-99' means the first 100 bytes.
#' @param ETag ETag of object.
#' @param ETag.match Copy when ETag matches or copy when Etag not match.
#' @param since POSIXct time.
#' @param modified.since Copy when object was modified since a time, or copy when object was not modified since a time.
#' @param ... Set header directly. Cache-Control, Content-Disposition, Content-Encoding, Expires, ...
#' @param .md5 boolean Calculate body md5sum automatically or not.
#' @param .meta Other meta info set to the object, < 8k.
#' @param body The content of object, character or upload_file(file_path).
#'
#' @return
#' @export
#'
#' @examples
.build.object.header <- function(acl=NULL, encryption=NULL,
                                 source=NULL, meta.directive=NULL,
                                 Range = NULL,
                                 ETag=NULL, ETag.match=NULL,
                                 since=NULL, modified.since=TRUE,
                                 ..., .md5=TRUE, .meta=NULL,
                                 body=NULL, target=NULL){

  if(!is.null(acl)){
    .check.acl(acl)
  }

  header <- list(
    'x-oss-object-acl' = acl,
    'x-oss-server-side-encryption' = encryption,
    'x-oss-copy-source' = source,
    'x-oss-metadata-directive' = meta.directive,
    'x-oss-symlink-target' = target)
  header <- c(header, list(...))

  if(!is.null(ETag)){
    if(ETag.match){
      header_title <- 'If-Match'
    }else{
      header_title <- 'If-None-Match'
    }
    if(!is.null(source)){
      header_title <- paste0('x-oss-copy-source-', tolower(header_title))
    }
    header[[header_title]] <- ETag
  }

  if(!is.null(since)){
    if(!identical(class(since), c("POSIXct", "POSIXt"))){
      stop('The class of since is not POSIXct or POSIXt')
    }
    if(modified.since){
      header_title <- 'If-Modified-Since'
    }else{
      header_title <- 'If-Unmodified-Since'
    }
    if(!is.null(source)){
      header_title <- paste0('x-oss-copy-source-', tolower(header_title))
    }
    header[[header_title]] <- httr::http_date(since)
  }

  if(is.list(.meta)){
    names(.meta) <- paste0('x-oss-meta-', names(.meta))
    header <- c(header, .meta)
  }

  if(.md5 && !is.null(body)){
    header[['Content-MD5']] <- md5(body)
  }

  if(is.null(header[['Content-Type']])){
    header[['Content-Type']] <- body_type(body)
  }

  if(!is.null(Range)){
    header_title <- 'Range'
    if(!is.null(source)){
      header_title <- paste0('x-oss-copy-source-', tolower(header_title))
    }
    Range <- sprintf("bytes=%s", Range)
    header[[header_title]] <- Range
  }

  .rm.null(header)
}

.extract.header <- function(x, .headers) {
  ifelse(x %in% names(.headers), .headers[[x]], '')
}

#' @import xml2
.except.http_error <- function(response){
  doc <- content(response, encoding = 'UTF-8')
  if(is.null(doc)){
    return(NULL)
  }
  Code <- xml_text(xml_find_all(doc, '/Error/Code'))
  Message <- xml_text(xml_find_all(doc, '/Error/Message'))
  RequestId <- xml_text(xml_find_all(doc, '/Error/RequestId'))
  warning(sprintf("%s:<%s %s> %s", RequestId, response$status_code, Code, Message))
}

#' @import httr
.check.http_error <- function(response){
  is_error <- http_error(response)
  if(is_error){
    .except.http_error(response)
    .print.debug(response)
  }
  is_error
}

.print.debug <- function(response){
  if(getOption('ross.debug')){
    print(xpath2list(content(response, encoding='UTF-8'), '/Error/StringToSign'))
  }
}

.check.acl <- function(acl){
  if(!acl %in% c('private', 'public-read-write', 'public-read')){
    stop('Invalid acl, choose from public-read-write, public-read, private')
  }
}

.get.cache.bucket.location <- function(bucketname) {
  location <- locationState(bucketname)
  if(is.null(location)){
    .get.bucket.location(bucketname)
  }else{
    location
  }
}

#' @import httr
#' @import xml2
.get.bucket.location <- function(bucketname){
  r <- GetBucketLocation(bucketname)
  if(r$status_code == 200){
    location <- unlist(xml2::as_list(httr::content(r, encoding = 'UTF-8')))
    locationState(bucketname, location)
  }else if(r$status_code == 403){
    stop(sprintf("Can't get bucket location. Please try set location manually with locationState('%s', 'oss-cn-xxx').", bucketname))
  }else{
    stop(sprintf("No Bucket Named %s.", bucketname))
  }
  location
}


#' xpath2list
#'
#' Turn xml nodes into list, xpath is supported
#'
#' @param xpath
#' @param doc
#'
#' @return
#' @import xml2
#' @export
#'
#' @examples
#' r <- GetService()
#' xpath2list(r)
#' xpath2list(r, '/ListAllMyBucketsResult/Owner')
#' xpath2list(r, '/ListAllMyBucketsResult/Buckets/Bucket')
xpath2list <- function(doc, xpath=NULL, smart=TRUE){

  extractNodes <- function(doc, xpath){
    nodes <- xml_find_all(doc, xpath)
    lst <- lapply(nodes, function(x){
      as.list(unlist(as_list(x)))
    })
    if(length(lst) == 1 && smart){
      lst[[1]]
    }else{
      lst
    }
  }

  if('xml_document' %in% class(doc)){
    if(is.null(xpath)){
      xml2::as_list(doc)
    }else{
      extractNodes(doc, xpath)
    }
  }else{
    list()
  }

}

#' Title
#'
#' @param x
#' @import digest
#' @import base64enc
#'
#' @return
#' @export
#'
#' @examples
md5 <- function(x) {
  if(class(x) == "form_file"){
    digested_md5 <- digest::digest(file=x$path, serialize=F, algo = 'md5', raw=T)
  }else if(is.character(x)){
    digested_md5 <- digest::digest(x, serialize=F, algo = 'md5', raw=T)
  }else{
    return(NULL)
  }
  base64enc::base64encode(digested_md5)
}

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
body_type <- function(x){
  if(class(x) == 'form_file'){
    x$type
  }else if(is.character(x)){
    'text/plain'
  }else{
    NULL
  }
}

states <- function(name, ..., state){
  idx <- paste(..., sep=":")
  if(length(idx) == 0) return(.state[[name]])
  if(missing(state)){
    .state[[name]][[idx]]
  }else{
    .state[[name]][[idx]] <- state
  }
}

saveStates <- function(bucketname, status_codes, success_code, state_func, status_name=NULL, ...){
  if(!is.null(status_name)) names(status_codes) <- status_name
  failed <- names(status_codes)[status_codes != success_code]
  if(length(failed) > 0){
    state_func(bucketname, ..., state=failed)
    warning('Some files failed:\n', paste0(failed, collapse = '\n'))
  }else{
    state_func(bucketname, ..., state=NULL)
  }
}

#' locationState
#'
#' @param bucketname
#' @param location
#'
#' @return
#' @export
#'
#' @examples
#' locationState('ross-test')
#' locationState('ross-test', 'oss-cn-beijing')
locationState <- function(bucketname, location){
  states('multipart', bucketname, state=location)
}

multiPartUploadState <- function(bucketname, key, state){
  states('multipart', bucketname, key, state=state)
}

uploadState <- function(bucketname, src, prefix, pattern, state){
  states('upload', bucketname, src, prefix, pattern, state=state)
}

downloadState <- function(bucketname, src, dest, pattern, state){
  states('download', bucketname, src, dest, pattern, state=state)
}

copyState <- function(src, dest, src_bucket, dest_bucket, state){
  states('copy', src, dest, src_bucket, dest_bucket, state=state)
}

aclState <- function(bucketname, prefix, acl, recursive, state){
  states('acl', bucketname, prefix, acl, recursive, state=state)
}

#' cdnState
#'
#' @param bucketname
#' @param domain
#'
#' @return
#' @export
#'
#' @examples
#' cdnState('igenecode-report', domain = 'report.igenecode.com')
#' cdnState('igenecode-report')
#' cdnState()
cdnState <- function(bucketname, domain){
  if(missing(bucketname)){
    states('CDN')
  }else{
    states('CDN', bucketname, state=domain)
  }
}

is.installed <- function(name){
  exit_code <- system(paste('which ', name), ignore.stdout = T, ignore.stderr = F)
  if(exit_code == 0){
    TRUE
  }else{
    FALSE
  }
}

is.folder.char <- function(string){
  grepl('/$', string)
}

is.root <- function(string){
  grepl('^/$', string)
}

add.slash <- function(string){
  ifelse(is.folder.char(string), string, paste0(string, '/'))
}

strip.slash <- function(string){
  sub('/$', '', string)
}

smartSize <- function(x, ...){
  if(is.na(x)) return(NA)
  units <- c('B', 'KB', 'MB', 'GB', 'TB', 'PB')
  for(i in 1:6){ if(x < 1024^i) break }
  x <- round(x / 1024^(i-1), ...)
  paste(x, units[i])
}
