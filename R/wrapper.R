#' createBucket
#'
#' @param bucketname
#' @param Location
#' @param acl
#' @param StorageClass
#' @inheritParams PutBucket
#'
#' @return
#' @export
#'
#' @examples
#' createBucket('ross-test')
createBucket <- function(bucketname, Location="oss-cn-beijing", acl = "private", StorageClass="Standard"){
  r <- PutBucket(bucketname, Location, acl, StorageClass)
  if(r$status_code == 200){
    message(sprintf("New Bucket %s with %s access and %s storage is created on %s.", bucketname, acl, StorageClass, Location))
    .state$location[[bucketname]] <- Location
  }
  invisible(r)
}

#' deleteBucket
#'
#' @inherit DeleteBucket
#'
#' @return
#' @export
#'
#' @examples
#' deleteBucket('ross-test')
deleteBucket <- function(bucketname){
  r <- DeleteBucket(bucketname)
  if(r$status == 204){
    .state$location[[bucketname]] <- NULL
  }
  invisible(r)
}

#' listBucket
#'
#' @inheritParams GetBucket
#' @param .all No Paging
#' @param .output output format, choose from data.frame, list, oss, character. default is data.frame
#'
#' @return
#' @export
#'
#' @examples
#' listBucket('ross-test')
#' listBucket('ross-test', 'upload')
#' listBucket('ross-test', 'upload', 'upload/file1', '/', '10')
listBucket <- function(bucketname, prefix=NULL, marker=NULL, delimiter='/', max_keys='1000', .all = TRUE, .output="data.frame"){
  isTruncated <- function(doc){
    xpath2list(doc, '/ListBucketResult/IsTruncated') == 'true'
  }

  parseXML <- function(doc){
    files <- xpath2list(doc, '/ListBucketResult/Contents', F)
    folders <- xpath2list(doc, '/ListBucketResult/CommonPrefixes', F)
    folders <- lapply(folders, 'names<-', 'Key')
    lapply(c(folders, files), as.data.frame, stringsAsFactors=F)
  }

  contents <- list()
  next_marker <- marker
  repeat{
    r <- GetBucket(bucketname, prefix, next_marker, delimiter, max_keys)
    doc <- httr::content(r, encoding = 'UTF-8')
    contents <- c(contents, parseXML(doc))
    next_marker <- xpath2list(doc, '/ListBucketResult/NextMarker')
    message(sprintf("%s objects listed.", length(contents)))
    if(!isTruncated(doc) || !.all){
      break
    }
  }

  if(.output == "data.frame"){
    plyr::ldply(contents)
  }else if(.output == 'list'){
    contents
  }else if(.output == 'oss'){

  }else if(.output == 'character'){
    sapply(contents, function(x) x$Key)
  }
}

#' removeObjects
#'
#' @param bucketname
#' @param keys Objects to delete.
#' @param confirm Auto confirm deletion or not.
#' @param step How much to delete at a time.
#'
#' @return
#' @export
#'
#' @examples
#' removeObjects('ross-test')
#' removeObjects('ross-test', 'upload/')
#' removeObjects('ross-test', 'upload/', confirm=TRUE)
removeObjects <- function(bucketname, prefix=NULL, confirm=FALSE, step=1000){
  if(!confirm){
    if(is.null(prefix)){
      question <- "Are you sure to delete all objects in this bucket?(yes/no): "
    }else{
      question <- sprintf("Are you sure to delete %s in this bucket?(yes/no): ", prefix)
    }
    confirm<-readline(question)
    if(confirm != 'yes'){
      return(message('Abort!'))
    }
  }

  deleteMultipleObjects <- function(keys){
    cnt <- ceiling(length(keys)/step)
    response <- list()
    for(i in 1:cnt){
      start = (cnt - 1 ) * step + 1
      end = ifelse(length(keys) < cnt * step, length(keys), cnt * step)
      response[[i]] <- DeleteMultipleObjects(bucketname, keys, quiet = T)
    }
    response
  }

  if(grepl("/$", prefix)){
    keys <- listBucket(bucketname, prefix, .all=T, .output = 'character')
  }else{
    keys <- prefix
  }

  if(length(keys) > 1){
    r <- deleteMultipleObjects(keys)
  }else if(length(keys) == 1){
    r <- DeleteObject(bucketname, keys)
  }else{
    warning(sprintf("No Such Keys: %s", prefix))
    r <- NULL
  }

  invisible(r)
}
