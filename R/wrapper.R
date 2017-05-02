createBucket <- function(bucketname, Location="oss-cn-beijing", acl = "private", StorageClass="Standard"){
  r <- PutBucket(bucketname, Location, acl, StorageClass)
  if(r$status_code == 200){
    message(sprintf("New Bucket %s with %s access and %s storage is created on %s.", bucketname, acl, StorageClass, Location))
    .state$location[[bucketname]] <- Location
  }
  invisible(r)
}

deleteBucket <- function(bucketname){
  r <- DeleteBucket(bucketname)
  if(r$status == 204){
    .state$location[[bucketname]] <- NULL
  }
  invisible(r)
}

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
  }else if(.output == 'oss-obj'){

  }else if(.output == 'character'){
    sapply(contents, function(x) x$Key)
  }
}

removeObjects <- function(bucketname, keys, step=1000){
  cnt <- ceiling(length(keys)/step)
  response <- list()
  for(i in 1:cnt){
    start = (cnt - 1 ) * step + 1
    end = ifelse(length(keys) < cnt * step, length(keys), cnt * step)
    response[[i]] <- DeleteMultipleObjects(bucketname, keys, quiet = T)
  }
  invisible(response)
}
