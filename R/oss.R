oss <- function(x){
  if('oss' %in% class(x)){
    return(x)
  }
  if(!grepl("^oss://", x)){
    stop("Invalid oss path")
  }
  x <- gsub("^oss://", '', x)
  bucketname <- gsub("/.*", "", x)
  key <- gsub(sprintf("^%s/?", bucketname), '', x)
  if(key == '') key <- NULL
  structure(list(bucketname=bucketname, key=key), class='oss')
}
##### ls
#' oss.ls
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.ls()
#' bl <- BucketList$new()
#' oss.ls(bl)
#'
#' oss.ls(oss('oss://ross-test'))
#' oss.ls('oss://ross-test')
#' b <- Bucket$new('ross-test')
#' oss.ls(b)
oss.ls <- function(x, ...){
  if(missing(x)){
    ls.BucketList()
  }else{
    UseMethod('ls', x)
  }
}

ls.oss <- function(x, ...){
  listBucket(x$bucketname, prefix=x$key, ...)
}

ls.character <- function(x, ...){
  x <- oss(x)
  if(x$bucketname == ""){
    ls.BucketList()
  }else{
    ls.oss(x, ...)
  }
}

ls.Bucket <- function(x, ...){
  x$list(...)
}

ls.BucketList <- function(x, ...){
  if(missing(x)){
    x <- BucketList$new()
  }
  x$list(...)
}
##### new bucket
#' oss.mb
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.mb(oss('oss://ross-test'))
#' oss.mb('oss://ross-test')
#' b <- Bucket$new('ross-test')
#' oss.mb(b)
oss.mb <- function(x, ...){
  UseMethod('mb', x)
}

mb.oss <- function(x, ...){
  createBucket(x$bucketname, ...)
}

mb.character <- function(x, ...){
  x <- oss(x)
  mb.oss(x, ...)
}

mb.Bucket <- function(x, ...){
  x$create()
}
##### rm
#' oss.rm
#'
#' @param x oss path, Bucket
#' @param confirm auto confirm
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.rm(oss('oss://ross-test/test.txt'))
#' oss.rm('oss://ross-test/test.txt', confirm=T)
#' oss.rm('oss://ross-test/', confirm=T)
#' oss.rm('oss://ross-test/upload/', confirm=T)
#' b <- Bucket$new('ross-test')
#' oss.rm(b)
oss.rm <- function(x, ...){
  UseMethod('rm', x)
}

rm.oss <- function(x, ...){
  if(x$bucketname == ""){
    stop('Invalid oss path.')
  }else{
    if(is.null(x$key)){
      deleteBucket(x$bucketname)
    }else{
      removeObjects(x$bucketname, x$key, ...)
    }
  }
}

rm.character <- function(x, ...){
  x <- oss(x)
  rm.oss(x, ...)
}

rm.Bucket <- function(x, ...){
  x$delete()
}
##### cp
#' oss.cp
#'
#' @param from
#' @param to
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' # Download
#' oss.cp('oss://ross-test/success', '/Volumes/RamDisk/')
#'
oss.cp <- function(from, to, ...){
  from <- format_oss(from)
  to <- format_oss(to)
  r <- copyObjects(from$key, to$key, from$bucket, to$bucket)

  invisible(r)
}


#####
oss.ln <- function(){}
##### acl
#' oss.acl
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.acl('oss://ross-test')
#' oss.acl('oss://ross-test', acl='public-read')
oss.acl <- function(x, ...){
  UseMethod('acl', x)
}

acl.oss <- function(x, ...){
  if(is.null(x$key)){
    aclBucket(x$bucket, ...)
  }else{
    aclObject(x$bucket, x$key, ...)
  }
}

acl.character <- function(x, ...){
  x <- oss(x)
  acl.oss(x, ...)
}
#####
oss.meta <- function(){}

oss.read <- function(){}

oss.write <- function(){}

oss.file <- function(){}
##### usage
oss.usage <- function(x, ...){
  UseMethod('usage', x)
}

usage.oss <- function(x, ...){
  usageBucket(x$bucketname, x$key, ...)
}

usage.character <- function(x, ...){
  x <- oss(x)
  usage.oss(x, ...)
}

#####
is_oss <- function(x){
  "oss" %in% class(x) || grepl('^oss://', x)
}

format_oss <- function(x){
  if(is_oss(x)){
    x <- oss(x)
    x
  }else{
    list(bucket=NULL, key=x)
  }
}
