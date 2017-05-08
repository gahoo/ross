oss <- function(x){
  if(!grepl("^oss://", x)){
    stop("Invalid oss path")
  }
  x <- gsub("^oss://", '', x)
  bucketname <- gsub("/.*", "", x)
  key <- gsub(sprintf("^%s/?", bucketname), '', x)
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
    if(x$key == ""){
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
#####
oss.cp <- function(){}
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
  aclBucket(x$bucket, ...)
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
