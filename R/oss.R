oss <- function(x){
  if(!grepl("^oss://", x)){
    stop("Invalid oss path")
  }
  x <- gsub("^oss://", '', x)
  bucketname <- gsub("/.*", "", x)
  key <- gsub(sprintf("^%s/?", bucketname), '', x)
  structure(list(bucketname=bucketname, key=key), class='oss')
}

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


oss.mkdir <- function(){}

oss.newbk <- function(){}

oss.rm <- function(){}

oss.cp <- function(){}

oss.acl <- function(){}

oss.meta <- function(){}

oss.read <- function(){}

oss.write <- function(){}

oss.file <- function(){}

oss.usage <- function(){}
