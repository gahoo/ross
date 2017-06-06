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

print.oss <- function(x){
  cat(sprintf("oss://%s/%s", x$bucket, x$key))
  invisible(x)
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
    listBucket()
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

ls.Object <- function(x, ...){
  listBucket(x$bucket, x$key, ...)
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

rm.Bucket <- rm.Object <- function(x, ...){
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
  from <- format.oss(from)
  to <- format.oss(to)
  r <- copyObjects(from$key, to$key, from$bucket, to$bucket)

  invisible(r)
}


##### ln
oss.ln <- function(x, ...){
  UseMethod('ln', x)
}

ln.oss <- function(x, target, ...){
  if(!missing(target) && grepl("^oss://", target)){
    target <- gsub("^oss://", "/", target)
  }
  linkObject(x$bucket, x$key, target, ...)
}

ln.character <- function(x, target, ...){
  x <- oss(x)
  ln.oss(x, target, ...)
}

ln.Object <- function(x, target){
  if(missing(target)){
    x$link
  }else{
    target <- ifelse(grepl("^oss://", target), gsub("^oss://", "/", target), target)
    x$link <- target
  }
}
##### exist
oss.exists <- function(x){
  UseMethod('exists', x)
}

exists.oss <- function(x){
  if(is.null(x$key)){
    isBucketExist(x$bucket)
  }else{
    isObjectExist(x$bucket, x$key)
  }
}

exists.character <- function(x){
  x <- oss(x)
  exists.oss(x)
}

exists.Object <- exists.Bucket <-function(x){
  x$exists()
}
##### restore
oss.restore <- function(x){
  UseMethod('restore', x)
}

restore.oss <- function(x){
  restoreObject(x$bucket, x$key)
}

restore.character <- function(x){
  x <- oss(x)
  restore.oss(x)
}

restore.Object <- function(x){
  x$restore()
}
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

"oss.acl<-" <- function(x, value, ...){
  x <- oss(x)
  acl.oss(x, acl=value, ...)
  x
}

acl.oss <- function(x, ...){
  if(is.null(x$key)){
    aclBucket(x$bucket, ...)
  }else{
    if('recursive' %in% names(list(...))){
      aclMultipleObjects(x$bucket, x$key, ...)
    }else{
      aclObject(x$bucket, x$key, ...)
    }
  }
}

acl.character <- function(x, ...){
  x <- oss(x)
  acl.oss(x, ...)
}

acl.Bucket <- acl.Object <- function(x, acl, ...){
  if(missing(acl)){
    x$acl
  }else{
    x$acl <- acl
  }
}
##### stat
oss.stat <- function(x, ...){
  UseMethod('stat', x)
}

stat.oss <- function(x, ...){
  if(is.null(x$key)){
    getBucketInfo(x$bucket)
  }else{
    getObjectInfo(x$bucket, x$key)
  }
}

stat.character <- function(x, ...){
  x <- oss(x)
  stat.oss(x)
}

stat.Bucket <- stat.Object <- function(x, ...){
  x$print()
}
##### meta
#' oss.meta
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.meta('oss://ross-test/test.txt')
#' oss.meta('oss://ross-test/test.txt', meta=list(b=2))
#' oss.meta('oss://ross-test/test.txt', meta=list(a=1))
#' oss.meta('oss://ross-test/test.txt', meta=list(a=NULL))
#' oss.meta('oss://ross-test', meta=NULL)
#' oss.meta('oss://ross-test/test/', recursive=T, meta=list(a=1))

oss.meta <- function(x, ...){
  UseMethod('meta', x)
}

"oss.meta<-" <- function(x, value, ...){
  x <- oss(x)
  meta.oss(x, meta=value, ...)
  x
}

meta.oss <- function(x, ...){
  if(is.null(x$key)){
    stop('Bucket has no meta.')
  }else{
    if('recursive' %in% names(list(...))){
      metaMultipleObjects(x$bucket, x$key, ...)
    }else{
      metaObject(x$bucket, x$key, ...)
    }
  }
}

meta.character <- function(x, ...){
  x <- oss(x)
  meta.oss(x, ...)
}

meta.Object <- function(x, meta, ...){
  if(missing(meta)){
    x$meta
  }else{
    x$meta <- meta
  }
}

##### url
oss.url <- function(x, ...){
  UseMethod('url', x)
}

url.oss <- function(x, expires = 1200){
  urlObject(x$bucket, x$key, expires = expires)
}

url.character <- function(x, expires = 1200){
  x <- oss(x)
  url.oss(x, expires)
}

url.Object <- function(x, expires = 1200){
  x$url(expires)
}
##### read
oss.read <- function(x, ...){
  UseMethod('read', x)
}

read.oss <- function(x, ...){
  readObject(x$bucket, x$key, ...)
}

read.character <- function(x, ...){
  x <- oss(x)
  read.oss(x)
}

read.Object <- function(x, ...){
  x$read(...)
}
##### write
oss.write <- function(x, ...){
  UseMethod('write', x)
}

write.oss <- function(x, content, ...){
  writeObject(x$bucket, x$key, content, ...)
}

write.character <- function(x, content, ...){
  x <- oss(x)
  write.oss(x, content, ...)
}

write.Object <- function(x, content, ...){
  x$write(content, ...)
}
##### file
oss.file <- function(){}
##### save
#' oss.save
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' a <- 1:10
#' oss.save('oss://ross-test/test.RData', a)
oss.save <- function(x, ...){
  UseMethod('save', x)
}

save.oss <- function(x, ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    saveObject(x$bucket, x$key, ...)
  }
}

save.character <- function(x, ...){
  x <- oss(x)
  save.oss(x, ...)
}

save.Object <- function(x, ...){
  x$save(...)
}
##### load
#' oss.load
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.load('oss://ross-test/test.RData')
#' ls()
#' e <- new.env()
#' oss.load('oss://ross-test/test.RData', envir=e)
#' ls(e)
oss.load <- function(x, ...){
  UseMethod('load', x)
}

load.oss <- function(x, envir = parent.frame(), ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    loadObject(x$bucket, x$key, envir = envir, ...)
  }
}

load.character <- function(x, envir = parent.frame(), ...){
  x <- oss(x)
  load.oss(x, envir = envir, ...)
}

load.Object <- function(x, envir = parent.frame(), ...){
  x$load(envir=envir, ...)
}
##### saveRDS
oss.saveRDS <- function(x, ...){
  UseMethod('saveRDS', x)
}

saveRDS.oss <- function(x, object, ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    saveRDS(x$bucket, x$key, ...)
  }
}

saveRDS.character <- function(x, object, ...){
  x <- oss(x)
  saveRDS.oss(x, object, ...)
}

saveRDS.Object <- function(x, ...){
  x$saveRDS(...)
}
##### readRDS
oss.readRDS <- function(x, ...){
  UseMethod('readRDS', x)
}

readRDS.oss <- function(x, ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    readRDSObject(x$bucket, x$key, ...)
  }
}

readRDS.character <- function(x, ...){
  x <- oss(x)
  readRDS.oss(x, ...)
}

readRDS.Object <- function(x, ...){
  x$readRDS(...)
}
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

usage.Bucket <- function(x, ...){
  x$usage(...)
}

usage.Object <- function(x, ...){
  usageBucket(x$bucket, x$key, ...)
}
#####
is.oss <- function(x){
  "oss" %in% class(x) || grepl('^oss://', x)
}

format.oss <- function(x){
  if(is.oss(x)){
    x <- oss(x)
    x
  }else{
    list(bucket=NULL, key=x)
  }
}
