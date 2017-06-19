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
    listBucket(...)
  }else{
    UseMethod('ls', x)
  }
}

#' @export
ls.oss <- function(x, ...){
  listBucket(x$bucketname, prefix=x$key, ...)
}

#' @export
ls.character <- function(x, ...){
  x <- oss(x)
  if(x$bucketname == ""){
    ls.BucketList()
  }else{
    ls.oss(x, ...)
  }
}

#' @export
ls.Bucket <- function(x, ...){
  x$list(...)
}

#' @export
ls.BucketList <- function(x, ...){
  if(missing(x)){
    x <- BucketList$new()
  }
  x$list(...)
}

#' @export
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

#' @export
mb.oss <- function(x, ...){
  createBucket(x$bucketname, ...)
}

#' @export
mb.character <- function(x, ...){
  x <- oss(x)
  mb.oss(x, ...)
}

#' @export
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

#' @export
rm.oss <- function(x, ..., .all=FALSE){
  if(x$bucketname == ""){
    stop('Invalid oss path.')
  }else{
    if(is.null(x$key)){
      if(.all) removeObjects(x$bucketname, prefix=x$key, ...)
      deleteBucket(x$bucketname)
    }else{
      removeObjects(x$bucketname, prefix=x$key, ...)
    }
  }
}

#' @export
rm.character <- function(x, ...){
  x <- oss(x)
  rm.oss(x, ...)
}

#' @export
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

##### mv
oss.mv <- function(from, to, ...){
  from <- format.oss(from)
  to <- format.oss(to)
  r <- moveObjects(from$key, to$key, from$bucket, to$bucket)

  invisible(r)
}

##### ln
#' oss.ln
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.ln('oss://ross-test/linked-test.txt', 'oss://ross-test/test.txt')
#' oss.ln('oss://ross-test/linked-test.txt')
#' oss.ln('oss://ross-test/linked-test.txt', '/ross-test/test2.txt')
#' oss.ln('oss://ross-test/linked-test.txt')
#'
#' o <- Object$new('ross-test', 'linked-test2.txt')
#' oss.ln(o, '/ross-test/test3.txt')
#' oss.ln(o)

oss.ln <- function(x, ...){
  UseMethod('ln', x)
}

#' @export
ln.oss <- function(x, target, ...){
  if(!missing(target) && grepl("^oss://", target)){
    target <- gsub(paste0("^oss://", x$bucket, "/"), "", target)
  }
  linkObject(x$bucket, x$key, target, ...)
}

#' @export
ln.character <- function(x, target, ...){
  x <- oss(x)
  ln.oss(x, target, ...)
}

#' @export
ln.Object <- function(x, target){
  if(missing(target)){
    x$link
  }else{
    target <- ifelse(grepl("^oss://", target), gsub("^oss://", "/", target), target)
    x$link <- target
  }
}
##### exist
#' oss.exists
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' oss.exists('oss://ross-test')
#' oss.exists('oss://ross-test/test.txt')
#'
#' o <- Object$new('ross-test', 'test.txt')
#' oss.exists(o)
#'
#' b <- Bucket$new('ross-test')
#' oss.exist(b)
oss.exists <- function(x){
  UseMethod('exists', x)
}

#' @export
exists.oss <- function(x){
  if(is.null(x$key)){
    isBucketExist(x$bucket)
  }else{
    isObjectExist(x$bucket, x$key)
  }
}

#' @export
exists.character <- function(x){
  x <- oss(x)
  exists.oss(x)
}

#' @export
exists.Object <- exists.Bucket <-function(x){
  x$exists()
}
##### restore
#' oss.restore
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' oss.mb('oss://ross-test-arch', StorageClass='Archive')
#' oss.write('oss://ross-test-arch/test.txt', 'test')
#' oss.restore('oss://ross-test-arch/test.txt')
#'
#' o <- Object$new('ross-test-arch', 'test.txt')
#' oss.restore(o)
oss.restore <- function(x){
  UseMethod('restore', x)
}

#' @export
restore.oss <- function(x){
  restoreObject(x$bucket, x$key)
}

#' @export
restore.character <- function(x){
  x <- oss(x)
  restore.oss(x)
}

#' @export
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
#'
#' oss.acl('oss://ross-test/test.txt')
#' oss.acl('oss://ross-test/test.txt', acl='private')
oss.acl <- function(x, ...){
  UseMethod('acl', x)
}

"oss.acl<-" <- function(x, value, ...){
  x <- oss(x)
  acl.oss(x, acl=value, ...)
  x
}

#' @export
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

#' @export
acl.character <- function(x, ...){
  x <- oss(x)
  acl.oss(x, ...)
}

#' @export
acl.Bucket <- acl.Object <- function(x, acl, ...){
  if(missing(acl)){
    x$acl
  }else{
    x$acl <- acl
  }
}
##### stat
#' oss.stat
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.stat('oss://ross-test')
#' oss.stat('oss://ross-test/test.txt')
oss.stat <- function(x, ...){
  UseMethod('stat', x)
}

#' @export
stat.oss <- function(x, ...){
  if(is.null(x$key)){
    getBucketInfo(x$bucket)
  }else{
    getObjectInfo(x$bucket, x$key)
  }
}

#' @export
stat.character <- function(x, ...){
  x <- oss(x)
  stat.oss(x)
}

#' @export
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

#' @export
"oss.meta<-" <- function(x, value, ...){
  x <- oss(x)
  meta.oss(x, meta=value, ...)
  x
}

#' @export
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

#' @export
meta.character <- function(x, ...){
  x <- oss(x)
  meta.oss(x, ...)
}

#' @export
meta.Object <- function(x, meta, ...){
  if(missing(meta)){
    x$meta
  }else{
    x$meta <- meta
  }
}

##### url
#' oss.url
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.url('oss://ross-test/test.txt', expires = 600)
oss.url <- function(x, ...){
  UseMethod('url', x)
}

#' @export
url.oss <- function(x, expires = 1200){
  urlObject(x$bucket, x$key, expires = expires)
}

#' @export
url.character <- function(x, expires = 1200){
  x <- oss(x)
  url.oss(x, expires)
}

#' @export
url.Object <- function(x, expires = 1200){
  x$url(expires)
}
##### read
#' oss.read
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.read('oss://ross-test/test.txt')
#' oss.read('oss://ross-test/test.txt', Range='0-100')
oss.read <- function(x, ...){
  UseMethod('read', x)
}

#' @export
read.oss <- function(x, ...){
  readObject(x$bucket, x$key, ...)
}

#' @export
read.character <- function(x, ...){
  x <- oss(x)
  read.oss(x)
}

#' @export
read.Object <- function(x, ...){
  x$read(...)
}
##### write
#' oss.write
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.write('oss://ross-test/test.txt', 'test')
oss.write <- function(x, ...){
  UseMethod('write', x)
}

#' @export
write.oss <- function(x, content, ...){
  writeObject(x$bucket, x$key, content, ...)
}

#' @export
write.character <- function(x, content, ...){
  x <- oss(x)
  write.oss(x, content, ...)
}

#' @export
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

#' @export
save.oss <- function(x, ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    saveObject(x$bucket, x$key, ...)
  }
}

#' @export
save.character <- function(x, ...){
  x <- oss(x)
  save.oss(x, ...)
}

#' @export
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

#' @export
load.oss <- function(x, envir = parent.frame(), ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    loadObject(x$bucket, x$key, envir = envir, ...)
  }
}

#' @export
load.character <- function(x, envir = parent.frame(), ...){
  x <- oss(x)
  load.oss(x, envir = envir, ...)
}

#' @export
load.Object <- function(x, envir = parent.frame(), ...){
  x$load(envir=envir, ...)
}
##### saveRDS
#' oss.saveRDS
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.saveRDS('oss://ross-test/test.rds', 1:5)
oss.saveRDS <- function(x, ...){
  UseMethod('saveRDS', x)
}

#' @export
saveRDS.oss <- function(x, object, ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    saveRDSObject(x$bucket, x$key, object, ...)
  }
}

#' @export
saveRDS.character <- function(x, object, ...){
  x <- oss(x)
  saveRDS.oss(x, object, ...)
}

#' @export
saveRDS.Object <- function(x, ...){
  x$saveRDS(...)
}
##### readRDS
#' oss.readRDS
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.readRDS('oss://ross-test/test.rds')
#' e <- new.env()
#' oss.readRDS('oss://ross-test/test.rds', envir=e)
oss.readRDS <- function(x, ...){
  UseMethod('readRDS', x)
}

#' @export
readRDS.oss <- function(x, ...){
  if(is.null(x$key)){
    stop('Key must be specified')
  }else{
    readRDSObject(x$bucket, x$key, ...)
  }
}

#' @export
readRDS.character <- function(x, ...){
  x <- oss(x)
  readRDS.oss(x, ...)
}

#' @export
readRDS.Object <- function(x, ...){
  x$readRDS(...)
}
##### usage
#' oss.usage
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' oss.usage('oss://ross-test/')
#' oss.usage('oss://ross-test/', unit='KB')
#' oss.usage('oss://ross-test/test/')
#' oss.usage('oss://ross-test/test/test.txt')
oss.usage <- function(x, ...){
  UseMethod('usage', x)
}

#' @export
usage.oss <- function(x, ...){
  usageBucket(x$bucketname, x$key, ...)
}

#' @export
usage.character <- function(x, ...){
  x <- oss(x)
  usage.oss(x, ...)
}

#' @export
usage.Bucket <- function(x, ...){
  x$usage(...)
}

#' @export
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
