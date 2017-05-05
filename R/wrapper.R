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
      question <- sprintf("Are you sure to delete all objects in bucket %s?(yes/no): ", bucketname)
    }else{
      question <- sprintf("Are you sure to delete `%s` in bucket `%s`?(yes/no): ", prefix, bucketname)
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

  if(is.null(prefix) || grepl("/$", prefix)){
    keys <- suppressMessages(listBucket(bucketname, prefix, .all=T, .output = 'character'))
  }else{
    keys <- prefix
  }

  if(length(keys) > 1){
    r <- deleteMultipleObjects(keys)
  }else if(length(keys) == 1){
    r <- DeleteObject(bucketname, keys)
  }else{
#    warning(sprintf("No Such Keys: %s", prefix))
    r <- NULL
  }

  invisible(r)
}

#' usageBucket
#'
#' @param bucketname
#' @param prefix
#' @param unit
#'
#' @return
#' @export
#'
#' @examples
#' usageBucket('ross-test')
#' usageBucket('ross-test', 'upload/')
usageBucket <- function(bucketname, prefix=NULL, unit='MB'){
  conversion <- list(B=1, KB=1024, MB=1024^2, GB=1024^3)
  files <- listBucket(bucketname, prefix, delimiter = '')
  sum(as.numeric(files$Size)) / conversion[[unit]]
}

#' aclBucket
#'
#' @param bucketname
#' @param acl
#'
#' @return
#' @export
#'
#' @examples
#' aclBucket('ross-test')
#' aclBucket('ross-test', 'public-read')
aclBucket <- function(bucketname, acl){
  if(missing(acl)){
    r <- GetBucketAcl(bucketname)
    doc <- httr::content(r, encoding = 'UTF-8')
    unlist(xpath2list(doc, '/AccessControlPolicy/AccessControlList/Grant'))
  }else{
    r <- PutBucket(bucketname, acl = acl)
    invisible(r)
  }
}

#' uploadObject
#'
#' @param bucketname
#' @param src
#' @param dest
#' @param split
#' @param ...
#' @param maxPartSize
#'
#' @return
#' @export
#'
#' @examples
#' uploadObject('ross-test', 'test.zip')
#' uploadObject('ross-test', 'test.zip', 'test/test.zip')
uploadObject <- function(bucketname, src, dest=NULL, resume=TRUE, split=10, maxPartSize = 20 * 1024^2, ...){
  uploadPart <- function(src, key, uploadId, partPosition, partSize, partNumber){
    raw_conn<-file(src, 'rb', raw = T)
    seek(raw_conn, partPosition)
    src_content<-readBin(raw_conn, what='raw', n=partSize)
    tryCatch(
      r <- UploadPart(bucketname, key, uploadId, partNumber, src_content),
      error = function(e){
        r <- list(status_code=404)
      },
      finally = function(){
        close(raw_conn)
      }
    )
    names(r$status_code)<-partNumber
    r$status_code
  }

  multiPartUpload <- function(bucketname, src, key, ...){
    uploadId <- getMultiPartUploadState(bucketname, key)$uploadId
    task_params <- makePartParams(file_size, split, maxPartSize)

    if(resume && !is.null(uploadId)){
      failed <- getMultiPartUploadState(bucketname, key)$failed
      task_params <- task_params[failed]
    }else{
      r <- InitiateMultipartUpload(bucketname, key, ...)
      uploadId <- unlist(xpath2list(httr::content(r, encoding = 'UTF-8'), '//UploadId'))
    }

    cl <- parallel::makeForkCluster(split)
    status_codes <- parallel::parLapply(cl, task_params, function(x) {
      uploadPart(src, key, uploadId, x$partPosition, x$partSize, x$partNumber)
    })
    parallel::stopCluster(cl)
    status_codes <- unlist(status_codes)
    if(all(status_codes == 200)){
      saveMultiPartUploadState(bucketname, key)
      CompleteMultipartUpload(bucketname, key, uploadId)
    }else{
      failed_idx <- which(status_codes != 200)
      failed_partNumber <- names(status_codes[failed_idx])
      failed_partNumber <- as.numeric(failed_partNumber)
      warning(sprintf("Part %s Failed.", paste0(failed_partNumber, collapse = ', ')))
      saveMultiPartUploadState(
        bucketname, key,
        list(uploadId=uploadId,
             failed = failed_partNumber)
        )
      failed_partNumber
    }

  }

  makePartParams <- function(file_size, split, maxPartSize){
    part_size <- ceiling(file_size / split)
    if(part_size > maxPartSize){
      part_size <- maxPartSize
      split <- ceiling(file_size / maxPartSize)
    }
    last_part_size <- file_size - (split-1) * part_size
    lapply(1:split, function(i){list(
      partNumber = i,
      partPosition = (i-1) * part_size,
      partSize = ifelse(i==split, part_size, last_part_size)
    )})
  }

  if(file.exists(src) && !dir.exists(src)){
    file_size <- file.size(src)
  }else{
    stop(sprintf('File not exists! %s', src))
  }

  if(is.null(dest)){
    key = src
  }else{
    key = dest
  }
  if(file_size < 10 * 1024^2){
    r <- PutObject(bucketname, key, body=httr::upload_file(src), ...)
  }else{
    r <- multiPartUpload(bucketname, src, key, ...)
  }
  invisible(r)
}

uploadMultipleObjects <- function(bucketname, src, prefix='/', pattern=NULL, resume=TRUE, split=10, ..., .parallel = TRUE){
  prepareSrc <- function(src, pattern=NULL){
    if(length(src) != 1){
      stop('src length not equals to 1.')
    }
    if(resume){
      files <- getMultiUploadState(bucketname, src, prefix, pattern)
      if(!is.null(files)){
        return(files)
      }
    }

    if(dir.exists(src)){
      files <- list.files(src, pattern, recursive = T, full.names = T)
    }else if(file.exists(src)){
      files <- src
    }else{
      stop(sprintf("%s not exist!\n", files[!is_exist]))
    }
    gsub("/+", "/", files)
  }

  prepareDest <- function(files, src, prefix='.'){
    if(grepl('/$', prefix)){
      path_fix <- dirname(src)
      if(path_fix == '.'){
        path_fix <- ""
      }
    }else{
      path_fix <- src
    }

    dest <- gsub(sprintf("^%s", path_fix), '', files)
    dest <- file.path(prefix, dest)
    dest <- gsub("/+", "/", dest)
    dest <- gsub('\\./+', '', dest)
    dest <- gsub('^/', '', dest)
    dest <- gsub('/$', '', dest)
    dest
  }

  makeTaskParams <- function(files, dests){
    if(length(files) == 0){
      return(invisible())
    }
    lapply(1:length(files), function(i){
      list(src = files[i],
           dest = dests[i])
    })
  }

  uploadEachFile <- function(task) {
    # tryCatch(
    #   r <- uploadObject(bucketname, task$src, task$dest, resume, split, ...),
    #   error = function(e){
    #     r <- list(status_code = 404)
    #   }
    # )
    r <- try(uploadObject(bucketname, task$src, task$dest, resume, split, ...))
    if(class(r) == 'try-error'){
      r <- list(status_code = 404)
    }
    r$status_code
  }

  files <- prepareSrc(src, pattern)
  dests <- prepareDest(files, src, prefix)
  message(length(files), ' files to upload')
  task_params <- makeTaskParams(files, dests)
  if(.parallel){
    cl <- parallel::makeForkCluster(split)
    status_codes <- parallel::parLapply(cl, task_params, uploadEachFile)
    parallel::stopCluster(cl)
  }else{
    status_codes <- lapply(task_params, uploadEachFile)
  }

  status_codes <- unlist(status_codes)
  names(status_codes) <- files
  failed <- names(status_codes)[status_codes != 200]
  if(length(failed) > 0){
    saveMultiUploadState(bucketname, src, prefix, pattern, failed)
    invisible(failed)
  }else{
    invisible(status_codes)
  }

}

abortAllMultipartUpload <- function(bucketname, prefix=NULL){
  r <- ListMultipartUploads(bucketname, prefix)
  doc <- httr::content(r, encoding = 'UTF-8')
  keys <- unlist(xpath2list(doc, '//Key'))
  uploadIds <- unlist(xpath2list(doc, '//UploadId'))
  if(!is.null(keys)){
    for(i in 1:length(keys)){
      message("Aborting", keys[i], ":", uploadIds[i])
      AbortMultipartUpload(bucketname, keys[i], uploadIds[i])
    }
  }
}
