#' createBucket
#'
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
#' @param prefix The prefix of objects to be deleted. NULL means all objects in the bucket. Prefix ended with "/" means it's a folder.
#' @param confirm Auto confirm deletion or not.
#' @param step How many to delete at a time.
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

  if(is.null(prefix)){
    keys <- suppressMessages(listBucket(bucketname, prefix, delimiter = '', .all=T, .output = 'character'))
  }else if(grepl("/$", prefix)){
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
#' Check disk usage of buckets or "folder".
#'
#' @param bucketname
#' @param prefix The prefix of objects. NULL means the whole bucket.
#' @param unit return in which unit, B, KB, MB, GB, TB
#'
#' @return
#' @export
#'
#' @examples
#' usageBucket('ross-test')
#' usageBucket('ross-test', 'upload/')
usageBucket <- function(bucketname, prefix=NULL, unit='MB'){
  conversion <- list(B=1, KB=1024, MB=1024^2, GB=1024^3, TB=1024^4)
  files <- listBucket(bucketname, prefix, delimiter = '')
  sum(as.numeric(files$Size)) / conversion[[unit]]
}

#' aclBucket
#'
#' @param bucketname
#' @param acl
#' @inheritParams PutBucket
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
#' @param src Path to the local file to be uploaded.
#' @param dest Destination path on bucket. Ended with "/" means its a folder.
#' @param split How many parts to be splited. Will be recacluated along with maxPartSize.
#' @param maxPartSize The max size of each part.
#' @param minMultiSize File szie greater than minMultiSize will be splited automatically.
#' @param ... Other arguments pass to InitiateMultipartUpload.
#' @param resume Auto resume from last failed upload or not.
#' @param .progressbar Show progress bar or not. progress bar only work with multipart upload.
#'
#' @import pbapply
#' @import parallel
#'
#' @return
#' @export
#'
#' @examples
#' uploadObject('ross-test', 'test.zip')
#' uploadObject('ross-test', 'test.zip', 'test/test.zip')
uploadObject <- function(bucketname, src, dest=NULL, resume=TRUE, split=5, maxPartSize = 20 * 1024^2, minMultiSize = 10 * 1024^2, .progressbar=TRUE, ...){
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
    uploadId <- multiPartUploadState(bucketname, key)$uploadId
    task_params <- makePartParams(file_size, split, maxPartSize)

    if(resume && !is.null(uploadId)){
      failed <- multiPartUploadState(bucketname, key)$failed
      task_params <- task_params[failed]
    }else{
      r <- InitiateMultipartUpload(bucketname, key, ...)
      uploadId <- unlist(xpath2list(httr::content(r, encoding = 'UTF-8'), '//UploadId'))
    }

    if(.progressbar){
      op <- pbapply::pboptions(type = "timer")
    }else{
      op <- pbapply::pboptions(type = "none")
    }

    cl <- parallel::makeForkCluster(split)
    status_codes <- pbapply::pbsapply(task_params, function(x) {
      uploadPart(src, key, uploadId, x$partPosition, x$partSize, x$partNumber)
    }, cl=cl)
    parallel::stopCluster(cl)
    pbapply::pboptions(op)

    if(all(status_codes == 200)){
      multiPartUploadState(bucketname, key, state=NULL)
      CompleteMultipartUpload(bucketname, key, uploadId)
    }else{
      failed_idx <- which(status_codes != 200)
      failed_partNumber <- names(status_codes[failed_idx])
      failed_partNumber <- as.numeric(failed_partNumber)
      warning(sprintf("Part %s Failed.", paste0(failed_partNumber, collapse = ', ')))
      multiPartUploadState(
        bucketname, key,
        state = list(uploadId=uploadId,
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
    if(grepl("^/", src)){
      key = basename(src)
    }else{
      key = src
    }
  }else{
    if(grepl("/$", dest)){
      key = gsub('/+', '/', file.path(dest, basename(src)))
    }else{
      key = dest
    }
  }
  if(file_size < minMultiSize){
    r <- PutObject(bucketname, key, body=httr::upload_file(src), ...)
  }else{
    r <- multiPartUpload(bucketname, src, key, ...)
  }
  invisible(r)
}

#' uploadMultipleObjects
#'
#' @param bucketname
#' @inheritParams uploadObject
#' @param src Path to local file or dir to be uploaded.
#' @param prefix Destination on Bucket. Ended with "/" means folder, files will place under "prefix/src/*". otherwise will be "prefix/*".
#' @param pattern Filter which files to be uploaded.
#' @param split How many upload progress at the same time.
#' @param ... Arguments pass to uploadObject.
#' @param .parallel Parallel multiple upload or not. When False, split will be disable too.
#' @param .progressbar Show progress bar or not.
#'
#' @import pbapply
#' @import parallel
#'
#' @return named status_codes indicates failed or success.
#' @export
#'
#' @examples
#' uploadMultipleObjects('ross-test', 'R',  .parallel = F)
#' uploadMultipleObjects('ross-test', 'R', 'test', .parallel = F)
#' uploadMultipleObjects('ross-test', 'R', 'test/', .parallel = T)

uploadMultipleObjects <- function(bucketname, src, prefix='/', pattern=NULL, resume=TRUE, split=5, .progressbar=TRUE, ..., .parallel = TRUE){
  prepareSrc <- function(src, pattern=NULL){
    if(length(src) != 1){
      stop('src length not equals to 1.')
    }
    if(resume){
      files <- uploadState(bucketname, src, prefix, pattern)
      if(!is.null(files)){
        return(files)
      }
    }

    if(dir.exists(src)){
      files <- list.files(src, pattern, recursive = T, full.names = T)
    }else if(file.exists(src)){
      files <- src
    }else{
      stop(sprintf("%s not exist!\n", files))
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
#    message(task$src, ' => ', task$dest)
    r <- try(uploadObject(bucketname, task$src, task$dest, resume, split, .progressbar=.progressbar, ...))
    if(class(r) == 'try-error'){
      r <- list(status_code = 404)
    }
    r$status_code
  }

  src <- path.expand(src)
  files <- prepareSrc(src, pattern)
  dests <- prepareDest(files, src, prefix)
  message(length(files), ' files to upload')
  task_params <- makeTaskParams(files, dests)
  if(is.null(task_params)){
    return(invisible())
  }
  if(.progressbar){
    op <- pbapply::pboptions(type = "timer")
  }else{
    op <- pbapply::pboptions(type = "none")
  }
  if(.parallel){
    cl <- parallel::makeForkCluster(split)
    status_codes <- pbapply::pbsapply(task_params, uploadEachFile, cl=cl)
    parallel::stopCluster(cl)
  }else{
    status_codes <- pbapply::pbsapply(task_params, uploadEachFile)
  }
  pbapply::pboptions(op)

  names(status_codes) <- files
  failed <- names(status_codes)[status_codes != 200]
  if(length(failed) > 0){
    uploadState(bucketname, src, prefix, pattern, state=failed)
    warning("Some files failed to upload:\n", paste0(failed, collapse = '\n'))
  }else{
    uploadState(bucketname, src, prefix, pattern, state=NULL)
  }
  invisible(status_codes)

}

#' downloadObject
#'
#' @param bucketname
#' @param src The object path on bucket to be downloaded.
#' @param dest Local destination of file or folder.
#' @param resume Auto resume from failed download or not.
#' @param split How many parts to be split.
#' @param method Same argument in download.file. Supports aria2 if installed.
#' @param quiet Suppress status messages or not.
#' @param .md5 Check md5 after download or not.
#' @param rpc aria2 rpc
#' @param extra additional command-line arguments for the "wget", "curl" and "aria2" methods.
#' @param ... Arguments pass to download.file
#'
#' @return
#' @export
#'
#' @examples
#' downloadObject('ross-test', 'jingyi.pdf')
#' downloadObject('ross-test', 'jingyi.pdf', '~')
#' downloadObject('ross-test', 'jingyi.pdf', '~/jingyi2.pdf', resume = F, method = 'aria2', quiet = T)
#' downloadObject('ross-test', 'jingyi.pdf', '~/jingyi2.pdf', resume = T, method = 'wget', quiet = F)
downloadObject <- function(bucketname, src, dest=NULL,
                           resume=TRUE, split=5, method='aria2',
                           quiet=FALSE, extra="", .md5=T, rpc=NULL, ...){
  if(is.null(dest)){
    dest <- file.path(getwd(), basename(src))
  }else{
    dest <- path.expand(dest)
    if(dir.exists(dest)){
      dest <- file.path(dest, basename(src))
    }
  }

  url <- GetObject(bucketname, src, expires = 1200, .url = T)
  if(resume) extra <- c(extra, " -c")

  if(method == 'aria2'){
    dir_dest <- dirname(dest)
    base_dest <- basename(dest)
    if(split) extra <- c(extra, " -s ", split)
    if(quiet) extra <- c(extra, " -q ")
    r <- system(paste("aria2c", paste(extra, collapse = " "),
                      shQuote(url), "-d", shQuote(dir_dest),
                      "--allow-overwrite",
                      "-o", shQuote(base_dest)))
  }else{
    r <- download.file(url, dest, method=method, quiet = quiet, extra = extra, ...)
  }

  if(.md5){
    r_head <- HeadObject(bucketname, src)
    remote_md5 <- httr::headers(r_head)$`content-md5`
    if(!is.null(remote_md5)){
      local_md5 <- md5(curl::form_file(dest))
      if(!identical(remote_md5, local_md5)){
        stop('md5sum mismatch ', dest)
      }
    }
  }
  invisible(r)
}

#' downloadMultipleObjects
#'
#' @param bucketname
#' @param src The objects to be downloaded. Ended with "/" means the whold "folder" will be download.
#' @param dest Local destination to save the files.
#' @param pattern Filter which files to be uploaded.
#' @param resume Auto resume from last failed download or not.
#' @param split How many download progress at the same time.
#' @param ... Arguments pass to downloadObject.
#'
#' @inheritParams downloadObject
#' @inheritParams uploadMultipleObjects
#'
#' @return named status_codes indicates failed or success.
#' @export
#'
#' @examples
#' downloadMultipleObjects('ross-test', 'test/tmp')
#' r<-downloadMultipleObjects('ross-test', 'test/tmp/cache2/')
#' r<-downloadMultipleObjects('ross-test', 'test', '~/asdf')
#' r<-downloadMultipleObjects('ross-test', 'test', '~/asdf', resume=F, .parallel = F)
#' r<-downloadMultipleObjects('ross-test', 'test', '/Volumes/RamDisk/asdf', pattern="tmp", quiet = F, split=10, .progressbar = F)
downloadMultipleObjects <- function(bucketname, src, dest='.', pattern=NULL,
                                    resume=TRUE, split=5, method='aria2', quiet=TRUE,
                                    ..., .progressbar=TRUE, .parallel = TRUE){
  prepareSrc <- function(bucketname, src, pattern){
    keys <- downloadState(bucketname, src, dest, pattern)
    if(is.null(keys)){
      keys <- suppressMessages(listBucket(bucketname, prefix=src, delimiter = '', .output='character'))
    }

    if(length(keys) == 0){
      stop('No such key: ', src)
    }
    if(!grepl("/$", src)){
      if(src %in% keys){
        keys <- src
      }else{
        keys <- keys[grep(paste0('^', src, '/'), keys)]
      }
    }
    if(!is.null(pattern)){
      keys <- keys[grep(pattern, keys)]
    }
    keys
  }

  prepareDirs <- function(src, keys){
    if(length(keys) == 0){
      stop('No such key: ', src)
    }

    dirs <- dirname(keys)
    prefix <- dirname(src)
    if(prefix != '.'){
      dirs <- gsub(paste0('^', prefix), '', dirs)
    }
    dirs <- file.path(path.expand(dest), dirs)
    dirs <- gsub('/+', '/', dirs)
    dirs <- gsub('/\\.$', '/', dirs)

    mkdirs(dirs)

    dirs
  }

  mkdirs <- function(dirs){
    sapply(unique(dirs), dir.create, recursive=T, showWarnings=F)
  }

  makeTaskParams <- function(keys, dirs){
    lapply(1:length(keys), function(i){
      list(src = keys[i],
           dest = dirs[i])
    })
  }

  downloadEachFile <- function(x){
    r <- try(downloadObject(bucketname, x$src, x$dest, resume, split, method, quiet, ...))
    r
  }

  keys <- prepareSrc(bucketname, src, pattern)
  dirs <- prepareDirs(src, keys)
  message(length(keys), ' files to download')

  task_params <- makeTaskParams(keys, dirs)

  if(.progressbar){
    op <- pbapply::pboptions(type = "timer")
  }else{
    op <- pbapply::pboptions(type = "none")
  }
  if(.parallel){
    cl <- parallel::makeForkCluster(split)
    status_codes <- pbapply::pbsapply(task_params, downloadEachFile, cl=cl)
    parallel::stopCluster(cl)
  }else{
    status_codes <- pbapply::pbsapply(task_params, downloadEachFile)
  }
  pbapply::pboptions(op)

  names(status_codes) <- keys
  failed <- names(status_codes)[status_codes != 0]
  if(length(failed) > 0){
    downloadState(bucketname, src, dest, pattern, state=failed)
    warning('Some files failed:\n', paste0(failed, collapse = '\n'))
  }else{
    downloadState(bucketname, src, dest, pattern, state=NULL)
  }

  invisible(status_codes)
}

#' listMultipartUploads
#'
#' @inheritParams ListMultipartUploads
#' @import tibble
#'
#' @return
#' @export
#'
#' @examples
listMultipartUploads <- function(bucketname, prefix=NULL, delimiter=NULL, max=1000,
                                 marker=NULL, id_marker=NULL, encoding_type=NULL){

  r <- ListMultipartUploads(bucketname, prefix=prefix, delimiter=delimiter, max=max,
                            marker=marker, id_marker=id_marker, encoding_type=encoding_type)
  doc <- httr::content(r, encoding = 'UTF-8')
  entries <- xpath2list(doc, '/ListMultipartUploadsResult/Upload', smart = F)
  entries <- lapply(entries, as.data.frame)
  plyr::ldply(entries)

}


#' abortMultipartUpload
#'
#' Abort failed multipart uploads.
#'
#' @param bucketname
#' @param prefix The prefix of objects on bucket.
#'
#' @return
#' @export
#'
#' @examples
#' abortMultipartUpload('ross-test', 'some-failed-multipart.gz')
#' abortMultipartUpload('ross-test')
abortMultipartUpload <- function(bucketname, prefix=NULL){
  res <- listMultipartUploads(bucketname, prefix)
  if(!is.null(res$Key)){
    for(i in 1:nrow(res)){
      message("Aborting", res$Key[i], ":", res$UploadId[i])
      AbortMultipartUpload(bucketname, res$Key[i], res$UploadId[i])
    }
  }
}

copyObjects <- function(src, dest, src_bucket=NULL, dest_bucket=NULL, ...){
  if(is.null(src_bucket) && !is.null(dest_bucket)){
    #Upload
    r <- uploadMultipleObjects(dest_bucket, src, dest, ...)
  }else if(!is.null(src_bucket) && is.null(dest_bucket)){
    #Download
    r <- downloadMultipleObjects(src_bucket, src, dest, ...)
  }else if(!is.null(src_bucket) && !is.null(dest_bucket)){
    #Copy
    source <- sprintf("/%s/%s", src_bucket, src)
    r <- CopyObject(source, dest_bucket, dest, ...)
  }else{
    #Local
    r <- file.copy(src, dest, ...)
  }
  invisible(r)
}
