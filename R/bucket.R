#' Bucket
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @export
#' @name Bucket
#'

Bucket <- R6::R6Class("Bucket",
  public = list(
    Name = NULL,
    CreationDate = NULL,
    Location = NULL,
    ExtranetEndpoint = NULL,
    IntranetEndpoint = NULL,
    VpcEndpoint = NULL,
    StorageClass = NULL,
    Owner = list(),
    lifecycle = NULL,
    cors = NULL,
#' @examples
#' ## new Bucket
#' b<-Bucket$new('ross-test',autoCreate=F)
#' ## auto create when bucket not exist.
#' b<-Bucket$new('ross-test',autoCreate=T)
    initialize = function(Name, Location='oss-cn-beijing', StorageClass="Standard", acl="private", autoCreate=FALSE) {
      self$Name = Name
      self$Location = Location
      self$StorageClass = StorageClass

      if(!self$exists() && autoCreate) {
        self$create(Location, acl, StorageClass)
      }else{
        self$refresh()
      }
    },
#' @examples
#'
#' ## create
#' b<-Bucket$new('ross-test')
#' # create bucket after init.
#' b$create()
    create = function(Location, StorageClass, acl='private') {
      if(missing(Location)){
        Location <- self$Location
      }

      if(missing(StorageClass)){
        StorageClass <- self$StorageClass
      }

      r <- createBucket(self$Name, Location, acl, StorageClass)
      if(r$status_code == 200){
        self$refresh()
      }
    },
    refresh = function() {
      r <- GetBucketInfo(self$Name)
      doc <- httr::content(r, encoding = 'UTF-8')
      self$Owner <- xpath2list(doc, '/BucketInfo/Bucket/Owner')
      info <- xpath2list(doc, '/BucketInfo/Bucket')
      private$setInfo(info)
      self$lifecycle <- BucketLifecycle$new(self$Name, FALSE)
      self$cors <- BucketCORS$new(self$Name, FALSE)
    },
#' @examples
#'
#' ## delete
#' b$delete
    delete = function() {
      deleteBucket(self$Name)
    },

#' @method list
#' @importFrom plyr ldply
#' @inheritParams GetBucket
#' @param .output output format
#'
#' @return
#'
#' @examples
#' b$list()
    list = function(prefix=NULL, marker=NULL, delimiter='/', max_keys='1000', .all = TRUE, .output="data.frame") {
      listBucket(self$Name, prefix, marker, delimiter, max_keys, .all, .output)
    },
    usage = function(prefix=NULL, unit='MB') {
      usageBucket(self$Name, prefix, unit)
    },
#' @method rm
#'
#' @return
#'
#' @examples
#' b$rm()
#' b$rm(confirm=T)
#' b$rm('test-', confirm=T)
    rm = function(prefix=NULL, confirm=FALSE, ...) {
      removeObjects(self$Name, prefix=prefix, confirm=confirm, ...)
    },
    read = function(key) {
      readObject(self$Name, key)
    },
    write = function(key, content=NULL) {
      writeObject(self$Name, key, content)
    },
    download = function(src, dest='.', pattern=NULL, resume=TRUE,
                        split=5, method='aria2', quiet=TRUE,
                         ..., .progressbar=TRUE, .parallel = TRUE) {

      downloadMultipleObjects(self$Name, src=src, dest=dest, pattern=pattern, resume=resume, split=split,
                              method=method, quiet=quiet, ..., .progressbar=.progressbar, .parallel=.parallel)
    },
    upload = function(src, prefix='/', pattern=NULL, resume=TRUE, split=5,
                      .progressbar=TRUE, ..., .parallel = TRUE) {

      uploadMultipleObjects(self$Name, src=src, prefix=prefix, pattern=pattern, resume=resume, split=split,
                            .progressbar=.progressbar, ..., .parallel=.parallel)
    },
    cp = function(from, to, ...) {
      copyMultipleObjects(from, to, self$Name, ...)
    },
    mv = function(from, to, ...) {
      moveObjects(from, to, self$Name, ...)
    },
    exists = function(){
      isBucketExist(self$Name)
    },
    print = function(...) {
      bucket_text <- sprintf(paste(
          "<Bucket>",
          "Name: %s",
          "CreationDate: %s",
          "Location: %s",
          "Endpoint: %s",
          "StorageClass: %s",
          sep = "\n"),
          self$Name, self$CreationDate, self$Location, self$ExtranetEndpoint, self$StorageClass)
      cat(bucket_text)
      invisible(self)
    }
  ),
  private = list(
    setInfo = function(bucket_info) {
      self$CreationDate = bucket_info$CreationDate
      self$Location = bucket_info$Location
      self$ExtranetEndpoint = .build.endpoint(bucket_info$Location, internal=FALSE)
      self$IntranetEndpoint = .build.endpoint(bucket_info$Location, internal=TRUE)
      self$VpcEndpoint = .build.endpoint(bucket_info$Location, internal=FALSE, vpc=TRUE)
      self$StorageClass = bucket_info$StorageClass
    }
  ),
  active = list(
#'
#' ## acl
#' b$acl
#' b$acl <- "private"
#' b$acl <- "public-read"
    acl = function(acl){
      aclBucket(self$Name, acl)
    },
#' @examples
#'
#' ## logging
#' b$logging
#' b$logging <- list(TargetBucket='ross-test', TargetPrefix='log-')
#' b$logging <- list(TargetBucket='ross-test')
#' b$logging <- list(TargetPrefix='log-')
#' b$logging <- list()
#' b$logging <- NULL
    logging = function(conf){
      if(missing(conf)){
        r <- GetBucketLogging(self$Name)
        doc <- httr::content(r, encoding = 'UTF-8')
        xpath2list(doc, '/BucketLoggingStatus/LoggingEnabled')
      }else if(is.null(conf) || identical(conf, list())){
        r <- DeleteBucketLogging(self$Name)
      }else{
        if(is.null(conf$TargetBucket)){
          conf$TargetBucket <- self$Name
        }
        r <- PutBucketLogging(self$Name, conf$TargetPrefix, conf$TargetBucket)
      }

    },
#' @examples
#'
#' ## website
#' b$website
#' b$website <- list(Suffix='index.html', Key='404.html')
#' b$website <- list(Suffix='index.html')
#' b$website <- list(Key='404.html')
#' b$website <- list()
#' b$website <- NULL
    website = function(conf){
      if(missing(conf)){
        suppressWarnings(r <- GetBucketWebsite(self$Name))
        doc <- httr::content(r, encoding = 'UTF-8')
        conf <- xpath2list(doc, '/WebsiteConfiguration')
        names(conf) <- gsub('.*\\.','',names(conf))
        conf
      }else if(is.null(conf) || identical(conf, list())){
        r <- DeleteBucketWebsite(self$Name)
      }else{
        if(is.null(conf$Suffix)){
          message("Suffix is missing index.html will be used.")
          conf$Suffix <- 'index.html'
        }
        if(is.null(conf$Key)){
          message("Key is missing 404.html will be used.")
          conf$Key <- '404.html'
        }
        r <- PutBucketWebsite(self$Name, conf$Suffix, conf$Key)
      }
    },
#' @examples
#'
#' ## referer
#' b$referer
#' b$referer <- list(AllowEmptyReferer=T, RefererList=c('*.igenecode.com', 'aliyun.com'))
#' b$referer <- list(AllowEmptyReferer=T)
#' b$referer <- list(RefererList=c('*.igenecode.com', 'aliyun.com'))
#' b$referer <- NULL
#' b$referer <- list()
    referer = function(conf){
      if(missing(conf)){
        suppressWarnings(r <- GetBucketReferer(self$Name))
        doc <- httr::content(r, encoding = 'UTF-8')
        list(
          AllowEmptyReferer = unlist(xpath2list(doc, '/RefererConfiguration/AllowEmptyReferer')),
          RefererList = unlist(xpath2list(doc, '/RefererConfiguration/RefererList'))
        )
      }else{
        if(is.null(conf$AllowEmptyReferer)){
          message("AllowEmptyReferer is missing TRUE will be used.")
          conf$AllowEmptyReferer <- TRUE
        }
        r <- PutBucketReferer(self$Name, conf$AllowEmptyReferer, conf$RefererList)
      }
    }
#' @examples
#'
#'  ## lifecycle
#'
#'  b$lifecycle$add('upload_', Object.Days = 5)
#'  b$lifecycle$save()
#'  b$lifecycle
#'  # lifecycle auto save is off to speedup by default when using Bucket class.
#'  # Turn on
#'  b$lifecycle$autoSave <- T
#'  b$lifecycle$add('upload_', Object.Days = 7)
#'  b$lifecycle
#'
#' @seealso \code{\link{BucketLifecycle}}
#'
#' @examples
#'
#'  ## CORS
#'
#'  b$cors$add('*', 'GET')
#'  b$cors$save()
#'  b$cors
#'  # cors auto save is off to speedup by default when using Bucket class.
#'  # Turn on
#'  b$cors$autoSave <- T
#'  b$cors$add('*', 'GET')
#'  b$cors
#'
#' @seealso \code{\link{BucketLifecycle}}
#''
  )
)


#' BucketLifecycle
#'
#' Convenient ways to manipulate lifecycle rules
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @importFrom plyr ldply
#' @export
#' @name BucketLifecycle
#'
#' @examples
#'
#' life<-BucketLifecycle$new('ross-test', autoSave=T)
#' life$add('upload_', Object.CreatedBeforeDate = "2017-04-01")
#' life$add('upload_', Object.Days=5)
#' life$add('upload_', Object.Days=7) # Modify on add
#' life$add('backup_', ID='backup-1', Object.Days=90)
#' life$add('Backup_', ID='backup-1', Object.Days=90) # Overwirte ID backup-1 rules
#' life
#' # Remove rules
#' life$remove('upload_')
#' life$remove(ID='backup-1') #By ID
#' # Clean up all rules
#' life$clear()
#'
#' # speedup without autoSave
#' life<-BucketLifecycle$new('ross-test', F)
#' life$add('backup1_', ID='backup-1', Object.Days=90)
#' life$add('backup2_', ID='backup-2', Object.Days=90)
#' life$add('backup3_', ID='backup-3', Object.Days=90)
#' life$save()
#' life
BucketLifecycle <- R6::R6Class("BucketLifecycle",
  public = list(
    name = NULL,
    rules = NULL,
    autoSave = FALSE,
    initialize = function(name, autoSave=TRUE) {
      self$name <- name
      self$autoSave = autoSave
      self$load()
    },
    add = function(Prefix, ID=NULL, Status='Enabled',
                   Object.CreatedBeforeDate=NULL, Object.Days=NULL,
                   Multpart.CreatedBeforeDate=NULL, Multpart.Days=NULL){

      rule <- .build.xml_body.PutBucketLifecycle.Rules(
        Prefix, ID, Status, Object.CreatedBeforeDate, Object.Days, Multpart.CreatedBeforeDate, Multpart.Days)
      self$remove(Prefix, ID)
      xml_add_child(self$rules, as_xml_document(list(Rule=rule)))

      if(self$autoSave){
        self$save()
      }

    },
    remove = function(Prefix=NULL, ID=NULL){
      deleteNode <- function(tag, value){
        xpath <- sprintf('//%s[text()="%s"]', tag, value)
        node <- xml_find_all(self$rules, xpath)
        node <- xml_parent(node)
        xml_remove(node)
      }

      if(!is.null(Prefix)){
        deleteNode('Prefix', Prefix)
      }
      if(!is.null(ID)){
        deleteNode('ID', ID)
      }

      if(is.null(Prefix) && is.null(ID)){
        stop('Either Prefix or ID must be specified.')
      }

      if(self$autoSave){
        self$save()
      }

    },
    clear = function(){
      r <- DeleteBucketLifecycle(self$name)
      if(r$status_code == 204){
        self$rules <- xml_new_root('LifecycleConfiguration')
      }
    },
    load = function(){
      suppressWarnings(r <- GetBucketLifecycle(self$name))
      if(r$status_code == 200){
        self$rules <- httr::content(r, encoding = 'UTF-8')
      }else if(r$status_code == 404){
        self$rules <- xml_new_root('LifecycleConfiguration')
      }
    },
    save = function(){
      if(self$length == 0){
        self$clear()
      }else{
        r <- PutBucketLifecycle(self$name, body=self$txt)
        if(r$status_code == 200){
          self$load()
        }
      }
    },
    print = function(){
      print(self$data.frame)
      invisible(self)
    }
  ),
  active = list(
    txt = function(){
      as.character(self$rules)
    },
    data.frame = function(){
      rules <- xpath2list(self$rules, '/LifecycleConfiguration/Rule', F)
      if(length(rules) == 0){
        NULL
      }else{
        rules <- lapply(rules, as.data.frame)
        plyr::ldply(rules)
      }
    },
    length = function(){
      rules <- xpath2list(self$rules, '/LifecycleConfiguration/Rule', F)
      length(rules)
    }
  )
)

#' BucketCORS
#'
#' Convenient ways to manipulate CORS rules
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @importFrom plyr ldply
#' @export
#' @name BucketLifecycle
#'
#' @examples
#'
BucketCORS <- R6::R6Class("BucketCORS",
  public=list(
    name = NULL,
    rules = NULL,
    autoSave = FALSE,
    initialize = function(name, autoSave=TRUE) {
      self$name <- name
      self$autoSave = autoSave
      self$load()
    },
    add = function(AllowedOrigin=NULL, AllowedMethod=NULL,
                   AllowedHeader=NULL, ExposeHeader=NULL,
                   MaxAgeSeconds=NULL){

      rule <- .build.xml_body.PutBucketcors.Rules(
        AllowedOrigin, AllowedMethod, AllowedHeader, ExposeHeader,MaxAgeSeconds)
      self$remove(AllowedOrigin, AllowedMethod)
      xml_add_child(self$rules, as_xml_document(list(CORSRule=rule)))

      if(self$autoSave){
        self$save()
      }

    },
    remove = function(AllowedOrigin, AllowedMethod){
      makeXpathAttr <- function(name){
        xpath_attr <- sprintf('%s/text()="%s"', name, get(name))
        paste(xpath_attr, collapse = ' and ')
      }
      xpath <- sprintf('//CORSRule[%s and %s]',
                       makeXpathAttr('AllowedOrigin'),
                       makeXpathAttr('AllowedMethod'))
      node <- xml_find_all(self$rules, xpath)
      xml_remove(node)

      if(self$autoSave){
        self$save()
      }

    },
    clear = function(){
      r <- DeleteBucketcors(self$name)
      if(r$status_code == 204){
        self$rules <- xml_new_root('CORSConfiguration')
      }
    },
    load = function(){
      suppressWarnings(r <- GetBucketcors(self$name))
      if(r$status_code == 200){
        self$rules <- httr::content(r, encoding = 'UTF-8')
      }else if(r$status_code == 404){
        self$rules <- xml_new_root('CORSConfiguration')
      }
    },
    save = function(){
      if(self$length == 0){
        self$clear()
      }else{
        r <- PutBucketcors(self$name, body=self$txt)
        if(r$status_code == 200){
          self$load()
        }
      }
    },
    print = function(){
      print(self$data.frame)
      invisible(self)
    }
  ),
  active = list(
    txt = function(){
      as.character(self$rules)
    },
    data.frame = function(){
      rules <- xpath2list(self$rules, '/CORSConfiguration/CORSRule', F)
      if(length(rules) == 0){
        NULL
      }else{
        rules <- lapply(rules, as.data.frame)
        plyr::ldply(rules)
      }
    },
    length = function(){
      rules <- xpath2list(self$rules, '/CORSConfiguration/CORSRule', F)
      length(rules)
    }
  )
)
