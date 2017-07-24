#' Browser
#'
#' @docType class
#' @format \code{\link{R6Class}} object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @import shiny
#' @import magrittr
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @export
#' @name Browser
#' @examples
#' @field bucket store the bucket.
#' @field root Stores the root.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://gahoo.github.io/ross/Guide}
#'   \item{\code{new(bucket, root)}}{This method is used to create object of this class with \code{bucket} as bucket and \code{root} as root of the browser object.}
#'   \item{\code{goto(path)}}{This method goto the \code{path}.}}
#'
#'

Browser <- R6::R6Class("Browser",
  public = list(
    bucket = NULL,
    root = '',
    initialize = function(bucket = NULL, root = ''){
      self$bucket = bucket
      self$root <- strip.slash(root)
      private$cwd <- ifelse(root != '', self$root, '')
    },
    goto = function(path){
      path <- strip.slash(path)
      if(self$root != ''){
        path <- file.path(self$root, path)
      }
      self$pwd <- path
    },
    navi = function(key){
      if(is.null(self$bucket)){
        self$bucket <- key
        return(invisible())
      }
      if(key == '..'){
        if(private$cwd == '') return(invisible())
        pwd <- dirname(private$cwd)
        if(nchar(pwd) < nchar(self$root)) return(invisible())
        if(pwd == '.') {
          private$cwd <- ''
          return(invisible())
        }
      }else{
        if(private$cwd == ''){
          pwd <- strip.slash(key)
        }else{
          pwd <- file.path(private$cwd, strip.slash(key))
        }
      }

      self$pwd <- pwd
    },
    getLinks = function(key){
      if(key == '..') return(invisible())
      if(private$cwd == ''){
        prefix <- key
      }else{
        prefix <- file.path(private$cwd, key)
      }
      if(isPseudoFolderExist(self$bucket, prefix)){
        keys <- listBucket(self$bucket, prefix, delimiter = '', .output = 'character')
        dirs <- gsub(self$root, '', dirname(keys))
        urls <- sapply(keys, function(x){urlObject(self$bucket, x, expires = 7200)})
        names(urls) <- NULL
      }else if(isObjectExist(self$bucket, prefix)){
        dirs <- self$relative_dir
        urls <- urlObject(self$bucket, prefix)
      }
      message('download: ', key, '\tprefix: ', prefix)
      list(url = as.list(urls),
           dir = as.list(dirs))
    },
    show = function(.DT=TRUE, .shiny=FALSE){
      if(.DT){
        formatKey <- function(x){
          filename <- gsub(paste0('^', prefix), '', x)
          if(is.folder.char(x)){
            # makeNavi
            sprintf("<a onclick='updateCWD(\"%s\")'>%s</a>", filename, filename)
          }else{
            # createLink
            link <- urlObject(self$bucket, x)
            sprintf('<a href="%s">%s</a>', link, filename)
          }
        }
      }else{
        formatKey <- function(x) {
          gsub(paste0('^', prefix), '', x)
        }
      }

      formatTable <- function(files){
        if(is.null(self$bucket)){
          files %>%
            select(Key = Name, Location, StorageClass)
        }else{
          files %>%
            mutate(
              Key = sapply(Key, formatKey),
              Size = sapply(as.numeric(Size), smartSize)
            ) %>%
            select(Key, LastModified, ETag, Size)
        }
      }

      renderDT <- function(files){
        parent_key <- "<a onclick='updateCWD(\"..\")'>Parent</a>"
        parent <- data.frame(Key = parent_key, LastModified = NA, ETag = NA, Size = NA)
        if(.shiny){
          if(!is.null(self$bucket)){
            files <- rbind(parent, files)
          }
        }
        if(.DT){
          DT::datatable(files, escape = F,
                        extensions = 'Scroller', options = list(
                          deferRender = TRUE,
                          scrollY = 300,
                          scroller = TRUE
                          )) %>%
            DT::formatDate('LastModified')
        }else{
          files
        }
      }

      prefix <- add.slash(private$cwd)
      self$files %>% formatTable %>% renderDT

    }
  ),
  private = list(
    cwd = ''
  ),
  active = list(
    files = function(){
      fillNA <- function(x, name){
        if(!name %in% names(x)){
          x[[name]] <- NA
        }
        x
      }

      prefix <- add.slash(private$cwd)
      if(is.null(self$bucket)){
        files <- listBucket()
      }else if(private$cwd == ''){
        files <- listBucket(self$bucket)
      }else{
        files <- listBucket(self$bucket, prefix)
      }
      for(column in c('LastModified', 'ETag', 'Size')){
        files <- fillNA(files, column)
      }
      files
    },
    pwd = function(wd){
      if(missing(wd)) return(private$cwd)
      if(wd == '' || isObjectExist(self$bucket, wd) || isPseudoFolderExist(self$bucket, wd)){
        private$cwd <- wd
      }else{
        warning("No Such Key: ", wd)
      }
      message(wd)
    },
    relative_dir = function(){
      gsub(self$root, '', private$cwd)
    }
  )
)
