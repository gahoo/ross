#' Browser
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
#' @import shiny
#' @import dplyr
#' @export
#' @name Bucket
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
      smartSize <- function(x){
        if(is.na(x)) return(NA)
        units <- c('B', 'KB', 'MB', 'GB', 'TB', 'PB')
        for(i in 1:6){ if(x < 1024^i) break }
        x <- round(x / 1024^(i-1))
        paste(x, units[i])
      }

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
          DT::datatable(files, selection = 'single', escape = F,
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
