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
    root = NULL,
    bucket = NULL,
    pwd = NULL,
    initialize = function(bucket = NULL, root = NULL){
      self$bucket = bucket
      if(!is.null(root)){
        self$root <- strip.slash(root)
        self$pwd <- strip.slash(root)
      }else{
        self$root <- ''
        self$pwd <- NULL
      }
    },
    navi = function(key){
      if(is.null(self$bucket)){
        self$bucket <- key
        return(invisible())
      }
      if(key == '..'){
        if(is.null(self$pwd)) return(invisible())
        pwd <- dirname(self$pwd)
        if(nchar(pwd) < nchar(self$root)) return(invisible())
        if(pwd == '.') {
          self$pwd <- NULL
          return(invisible())
        }
      }else{
        if(is.null(self$pwd)){
          pwd <- strip.slash(key)
        }else{
          pwd <- file.path(self$pwd, strip.slash(key))
        }

      }
      if(isObjectExist(self$bucket, pwd) || isPseudoFolderExist(self$bucket, pwd)){
        self$pwd <- pwd
      }else{
        warning("No Such Key: ", pwd)
      }
      message(self$pwd)
    },
    show = function(.DT=TRUE, .shiny=FALSE){
      createLink <- function(x){
        if(is.folder.char(x)){
          NULL
        }else{
          link <- urlObject(self$bucket, x)
          HTML(sprintf('<a href="%s">%s</a>', link, x))
        }
      }

      smartSize <- function(x){
        if(is.na(x)) return()
        units <- c('B', 'KB', 'MB', 'GB', 'TB', 'PB')
        for(i in 1:6){ if(x < 1024^i) break }
        x <- round(x / 1024^(i-1))
        paste(x, units[i])
      }

      formatTable <- function(files){
        if(is.null(self$bucket)){
          files %>%
            select(Key = Name, Location, StorageClass)
        }else{
          files %>%
            mutate(
              Link = sapply(Key, createLink),
              # Preview = sapply(Key, createLink),
              Key = gsub(paste0('^', prefix), '', Key),
              Size = sapply(as.numeric(Size), smartSize)
            ) %>%
            select(Key, LastModified, ETag, Size, Link)
        }
      }

      renderDT <- function(files){
        parent <- data.frame(Key = '..', LastModified = NA, ETag = NA, Size = NA, Link = NA)
        if(.shiny){
          if(!is.null(self$bucket)){
            files <- rbind(parent, files)
            class(files$Link) <- 'list'
            class(files$Size) <- 'character'
            # files
          }
        }
        if(.DT){
          DT::datatable(files, selection = 'single',
                        extensions = 'Scroller', options = list(
                          deferRender = TRUE,
                          scrollY = 500,
                          scroller = TRUE)) %>%
            DT::formatDate('LastModified')
        }else{
          files
        }
      }

      prefix <- paste0(self$pwd, '/')
      self$files %>% formatTable %>% renderDT

    },
    run = function(){
      shinyApp(ui = self$ui, server = self$server)
    }
  ),
  active = list(
    files = function(){
      fillNA <- function(x, name){
        if(!name %in% names(x)){
          x[[name]] <- NA
        }
        x
      }

      prefix <- paste0(self$pwd, '/')
      if(is.null(self$bucket)){
        files <- listBucket()
      }else if(is.null(self$pwd)){
        files <- listBucket(self$bucket)
      }else{
        files <- listBucket(self$bucket, prefix)
      }
      for(column in c('LastModified', 'ETag', 'Size', 'Link')){
        files <- fillNA(files, column)
      }
      files
    },
    relative_dir = function(){
      gsub(self$root, '', self$pwd)
    },
    ui = function(){
      navbarPage(
        "OSS Browser",
        tabPanel(
          "Files",
          DT::dataTableOutput('oss'),
          textOutput('debug'),
          actionButton('go', 'GO'),
          actionButton('download', 'Download'),
          actionButton('download_all', 'Download All')
        ),
        tabPanel(
          "Download",
#          includeScript('inst/aria2/bundle.js'),
          htmltools::htmlDependency('aria2js', '3.0.0', 'inst/aria2/', script=c('bundle.js', 'ross.js'))
          # HTML(sprintf("<iframe src='file://%s' width='100%%' height='600px'>", system.file('yaaw/index.html', package = 'ross')))
          # htmltools::includeHTML(system.file('yaaw/index.html', package = 'ross'))
        ),
        navbarMenu(
          "More",
          tabPanel("Help"),
          "----",
          "Section header",
          tabPanel("Table")
        )
      )
    },
    server = function(){
      function(input, output, session) {
        output$oss <- DT::renderDataTable({
          click <- isolate(input$oss_cell_clicked)
          if(!is.null(click)){
            key <- self$show(.shiny = TRUE, .DT = FALSE)$Key[click$row]
            if(is.folder.char(key) || key == '..'){
              self$navi(key)
            }
          }
          input$go
          self$show(.shiny = TRUE)
        })

        observeEvent(input$download, {
          click <- isolate(input$oss_cell_clicked)
          if(!is.null(click)){
            key <- self$show(.shiny = TRUE, .DT = FALSE)$Key[click$row]
            message('download:', key)
            if(key == '..') return(invisible())
            if(is.null(self$pwd)){
              prefix <- key
            }else{
              prefix <- file.path(self$pwd, key)
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
            session$sendCustomMessage(
              type = 'addLinks',
              message = list(
                url = as.list(urls),
                dir = as.list(dirs)
                )
              )
          }
        })

        observeEvent(input$download_all, {
          if(self$root == ''){
            prefix <- NULL
          }else{
            prefix <- add.slash(self$root)
          }
          keys <- listBucket(self$bucket, prefix, delimiter = '', .output = 'character')
          message(paste0(keys, collapse = '\n'))
          dirs <- gsub(add.slash(self$root), '', dirname(keys))
          dirs[dirs == '.'] <- ''
          message(paste0(dirs, collapse = '\n'))
          urls <- sapply(keys, function(x){urlObject(self$bucket, x, expires = 7200)})
          names(urls) <- NULL
          session$sendCustomMessage(
            type = 'addLinks',
            message = list(
              url = as.list(urls),
              dir = as.list(dirs)
            )
          )
        })

        output$debug <- renderText({
          click <- input$oss_cell_clicked
          str(click)
          click$value
          gsub(self$root, '', self$pwd)
        })
      }
    }
  )
)
