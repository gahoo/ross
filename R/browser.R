#' Browser
#'
#' @docType class
#' @format An R6 class object.
#' @importFrom R6 R6Class
#' @import xml2
#' @import httr
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
        if(pwd == '.') {
          self$pwd <- NULL
          return(invisible())
        }
        if(nchar(pwd) < nchar(self$root)) return(invisible())
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
    show = function(.DT=TRUE){
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
        if(.DT){
          DT::datatable(files)
        }else{
          files
        }
      }

      prefix <- paste0(self$pwd, '/')
      self$files %>% formatTable %>% renderDT

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
      for(column in c('LastModified', 'ETag', 'Size')){
        files <- fillNA(files, column)
      }
      files
    }
  )
)


#' oss.browser
#'
#' @return
#' @export
#' @import shiny
#' @import DT
#' @import dplyr
#'
#' @examples
oss.browser <- function(root=NULL, title = 'test'){
  ui <- navbarPage("OSS Browser",
    tabPanel(
      "Files",
      DT::dataTableOutput('oss')
      ),
    tabPanel(
      "Download",
      HTML("<iframe src='yaaw/index.html' width='100%' height='600px'>")
      ),
    navbarMenu(
      "More",
      tabPanel("Help"),
      "----",
      "Section header",
      tabPanel("Table")
    )
  )

  server <- function(input, output) {
    output$oss <- DT::renderDataTable({
      oss.ls(root) %>%
        select(Key, LastModified, Size, ETag)
    })
  }

  shinyApp(ui = ui, server = server)
}
