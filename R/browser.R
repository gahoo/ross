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
    pwd = NULL,
    initialize = function(root){
      self$root = format.folder(root)
      self$pwd = self$root
    },
    navi = function(key){
      if(key == '..'){
        pwd <- format.folder(dirname(self$pwd))
        test <- gsub(self$root, '', pwd)
        message(test)
        is_beyond_root <- grepl('^oss', gsub(self$root, '', pwd))
        if(is_beyond_root) return(invisible())
      }else{
        pwd <- paste0(self$pwd, key)
      }
      message(pwd)
      message(self$root)
      x <- oss(pwd)
      if(pwd == self$root || isObjectExist(x$bucketname, x$key) || isPseudoFolderExist(x$bucketname, x$key)){
        self$pwd <- format.folder(pwd)
      }else{
        stop("No Such Key: ", pwd)
      }
    },
    show = function(){
      oss.ls(self$pwd) %>%
        mutate(gsub(self$pwd, '', Key))
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
