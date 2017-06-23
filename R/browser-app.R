browserApp <- function(bucket=NULL, root=NULL){
  ui = navbarPage(
    "OSS Browser",
    tabPanel(
      "Files",
      DT::dataTableOutput('oss'),
      textOutput('debug'),
      tabsetPanel(
        tabPanel(
          'Download',
          actionButton('go', 'GO'),
          actionButton('download', 'Download'),
          actionButton('download_all', 'Download All'),
          htmltools::htmlDependency('aria2js', '3.0.0', 'inst/aria2/', script=c('bundle.js', 'ross.js'))
        ),
        tabPanel(
          'Preview'
        ),
        tabPanel('ELF'),
        tabPanel('Info')
      )
    ),
    tabPanel(
      "Download"
      #          includeScript('inst/aria2/bundle.js'),

      # HTML(sprintf("<iframe src='file://%s' width='100%%' height='600px'>", system.file('yaaw/index.html', package = 'ross')))
      #          HTML("<iframe src='http://report.igenecode.com/yaaw/index.html' width='100%' height='600px'>")
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
  server = function(input, output, session) {
    output$oss <- DT::renderDataTable({
      click <- isolate(input$oss_cell_clicked)
      if(!is.null(click)){
        key <- browser$show(.shiny = TRUE, .DT = FALSE)$Key[click$row]
        if(is.folder.char(key) || key == '..'){
          browser$navi(key)
        }
      }
      input$go
      browser$show(.shiny = TRUE)
    })

    observeEvent(input$download, {
      click <- isolate(input$oss_cell_clicked)
      if(!is.null(click)){
        key <- browser$show(.shiny = TRUE, .DT = FALSE)$Key[click$row]
        links <- browser$getLinks(key)
        session$sendCustomMessage(
          type = 'addLinks',
          message = links
        )
      }
    })

    observeEvent(input$download_all, {
      if(browser$root == ''){
        prefix <- NULL
      }else{
        prefix <- add.slash(browser$root)
      }
      keys <- listBucket(browser$bucket, prefix, delimiter = '', .output = 'character')
      message(paste0(keys, collapse = '\n'))
      dirs <- gsub(add.slash(browser$root), '', dirname(keys))
      dirs[dirs == '.'] <- ''
      message(paste0(dirs, collapse = '\n'))
      urls <- sapply(keys, function(x){urlObject(browser$bucket, x, expires = 7200)})
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
      gsub(browser$root, '', browser$pwd)
    })
  }

  browser <- Browser$new(bucket, root)

  shinyApp(ui = ui, server = server)
}
