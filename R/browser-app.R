browserApp <- function(bucket=NULL, root=''){
  enableBookmarking("url")
  ui = navbarPage(
    "OSS Browser",
    tabPanel(
      "Files",
      DT::dataTableOutput('oss'),
      textOutput('debug'),
      div(
      textInput('cwd', 'cwd'),
      textInput('root', 'root'),
      style='display: none'
      ),
      tabsetPanel(
        tabPanel(
          'Download',
          actionButton('select', 'Select All'),
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
    browser <- reactive({
      if(!is.null(input$root) && input$root != ''){
        Browser$new(bucket, input$root)
      }else{
        Browser$new(bucket, '.')
        Browser$new(bucket)
      }
    })

    output$oss <- DT::renderDataTable({
      if(!is.null(input$cwd)){
        browser()$goto(input$cwd)
      }
      browser()$show(.shiny = T)
    })

    proxy <- DT::dataTableProxy('oss')

    observeEvent(input$select, {
      row_cnts <- nrow(browser()$files) + 1
      if(is.null(input$oss_rows_selected) || length(input$oss_rows_selected) != row_cnts - 1){
        if(input$oss_search != ''){
          selected_rows <- input$oss_rows_current
        }else{
          selected_rows <- 2:row_cnts
        }
        updateActionButton(session, 'select', 'Select None')
      }else{
        selected_rows <- NULL
        updateActionButton(session, 'select', 'Select All')
      }
      proxy %>% DT::selectRows(selected_rows)
    })

    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['cwd']])) {
        cwd <- URLdecode(query[['cwd']])
        updateTextInput(session, "cwd", value = cwd)
      }
      if (!is.null(query[['root']])) {
        root <- URLdecode(query[['root']])
        updateTextInput(session, "root", value = root)
        message(browser()$root)
      }
    })

    observeEvent(input$download, {
      selected_rows <- input$oss_rows_selected
      if(!is.null(selected_rows)){
        keys <- browser()$show(.shiny = TRUE, .DT = FALSE)$Key[selected_rows]
        links <- list(url=list(), dir=list)
        for(key in keys){
          message(key)
          key_link <- browser()$getLinks(key)
          links$url <- c(links$url, key_link$url)
          links$dir <- c(links$dir, key_link$dir)
        }
        session$sendCustomMessage(
          type = 'addLinks',
          message = links
        )
      }
    })

    observeEvent(input$download_all, {
      if(browser()$root == ''){
        prefix <- NULL
      }else{
        prefix <- add.slash(browser()$root)
      }
      keys <- listBucket(browser()$bucket, prefix, delimiter = '', .output = 'character')
      message(paste0(keys, collapse = '\n'))
      if(browser()$root == ''){
        dirs <- dirname(keys)
      }else{
        dirs <- gsub(add.slash(browser()$root), '', dirname(keys))
      }
      dirs[dirs == '.'] <- ''
      message(paste0(dirs, collapse = '\n'))
      urls <- sapply(keys, function(x){urlObject(browser()$bucket, x, expires = 7200)})
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
      input$cwd
    })
  }

  shinyApp(ui = ui, server = server)
}
