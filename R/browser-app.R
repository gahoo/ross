#' browserApp
#'
#' @param bucket
#' @param root
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjs extendShinyjs
#' @importFrom shinyjs js
#'
#' @return
#' @export
#'
#' @examples
browserApp <- function(bucket=NULL, root=''){
  jsCode <- '
  shinyjs.updateTasks = function(){updateTasks();}
  shinyjs.setMaxCon = function(){setMaxCon();}
  shinyjs.setMaxCon2 = function(max_concurrent){
    console.log(max_concurrent[0])
    aria2.changeGlobalOption({"max-concurrent-downloads": max_concurrent[0]});
  }
  shinyjs.setMaxOverallDonwloadLimit = function(){setMaxOverallDonwloadLimit();}
  '

  extractAria2 <- function(res, name, as.fun = as.character){
    res <- sapply(res, function(x) x[[name]])
    as.fun(res)
  }

  extractAria2TaskDF <- function(res){
    plyr::ldply(res, function(x){
      x$files <-x$files[[1]]$path;
      as.data.frame(x, stringsAsFactors=F)
    })
  }

  makeCtrlButton <- function(gid, status){
    if(status == 'complete'){
      ctrl_btn <- NULL
    }else if(status %in% c('active', 'waiting')){
      ctrl_btn <- actionLink(paste0('pause-', gid), '', icon = icon('pause'), onclick = sprintf("aria2.pause('%s')", gid))
    }else{
      ctrl_btn <- actionLink(paste0('unpause-', gid), '', icon = icon('play'), onclick = sprintf("aria2.unpause('%s')", gid))
    }
    as.character(div(
      id = gid,
      actionLink(paste0('remove-', gid), '', icon = icon('trash'), onclick = sprintf("aria2.remove('%s')", gid)),
      ctrl_btn
    ))
  }

  enableBookmarking("url")
  registerInputHandler("aria2_tasks", function(data, ...) {
    as.list(data)
  }, force = TRUE)

  ui = navbarPage(
    "OSS Browser",
    tabPanel(
      "Files",
      DT::dataTableOutput('oss'),
      textOutput('debug'),
      div(
        textInput('cwd', 'cwd'),
        textInput('root', 'root'),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = jsCode),
        style='display: none'
      ),
      tabsetPanel(
        tabPanel(
          'Download',
          actionButton('select', 'Select All', icon = icon('check', lib = 'glyphicon')),
          actionButton('download', 'Download', icon = icon('download')),
          actionButton('download_all', 'Download All', icon = icon('download')),
          checkboxInput('aria2_task_hide_stopped', 'Hide Stopped', value = TRUE),
          #actionLink('refresh', "", icon = icon('refresh')),
          actionLink("settings", "", icon = icon('cog')),
          actionLink("unpause_all", "", icon = icon('play'), onclick = 'aria2.unpauseAll()'),
          actionLink("pause_all", "", icon = icon('pause'), onclick = 'aria2.pauseAll()'),
          actionLink("remove_all_stopped", "", icon = icon('trash'), onclick = 'aria2.purgeDownloadResult()'),
          #actionLink("show_task", "", icon = icon('tasks')),
          htmltools::htmlDependency('aria2js', '3.0.0', 'inst/aria2/',
                                    script=c('bundle.js', 'ross.js'),
                                    stylesheet=c('ross.css')),
          DT::dataTableOutput('aria2tasks_list')
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
    getInputValue <- function(input_name, default){
      if(is.null(input[[input_name]])){
        default
      }else{
        input[[input_name]]
      }
    }

    browser <- reactive({
      if(!is.null(input$root) && input$root != ''){
        Browser$new(bucket, input$root)
      }else if(root != ''){
        Browser$new(bucket, root)
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
    aria2task_proxy <- DT::dataTableProxy('aria2tasks_list')

    observeEvent(input$select, {
      row_cnts <- nrow(browser()$files) + 1
      if(is.null(input$oss_rows_selected) || length(input$oss_rows_selected) != row_cnts - 1){
        if(input$oss_search != ''){
          selected_rows <- input$oss_rows_current
        }else{
          selected_rows <- 2:row_cnts
        }
        updateActionButton(session, 'select', 'Select None', icon = icon('unchecked', lib = 'glyphicon'))
      }else{
        selected_rows <- NULL
        updateActionButton(session, 'select', 'Select All', icon = icon('check', lib = 'glyphicon'))
      }
      proxy %>% DT::selectRows(selected_rows)
    })

    observe({
      input$refresh
      aria2task_proxy %>%
        DT::replaceData(aria2tasks(), resetPaging = FALSE, clearSelection = "none")
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
        links <- list(url=list(), dir=list())
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

    aria2tasks <- reactivePoll(5000, session,
      checkFunc = function() {
        js$updateTasks()
      },
    valueFunc = function(){
      status <- extractAria2TaskDF(input$tasks)
      progress_html_template <- '<div class="progress %s"><div class="progress-bar" style="width: %s%%"><span>%s%%</span></div></div>'
      if(nrow(status) > 0){
        status %>%
          dplyr::mutate(Key=gsub('/Volumes/RamDisk/', '', files),
                        progress_status = ifelse(status == 'complete', '', 'progress-striped active'),
                        progress = round(100 * as.numeric(completedLength) / as.numeric(totalLength), digits = 2),
                        progress = ifelse(is.nan(progress), 0, progress),
                        progress = sprintf(progress_html_template, progress_status, progress, progress),
                        speed = paste0(sapply(as.numeric(downloadSpeed), smartSize, digit = 2), '/s'),
                        gid = mapply(makeCtrlButton, gid, status)
          ) %>%
          dplyr::select(gid, progress, speed, status, files)
      }else{
        NULL
      }
    })

    output$aria2tasks_list <- DT::renderDataTable({
      status <- data.frame(gid='', progress='', speed='', status='', files='')
      DT::datatable(status, escape = F,
                    extensions = 'Scroller', options = list(
                      dom = 't',
                      deferRender = TRUE,
                      scrollY = 200,
                      scroller = TRUE
                    ))
    })

    observeEvent(input$settings, {
      if(is.null(input$max_concurrent)){
        max_concurrent_value = 1
      }else{
        max_concurrent_value = input$max_concurrent
      }

      if(is.null(input$max_overall_download_limit)){
        max_overall_download_limit_value = '10k'
      }else{
        max_overall_download_limit_value = input$max_overall_download_limit
      }

      showModal(modalDialog(
        title = "Aria2 Settings",
        "Ross use aria2 as client to download files.",
        numericInput('max_concurrent', 'Concurrent Downs', value = getInputValue('max_concurrent', 1), min = 1, max = 20),
        textInput('max_overall_download_limit', 'Download limit', value = getInputValue('max_overall_download_limit', '10k')),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$max_concurrent, {
      js$setMaxCon()
      #js$setMaxCon2(as.character(input$max_concurrent))
    })

    observeEvent(input$max_overall_download_limit, {
      js$setMaxOverallDonwloadLimit()
    })

    output$debug <- renderText({
      click <- input$oss_cell_clicked
      str(click)
#      autoInvalidate()
      # message(input$tasks[[1]]$completedLength)
      # str(input$tasks)
      completedLength <- extractAria2(input$tasks, 'completedLength', as.numeric)
      totalLength <- extractAria2(input$tasks, 'totalLength', as.numeric)
#      str(completedLength/totalLength)
      input$cwd
      input$download_dir
    })
  }

  shinyApp(ui = ui, server = server)
}
