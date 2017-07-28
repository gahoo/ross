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
browserApp <- function(bucket=NULL, root='', forbid_empty_root_access=F){
  Sys.setlocale(category = "LC_ALL", locale="UTF-8")

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
    newButton <- function(command, icon_name=command, title=command){
      actionLink(paste0(command, '-', gid), '', icon = icon(icon_name), onclick = sprintf("aria2.%s('%s')", command, gid), title = title)
    }

    if(status %in% c('complete', 'removed', 'error')){
      pause_btn <- NULL
    }else if(status %in% c('active', 'waiting')){
      pause_btn <- newButton('pause')
    }else{ #pause
      pause_btn <- newButton('unpause', 'play', 'start')
    }

    if(status %in% c('active', 'waiting', 'paused')){
      trash_btn <- newButton('remove', 'trash')
    }else{ # complete, removed, error
      trash_btn <- newButton('removeDownloadResult', 'trash', 'remove')
    }

    as.character(div(id = gid, trash_btn, pause_btn))
  }

  makeNaviBar <- function(cwd){
    cwd <- unlist(strsplit(cwd, '/'))
    home <- list(a('Home /', href='#', onclick = 'setCWD("")'))
    if(length(cwd) == 0){
      return(home)
    }
    navi_bar <- lapply(1:length(cwd), function(i){
      partial_cwd <- paste0(cwd[1:i], collapse = '/')
      a(cwd[i], '/', href='#', onclick = sprintf('setCWD("%s/")', partial_cwd))
    })
    c(home, navi_bar)
  }

  enableBookmarking("url")
  registerInputHandler("aria2_tasks", function(data, ...) {
    as.list(data)
  }, force = TRUE)

  ui = navbarPage(
    "OSS Browser",
    tabPanel(
      "Files",
      uiOutput('cwd_navi_bar'),
      DT::dataTableOutput('oss'),
      textOutput('debug'),
      div(
        textInput('cwd', 'cwd'),
        checkboxInput('aria2_task_hide_stopped', 'Hide Stopped', value = TRUE),
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs(text = jsCode),
        style='display: none'
      ),
      actionLink("show_task", "", icon = icon('tasks'), style="float: right;"),
      conditionalPanel("input.show_task % 2 == 0",
        tabsetPanel(
          tabPanel(
            'Download',
            conditionalPanel("'undefined' != typeof aria2_version",
              actionButton('select', 'Select All', icon = icon('check', lib = 'glyphicon')),
              actionButton('download', 'Download', icon = icon('download'), title = 'download selected items.'),
              actionButton('download_all', 'Download All', icon = icon('download'), title = 'download everything.'),
              #actionLink('refresh', "", icon = icon('refresh')),
              div(
                actionLink("unpause_all", "", icon = icon('play'), onclick = 'aria2.unpauseAll()', title = 'start all paused tasks.'),
                actionLink("pause_all", "", icon = icon('pause'), onclick = 'aria2.pauseAll()', title = 'pause all tasks.'),
                actionLink("remove_all_stopped", "", icon = icon('trash'), onclick = 'aria2.purgeDownloadResult()', title = 'remove all stopped tasks.'),
                actionLink("settings", "", icon = icon('cog'), title = 'aria2 settings'),
                style = 'float: right'
              ),
              htmltools::htmlDependency('aria2js', '3.0.0', 'inst/aria2/',
                                        script=c('bundle.js', 'ross.js'),
                                        stylesheet=c('ross.css')),
              DT::dataTableOutput('aria2tasks_list')
            ),
            conditionalPanel("'undefined' === typeof aria2_version",
              includeMarkdown('inst/aria2/setup_aria2.md')
            )
          ),
          tabPanel(
            'Preview'
          )
        )
      )
    ),
    tabPanel(
      "FAQ"
    ),
    navbarMenu(
      "More",
      tabPanel('ELF'),
      tabPanel("Help"),
      "----",
      tabPanel("About")
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

    root <- reactiveVal(root)

    browser <- reactive({
      Browser$new(bucket, root(), forbid_empty_root_access)
    })

    output$oss <- DT::renderDataTable({
      height <- ifelse(input$show_task %% 2 == 0, 300, 500)
      browser()$formatDT(add.parent = T) %>%
        DT::datatable(escape = F,
                      extensions = 'Scroller', options = list(
                        deferRender = TRUE,
                        scrollY = height,
                        scroller = TRUE
                      )) %>%
        DT::formatDate('LastModified')
    })

    observeEvent(input$cwd, {
      if(!is.null(input$cwd)){
        browser()$goto(input$cwd)
      }
      oss_files <- browser()$formatDT(add.parent = T)
      oss_proxy %>%
        DT::replaceData(oss_files)
    })

    oss_proxy <- DT::dataTableProxy('oss')
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
      oss_proxy %>% DT::selectRows(selected_rows)
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
        root(URLdecode(query[['root']]))
        message(browser()$root)
      }
    })

    observeEvent(input$download, {
      selected_rows <- input$oss_rows_selected
      if(!is.null(selected_rows)){
        keys <- browser()$formatDT(key.type='full', add.parent = TRUE)$Key[selected_rows]
        links <- list(url=list(), dir=list())
        for(key in keys){
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

      if(browser()$root != ''){
        dirs <- gsub(add.slash(browser()$root), '', keys)
      }
      dirs <- dirname(keys)
      dirs[dirs == '.'] <- ''
      message(paste0(keys, "\t", dirs, collapse = '\n'))

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
                        ctrl = mapply(makeCtrlButton, gid, status)
          ) %>%
          dplyr::select(ctrl, progress, speed, status, files)
      }else{
        NULL
      }
    })

    output$aria2tasks_list <- DT::renderDataTable({
      status <- data.frame(ctrl='', progress='', speed='', status='', files='')
      DT::datatable(status, escape = F, selection = 'none', class = 'hover stripe compact row-border',
                    extensions = 'Scroller', options = list(
                      dom = 't',
                      deferRender = TRUE,
                      scrollY = 200,
                      scroller = TRUE
                    ))
    })

    observeEvent(input$settings, {

      showModal(modalDialog(
        title = "Aria2 Settings",
        "Ross use aria2 as client to download files.",
        checkboxInput('aria2_task_hide_stopped_modal', 'Hide Stopped', value = input$aria2_task_hide_stopped),
        numericInput('max_concurrent', 'Concurrent Downs', value = getInputValue('max_concurrent', 1), min = 1, max = 20),
        textInput('max_overall_download_limit', 'Download limit', value = getInputValue('max_overall_download_limit', '10k')),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$aria2_task_hide_stopped_modal, {
      updateCheckboxInput(session, 'aria2_task_hide_stopped', label = 'Hide Stopped', value = input$aria2_task_hide_stopped_modal)
    })

    observeEvent(input$max_concurrent, {
      js$setMaxCon()
      #js$setMaxCon2(as.character(input$max_concurrent))
    })

    observeEvent(input$max_overall_download_limit, {
      js$setMaxOverallDonwloadLimit()
    })

    output$cwd_navi_bar <- renderUI({
      div(makeNaviBar(input$cwd), style="background-color: #E8E8E8;float: left;")
    })

    output$debug <- renderText({
      click <- input$oss_cell_clicked
      str(click)
#      autoInvalidate()
      # message(input$tasks[[1]]$completedLength)
      # str(input$tasks)
#      str(completedLength/totalLength)
      input$cwd
      input$download_dir
    })
  }

  shinyApp(ui = ui, server = server)
}
