#' one_way_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_one_way_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("csv_input"),"Choose input file",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),buttonLabel = "Upload..."

        ),
        selectInput(ns("value_one"),"Observation",""),
        selectInput(ns("trt_one"), "Treatment",""),
        selectInput(ns("follow_1"),"Choose follow up test",
                    choices = c("LSD","HSD","Duncan")),
        selectInput(ns("focus"),"select focus",""),
        submitButton("Apply Change", icon("refresh"))
      ),
      ######################################################
      mainPanel(
        tabsetPanel(
          id = ns("wizard"),
          type = "hidden",
          tabPanel(value = "pg1", DT::dataTableOutput(ns("one_data")),
                                               actionButton(ns("pg_12"),
                                                            "Go to analysis page"),
                                               actionButton(ns("pg_13"),
                                                            "Go to Visualization page")

          ),
          tabPanel(value = "pg2",
                   verbatimTextOutput(ns("aov_one")),
                   downloadButton(ns("downloadaov_one"),
                                  "Download result"),
                   actionButton(ns("pg_21"),"Data table"),
                   actionButton(ns("pg_23"),"Follow up tests")),
          tabPanel(value = "pg3",
                   plotOutput(ns("fut_one")),
                   downloadButton(ns("downloadfut_one"),
                                  "Download plot"),
                   tableOutput(ns("fut_table_one")),
                   downloadButton(ns("downloadfut_table_one"),
                                  "Download table"),
                   actionButton(ns("pg_31"),"Data table"),
                   actionButton(ns("pg_32"), "Analysis Page")
          )
        )
      )
    )
  )
}

#' one_way_module Server Functions
#'
#' @noRd
mod_one_way_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ############
    output$one_data <- DT::renderDataTable(load_data(input$csv_input))
    ############
    reactives <- rv(

      mydata = NULL

    )
    observeEvent(input$pg_12,switch_page(2))
    observeEvent(input$pg_13, switch_page(3))
    observeEvent(input$pg_21, switch_page(1))
    observeEvent(input$pg_23, switch_page(3))
    observeEvent(input$pg_31, switch_page(1))
    observeEvent(input$pg_32, switch_page(2))

    observeEvent(input$csv_input, {

      #Store loaded data in reactive
      reactives$mydata <- read.csv(file = input$csv_input$datapath)

      #Update select input
      updateSelectInput(session, inputId = 'focus', label = 'select focus', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'value_one', label = 'Observation', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'trt_one', label = 'Treatment', choices  = colnames(reactives$mydata))
    })
    ##############
    output$aov_one <- renderPrint(broom::tidy(one_aov(input$csv_input,input$trt_one,
                                                      input$value_one)))
    ##############
    output$downloadaov_one <- downloadHandler(
      filename = function(){
        paste("aov","csv",sep = ".")
      } ,
      content = function(file){
        write.csv((broom::tidy(one())),file)
      }
    )
    #############
    output$fut_one <- renderPlot({
      datas <- load_data(input$csv_input)
      dd.aov <- aov(as.formula(paste("datas[,input$value_one]~",
                                     input$trt_one)), data = datas)
      switch (input$follow_1,
              LSD = lsd_plot(dd.aov,input$focus),
              HSD = hsd_plot(dd.aov,input$focus),
              Duncan = dun_plot(dd.aov,input$focus))
    })
    ############
    output$downloadfut_one <- downloadHandler(
      filename = function(){
        switch (input$follow_1,
                LSD = paste("lsd_plot",".jpeg",sep = ""),
                HSD = paste("hsd_plot",".jpeg",sep = ""),
                Duncan = paste("duncan_plot",".jpeg",sep = ""))
      },
      content = function(file){
        jpeg(file,width = 900,height = 450)
        datas <- load_data(input$csv_input)
        dd.aov <- aov(as.formula(paste("datas[,input$value_one]~",
                                       input$trt_one)), data = datas)
        switch (input$follow_1,
                LSD = lsd_plot(dd.aov,input$focus),
                HSD = hsd_plot(dd.aov,input$focus),
                Duncan = dun_plot(dd.aov,input$focus)
        )
        dev.off()
      }
    )
    ##############
    output$fut_table_one <- renderTable({
      datas <- load_data(input$csv_input)
      dd.aov <- aov(as.formula(paste("datas[,input$value_one]~",
                                     input$trt_one)), data = datas)
      tt <- switch (input$follow_1,
                    LSD = lsd_t(dd.aov,input$focus),
                    HSD = hsd_t(dd.aov,input$focus),
                    Duncan = dun_t(dd.aov,input$focus)
      )
      cc <-  as.data.frame(tt[["groups"]])
      names(cc) <- c("average","groups")
      cc
    })
    ###########
    output$downloadfut_table_one <- downloadHandler(
      filename = function(){
        switch (input$follow_1,
                LSD = paste("lsd_table","csv",sep = "."),
                HSD =  paste("hsd_table","csv",sep = "."),
                Duncan =  paste("duncan_table","csv",sep = "."))

      },
      content = function(file){
        datas <- load_data(input$csv_input)
        dd.aov <- aov(as.formula(paste("datas[,input$value_one]~",
                                       input$trt_one)), data = datas)
        tt <- switch (input$follow_1,
                      LSD = lsd_t(dd.aov,input$focus),
                      HSD = hsd_t(dd.aov,input$focus),
                      Duncan = dun_t(dd.aov,input$focus)
        )
        result <-  tt[["groups"]]
        names(result) <- c("average","groups")
        result
        write.csv(result,file)
      }
    )
    })
}

## To be copied in the UI
# mod_one_way_module_ui("one_way_module_1")

## To be copied in the server
# mod_one_way_module_server("one_way_module_1")
