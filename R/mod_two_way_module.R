#' two_way_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_two_way_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabItem(tabName = "two",
            sidebarLayout(
              sidebarPanel(
                fileInput(ns("csv_input_two"),"Choose input file",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
                selectInput(ns("value_two"),"Observation",""),
                selectInput(ns("trt_two"), "Treatment",""),
                selectInput(ns("block"), "Block",""),
                selectInput(ns("follow_2"),"Choose follow up test",
                            choices = c("LSD","HSD","Duncan")),
                selectInput(ns("focus_2"),"Select focus",""),
                submitButton("Apply Change",icon("refresh"))
              ),
              ########################################################
              mainPanel(
                tabsetPanel(
                  type = "tab",
                  tabPanel("Data", DT::dataTableOutput(ns("two_data"))
                  ),
                  tabPanel("Analysis of Variance",
                           verbatimTextOutput(ns("aov_two")),
                           downloadButton(ns("downloadaov_two"),
                                          "Download result")),
                  tabPanel("Post hoc analysis",
                           plotOutput(ns("fut_two")),
                           downloadButton(ns("downloadfut_two"),
                                          "Download plot"),
                           tableOutput(ns("fut_table_two")),
                           downloadButton(ns("downloadfut_table_two"),
                                          "Download table")
                  )
                )
              )
            )
    )
  )
}

#' two_way_module Server Functions
#'
#' @noRd
mod_two_way_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ############
    output$two_data <- DT::renderDataTable(load_data(input$csv_input_two))
    #############
    reactives <- rv(

      mydata = NULL

    )
    observeEvent(input$csv_input_two, {

      #Store loaded data in reactive
      reactives$mydata <- read.csv(file = input$csv_input_two$datapath)

      #Update select input
      updateSelectInput(session, inputId = 'focus_2', label = 'select focus', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'value_two', label = 'Observation', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'trt_two', label = 'Treatment', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'block', label = 'Block', choices  = colnames(reactives$mydata))
    })
    ################
    output$aov_two <- renderPrint(broom::tidy(
      two_aov(input$csv_input_two,input$trt_two,input$block,input$value_two))
    )
    ###############
    output$downloadaov_two <- downloadHandler(
      filename = function(){
        paste("aov","csv",sep = ".")
      } ,
      content = function(file){
        write.csv((broom::tidy(two())),file)
      }
    )
    ###############
    output$fut_two <- renderPlot({
      datas <- load_data(input$csv_input_two)
      dd.aov <- aov(as.formula(paste("datas[,input$value_two]~",input$trt_two,"+", input$block)), data = datas)
      switch (input$follow_2,
              LSD = lsd_plot(dd.aov,input$focus_2),
              HSD = hsd_plot(dd.aov,input$focus_2),
              Duncan = dun_plot(dd.aov,input$focus_2)
      )
    })
    ###############
    output$downloadfut_two <- downloadHandler(
      filename = function(){
        switch (input$follow_2,
                LSD = paste("lsd_plot",".jpeg",sep = ""),
                HSD = paste("hsd_plot",".jpeg",sep = ""),
                Duncan = paste("duncan_plot",".jpeg",sep = ""))
      },
      content = function(file){
        jpeg(file,width = 900,height = 450)
        datas <- load_data(input$csv_input_two)
        dd.aov <- aov(as.formula(paste("datas[,input$value_two]~",input$trt_two,"+", input$block)), data = datas)
        switch (input$follow_2,
                LSD = lsd_plot(dd.aov,input$focus_2),
                HSD = hsd_plot(dd.aov,input$focus_2),
                Duncan = dun_plot(dd.aov,input$focus_2)
        )
        dev.off()
      }
    )
    ###########
    output$fut_table_two <- renderTable({
      datas <- load_data(input$csv_input_two)
      dd.aov <- aov(as.formula(paste("datas[,input$value_two]~",input$trt_two,"+", input$block)), data = datas)
      tt <- switch (input$follow_2,
                    LSD = lsd_t(dd.aov,input$focus_2),
                    HSD = hsd_t(dd.aov,input$focus_2),
                    Duncan = dun_t(dd.aov,input$focus_2)
      )
      cc <-  as.data.frame(tt[["groups"]])
      names(cc) <- c("average","groups")
      cc
    })
    ##########
    output$downloadfut_table_two <- downloadHandler(
      filename = function(){
        switch (input$follow_2,
                LSD = paste("lsd_table","csv",sep = "."),
                HSD =  paste("hsd_table","csv",sep = "."),
                Duncan =  paste("duncan_table","csv",sep = "."))

      },
      content = function(file){
        datas <- load_data(input$csv_input_two)
        dd.aov <- aov(as.formula(paste("datas[,input$value_two]~",input$trt_two,"+", input$block)), data = datas)
        tt <- switch (input$follow_2,
                      LSD = lsd_t(dd.aov,input$focus_2),
                      HSD = hsd_t(dd.aov,input$focus_2),
                      Duncan = dun_t(dd.aov,input$focus_2)
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
#

## To be copied in the server
#
