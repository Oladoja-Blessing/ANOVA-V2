#' latin_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_latin_module_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabItem(tabName = "latin",
            sidebarLayout(
              sidebarPanel(
                fileInput(ns("csv_input_three"),"Choose input file",
                          accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
                selectInput(ns("value_three"),"Observation",""),
                selectInput(ns("trt_three"), "Treatment",""),
                selectInput(ns("row"), "Row",""),
                selectInput(ns("col"), "Column",""),
                selectInput(ns("follow_3"),"Choose follow up test",
                            choices = c("LSD","HSD","Duncan")),
                selectInput(ns("focus_3"),"Select focus",""),
                submitButton("Apply Change",icon("refresh"))
              ),
              ########################################################
              mainPanel(
                tabsetPanel(
                  type = "tab",
                  tabPanel("Data", DT::dataTableOutput(ns("latin_data"))
                  ),
                  tabPanel("Analysis of Variance",
                           verbatimTextOutput(ns("aov_three")),
                           downloadButton(ns("downloadaov_three"),
                                          "Download result")),
                  tabPanel("Follow up Test",
                           plotOutput(ns("fut_three")),
                           downloadButton(ns("downloadfut_three"),
                                          "Download plot"),
                           tableOutput(ns("fut_table_three")),
                           downloadButton(ns("downloadfut_table_three"),
                                          "Download table")
                  )
                )
              )
            )
    )
  )
}

#' latin_module Server Functions
#'
#' @noRd
mod_latin_module_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    ##########
    output$latin_data <- DT::renderDataTable(load_data(input$csv_input_three))
    ##########
    reactives <- rv(

      mydata = NULL

    )
    observeEvent(input$csv_input_three, {

      #Store loaded data in reactive
      reactives$mydata <- read.csv(file = input$csv_input_three$datapath)

      #Update select input
      updateSelectInput(session, inputId = 'focus_3', label = 'select focus', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'value_three', label = 'Observation', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'trt_three', label = 'Treatment', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'row', label = 'Row', choices  = colnames(reactives$mydata))
      updateSelectInput(session, inputId = 'col', label = 'Column', choices  = colnames(reactives$mydata))
    })
    ###########
    output$aov_three <- renderPrint(broom::tidy(latin_aov(input$csv_input_three,
                                                          input$trt_three,
                                                          input$row,
                                                          input$col,
                                                          input$value_three)))
    ###########
    output$downloadaov_three <- downloadHandler(
      filename = function(){
        paste("aov","csv",sep = ".")
      } ,
      content = function(file){
        write.csv((broom::tidy(latin())),file)
      }
    )
    ###########
    output$fut_three <- renderPlot({
      datas <- load_data(input$csv_input_three)
      dd.aov <- aov(as.formula(paste("datas[,input$value_three]~",
                                     input$trt_three,"+", input$row,
                                     "+",input$col)),
                    data = datas)
      switch (input$follow_3,
              LSD = lsd_plot(dd.aov,input$focus_3),
              HSD = hsd_plot(dd.aov,input$focus_3),
              Duncan = dun_plot(dd.aov,input$focus_3)
      )
    })
    ############
    output$downloadfut_three <- downloadHandler(
      filename = function(){
        switch (input$follow_3,
                LSD = paste("lsd_plot",".jpeg",sep = ""),
                HSD = paste("hsd_plot",".jpeg",sep = ""),
                Duncan = paste("duncan_plot",".jpeg",sep = ""))
      },
      content = function(file){
        jpeg(file,width = 900,height = 450)
        datas <- load_data(input$csv_input_three)
        dd.aov <- aov(as.formula(paste("datas[,input$value_three]~",
                                       input$trt_three,"+", input$row,
                                       "+",input$col)),
                      data = datas)
        switch (input$follow_3,
                LSD = lsd_plot(dd.aov,input$focus_3),
                HSD = hsd_plot(dd.aov,input$focus_3),
                Duncan = dun_plot(dd.aov,input$focus_3)
        )
        dev.off()
      }
    )
    ##########
    output$fut_table_three <- renderTable({
      datas <- load_data(input$csv_input_three)
      dd.aov <- aov(as.formula(paste("datas[,input$value_three]~",
                                     input$trt_three,"+", input$row,
                                     "+",input$col)),
                    data = datas)
      tt <- switch (input$follow_3,
                    LSD = lsd_t(dd.aov,input$focus_3),
                    HSD = hsd_t(dd.aov,input$focus_3),
                    Duncan = dun_t(dd.aov,input$focus_3)
      )
      cc <-  tt[["groups"]]
      names(cc) <- c("average","groups")
      cc
    })
    ############
    output$downloadfut_table_three <- downloadHandler(
      filename = function(){
        switch (input$follow_3,
                LSD = paste("lsd_table","csv",sep = "."),
                HSD =  paste("hsd_table","csv",sep = "."),
                Duncan =  paste("duncan_table","csv",sep = "."))

      },
      content = function(file){
        datas <- load_data(input$csv_input_three)
        dd.aov <- aov(as.formula(paste("datas[,input$value_three]~",
                                       input$trt_three,"+", input$row,
                                       "+",input$col)),
                      data = datas)
        tt <- switch (input$follow_3,
                      LSD = lsd_t(dd.aov,input$focus_3),
                      HSD = hsd_t(dd.aov,input$focus_3),
                      Duncan = dun_t(dd.aov,input$focus_3)
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
