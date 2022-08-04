#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  ######## One way anova
  mod_one_way_module_server("one_way_module_1")

  ################# Two way anova
  mod_two_way_module_server("two_way_module_1")

  ############## latin anova
  mod_latin_module_server("latin_module_1")

  }
