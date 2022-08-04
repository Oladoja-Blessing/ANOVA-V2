#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(

      dashboardHeader(title = "Analysis of Variance"),

      dashboardSidebar(
        sidebarMenu(
          menuItem("CRD ANOVA",
                   tabName = "one"),
          menuItem("RCBD ANOVA",
                   tabName = "two"),
          menuItem("Latin Square ANOVA",
                   tabName = "latin")

        )
      ),
      dashboardBody(
        tabItems(
          tabItem( tabName = "one",
                   mod_one_way_module_ui("one_way_module_1") ),
          tabItem(tabName = "two",
                  mod_two_way_module_ui("two_way_module_1") ),
          tabItem(tabName = "latin",
                  mod_latin_module_ui("latin_module_1")
                  )
          )
        )
      )
  )

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Analysis of Variance"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()

  )
}
