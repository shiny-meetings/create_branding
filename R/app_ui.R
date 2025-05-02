#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_fillable(
      theme = bs_theme(),
      h4("Use LLM to Create _brand.yml and ggplot scales & theme for your brand"),
      # _brand.yml generation
      mod_create_brand_yml_ui("create_brand_yml"),
      br(),
      mod_create_functions_ui("create_functions")
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
      app_title = "create_branding"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
