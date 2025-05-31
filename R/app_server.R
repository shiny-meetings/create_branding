#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # _brand_yml generation
  brand_yml_and_btn <- mod_create_brand_yml_server("create_brand_yml")
  # observe(cat(brand_yml()))

  # create functions
  # mod_create_functions_server("create_functions", brand_yml)

  observeEvent(brand_yml_and_btn$brand_btn, {
    bslib::nav_select(id = "page", selected = "ggplot scales")
  })

  mod_create_scales_server("create_scales_1", brand_yml_and_btn)
}
