#' create_functions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import bslib
#' @import shinyAce
mod_create_functions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(ns("create_functions"), "Create Functions", class = "btn-small"),
    br(),
    layout_column_wrap(
      width = 1/3,
      # use_brand
      card(
        full_screen = TRUE,
        min_height = "600px",
        card_header("use_brand"),
        aceEditor(
          ns("brand"),
          mode = "r",
          theme = "xcode",
          height = "500px",
          value = "# Press the 'Create Functions' button to generate the code"
        )
      )
    )
  )
}

#' create_functions Server Functions
#'
#' @noRd
mod_create_functions_server <- function(id, user_string){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # use_brand
    observeEvent(input$create_functions, {
      code_use_brand_yml <- paste0(
        "use_brand_yml <- function(dir = \".\") {\n",
        "  # Ensure the directory exists\n",
        "  if (!dir.exists(dir)) {\n",
        "    stop(\"Directory '\", dir, \"' does not exist.\")\n",
        "  }\n",
        "\n",
        "  # Create the full file path\n",
        "  file_path <- file.path(dir, \"_brand.yml\")\n",
        "\n",
        "  # Write the content to the file\n",
        "  writeLines(brand_yml_content, file_path)\n",
        "\n",
        "  message(\"Brand YAML file saved to: \", file_path)\n",
        "\n",
        "  # Return the file path invisibly\n",
        "  invisible(file_path)\n",
        "}"
      )
      ## Create the function code with proper indentation
      code_brand <- paste0(
        "brand_yml_content <- function() {\n",
        "'", user_string(), "'\n",
        "}\n\n",
        code_use_brand_yml
      )
      # Update the ace editor with the code
      updateAceEditor(session, "brand", value = code_brand)
    })
  })
}

## To be copied in the UI
# mod_create_functions_ui("create_functions_1")

## To be copied in the server
# mod_create_functions_server("create_functions_1")
