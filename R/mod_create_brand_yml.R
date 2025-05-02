#' create_brand_yml UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @import shinyAce
#' @import bslib
#' @import ellmer
#' @import shinychat
mod_create_brand_yml_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = sidebar(
      fileInput(ns("image_upload"), h5("Upload Screenshot(s) of Company Branding"),
                multiple = TRUE,
                accept = c('image/png', 'image/jpeg'),
                placeholder = "Drag and drop images",
                width = "100%"),
      chat_ui(ns("chat")),
      width = "40%"
    ),
    card(
      full_screen = TRUE,
      min_height = "600px",
      card_title("_brand.yml (Editable)"),
      aceEditor(ns("brand_editor"),
                value = "Brand yml will appear here",
                mode = "yaml",
                theme = "xcode",
                height = "400px",
                fontSize = 14,
                wordWrap = TRUE,
                showPrintMargin = FALSE,
                highlightActiveLine = TRUE)
    )
  )
}

#' create_brand_yml Server Functions
#'
#' @noRd
mod_create_brand_yml_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Create chat object for Google Gemini with specified system prompt
    chat <- chat_google_gemini(system_prompt = sys_prompt())

    # Initialize a flag to check if images are sent
    images_sent <- reactiveVal(FALSE)

    # Variable to store image contents
    image_contents <- reactiveVal(NULL)

    # Observe the file input for image upload
    observeEvent(input$image_upload, {
      # Reset the flag
      images_sent(FALSE)

      # Store image contents
      image_contents(lapply(input$image_upload$datapath, content_image_file))
    })

    brand_code <- reactiveVal(NULL)

    # Observe user input event for chat
    observeEvent(input$chat_user_input, {
      ## If images are uploaded, attach them with the chat
      if (!images_sent() && !is.null(image_contents())) {
        response_promise <- do.call(chat$chat_async, c(list(input$chat_user_input), image_contents()))
        images_sent(TRUE)
      } else {
        ## Otherwise just chat without images
        response_promise <- chat$chat_async(input$chat_user_input)
      }

      ## Since `chat_async` is used, `then` is used so that `response` is utilized when available
      response_promise$then(function(response) {
        brand_code(response)
        chat_append(ns("chat"), response)
      })
    })

    # Update the ace editor when brand_code changes
    observeEvent(brand_code(), {
      # Remove the backticks before and after the _brand.yml
      cleaned_brand_code <- sub("^```yaml\\s*", "", brand_code())
      cleaned_brand_code <- sub("\\s*```$", "", cleaned_brand_code)

      updateAceEditor(session, "brand_editor", value = cleaned_brand_code)
    })

    # Return the final value of _brand.yml
    return(reactive(input$brand_editor))
  })
}

## To be copied in the UI
# mod_create_brand_yml_ui("create_brand_yml_1")

## To be copied in the server
# mod_create_brand_yml_server("create_brand_yml_1")
