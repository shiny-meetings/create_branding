#' create_scales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h3 uiOutput renderUI
#' @importFrom bslib navset_pill_list nav_panel card card_header
#' @importFrom yaml yaml.load
#' @importFrom ellmer chat_google_gemini
#' @import ggplot2
mod_create_scales_ui <- function(id) {
  ns <- NS(id)
  tagList(
    navset_pill_list(
      widths = c(2, 10),
      nav_panel(title = "Palettes",
                h3("Palettes"),
                uiOutput(ns("palettes_card"))),
      nav_panel(title = "Scales",
                card(
                  card_header("Bar Plot"),
                  verbatimTextOutput(ns("bar_code")),
                  plotOutput(ns("bar_plot"))
                ))
    )
  )
}

#' create_scales Server Functions
#'
#' @noRd
mod_create_scales_server <- function(id, brand_yml_content){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Read brand_yml
    brand_yml <- reactive(yaml.load(brand_yml_content()))

    # Get all colors as hex codes
    colors_list <- reactive({
      if (brand_yml_content() == "Brand yml will appear here"){
        return(NULL)
      }
      colorz_list(brand_yml())
    })

    # Chat object
    chat <- chat_google_gemini(system_prompt = system_prompt_palettes)

    # Get complete palettes from LLM
    palettes_from_llm <- reactive({
        req(colors_list())
        chat$chat_structured(
        paste("Semantic colors:", paste(names(colors_list()), colors_list(), sep = "=", collapse = ", ")),
        type = palette_type
      )
    })

    output$palettes_card <- renderUI({
      req(palettes_from_llm())
      color_palette_card(palettes_from_llm())
    })



    output$bar_code <- renderText({
      paste0(
        "bar_data <- mtcars |>\n
            count(cyl) |>\n
            mutate(cyl = as.factor(cyl))\n",
        "bar_plot <- ggplot(bar_data, aes(x = cyl, y = n, fill = cyl)) +\n
            geom_bar(stat = 'identity')\n",
        "bar_plot +
          scale_fill_brand(palette = 'qual') +\n
          labs(title ='scale_fill_brand(palette = 'qual')',
              subtitle = 'theme_brand()') +\n
          theme_brand()"
      )
    })

    output$bar_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      bar_plot <- ggplot(bar_data, aes(x = cyl, y = n, fill = cyl)) +
        geom_bar(stat = "identity")

      bar_plot +
        scale_fill_brand(palette = "qual", brand_palettes = palettes_from_llm()) +
        labs(title ='scale_fill_brand(palette = "qual")',
             subtitle = 'theme_brand()') +
        theme_brand(colors = colors_list())
    })

  })
}
