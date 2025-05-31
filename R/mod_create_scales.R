#' create_scales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h3 uiOutput renderUI br
#' @importFrom bslib navset_pill_list nav_panel card card_header
#' @importFrom yaml yaml.load
#' @importFrom ellmer chat_google_gemini
#' @importFrom shinyAce aceEditor
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
                  card_header("scale_fill_brand"),
                  navset_card_tab(
                    nav_panel(
                      title = "Bar Chart",
                      aceEditor(ns("bar_code"), value = bar_plot_code, mode = "r", theme = "github",
                                height = "250px", readOnly = TRUE),
                      plotOutput(ns("bar_plot"))
                    ),
                    nav_panel(
                      title = "Boxplot",
                      aceEditor(ns("box_code"), value = box_plot_code, mode = "r", theme = "github",
                                height = "200px", readOnly = TRUE),
                      plotOutput(ns("box_plot"))
                    ),
                    nav_panel(
                      title = "Heatmap",
                      card(
                        card_header("Sequential 1"),
                        aceEditor(ns("heat1_code"), value = heat1_plot_code, mode = "r", theme = "github",
                                  height = "200px", readOnly = TRUE),
                        plotOutput(ns("heat1_plot"), height = "600px")
                      ),
                      br(),
                      card(
                        card_header("Sequential 2"),
                        aceEditor(ns("heat2_code"), value = heat2_plot_code, mode = "r", theme = "github",
                                  height = "200px", readOnly = TRUE),
                        plotOutput(ns("heat2_plot"), height = "600px")
                      ),
                      br(),
                      card(
                        card_header("Sequential 3"),
                        aceEditor(ns("heat3_code"), value = heat3_plot_code, mode = "r", theme = "github",
                                  height = "200px", readOnly = TRUE),
                        plotOutput(ns("heat3_plot"), height = "600px")
                      ),
                      br(),
                      card(
                        card_header("Diverging"),
                        aceEditor(ns("heat4_code"), value = heat4_plot_code, mode = "r", theme = "github",
                                  height = "200px", readOnly = TRUE),
                        plotOutput(ns("heat4_plot"), height = "600px")
                      )
                    ),
                    nav_panel(
                      title = "Distributions",
                      card(
                           card_header("Density Plot"),
                           aceEditor(ns("density_code"), value = density_plot_code, mode = "r", theme = "github",
                                height = "200px", readOnly = TRUE),
                           plotOutput(ns("density_plot"))
                      ),
                      br(),
                      card(
                        card_header("Histogram"),
                        aceEditor(ns("hist_code"), value = hist_plot_code, mode = "r", theme = "github",
                                  height = "200px", readOnly = TRUE),
                        plotOutput(ns("hist_plot"), height = "500px")
                      )
                    )
                  )

                ))
    )
  )
}

#' create_scales Server Functions
#'
#' @noRd
mod_create_scales_server <- function(id, brand_yml_and_btn){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Read brand_yml
    brand_yml <- reactive({
      req(brand_yml_and_btn$brand_editor)
      yaml.load(brand_yml_and_btn$brand_editor)
      })

    # Get all colors as hex codes
    colors_list <- reactive({
      req(brand_yml())
      if (length(brand_yml()) == 1){
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



    output$bar_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = bar_plot_code))
    })

    output$box_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = box_plot_code))
    })

    output$heat1_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = heat1_plot_code))
    })

    output$heat2_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = heat2_plot_code))
    })

    output$heat3_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = heat3_plot_code))
    })

    output$heat4_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = heat4_plot_code))
    })


    output$density_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = density_plot_code))
    })


    output$hist_plot <- renderPlot({
      req(palettes_from_llm(), colors_list())
      eval(parse(text = hist_plot_code))
    })


  })
}
