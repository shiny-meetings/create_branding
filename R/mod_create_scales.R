#' create_scales UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList h3 uiOutput renderUI br downloadButton
#' @importFrom bslib navset_pill_list nav_panel nav_item card card_header input_task_button
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
                aceEditor(ns("palette_editor"),
                          value = "Palette code will appear here",
                          mode = "r",
                          theme = "xcode",
                          height = "200px",
                          fontSize = 14,
                          wordWrap = TRUE,
                          showPrintMargin = FALSE,
                          highlightActiveLine = TRUE),
                input_task_button(ns("update_palettes"), "Update Palettes"),
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

                )),
      #nav_spacer(),
      nav_item(downloadButton(ns("download_all"), "Download Palettes and Scales"))
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


    palettes_from_llm_edited <- reactiveVal()

    observeEvent(palettes_from_llm(), {
      list_str <- format_list(palettes_from_llm())
      #list_str <- paste(list_str, collapse = "\n")

      palettes_from_llm_edited(palettes_from_llm())

      updateAceEditor(session, "palette_editor", value = list_str)
    })




    observeEvent(input$update_palettes, {
      cleaned_code <- gsub("\n", "", input$palette_editor)
      palettes_from_llm_edited(eval(parse(text = cleaned_code)))
    })


    output$palettes_card <- renderUI({
      req(palettes_from_llm())
      if (!is.null(palettes_from_llm_edited())){
        return(color_palette_card(palettes_from_llm_edited()))
      }
      color_palette_card(palettes_from_llm())
    })



    output$bar_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = bar_plot_code))
    })

    output$box_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = box_plot_code))
    })

    output$heat1_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = heat1_plot_code))
    })

    output$heat2_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = heat2_plot_code))
    })

    output$heat3_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = heat3_plot_code))
    })

    output$heat4_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = heat4_plot_code))
    })


    output$density_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = density_plot_code))
    })


    output$hist_plot <- renderPlot({
      req(palettes_from_llm_edited(), colors_list())
      eval(parse(text = hist_plot_code))
    })


    output$download_all <- downloadHandler(
      filename = function() {
        "brand_palettes_scales.R"
      },
      content = function(file) {
        # Create the content of the R script
        script_content <- paste0(
          "# Brand Color Palettes\n\n",
          paste0("brand_palettes <- ", input$palette_editor),
          "\n\n# Custom ggplot2 scale_color function\n",
          "scale_color_brand <- function(palette = \"qual\", brand_palettes, reverse = FALSE, ...) {\n",
          "  pal <- switch(palette,\n",
          "                qual = brand_palettes$qualitative,\n",
          "                seq1 = brand_palettes$sequential1,\n",
          "                seq2 = brand_palettes$sequential2,\n",
          "                seq3 = brand_palettes$sequential3,\n",
          "                div  = brand_palettes$diverging,\n",
          "                stop(\"Invalid palette name\"))\n",
          "  if (reverse) pal <- rev(pal)\n",
          "  if (palette == \"qual\") {\n",
          "    ggplot2::scale_color_manual(values = pal, ...)\n",
          "  } else {\n",
          "    ggplot2::scale_color_gradientn(colors = pal, ...)\n",
          "  }\n",
          "}\n\n",
          "\n\n# Custom ggplot2 scale_fill function\n",
          "scale_fill_brand <- function(palette = \"qual\", brand_palettes, reverse = FALSE, ...) {\n",
          "  pal <- switch(palette,\n",
          "                qual = brand_palettes$qualitative,\n",
          "                seq1 = brand_palettes$sequential1,\n",
          "                seq2 = brand_palettes$sequential2,\n",
          "                seq3 = brand_palettes$sequential3,\n",
          "                div  = brand_palettes$diverging,\n",
          "                stop(\"Invalid palette name\"))\n",
          "  if (reverse) pal <- rev(pal)\n",
          "  if (palette == \"qual\") {\n",
          "    ggplot2::scale_fill_manual(values = pal, ...)\n",
          "  } else {\n",
          "    ggplot2::scale_fill_gradientn(colors = pal, ...)\n",
          "  }\n",
          "}\n\n",
          "# Theme colors\n\n",
          paste0("colors <- ", format_list(colors_list())),
          "\n\n# Custom ggplot2 theme function\n",
          "theme_brand <- function(colors,\n",
          "                        base_size = 12,\n",
          "                        base_theme = \"minimal\",\n",
          "                        title_font = \"Arial\",\n",
          "                        text_font = \"Arial\",\n",
          "                        ...) {\n\n",
          "  base <- switch(base_theme,\n",
          "                 minimal = ggplot2::theme_minimal(base_size = base_size, base_family = text_font),\n",
          "                 bw = ggplot2::theme_bw(base_size = base_size, base_family = text_font),\n",
          "                 classic = ggplot2::theme_classic(base_size = base_size, base_family = text_font),\n",
          "                 ggplot2::theme_minimal(base_size = base_size, base_family = text_font))\n\n",
          "  base +\n",
          "    ggplot2::theme(\n",
          "      plot.title = ggplot2::element_text(\n",
          "        family = title_font,\n",
          "        size = base_size * 1.5,\n",
          "        color = colors$primary,\n",
          "        margin = ggplot2::margin(b = 10)\n",
          "      ),\n",
          "      plot.subtitle = ggplot2::element_text(\n",
          "        family = text_font,\n",
          "        size = base_size * 1.2,\n",
          "        color = colors$secondary,\n",
          "        margin = ggplot2::margin(b = 10)\n",
          "      ),\n",
          "      plot.background = ggplot2::element_rect(fill = colors$background, color = NA),\n",
          "      panel.background = ggplot2::element_rect(fill = colors$background, color = NA),\n",
          "      axis.title = ggplot2::element_text(color = colors$foreground),\n",
          "      axis.text = ggplot2::element_text(color = colors$foreground),\n",
          "      legend.title = ggplot2::element_text(family = text_font, color = colors$foreground),\n",
          "      legend.text = ggplot2::element_text(family = text_font, color = colors$foreground),\n",
          "      plot.title.position = \"plot\",\n",
          "      ...\n",
          "    )\n",
          "}"
        )

        # Write the content to the file
        writeLines(script_content, file)
      }
    )


  })
}
