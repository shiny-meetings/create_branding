library(shiny)
library(bslib)
library(yaml)
# library(shinyjs)
# library(shinyWidgets)
library(colourpicker)

# Define the UI
ui <- page_sidebar(
  title = "Brand.yml Editor",
  sidebar = sidebar(
    width = 350,
    accordion(
      accordion_panel(
        "Meta Information",
        textInput("meta_name_short", "Short Name", ""),
        textInput("meta_name_full", "Full Name", ""),
        textInput("meta_link_home", "Home URL", ""),
        textInput("meta_link_github", "GitHub URL", ""),
        textInput("meta_link_linkedin", "LinkedIn URL", "")
      ),

      accordion_panel(
        "Logo",
        textInput("logo_small", "Small Logo Path", ""),
        textInput("logo_medium_light", "Medium Logo (Light) Path", ""),
        textInput("logo_medium_dark", "Medium Logo (Dark) Path", ""),
        textInput("logo_large", "Large Logo Path", "")
      ),

      accordion_panel(
        "Color Palette",
        tags$div(
          id = "color_palette_container",
          actionButton("add_color", "Add Color", class = "btn-sm"),
          tags$div(
            id = "color_palette_inputs",
            tags$div(
              class = "d-flex mb-2",
              textInput("color_palette_name_1", NULL, "", placeholder = "Color name"),
              colourInput("color_palette_value_1", NULL, "#FFFFFF", returnName = TRUE)
            ),
            tags$div(
              class = "d-flex mb-2",
              textInput("color_palette_name_2", NULL, "", placeholder = "Color name"),
              colourInput("color_palette_value_2", NULL, "#000000", returnName = TRUE)
            )
          )
        )
      ),

      accordion_panel(
        "Theme Colors",
        colourInput("color_foreground", "Foreground", "#151515", returnName = TRUE),
        colourInput("color_background", "Background", "#FFFFFF", returnName = TRUE),
        colourInput("color_primary", "Primary", "#447099", returnName = TRUE),
        colourInput("color_secondary", "Secondary", NULL, returnName = TRUE),
        colourInput("color_success", "Success", NULL, returnName = TRUE),
        colourInput("color_info", "Info", NULL, returnName = TRUE),
        colourInput("color_warning", "Warning", NULL, returnName = TRUE),
        colourInput("color_danger", "Danger", NULL, returnName = TRUE)
      ),

      accordion_panel(
        "Typography",
        tags$div(
          id = "fonts_container",
          actionButton("add_font", "Add Font", class = "btn-sm"),
          tags$div(
            id = "font_inputs",
            tags$div(
              class = "font-entry mb-3 p-2 border rounded",
              textInput("font_family_1", "Font Family", ""),
              selectInput("font_source_1", "Font Source",
                          choices = c("google", "bunny", "file", "system")),
              textInput("font_weight_1", "Font Weights (comma separated)", "400,700"),
              textInput("font_style_1", "Font Styles (comma separated)", "normal,italic")
            )
          )
        ),

        tags$hr(),

        tags$h5("Base Typography"),
        textInput("typography_base_family", "Base Font Family", ""),
        textInput("typography_base_weight", "Base Font Weight", "400"),
        textInput("typography_base_size", "Base Font Size", "16px"),
        textInput("typography_base_line_height", "Base Line Height", "1.5"),

        tags$h5("Headings Typography"),
        textInput("typography_headings_family", "Headings Font Family", ""),
        textInput("typography_headings_weight", "Headings Font Weight", "600"),
        textInput("typography_headings_style", "Headings Font Style", "normal"),
        colourInput("typography_headings_color", "Headings Color", NULL, returnName = TRUE),

        tags$h5("Monospace Typography"),
        textInput("typography_monospace_family", "Monospace Font Family", ""),
        textInput("typography_monospace_weight", "Monospace Font Weight", "400"),
        textInput("typography_monospace_size", "Monospace Font Size", "0.9em")
      )
    )
  ),

  layout_columns(
    card(
      full_screen = TRUE,
      card_header("_brand.yml Preview"),
      card_body(
        verbatimTextOutput("yaml_output", placeholder = TRUE)
      ),
      card_footer(
        downloadButton("download_yaml", "Download _brand.yml")
      )
    )
  )#,

  # useShinyjs()
)

# Define the server
server <- function(input, output, session) {

  # Reactive to store color palette entries
  color_palette_count <- reactiveVal(2)

  # Reactive to store font entries
  font_count <- reactiveVal(1)

  # Add new color to the palette
  observeEvent(input$add_color, {
    current_count <- color_palette_count()
    new_count <- current_count + 1

    insertUI(
      selector = "#color_palette_inputs",
      where = "beforeEnd",
      ui = tags$div(
        class = "d-flex mb-2",
        textInput(paste0("color_palette_name_", new_count), NULL, "", placeholder = "Color name"),
        colourInput(paste0("color_palette_value_", new_count), NULL, "#CCCCCC", returnName = TRUE)
      )
    )

    color_palette_count(new_count)
  })

  # Add new font entry
  observeEvent(input$add_font, {
    current_count <- font_count()
    new_count <- current_count + 1

    insertUI(
      selector = "#font_inputs",
      where = "beforeEnd",
      ui = tags$div(
        class = "font-entry mb-3 p-2 border rounded",
        textInput(paste0("font_family_", new_count), "Font Family", ""),
        selectInput(paste0("font_source_", new_count), "Font Source",
                    choices = c("google", "bunny", "file", "system")),
        textInput(paste0("font_weight_", new_count), "Font Weights (comma separated)", "400,700"),
        textInput(paste0("font_style_", new_count), "Font Styles (comma separated)", "normal,italic")
      )
    )

    font_count(new_count)
  })

  # Generate the YAML output based on user inputs
  output$yaml_output <- renderText({
    # Build the YAML structure
    brand_yml <- list()

    # Meta section
    if (input$meta_name_short != "" || input$meta_name_full != "") {
      brand_yml$meta <- list()

      if (input$meta_name_short != "" && input$meta_name_full != "") {
        brand_yml$meta$name <- list(
          short = input$meta_name_short,
          full = input$meta_name_full
        )
      } else if (input$meta_name_short != "") {
        brand_yml$meta$name <- input$meta_name_short
      }

      # Handle links
      link_fields <- c(
        home = input$meta_link_home,
        github = input$meta_link_github,
        linkedin = input$meta_link_linkedin
      )

      non_empty_links <- link_fields[link_fields != ""]

      if (length(non_empty_links) > 0) {
        if (length(non_empty_links) == 1 && !is.null(non_empty_links$home) && non_empty_links$home != "") {
          brand_yml$meta$link <- non_empty_links$home
        } else {
          brand_yml$meta$link <- as.list(non_empty_links)
        }
      }
    }

    # Logo section
    if (input$logo_small != "" || input$logo_medium_light != "" ||
        input$logo_medium_dark != "" || input$logo_large != "") {
      brand_yml$logo <- list()

      if (input$logo_small != "") {
        brand_yml$logo$small <- input$logo_small
      }

      if (input$logo_medium_light != "" || input$logo_medium_dark != "") {
        brand_yml$logo$medium <- list()
        if (input$logo_medium_light != "") brand_yml$logo$medium$light <- input$logo_medium_light
        if (input$logo_medium_dark != "") brand_yml$logo$medium$dark <- input$logo_medium_dark
      }

      if (input$logo_large != "") {
        brand_yml$logo$large <- input$logo_large
      }
    }

    # Color palette
    color_palette <- list()
    for (i in 1:color_palette_count()) {
      name_input <- input[[paste0("color_palette_name_", i)]]
      value_input <- input[[paste0("color_palette_value_", i)]]

      if (!is.null(name_input) && name_input != "" && !is.null(value_input)) {
        color_palette[[name_input]] <- value_input
      }
    }

    # Theme colors
    color_theme <- list()
    theme_colors <- c(
      "foreground", "background", "primary", "secondary",
      "success", "info", "warning", "danger"
    )

    for (color in theme_colors) {
      color_value <- input[[paste0("color_", color)]]
      if (!is.null(color_value) && color_value != "") {
        color_theme[[color]] <- color_value
      }
    }

    # Combine color palette and theme
    if (length(color_palette) > 0 || length(color_theme) > 0) {
      brand_yml$color <- list()
      if (length(color_palette) > 0) {
        brand_yml$color$palette <- color_palette
      }

      for (name in names(color_theme)) {
        brand_yml$color[[name]] <- color_theme[[name]]
      }
    }

    # Typography section
    typography <- list()

    # Fonts
    fonts <- list()
    for (i in 1:font_count()) {
      family <- input[[paste0("font_family_", i)]]
      source <- input[[paste0("font_source_", i)]]
      weights <- input[[paste0("font_weight_", i)]]
      styles <- input[[paste0("font_style_", i)]]

      if (!is.null(family) && family != "") {
        font_entry <- list(
          family = family,
          source = source
        )

        if (!is.null(weights) && weights != "") {
          weight_list <- strsplit(weights, ",")[[1]]
          weight_list <- trimws(weight_list)
          if (length(weight_list) > 0) {
            # Convert numeric weights to numeric type
            numeric_weights <- suppressWarnings(as.numeric(weight_list))
            if (all(!is.na(numeric_weights))) {
              font_entry$weight <- numeric_weights
            } else {
              font_entry$weight <- weight_list
            }
          }
        }

        if (!is.null(styles) && styles != "") {
          style_list <- strsplit(styles, ",")[[1]]
          style_list <- trimws(style_list)
          if (length(style_list) > 0) {
            font_entry$style <- style_list
          }
        }

        fonts[[i]] <- font_entry
      }
    }

    # Base typography
    base <- list()
    if (input$typography_base_family != "") base$family <- input$typography_base_family
    if (input$typography_base_weight != "") base$weight <- as.numeric(input$typography_base_weight)
    if (input$typography_base_size != "") base$size <- input$typography_base_size
    if (input$typography_base_line_height != "") base$`line-height` <- as.numeric(input$typography_base_line_height)

    # Headings typography
    headings <- list()
    if (input$typography_headings_family != "") headings$family <- input$typography_headings_family
    if (input$typography_headings_weight != "") headings$weight <- as.numeric(input$typography_headings_weight)
    if (input$typography_headings_style != "") headings$style <- input$typography_headings_style
    if (!is.null(input$typography_headings_color) && input$typography_headings_color != "") {
      headings$color <- input$typography_headings_color
    }

    # Monospace typography
    monospace <- list()
    if (input$typography_monospace_family != "") monospace$family <- input$typography_monospace_family
    if (input$typography_monospace_weight != "") monospace$weight <- as.numeric(input$typography_monospace_weight)
    if (input$typography_monospace_size != "") monospace$size <- input$typography_monospace_size

    # Combine typography
    if (length(fonts) > 0) typography$fonts <- fonts
    if (length(base) > 0) typography$base <- base
    if (length(headings) > 0) typography$headings <- headings
    if (length(monospace) > 0) typography$monospace <- monospace

    if (length(typography) > 0) {
      brand_yml$typography <- typography
    }

    # Convert to YAML
    if (length(brand_yml) > 0) {
      yaml_content <- as.yaml(brand_yml, indent.mapping.sequence = TRUE)
      return(yaml_content)
    } else {
      return("# Your _brand.yml will appear here as you fill in the form")
    }
  })

  # Download the YAML file
  output$download_yaml <- downloadHandler(
    filename = function() {
      "_brand.yml"
    },
    content = function(file) {
      yaml_content <- as.yaml(yaml.load(input$yaml_output), indent.mapping.sequence = TRUE)
      writeLines(yaml_content, file)
    }
  )
}

# Run the app
shinyApp(ui, server)
