qualitative_scales <- "Hues are what a five year old would understand under “different colors”: red, yellow, blue, etc. They’re perfect to distinguish between categories that don’t have an intrinsic order, like countries or ethnicities, genders or industries – that’s why these qualitative color scales are sometimes called unordered color scales. In such a color scale, colors say “I’m not worth more or less than these other colors here!”. Give your hues different lightnesses so that they’d work in greyscale, too. It makes them look better and easier to distinguish, which is especially important for colorblind readers."


sequential_scales <- "Sequential color scales are gradients that go from bright to dark or the other way round. They’re great for visualizing numbers that go from low to high, like income, temperature, or age. A medium blue on a white background, for example, lets your readers know: “My value is a bit higher than the light blue and a bit lower than the dark blue. Gradients can be classed (=split into brackets, also called classified, stepped, quantized, graduated, binned or discrete) or unclassed (=one continuous gradient). Using two or even more hues increases the color contrast between segments of your gradient, making it easier for readers to distinguish between them. To decide which data values correspond to which color in your gradient is called “interpolation” and has a massive influence on how readers perceive your values."

diverging_scales <- "Diverging (also called bipolar or double-ended) color scales are the same as sequential color scales – but instead of just going from low to high, they have a bright middle value and then go darker to both ends of the scale in different hues. Diverging color scales are often used to visualize negative and positive values, election results, or Likert scales (“strongly agree, agree, neutral, disagree, strongly disagree”). Like sequential color scales, diverging ones can be classed or unclassed."


system_prompt_palettes <- paste0("You are an expert in making color palettes for data visualization that are good for all users including colorblind users. You are also an expert in bootstrap semantic colors. Use the provided semantic colors and information about different types of color scales below and create 1 qualitative (at least 8 colors), 3 sequential (no #FFFFFF), and 1 diverging color palettes for ggplot2. You are free to use other colors (IF NEEDED) along with the provided semantic colors. Remember that the provided semantic colors are based on company branding so they are the most important. No color must repeat within the same color palette. Return only a list of color palettes without assigning names to individual colors and without any comments.", "\n", "\nColor scales: ", qualitative_scales, " ", sequential_scales, " ", diverging_scales)




palette_type <- ellmer::type_object(
  "A collection of color palettes for ggplot2",
  qualitative = ellmer::type_array(
    "A qualitative palette: a list of color hex codes without names or comments.",
    items = ellmer::type_string()
  ),
  sequential1 = ellmer::type_array(
    "First sequential palette: a list of color hex codes without names or comments.",
    items = ellmer::type_string()
  ),
  sequential2 = ellmer::type_array(
    "Second sequential palette: a list of color hex codes without names or comments.",
    items = ellmer::type_string()
  ),
  sequential3 = ellmer::type_array(
    "Third sequential palette: a list of color hex codes without names or comments.",
    items = ellmer::type_string()
  ),
  diverging = ellmer::type_array(
    "A diverging palette: a list of color hex codes without names or comments.",
    items = ellmer::type_string()
  )
)



colorz_list <- function(brand_yml){
  palette_colors <- brand_yml$color$palette
  brand_colors <- brand_yml$color
  brand_colors$palette <- NULL
  lapply(brand_colors, \(x) if (grepl("#", x)) x else palette_colors[[x]])
}




# Function to determine contrast color for text
get_contrast_color <- function(hex_color) {
  # Remove hash if present
  hex_color <- gsub("^#", "", hex_color)

  # Convert hex to RGB
  r <- strtoi(substr(hex_color, 1, 2), base = 16)
  g <- strtoi(substr(hex_color, 3, 4), base = 16)
  b <- strtoi(substr(hex_color, 5, 6), base = 16)

  # Calculate luminance (perceived brightness)
  luminance <- (0.299 * r + 0.587 * g + 0.114 * b) / 255

  # Return white for dark colors, black for light colors
  if(luminance > 0.5) {
    return("#000000")
  } else {
    return("#FFFFFF")
  }
}


create_color_swatches <- function(colors) {
  shiny::div(
    style = "display: flex; flex-direction: row;",
    lapply(colors, function(color) {
      shiny::div(
        style = paste0(
          "background-color: ", color, "; ",
          "width: 60px; height: 60px; ",
          "margin: 5px; ",
          "display: flex; justify-content: center; align-items: center; ",
          "border-radius: 5px; ",
          "color: ", get_contrast_color(color), "; ",
          "font-size: 12px;"
        ),
        color
      )
    })
  )
}



color_palette_card <- function(palettes){
  bslib::card(
    min_height = "1000px",
    bslib::card_header("Color Palettes"),
    lapply(names(palettes), function(palette_name) {
      bslib::card(
        bslib::card_header(palette_name),
        create_color_swatches(palettes[[palette_name]]),
        # Display hex codes in a row
        # shiny::div(
        #   style = "display: flex; flex-wrap: wrap; margin-top: 10px;",
        #   lapply(palettes[[palette_name]], function(color) {
        #     shiny::div(
        #       style = "margin-right: 10px; font-family: monospace;",
        #       color
        #     )
        #   })
        # )
      )
    })
  )
}





scale_color_brand <- function(palette = "qual", brand_palettes, reverse = FALSE, ...) {
  pal <- switch(palette,
                qual = brand_palettes$qualitative,
                seq1 = brand_palettes$sequential1,
                seq2 = brand_palettes$sequential2,
                seq3 = brand_palettes$sequential3,
                div  = brand_palettes$diverging,
                stop("Invalid palette name"))
  if (reverse) pal <- rev(pal)
  if (palette == "qual") {
    ggplot2::scale_color_manual(values = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal, ...)
  }
}

scale_fill_brand <- function(palette = "qual", brand_palettes, reverse = FALSE, ...) {
  pal <- switch(palette,
                qual = brand_palettes$qualitative,
                seq1 = brand_palettes$sequential1,
                seq2 = brand_palettes$sequential2,
                seq3 = brand_palettes$sequential3,
                div  = brand_palettes$diverging,
                stop("Invalid palette name"))
  if (reverse) pal <- rev(pal)
  if (palette == "qual") {
    ggplot2::scale_fill_manual(values = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = pal, ...)
  }
}

theme_brand <- function(colors,
                        base_size = 12,
                        base_theme = "minimal",
                        title_font = "Arial",
                        text_font = "Arial",
                        ...) {

  # Select base theme
  base <- switch(base_theme,
                 minimal = ggplot2::theme_minimal(base_size = base_size, base_family = text_font),
                 bw = ggplot2::theme_bw(base_size = base_size, base_family = text_font),
                 classic = ggplot2::theme_classic(base_size = base_size, base_family = text_font),
                 ggplot2::theme_minimal(base_size = base_size, base_family = text_font))

  # Customize theme
  base +
    ggplot2::theme(
      # Text elements
      plot.title = ggplot2::element_text(
        family = title_font,
        size = base_size * 1.5,
        color = colors$primary,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        family = text_font,
        size = base_size * 1.2,
        color = colors$secondary,
        margin = ggplot2::margin(b = 10)
      ),

      # Plot elements
      plot.background = ggplot2::element_rect(fill = colors$background, color = NA),
      panel.background = ggplot2::element_rect(fill = colors$background, color = NA),

      # Axis elements
      axis.title = ggplot2::element_text(color = colors$foreground),
      axis.text = ggplot2::element_text(color = colors$foreground),

      # Legend elements
      legend.title = ggplot2::element_text(family = text_font, color = colors$foreground),
      legend.text = ggplot2::element_text(family = text_font, color = colors$foreground),

      # Additional customizations
      plot.title.position = "plot",
      ...
    )
}
