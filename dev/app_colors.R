library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(shinydashboard)

# Define the palettes and colors_list as provided
palettes <- function() {
  list(
    qualitative = c("#0071DC", "#FFC200", "#72994E", "#8CD1FC", "#DE1C24", "#D3EFF8", "#6c757d", "#a83291"),
    sequential1 = c("#D3EFF8", "#8CD1FC", "#0071DC"),
    sequential2 = c("#FFF3CD", "#FFC220", "#DE7A00"),
    sequential3 = c("#C8D6BE", "#72994E", "#386641"),
    diverging = c("#DE1C24", "#FFC220", "#D3EFF8", "#8CD1FC", "#0071DC")
  )
}

colors_list <- list(
  foreground = "#041E42",
  background = "#FFFFFF",
  primary = "#0071DC",
  secondary = "#041E42",
  tertiary = "#D3EFF8",
  success = "#72994E",
  info = "#8CD1FC",
  warning = "#FFC200",
  danger = "#DE1C24",
  light = "#FFFFFF",
  dark = "#041E42"
)

# Define the scale and theme functions
scale_color_brand <- function(palette = "qual",
                              brand_palettes = palettes(),
                              reverse = FALSE,
                              ...) {

  # Select the right palette
  if (palette == "qual") {
    pal <- brand_palettes$qualitative
  } else if (palette == "seq1") {
    pal <- brand_palettes$sequential1
  } else if (palette == "seq2") {
    pal <- brand_palettes$sequential2
  } else if (palette == "seq3") {
    pal <- brand_palettes$sequential3
  } else if (palette == "div") {
    pal <- brand_palettes$diverging
  } else {
    stop("Invalid palette name")
  }

  # Reverse palette if needed
  if (reverse) pal <- rev(pal)

  # Return the appropriate scale
  if (palette == "qual") {
    ggplot2::scale_color_manual(values = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal, ...)
  }
}

scale_fill_brand <- function(palette = "qual",
                             brand_palettes = palettes(),
                             reverse = FALSE,
                             ...) {

  # Select the right palette
  if (palette == "qual") {
    pal <- brand_palettes$qualitative
  } else if (palette == "seq1") {
    pal <- brand_palettes$sequential1
  } else if (palette == "seq2") {
    pal <- brand_palettes$sequential2
  } else if (palette == "seq3") {
    pal <- brand_palettes$sequential3
  } else if (palette == "div") {
    pal <- brand_palettes$diverging
  } else {
    stop("Invalid palette name")
  }

  # Reverse palette if needed
  if (reverse) pal <- rev(pal)

  # Return the appropriate scale
  if (palette == "qual") {
    ggplot2::scale_fill_manual(values = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = pal, ...)
  }
}

theme_brand <- function(colors = colors_list,
                        base_size = 12,
                        base_theme = "minimal",
                        title_font = "Arial",
                        text_font = "Arial",
                        ...) {

  # Select base theme
  if (base_theme == "minimal") {
    base <- ggplot2::theme_minimal(base_size = base_size, base_family = text_font)
  } else if (base_theme == "bw") {
    base <- ggplot2::theme_bw(base_size = base_size, base_family = text_font)
  } else if (base_theme == "classic") {
    base <- ggplot2::theme_classic(base_size = base_size, base_family = text_font)
  } else {
    # Default to minimal
    base <- ggplot2::theme_minimal(base_size = base_size, base_family = text_font)
  }

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

ui <- dashboardPage(
  dashboardHeader(title = "Brand Palette Visualizer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Palettes", tabName = "palettes"),
      menuItem("Scales", tabName = "scales"),
      menuItem("Theme", tabName = "theme"),
      menuItem("Bar Plot", tabName = "bar"),
      menuItem("Scatter Plots", tabName = "scatter"),
      menuItem("Box Plot", tabName = "box"),
      menuItem("Heat Map", tabName = "heat"),
      menuItem("Density Plot", tabName = "density")
    )
  ),
  dashboardBody(
    tabItems(
      # Palettes tab
      tabItem(
        tabName = "palettes",
        box(
          title = "Color Palettes", width = 12, status = "primary", solidHeader = TRUE,
          h3("Qualitative Palette"),
          plotOutput("qual_palette", height = "100px"),
          h3("Sequential Palette 1"),
          plotOutput("seq1_palette", height = "100px"),
          h3("Sequential Palette 2"),
          plotOutput("seq2_palette", height = "100px"),
          h3("Sequential Palette 3"),
          plotOutput("seq3_palette", height = "100px"),
          h3("Diverging Palette"),
          plotOutput("div_palette", height = "100px"),
          h3("Theme Colors"),
          verbatimTextOutput("theme_colors")
        )
      ),

      # Scales tab
      tabItem(
        tabName = "scales",
        box(
          title = "Scale Functions", width = 12, status = "primary", solidHeader = TRUE,
          h3("scale_color_brand()"),
          verbatimTextOutput("scale_color_code"),
          h3("scale_fill_brand()"),
          verbatimTextOutput("scale_fill_code")
        )
      ),

      # Theme tab
      tabItem(
        tabName = "theme",
        box(
          title = "Theme Function", width = 12, status = "primary", solidHeader = TRUE,
          verbatimTextOutput("theme_code")
        )
      ),

      # Bar plot tab
      tabItem(
        tabName = "bar",
        box(
          title = "Bar Plot Example", width = 12, status = "primary", solidHeader = TRUE,
          h3("Code"),
          verbatimTextOutput("bar_code"),
          h3("Plot"),
          plotOutput("bar_plot")
        )
      ),

      # Scatter plots tab
      tabItem(
        tabName = "scatter",
        box(
          title = "Scatter Plot Examples", width = 12, status = "primary", solidHeader = TRUE,
          h3("Code for Sequential 1"),
          verbatimTextOutput("scatter_seq1_code"),
          h3("Plot with Sequential 1"),
          plotOutput("scatter_seq1_plot"),

          h3("Code for Sequential 2"),
          verbatimTextOutput("scatter_seq2_code"),
          h3("Plot with Sequential 2"),
          plotOutput("scatter_seq2_plot"),

          h3("Code for Sequential 3"),
          verbatimTextOutput("scatter_seq3_code"),
          h3("Plot with Sequential 3"),
          plotOutput("scatter_seq3_plot"),

          h3("Code for Classic Theme"),
          verbatimTextOutput("scatter_classic_code"),
          h3("Plot with Classic Theme"),
          plotOutput("scatter_classic_plot")
        )
      ),

      # Box plot tab
      tabItem(
        tabName = "box",
        box(
          title = "Box Plot Example", width = 12, status = "primary", solidHeader = TRUE,
          h3("Code"),
          verbatimTextOutput("box_code"),
          h3("Plot"),
          plotOutput("box_plot")
        )
      ),

      # Heat map tab
      tabItem(
        tabName = "heat",
        box(
          title = "Heat Map Examples", width = 12, status = "primary", solidHeader = TRUE,
          h3("Code for Sequential 1"),
          verbatimTextOutput("heat_seq1_code"),
          h3("Plot with Sequential 1"),
          plotOutput("heat_seq1_plot"),

          h3("Code for Sequential 2"),
          verbatimTextOutput("heat_seq2_code"),
          h3("Plot with Sequential 2"),
          plotOutput("heat_seq2_plot"),

          h3("Code for Sequential 3"),
          verbatimTextOutput("heat_seq3_code"),
          h3("Plot with Sequential 3"),
          plotOutput("heat_seq3_plot")
        )
      ),

      # Density plot tab
      tabItem(
        tabName = "density",
        box(
          title = "Density Plot Example", width = 12, status = "primary", solidHeader = TRUE,
          h3("Code"),
          verbatimTextOutput("density_code"),
          h3("Plot"),
          plotOutput("density_plot")
        )
      )
    )
  )
)

server <- function(input, output) {
  # Palette visualizations
  output$qual_palette <- renderPlot({
    df <- data.frame(x = 1:length(palettes()$qualitative),
                     y = 1,
                     color = factor(1:length(palettes()$qualitative)))
    ggplot(df, aes(x = x, y = y, fill = color)) +
      geom_tile() +
      scale_fill_manual(values = palettes()$qualitative) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$seq1_palette <- renderPlot({
    df <- data.frame(x = 1:length(palettes()$sequential1),
                     y = 1,
                     color = factor(1:length(palettes()$sequential1)))
    ggplot(df, aes(x = x, y = y, fill = color)) +
      geom_tile() +
      scale_fill_manual(values = palettes()$sequential1) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$seq2_palette <- renderPlot({
    df <- data.frame(x = 1:length(palettes()$sequential2),
                     y = 1,
                     color = factor(1:length(palettes()$sequential2)))
    ggplot(df, aes(x = x, y = y, fill = color)) +
      geom_tile() +
      scale_fill_manual(values = palettes()$sequential2) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$seq3_palette <- renderPlot({
    df <- data.frame(x = 1:length(palettes()$sequential3),
                     y = 1,
                     color = factor(1:length(palettes()$sequential3)))
    ggplot(df, aes(x = x, y = y, fill = color)) +
      geom_tile() +
      scale_fill_manual(values = palettes()$sequential3) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$div_palette <- renderPlot({
    df <- data.frame(x = 1:length(palettes()$diverging),
                     y = 1,
                     color = factor(1:length(palettes()$diverging)))
    ggplot(df, aes(x = x, y = y, fill = color)) +
      geom_tile() +
      scale_fill_manual(values = palettes()$diverging) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$theme_colors <- renderPrint({
    colors_list
  })

  # Scale functions code
  output$scale_color_code <- renderPrint({
    cat('scale_color_brand <- function(palette = "qual",
                             brand_palettes = palettes(),
                             reverse = FALSE,
                             ...) {

  # Select the right palette
  if (palette == "qual") {
    pal <- brand_palettes$qualitative
  } else if (palette == "seq1") {
    pal <- brand_palettes$sequential1
  } else if (palette == "seq2") {
    pal <- brand_palettes$sequential2
  } else if (palette == "seq3") {
    pal <- brand_palettes$sequential3
  } else if (palette == "div") {
    pal <- brand_palettes$diverging
  } else {
    stop("Invalid palette name")
  }

  # Reverse palette if needed
  if (reverse) pal <- rev(pal)

  # Return the appropriate scale
  if (palette == "qual") {
    ggplot2::scale_color_manual(values = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colors = pal, ...)
  }
}')
  })

  output$scale_fill_code <- renderPrint({
    cat('scale_fill_brand <- function(palette = "qual",
                            brand_palettes = palettes(),
                            reverse = FALSE,
                            ...) {

  # Select the right palette
  if (palette == "qual") {
    pal <- brand_palettes$qualitative
  } else if (palette == "seq1") {
    pal <- brand_palettes$sequential1
  } else if (palette == "seq2") {
    pal <- brand_palettes$sequential2
  } else if (palette == "seq3") {
    pal <- brand_palettes$sequential3
  } else if (palette == "div") {
    pal <- brand_palettes$diverging
  } else {
    stop("Invalid palette name")
  }

  # Reverse palette if needed
  if (reverse) pal <- rev(pal)

  # Return the appropriate scale
  if (palette == "qual") {
    ggplot2::scale_fill_manual(values = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colors = pal, ...)
  }
}')
  })

  # Theme function code
  output$theme_code <- renderPrint({
    cat('theme_brand <- function(colors = colors_list,
                       base_size = 12,
                       base_theme = "minimal",
                       title_font = "Arial",
                       text_font = "Arial",
                       ...) {

  # Select base theme
  if (base_theme == "minimal") {
    base <- ggplot2::theme_minimal(base_size = base_size, base_family = text_font)
  } else if (base_theme == "bw") {
    base <- ggplot2::theme_bw(base_size = base_size, base_family = text_font)
  } else if (base_theme == "classic") {
    base <- ggplot2::theme_classic(base_size = base_size, base_family = text_font)
  } else {
    # Default to minimal
    base <- ggplot2::theme_minimal(base_size = base_size, base_family = text_font)
  }

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
}')
  })

  # Bar plot
  output$bar_code <- renderPrint({
    cat('bar_data <- mtcars |>
  count(cyl) |>
  mutate(cyl = as.factor(cyl))

bar_plot <- ggplot(bar_data, aes(x = cyl, y = n, fill = cyl)) +
  geom_bar(stat = "identity")

bar_plot +
  scale_fill_brand(palette = "qual") +
  labs(title = \'scale_fill_brand(palette = "qual")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$bar_plot <- renderPlot({
    bar_data <- mtcars |>
      count(cyl) |>
      mutate(cyl = as.factor(cyl))

    bar_plot <- ggplot(bar_data, aes(x = cyl, y = n, fill = cyl)) +
      geom_bar(stat = "identity")

    bar_plot +
      scale_fill_brand(palette = "qual") +
      labs(title = 'scale_fill_brand(palette = "qual")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  # Scatter plots
  output$scatter_seq1_code <- renderPrint({
    cat('scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
  geom_point()

scatter_plot +
  scale_color_brand(palette = "seq1") +
  labs(title = \'scale_fill_brand(palette = "seq1")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$scatter_seq1_plot <- renderPlot({
    scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
      geom_point()

    scatter_plot +
      scale_color_brand(palette = "seq1") +
      labs(title = 'scale_fill_brand(palette = "seq1")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  output$scatter_seq2_code <- renderPrint({
    cat('scatter_plot +
  scale_color_brand(palette = "seq2") +
  labs(title = \'scale_fill_brand(palette = "seq2")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$scatter_seq2_plot <- renderPlot({
    scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
      geom_point()

    scatter_plot +
      scale_color_brand(palette = "seq2") +
      labs(title = 'scale_fill_brand(palette = "seq2")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  output$scatter_seq3_code <- renderPrint({
    cat('scatter_plot +
  scale_color_brand(palette = "seq3") +
  labs(title = \'scale_fill_brand(palette = "seq3")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$scatter_seq3_plot <- renderPlot({
    scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
      geom_point()

    scatter_plot +
      scale_color_brand(palette = "seq3") +
      labs(title = 'scale_fill_brand(palette = "seq3")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  output$scatter_classic_code <- renderPrint({
    cat('scatter_plot +
  scale_color_brand(palette = "seq1") +
  labs(title = \'scale_fill_brand(palette = "seq1")\',
       subtitle = \'theme_brand(base_theme = "classic")\') +
  theme_brand(base_theme = "classic")')
  })

  output$scatter_classic_plot <- renderPlot({
    scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
      geom_point()

    scatter_plot +
      scale_color_brand(palette = "seq1") +
      labs(title = 'scale_fill_brand(palette = "seq1")',
           subtitle = 'theme_brand(base_theme = "classic")') +
      theme_brand(base_theme = "classic")
  })

  # Box plot
  output$box_code <- renderPrint({
    cat('box_plot <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_boxplot()

box_plot +
  scale_fill_brand(palette = "qual") +
  labs(title = \'scale_fill_brand(palette = "qual")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$box_plot <- renderPlot({
    box_plot <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
      geom_boxplot()

    box_plot +
      scale_fill_brand(palette = "qual") +
      labs(title = 'scale_fill_brand(palette = "qual")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  # Heat maps
  output$heat_seq1_code <- renderPrint({
    cat('arrests_data <- USArrests |>
  rownames_to_column(var = "State") |>
  pivot_longer(cols = -State, names_to = "Crime", values_to = "Rate") |>
  filter(Crime != "UrbanPop")

heat_map <- ggplot(arrests_data, aes(x = Crime, y = State, fill = Rate)) +
  geom_tile()

heat_map +
  scale_fill_brand(palette = "seq1") +
  labs(title = \'scale_fill_brand(palette = "seq1")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$heat_seq1_plot <- renderPlot({
    arrests_data <- USArrests |>
      rownames_to_column(var = "State") |>
      pivot_longer(cols = -State, names_to = "Crime", values_to = "Rate") |>
      filter(Crime != "UrbanPop")

    heat_map <- ggplot(arrests_data, aes(x = Crime, y = State, fill = Rate)) +
      geom_tile()

    heat_map +
      scale_fill_brand(palette = "seq1") +
      labs(title = 'scale_fill_brand(palette = "seq1")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  output$heat_seq2_code <- renderPrint({
    cat('heat_map +
  scale_fill_brand(palette = "seq2") +
  labs(title = \'scale_fill_brand(palette = "seq2")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$heat_seq2_plot <- renderPlot({
    arrests_data <- USArrests |>
      rownames_to_column(var = "State") |>
      pivot_longer(cols = -State, names_to = "Crime", values_to = "Rate") |>
      filter(Crime != "UrbanPop")

    heat_map <- ggplot(arrests_data, aes(x = Crime, y = State, fill = Rate)) +
      geom_tile()

    heat_map +
      scale_fill_brand(palette = "seq2") +
      labs(title = 'scale_fill_brand(palette = "seq2")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  output$heat_seq3_code <- renderPrint({
    cat('heat_map +
  scale_fill_brand(palette = "seq3") +
  labs(title = \'scale_fill_brand(palette = "seq3")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$heat_seq3_plot <- renderPlot({
    arrests_data <- USArrests |>
      rownames_to_column(var = "State") |>
      pivot_longer(cols = -State, names_to = "Crime", values_to = "Rate") |>
      filter(Crime != "UrbanPop")

    heat_map <- ggplot(arrests_data, aes(x = Crime, y = State, fill = Rate)) +
      geom_tile()

    heat_map +
      scale_fill_brand(palette = "seq3") +
      labs(title = 'scale_fill_brand(palette = "seq3")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })

  # Density plot
  output$density_code <- renderPrint({
    cat('density_plot <- ggplot(mtcars, aes(x = qsec, fill = as.factor(gear))) +
  geom_density(alpha = 0.7)

density_plot +
  scale_fill_brand(palette = "qual") +
  labs(title = \'scale_fill_brand(palette = "qual")\',
       subtitle = \'theme_brand()\') +
  theme_brand()')
  })

  output$density_plot <- renderPlot({
    density_plot <- ggplot(mtcars, aes(x = qsec, fill = as.factor(gear))) +
      geom_density(alpha = 0.7)

    density_plot +
      scale_fill_brand(palette = "qual") +
      labs(title = 'scale_fill_brand(palette = "qual")',
           subtitle = 'theme_brand()') +
      theme_brand()
  })
}

shinyApp(ui, server)
