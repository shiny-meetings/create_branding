library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(thematic)
library(ragg)

options(shiny.useragg = TRUE)
thematic_shiny(font = "auto")

# UI
ui <- page_fluid(
  title = "MTCars Dashboard",
  theme = bs_theme(),

  # Header with value boxes
  layout_columns(
    value_box(
      title = "Total Cars",
      value = textOutput("total_cars"),
      showcase = bsicons::bs_icon("car-front")
    ),
    value_box(
      title = "Avg MPG",
      value = textOutput("avg_mpg"),
      showcase = bsicons::bs_icon("speedometer")
    ),
    value_box(
      title = "Avg HP",
      value = textOutput("avg_hp"),
      showcase = bsicons::bs_icon("lightning")
    ),
    value_box(
      title = "Max HP",
      value = textOutput("max_hp"),
      showcase = bsicons::bs_icon("trophy")
    )
  ),

  # Controls in a card
  card(
    card_header("Filters"),
    layout_columns(
      card_body(
        selectInput("cylinder", "Cylinders:",
                    choices = c("All", sort(unique(mtcars$cyl))),
                    selected = "All")
      ),
      card_body(
        sliderInput("hp_range", "Horsepower Range:",
                    min = min(mtcars$hp), max = max(mtcars$hp),
                    value = c(min(mtcars$hp), max(mtcars$hp)))
      ),
      card_body(
        checkboxGroupInput("transmission", "Transmission:",
                           choices = c("Automatic" = 0, "Manual" = 1),
                           selected = c(0, 1))
      ),
      card_body(
        radioButtons("carb", "Carburetors:",
                     choices = c("All", sort(unique(mtcars$carb))),
                     selected = "All")
      )
    )
  ),

  # First row with 2 plots
  layout_columns(
    card(
      card_header("Scatter Plot: MPG vs HP"),
      card_body(plotOutput("scatter_plot"))
    ),
    card(
      card_header("Bar Chart: Average MPG by Cylinder"),
      card_body(plotOutput("bar_plot"))
    )
  ),

  # Second row with 2 plots
  layout_columns(
    card(
      card_header("Box Plot: MPG by Cylinder"),
      card_body(plotOutput("box_plot"))
    ),
    card(
      card_header("Density Plot: HP Distribution"),
      card_body(plotOutput("density_plot"))
    )
  ),

  # Table
  card(
    card_header("Car Data Table"),
    card_body(DTOutput("car_table"))
  )
)

# Server
server <- function(input, output, session) {

  # Reactive dataset based on inputs
  filtered_data <- reactive({
    data <- mtcars

    # Add car names as a column
    data$car_name <- rownames(mtcars)

    # Filter by cylinders
    if (input$cylinder != "All") {
      data <- data %>% filter(cyl == as.numeric(input$cylinder))
    }

    # Filter by HP range
    data <- data %>% filter(hp >= input$hp_range[1] & hp <= input$hp_range[2])

    # Filter by transmission
    if (length(input$transmission) < 2) {
      data <- data %>% filter(am %in% as.numeric(input$transmission))
    }

    # Filter by carburetors
    if (input$carb != "All") {
      data <- data %>% filter(carb == as.numeric(input$carb))
    }

    data
  })

  # Value box outputs
  output$total_cars <- renderText({
    nrow(filtered_data())
  })

  output$avg_mpg <- renderText({
    round(mean(filtered_data()$mpg), 1)
  })

  output$avg_hp <- renderText({
    round(mean(filtered_data()$hp), 1)
  })

  output$max_hp <- renderText({
    max(filtered_data()$hp)
  })

  # Scatter Plot (geom_point)
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = hp, y = mpg, color = factor(cyl))) +
      geom_point(size = 3, alpha = 0.7) +
      labs(x = "Horsepower", y = "Miles Per Gallon", color = "Cylinders") +
      # theme_minimal() +
      theme(legend.position = "bottom")
  })

  # Bar Plot (geom_col)
  output$bar_plot <- renderPlot({
    avg_mpg_by_cyl <- filtered_data() %>%
      group_by(cyl) %>%
      summarize(avg_mpg = mean(mpg))

    ggplot(avg_mpg_by_cyl, aes(x = factor(cyl), y = avg_mpg, fill = factor(cyl))) +
      geom_col() +
      labs(x = "Cylinders", y = "Average MPG", fill = "Cylinders") +
      # theme_minimal() +
      theme(legend.position = "none")
  })

  # Box Plot (geom_boxplot)
  output$box_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = factor(cyl), y = mpg, fill = factor(cyl))) +
      geom_boxplot() +
      labs(x = "Cylinders", y = "Miles Per Gallon", fill = "Cylinders") +
      # theme_minimal() +
      theme(legend.position = "none")
  })

  # Density Plot (geom_density)
  output$density_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = hp, fill = factor(am))) +
      geom_density(alpha = 0.5) +
      labs(x = "Horsepower", y = "Density", fill = "Transmission") +
      scale_fill_discrete(labels = c("Automatic", "Manual")) +
      # theme_minimal() +
      theme(legend.position = "bottom")
  })

  # Data table
  output$car_table <- renderDT({
    filtered_data() %>%
      select(car_name, mpg, cyl, hp, wt, qsec, am) %>%
      rename("Car" = car_name,
             "MPG" = mpg,
             "Cylinders" = cyl,
             "Horsepower" = hp,
             "Weight" = wt,
             "1/4 Mile Time" = qsec,
             "Transmission" = am) %>%
      mutate(Transmission = ifelse(Transmission == 0, "Automatic", "Manual"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
