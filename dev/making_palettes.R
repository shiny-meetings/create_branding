library(ellmer)
library(yaml)


brand_yml_content <- 'meta:
  name:
    short: Home Depot
    full: The Home Depot, Inc.
  link:
    home: https://www.homedepot.com/

logo:
  images:
    full: logos/homedepot-logo.svg
  small: logos/homedepot-icon.svg
  medium:
    light: full
    dark: full
  large: full

color:
  palette:
    white: "#FFFFFF"
    black: "#000000"
    orange: "#F96302"
    gray-dark: "#747474"
    gray-medium: "#C4C4C4"
    gray-light: "#F5F5F5"
  foreground: black
  background: white
  primary: orange
  secondary: gray-dark
  tertiary: gray-medium
  light: white
  dark: black

typography:
  fonts:
    - family: Helvetica Neue LT Pro
      source: file
      files:
        - path: fonts/helvetica-neue-lt-pro/HelveticaNeueLTPro-BdCn.otf
          weight: 700
          style: normal
    - family: Helvetica Neue LT Pro
      source: file
      files:
        - path: fonts/helvetica-neue-lt-pro/HelveticaNeueLTPro-Roman.otf
          weight: 400
          style: normal

  base:
    family: Helvetica Neue LT Pro
    weight: 400
    size: 14px
    line-height: 1.4
  headings:
    family: Helvetica Neue LT Pro
    weight: 700
    style: normal
    line-height: 1.2
  monospace:
    family: monospace
    weight: 400
    size: 0.9em'









qualitative_scales <- "Hues are what a five year old would understand under “different colors”: red, yellow, blue, etc. They’re perfect to distinguish between categories that don’t have an intrinsic order, like countries or ethnicities, genders or industries – that’s why these qualitative color scales are sometimes called unordered color scales. In such a color scale, colors say “I’m not worth more or less than these other colors here!”. Give your hues different lightnesses so that they’d work in greyscale, too. It makes them look better and easier to distinguish, which is especially important for colorblind readers."


sequential_scales <- "Sequential color scales are gradients that go from bright to dark or the other way round. They’re great for visualizing numbers that go from low to high, like income, temperature, or age. A medium blue on a white background, for example, lets your readers know: “My value is a bit higher than the light blue and a bit lower than the dark blue. Gradients can be classed (=split into brackets, also called classified, stepped, quantized, graduated, binned or discrete) or unclassed (=one continuous gradient). Using two or even more hues increases the color contrast between segments of your gradient, making it easier for readers to distinguish between them. To decide which data values correspond to which color in your gradient is called “interpolation” and has a massive influence on how readers perceive your values."

diverging_scales <- "Diverging (also called bipolar or double-ended) color scales are the same as sequential color scales – but instead of just going from low to high, they have a bright middle value and then go darker to both ends of the scale in different hues. Diverging color scales are often used to visualize negative and positive values, election results, or Likert scales (“strongly agree, agree, neutral, disagree, strongly disagree”). Like sequential color scales, diverging ones can be classed or unclassed."

# colors_list <- c(
#   primary= "#0071DC",
#   secondary= "#FFC200",
#   tertiary= "#D3EFF8",
#   success= "#72994E",
#   info= "#8CD1FC",
#   warning= "#FFC220",
#   danger= "#DE1C24"
# )

brand_yml <- yaml::yaml.load(brand_yml_content)

colors_list <- function(brand_yml){
  palette_colors <- brand_yml$color$palette
  brand_colors <- brand_yml$color
  brand_colors$palette <- NULL
  lapply(brand_colors, \(x){
    if (grepl("#", x)) x else palette_colors[[x]]
  })
}

colors_list <- colors_list(brand_yml)

system_prompt_palettes <- paste0("You are an expert in making color palettes for data visualization that are good for all users including colorblind users. You are also an expert in bootstrap semantic colors. Use the provided semantic colors and information about different types of color scales below and create 1 qualitative (at least 8 colors), 3 sequential (no #FFFFFF), and 1 diverging color palettes for ggplot2. You are free to use other colors (IF NEEDED) along with the provided semantic colors. Remember that the provided semantic colors are based on company branding so they are the most important. No color must repeat within the same color palette. Return only a list of color palettes without assigning names to individual colors and without any comments.", "\n", "\nColor scales: ", qualitative_scales, " ", sequential_scales, " ", diverging_scales)



# chat <- chat_google_gemini(system_prompt = system_prompt_palettes)
#
# prompt <- paste("Semantic colors:", paste(names(colors_list), colors_list, sep = "=", collapse = ", "))
#
# chat$chat(prompt)


palette_type <- type_object(
  "A collection of color palettes for ggplot2",
  qualitative = type_array(
    "A qualitative palette: a list of color hex codes without names or comments.",
    items = type_string()
  ),
  sequential1 = type_array(
    "First sequential palette: a list of color hex codes without names or comments.",
    items = type_string()
  ),
  sequential2 = type_array(
    "Second sequential palette: a list of color hex codes without names or comments.",
    items = type_string()
  ),
  sequential3 = type_array(
    "Third sequential palette: a list of color hex codes without names or comments.",
    items = type_string()
  ),
  diverging = type_array(
    "A diverging palette: a list of color hex codes without names or comments.",
    items = type_string()
  )
)

chat <- chat_google_gemini(system_prompt = system_prompt_palettes)

# Extract structured data
palettes_from_llm <- chat$chat_structured(
  paste("Semantic colors:", paste(names(colors_list), colors_list, sep = "=", collapse = ", ")),
  type = palette_type
)

palettes <- function(){
  palettes_from_llm
}
