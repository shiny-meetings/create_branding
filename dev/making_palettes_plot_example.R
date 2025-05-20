library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)

scale_color_brand <- function(palette = "qual", brand_palettes = palettes(), reverse = FALSE, ...) {
  pal <- switch(palette,
                qual = brand_palettes$qualitative,
                seq1 = brand_palettes$sequential1,
                seq2 = brand_palettes$sequential2,
                seq3 = brand_palettes$sequential3,
                div  = brand_palettes$diverging,
                stop("Invalid palette name"))
  if (reverse) pal <- rev(pal)
  if (palette == "qual") {
    scale_color_manual(values = pal, ...)
  } else {
    scale_color_gradientn(colors = pal, ...)
  }
}

scale_fill_brand <- function(palette = "qual", brand_palettes = palettes(), reverse = FALSE, ...) {
  pal <- switch(palette,
                qual = brand_palettes$qualitative,
                seq1 = brand_palettes$sequential1,
                seq2 = brand_palettes$sequential2,
                seq3 = brand_palettes$sequential3,
                div  = brand_palettes$diverging,
                stop("Invalid palette name"))
  if (reverse) pal <- rev(pal)
  if (palette == "qual") {
    scale_fill_manual(values = pal, ...)
  } else {
    scale_fill_gradientn(colors = pal, ...)
  }
}

theme_brand <- function(colors = colors_list,
                        base_size = 12,
                        base_theme = "minimal",
                        title_font = "Arial",
                        text_font = "Arial",
                         ...) {

  # Select base theme
  base <- switch(base_theme,
                 minimal = theme_minimal(base_size = base_size, base_family = text_font),
                 bw = theme_bw(base_size = base_size, base_family = text_font),
                 classic = theme_classic(base_size = base_size, base_family = text_font),
                 theme_minimal(base_size = base_size, base_family = text_font))

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

bar_data <- mtcars |>
  count(cyl) |>
  mutate(cyl = as.factor(cyl))

bar_plot <- ggplot(bar_data, aes(x = cyl, y = n, fill = cyl)) +
  geom_bar(stat = "identity")

bar_plot +
  scale_fill_brand(palette = "qual") +
  labs(title ='scale_fill_brand(palette = "qual")',
       subtitle = 'theme_brand()') +
  theme_brand()


scatter_plot <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Petal.Length)) +
  geom_point()

scatter_plot + scale_color_brand(palette = "seq1") + labs(title ='scale_fill_brand(palette = "seq1")', subtitle = 'theme_brand()') + theme_brand()
scatter_plot + scale_color_brand(palette = "seq2") + labs(title ='scale_fill_brand(palette = "seq2")', subtitle = 'theme_brand()') + theme_brand()
scatter_plot + scale_color_brand(palette = "seq3") + labs(title ='scale_fill_brand(palette = "seq3")', subtitle = 'theme_brand()') + theme_brand()
scatter_plot + scale_color_brand(palette = "seq1") + labs(title ='scale_fill_brand(palette = "seq1")', subtitle = 'theme_brand(base_theme = "classic")') + theme_brand(base_theme = "classic")



box_plot <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_boxplot()

box_plot + scale_fill_brand(palette = "qual") + labs(title ='scale_fill_brand(palette = "qual")', subtitle = 'theme_brand()') + theme_brand()



arrests_data <- USArrests |>
  rownames_to_column(var = "State") |>
  pivot_longer(cols = -State, names_to = "Crime", values_to = "Rate") |>
  filter(Crime != "UrbanPop")

heat_map <- ggplot(arrests_data, aes(x = Crime, y = State, fill = Rate)) +
  geom_tile()

heat_map + scale_fill_brand(palette = "seq1") + labs(title ='scale_fill_brand(palette = "seq1")', subtitle = 'theme_brand()') + theme_brand()
heat_map + scale_fill_brand(palette = "seq2") + labs(title ='scale_fill_brand(palette = "seq2")', subtitle = 'theme_brand()') + theme_brand()
heat_map + scale_fill_brand(palette = "seq3") + labs(title ='scale_fill_brand(palette = "seq3")', subtitle = 'theme_brand()') + theme_brand()


density_plot <- ggplot(mtcars, aes(x = qsec, fill = as.factor(gear))) +
  geom_density(alpha = 0.7)

density_plot + scale_fill_brand(palette = "qual") + labs(title ='scale_fill_brand(palette = "qual")', subtitle = 'theme_brand()') + theme_brand()




facet_hist <- ggplot(mpg, aes(x = hwy, fill = class)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~class, scales = "free_y")

facet_hist + scale_fill_brand(palette = "qual") + labs(title ='scale_fill_brand(palette = "qual")', subtitle = 'theme_brand()') + theme_brand()




cor_mtcars <- cor(mtcars)
cor_data <- as.data.frame(as.table(cor_mtcars))
names(cor_data) <- c("Var1", "Var2", "Correlation")

# Create heatmap with custom diverging palette
diverging_heatmap <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", size = 0.5)

diverging_heatmap + scale_fill_brand(palette = "div", limits = c(-1, 1)) + labs(title ='scale_fill_brand(palette = "div")', subtitle = 'theme_brand()') + theme_brand()






scatter_diverging <- ggplot(mtcars, aes(x = wt, y = mpg, color = disp)) +
  geom_point(size = 4, alpha = 0.8)

scatter_diverging + scale_color_brand(palette = "div") +
  labs(title ='scale_color_brand(palette = "div")', subtitle = 'theme_brand()') + theme_brand()
