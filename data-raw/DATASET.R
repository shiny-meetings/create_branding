brand_instructions <-  paste(readLines("inst/extdata/brand_yml_instructions.txt"), collapse = "\n")

library(dplyr)
library(tidyr)
library(tibble)

bar_data <- mtcars |>
  count(cyl) |>
  mutate(cyl = as.factor(cyl))

arrests_data <- USArrests |>
  rownames_to_column(var = "State") |>
  pivot_longer(cols = -State, names_to = "Crime", values_to = "Rate") |>
  filter(Crime != "UrbanPop")


cor_mtcars <- cor(mtcars)
cor_data <- as.data.frame(as.table(cor_mtcars))
names(cor_data) <- c("Var1", "Var2", "Correlation")

usethis::use_data(brand_instructions, bar_data, arrests_data, internal = TRUE, overwrite = TRUE)
