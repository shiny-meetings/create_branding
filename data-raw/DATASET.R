brand_instructions <-  paste(readLines("inst/extdata/brand_yml_instructions.txt"), collapse = "\n")

usethis::use_data(brand_instructions, internal = TRUE, overwrite = TRUE)
