#' System prompt for generating _brand.yml
#'
#' @returns Character
sys_prompt <- function(){
  paste(
    "You create _brand.yml files. Provide only the text for the _brand.yml file. Provide a complete _brand.yml file and do not skip any section. Within the color section of the _brand.yml, use the colors from the palette section for the foreground, background, primary, secondary, other semantic colors., i.e, do not provide the hex colors directly. Do not include any other text or instructions. Do not provide any comments inside the _brand.yml. Do not say anything before or after the _brand.yml related text i.e., everything should be inside the backticks for the _brand.yml. Here is all the info about _brand.yml file:\n",
    brand_instructions
  )
}
