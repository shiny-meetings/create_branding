# library(ellmer)
#
# url_txt <- paste0("https://api.cloudflare.com/client/v4/accounts/",
#                   "b1d6dc92d29ce3a2d407249b2b2164ec",
#                   "/ai/run/@cf/meta/llama-3.3-70b-instruct-fp8-fast")
#
# chat <- chat_openai(
#   base_url = url_txt,
#   api_key = "BvvyHzmxg6hHx_Sm_reRaiEU3sMU2Kku0ImZ-hK4",
#   system_prompt = NULL,
#   model = ""
# )
# live_console(chat)

instructions <- "Create a _brand.yml for starwars."

system_prompt <- paste0(
  "You make _brand.yml files. Respond with the complete _brand.yaml file only. Do not include any instructions or any other words. Do not include ```yml and ``` at the start and end of the response.",
  " The following tells you what _brand.yml file is: ",
  paste(readLines("data/about_brand.txt"), collapse = "\n")
  )

get_brand <- function(prompt,
                      system_prompt,
                      model = "@cf/meta/llama-3.3-70b-instruct-fp8-fast",
                      max_tokens = 1000,
                      ACCOUNT_ID = Sys.getenv("ACCOUNT_ID"),
                      API_KEY = Sys.getenv("API_KEY"),
                      base_url = "https://api.cloudflare.com/client/v4/accounts/"){

  if (is.null(prompt)){
    return(NULL)
  }

  url_txt <- paste0(base_url, ACCOUNT_ID, "/ai/run/", model)

  # Make an API request
  response_text <- httr2::request(url_txt) |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", API_KEY)
    ) |>
    httr2::req_body_json(list(
      max_tokens = max_tokens,
      messages = list(
        list(role = "system",
             content = system_prompt),
        list(
          role = "user",
          content = prompt
        )
      ))) |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform() |>
    httr2::resp_body_json()


  # If response is successful, append it to the user prompt
  # clean it, and split the text into 5 sentences
  if (isTRUE(response_text$success)){
    full_text <- response_text$result$response
    # cleaned_text <- gsub("\n", "", full_text)
    # split_text <- unlist(strsplit(cleaned_text, "(?<=[.])\\s*(?=[A-Z])", perl = TRUE))
  } else {
    full_text <- NULL
  }
  full_text
}

res <- get_brand(instructions, system_prompt)


# Write to file
writeLines(res, "_brand.yml")
