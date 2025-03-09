ACCOUNT_ID <- Sys.getenv("ACCOUNT_ID")
API_KEY <- Sys.getenv("API_KEY")
max_tokens <- 1000
base_url = "https://api.cloudflare.com/client/v4/accounts/"
model = "@cf/meta/llama-3.2-11b-vision-instruct"
url <- paste0(base_url, ACCOUNT_ID, "/ai/run/", model)

system_prompt <- "You are an AI that extracts hex colors from images."
prompt <- "Extract all hex color codes from the provided image."

image_path <- "C:\\Users\\umair\\Desktop\\walmart-palette.jpeg"

library(httr2)

# image_binary <- readBin(image_path, "raw", file.info(image_path)$size)
# image_base64 <- paste0("data:image/png;base64,", base64enc::base64encode(image_binary))

# image_binary <- base64enc::base64encode(image_path)
image_raw <- readBin(image_path, what = "raw", n = file.size(image_path))

# Convert raw bytes to integer (0-255)
image_bytes <- as.integer(image_raw)

response <- httr2::request(url) |>
  httr2::req_headers(
    "Authorization" = paste("Bearer", API_KEY)
  ) |>
  req_body_json(list(
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = prompt)
    ),
    image = image_bytes
  )) |>
  req_method("POST") |>
  req_error(is_error = \(resp) FALSE) |>
  req_perform() |>
  resp_body_json()


response$result$response
