token = Sys.getenv("KEK_ACCESS_TOKEN")

base_url <- "https://kekunltest.api.crm4.dynamics.com/api/data/v9.2/"

# Construct the URL
# Remove test in URL for production
url <- paste0(base_url, "unl_", "vaks")

next_link = ""
result = tibble::tibble()
i = 1

while (!is.null(url)) {
  print(paste(i, next_link))
  # Make the GET request

  response = httr::GET(
    url = url,
    httr::add_headers("Authorization" = paste("Bearer", token)))

  
  # Check for HTTP errors
  httr::stop_for_status(response)
  
  # Process the response
  json <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON() 
  
  url <- json$`@odata.nextLink`
  
  df <- json %>%
    as.data.frame() %>%
    select(starts_with("value.unl_")) %>%
    rename_with(~ str_remove(., "value."))
  
  print(paste("Response size: ", nrow(df)))
  
  result <- result %>% 
    bind_rows(df)
  
  if (i > 100){
    break
  }
   i = i + 1
}



dfopleidings <- get_kek_data("opleidings")