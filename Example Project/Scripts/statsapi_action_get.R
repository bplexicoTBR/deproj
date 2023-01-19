statsapi_action_get <- function(base_url = "https://statsapi.mlb.com",
                                api_path = "api/v1",
                                endpoint,
                                url_path = "",
                                params = data.frame(),
                                authentication = FALSE) {
  # Build URL from parameters
  # baseurl.com/apipath1/apipath2/endpoint/urlpath1/urlpath2?param1=value1&param2=value2
  url <- file.path(base_url, api_path, endpoint, url_path)
  
  if(!(length(params) %in% c(0,2))) {stop("ERROR: The params data frame should only have key and value columns.")}
  
  # Add Parameters
  if (length(params > 0)) {
    names(params) = c("key", "value")
    for (i in 1:nrow(params)) {
      url <- urltools::param_set(url, key = params$key[i], value = params$value[i])
    }
  }
  
  # Call Webservice
  # Statcast Data requires authentication
  if (authentication == FALSE) {
    response <- httr::GET(url = url, httr::content_type_json())
  } 
  
  # call URL
  content <- rawToChar(response$content) %>%
    jsonlite::fromJSON()
  
  return(content)
}
