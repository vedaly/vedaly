#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Sign into Vedaly from R
#'
#' @param email User email
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
delete_user <- function() {

  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))

  email = auth_config$email
 
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/userDelete")


  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(email = email)
  )
  
  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Deleting user failed: ", msg)
  }
  
  content <- httr::content(response)
  
  message("")
  message("content:")
  print(class(content))
  message("")
  print(content)
  message("")
  
  if (!content$success) {
    stop(content$message)
  }
  
  message(content$message)

}
