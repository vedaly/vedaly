#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Sign into Vedaly from R
#'
#' @param email User email
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
delete_user <- function() {

message("vorher")

  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))

  email = auth_config$email
 
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/userDelete")
   
message(email)
message(api_url)
message(endpoint)

message("")
message("hier_1")
message("")

# message("==================")
# message("Content type:")
# print(httr::headers(response)[["content-type"]])

# content <- jsonlite::fromJSON(
#  httr::content(response, as = "text", encoding = "UTF-8")
#)

# message("Raw content:")
# print(httr::content(response))
# message("==================")

  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(email = email)
  )

message("")
message("vorher")
message("hier_2")
#print(class(response))
message("response")
message(response)
message(class(response))
message("")
message("bis hier")

 if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Deleting user failed: ", msg)
  }

message("hier_3")

  content <- jsonlite::fromJSON(httr::content(response))
  if (!content$success) stop("Deleting user failed. Try again later.")

  message("Deleting user successful.")
  invsibile(TRUE)

message("nachher")

}
