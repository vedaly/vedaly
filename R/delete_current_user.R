#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Sign into Vedaly from R
#'
#' @param email User email
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
delete_current_user <- function(email) {
  
  # message("hallo")
  # 
  # stop()
  # 
  # print(auto)
  
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'")
  if (!requireNamespace("askpass", quietly = TRUE)) stop("Please install 'askpass'")
  if (!grepl("@", email)) stop("Invalid email format")

#  password <- askpass::askpass("Enter your Vedaly password:")
#  if (is.null(password) || password == "") stop("Sign-in cancelled.")

  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/userDelete")
  # endpoint <- paste0(api_url, "/userSignIn")

  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(email = email)
#    body = list(email = email, password = password)
  )

  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Deleting user failed: ", msg)
#    stop("Sign-in failed: ", msg)
  }

  content <- jsonlite::fromJSON(httr::content(response))
  if (!content$success) stop("Deleting user failed. Try again later.")

  message("Deleting user successful.")
  invsibile(TRUE)
}

