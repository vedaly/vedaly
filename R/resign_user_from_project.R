#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new project to Vedaly
#'
#' @param user_email_to_be_resigned  [== user email address] (resigned from project)
#' @param project_name
#' 
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
resign_user_from_project <- function(user_email_to_be_resigned, project_name) {
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
 
  # current user's email address (== user who owns the projec and is currently logged in)
  email <- auth_config$email
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/resignUserFromProject")
  
  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(
      email = email,
      user_email_to_be_resigned = user_email_to_be_resigned ,
      project_name = project_name
    )
  )
  
  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Resigning user failed: ", msg)
  }
  
  content <- jsonlite::fromJSON(httr::content(response))
  
  if (content$success) {
    message(content$message)
  } else {
    if (!content$success) {
      stop(content$message)
    }
  }
}