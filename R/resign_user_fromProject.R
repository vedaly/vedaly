#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new project to Vedaly
#'
#' @param userEmail_to_beResigned  [== user email address] (resigned from project)
#' @param projectName
#' 
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
resign_user_fromProject <- function(userEmail_to_beResigned, projectName) {
  
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
      userEmail_to_beResigned = userEmail_to_beResigned ,
      projectName = projectName
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