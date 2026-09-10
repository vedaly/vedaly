#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new project to Vedaly
#'
#' @param userEmail_to_beAssigned  [== user email address] (assigned to project)
#' @param projectName
#' 
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
assign_user_toProject <- function(userEmail_to_beAssigned, projectName) {
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
 
  # current user's email address (== user who owns the projec and is currently logged in)
  email <- auth_config$email
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/assignUserToProject")
  
  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(
      email = email,
      userEmail_to_beAssigned = userEmail_to_beAssigned ,
      projectName = projectName
    )
  )
  
  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Assigning user failed: ", msg)
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