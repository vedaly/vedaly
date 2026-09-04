#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new project to Vedaly
#'
#' @param projectName (to be created)
#' 
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
add_project <- function(projectName, description = NULL) {
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
 
  # current user's email address
  email <- auth_config$email
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/addNewProject")
  
  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(
      email = email,
      projectName = projectName,
      description = description
    )
  )
  
  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Creating project failed: ", msg)
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