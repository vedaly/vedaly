#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new project to Vedaly
#'
#' @param projectName (to be created)
#' 
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
add_project <- function(projectName) {
  
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
      projectName = projectName
    )
  )
  
  # debugging:
  
  cat("\nHTTP status:\n")
  print(httr::status_code(response))
  
  cat("\nHTTP response:\n")
  print(httr::content(response, as = "text"))
  
  # debugging ended:

  cat("\n")
  cat("------------------")
  cat("\n")
  
  cat("\n")
  cat("back to frontend:")
  cat("\n")
  cat("email:")
  print(email)
  cat("\n")
}