#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new user to Vedaly
#'
#' @param new_user_email The email address of the user.
#' @param new_user_first_name First (given) name of the user.
#' @param new_user_last_name Last (family) name of the user.
#' @param organization_name Organization or affiliation of the user.
#' @param company_roles users' company's
#' @param admin_email Already existing admin_email address of the company
#'
#' @return Invisibly returns `TRUE` if the request was successful.
#' @export
add_new_user <- function(
    new_user_email,
    new_user_first_name,
    new_user_last_name,
    company_roles) {

  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
  
  admin_email <- auth_config$email
  
  allowed_roles = list("admin", "user")

  company_roles_isList = is.list(company_roles)
  
  if (company_roles_isList == FALSE) {
    cat("\n")
    message("company_roles must be a list")
    cat("\n")
    stop("company_roles must be a list")
  }
  
  for (role in company_roles) {
    if (!(role %in% allowed_roles)) {
      print("hier")
      stop("Invalid role: ", role, "; allowed roles are only: ", paste(unlist(allowed_roles), collapse=" "), " given as list")
    }
  }
  
  # stop("halt")

  print(admin_email)
  
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Please install the 'httr' package.")
  }
  
  if (!grepl("^.+@.+\\..+$", new_user_email)) {
    stop("Invalid email address format.")
  }
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/addNewUser")
  
  #####
  
  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(
      email = new_user_email,
      first_name = new_user_first_name,
      last_name = new_user_last_name,
      admin_email = admin_email
    )
  )
  
  
  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Adding new user failed: ", msg)
  }
  
  content <- jsonlite::fromJSON(httr::content(response))
  
  if (content$success) {
    message(content$message)
  } else {
    if (!content$success) {
      stop(content$message)
    }
  }
  
  ######
  
  message("This function is under development. Email us to info@vedaly.io to let us know you need it")
}
