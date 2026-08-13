#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Add new user to Vedaly
#'
#' @param new_user_email The email address of the user.
#' @param new_user_first_name First (given) name of the user.
#' @param new_user_last_name Last (family) name of the user.
#' @param new_user_company_roles users' company's
#'
#' @return Invisibly returns `TRUE` if the request was successful.
#' @export
add_new_user <- function(
    new_user_email,
    new_user_first_name,
    new_user_last_name,
    new_user_company_roles){

  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
  
  admin_email <- auth_config$email
  
  # currently according the database schema (table "companies")
  # only one user can have "admin" privileges/company_roles
  # (the generation of this first user is started by calling the vedaly frontend
  #  "vedaly/R/sign_up")
  allowed_roles = list("admin", "user")

  company_roles_isList = is.list(new_user_company_roles)
  
  if (company_roles_isList == FALSE) {
    cat("\n")
    message("new_user_company_roles must be a list")
    cat("\n")
    stop("new_user_company_roles must be a list")
  }
  
  for (role in new_user_company_roles) {
    if (!(role %in% allowed_roles)) {
      stop("Invalid role: ", role, "; allowed roles are only: ", paste(unlist(allowed_roles), collapse=" "), " given as list")
    }
  }
  
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
      user_email = new_user_email,
      first_name = new_user_first_name,
      last_name = new_user_last_name,
      company_roles = new_user_company_roles,
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
}
