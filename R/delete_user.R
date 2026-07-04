#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Sign into Vedaly from R
#'
#' @param email User email
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
delete_user <- function() {

  gql_api_url = "https://graphql-dev.omicschart.com/v1/graphql"
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))

  email <- auth_config$email
  email_asList <- list(email = email)
  
  # get the company id of the user using his/her email address
  get_user_companyId_query <- "
    query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          company_id
        }
      }
    }
  "

  response <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_user_companyId_query,
      variables = email_asList
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )

  result <- httr::content(response, as = "parsed", encoding = "UTF-8")
  
  company_id <- result$data$preon_op$users[[1]]$company_id
  company_id_asList = list(company_id = company_id)
  
  # determing the users (their email addresses) with the company id companyId
  get_users_emails_query <- "
    query myQuery($company_id: Int!) {
      preon_op {
        users(where: {company_id: {_eq: $company_id}}) {
          email
        }
      }
    }
  "
  
  response_2 <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_users_emails_query,
      variables = company_id_asList
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_2 <- httr::content(response_2, as = "parsed", encoding = "UTF-8")
  
  company_emails <- lapply(result_2$data$preon_op$users, function(x) x$email)
  
  company_emails_withoutAIUser <- company_emails[
    !grepl("-ai@", unlist(company_emails))
  ]
  
  users_count = length(company_emails_withoutAIUser)
  
  # if users_count == 1: only 1 (real) user belong to the company
  if (users_count == 1) {
    message("You're the only user of your company.")
    message("If you continue not only your user data, but")
    message("all data of your company will be deleted")
    
    answer <- tolower(
      readline("Do you want to continue to delete your account? (yes/no): ")
    )
    
    while (!answer %in% c("yes", "no")) {
      answer <- tolower(
        readline("Please enter yes or no: ")
      )
    }
    
    if (answer == "yes") {
      
      cat("Account deletion started...\n")
      cat("\n")
      
    } else {
      
      stop("Account deletion aborted")
      
    }
  }
  
  ## start to delete user account with email addess 'email'
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/userDelete")

  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(email = email, users_count = users_count, company_id = company_id)
  )
  
  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Deleting user failed: ", msg)
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
