#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Sign into Vedaly from R
#'
#' @param email User email
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
delete_user <- function() {

  # csv file with header and user accounts/emails to
  # be delete (only emails belonging to the company
  # of the current admin can be delete)
  fileWithEmails_toBedeleted = "/home/shenz/vedaly/accounts/emailAdressAcounts_toBeDeleted.csv"
  
  possible_emails_acountsToBeDeleted_list <- as.list(read.delim(fileWithEmails_toBedeleted, header=TRUE, sep=",")[[1]])
  
  gql_api_url = "https://graphql-dev.omicschart.com/v1/graphql"
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))

  # current user email address
  email <- auth_config$email
  email_asList <- list(email = email)
  
  # get the company id of the current user
  get_user_companyId_query <- "
    query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          company_id
        }
      }
    }
  "

  response_companyId <- httr::POST(
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

  result_companyId <- httr::content(response_companyId, as = "parsed", encoding = "UTF-8")
  
  company_id <- result_companyId$data$preon_op$users[[1]]$company_id
  company_id_asList = list(company_id = company_id)
  
  # determing all users email addresses with the company id companyId
  get_users_emails_query <- "
    query myQuery($company_id: Int!) {
      preon_op {
        users(where: {company_id: {_eq: $company_id}}) {
          email
        }
      }
    }
  "
  
  response_allUsers_emails <- httr::POST(
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
 
  result_allUsers_emails <- httr::content(response_allUsers_emails, as = "parsed", encoding = "UTF-8")
  
  company_allUsers_emails <- lapply(result_allUsers_emails$data$preon_op$users, function(x) x$email)
  
  #---
  
  # get the company_roles of the current user with the email address 'email'
  get_currentUser_companyRoles_query <- "
   query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          company_roles
        }
      }
    }
  "
  
  response_currentUser_companyRoles <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_currentUser_companyRoles_query,
      variables = email_asList
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_currentUser_companyRoles <- httr::content(response_currentUser_companyRoles, as = "parsed", encoding = "UTF-8")
  
  company_currentUser_roles <- result_currentUser_companyRoles$data$preon_op$users[[1]]$company_roles
  
  hasAdminRole <- any(sapply(company_currentUser_roles, function(x) x == "admin"))
  
  if (hasAdminRole == FALSE) {
    message("You don't have the persmission to delete user accounts.")
    message("Please contact admin in your organization.")
    stop("Deletion of user account(s) has been interrupted.")
  }
  
  #---
  
  # determing now the counts of user email addresses belonging to the company
  # without ai
  company_allUsers_emails_withoutAIUser <- company_allUsers_emails[
    !grepl("-ai@", unlist(company_allUsers_emails))
  ]
  
  emailAccounts_toBeDeleted <- unlist(intersect(unlist(possible_emails_acountsToBeDeleted_list), unlist(company_allUsers_emails_withoutAIUser)))
  
  # is current user email (with admin role) also availabel in emailAccounts_toBeDeleted
  currentAdminAccount_inAccounts_toBeDeleted = email %in% emailAccounts_toBeDeleted
  
  users_count = length(company_allUsers_emails_withoutAIUser)
  
  # if users_count == 1: only 1 (real) user belong to the company
  if (users_count == 1 && currentAdminAccount_inAccounts_toBeDeleted) {
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
  
  #--- start to delete user account with email address 'email'
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/userDelete")
  
  for (emailAccount in emailAccounts_toBeDeleted) {

  # response <- httr::POST(
  #   url = endpoint,
  #   encode = "json",
  #   body = list(email = email, emailAccount = emailAccount, users_count = users_count, company_id = company_id)
  # )
  # 
  #  For security reasons, i.e., prevent company_id source code manipulation on client side:
  #  the admin company_id and for each email account the company_id will be determined on the backend
  #  => Only if they are identical the user email account will be deleted on the backend
  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(email = email, emailAccount = emailAccount, users_count = users_count)
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
}
