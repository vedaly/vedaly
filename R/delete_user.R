#!/usr/bin/env R

# Copyright © 2026 Vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' Sign into Vedaly from R
#'
#' @param email_account_to_be_deleted email accounts (as list) which shall be deleted
#' 
#' @return Invisibly returns `TRUE` if request was successful.
#' @export
delete_user <- function(email_account_to_be_deleted) {
  
  gql_api_url = "https://graphql-dev.omicschart.com/v1/graphql"
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
  
  # current user email address
  email <- auth_config$email
  email_admin <- email
  email_as_list <- list(email = email)
  
  # get the company id of the current user
  get_user_company_id_query <- "
    query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          company_id
        }
      }
    }
  "
  
  response_company_id <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_user_company_id_query,
      variables = email_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  
  result_company_id <- httr::content(response_company_id, as = "parsed", encoding = "UTF-8")
  
  company_id <- result_company_id$data$preon_op$users[[1]]$company_id
  company_id_as_list = list(company_id = company_id)
  
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
  
  response_all_users_emails <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_users_emails_query,
      variables = company_id_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_all_users_emails <- httr::content(response_all_users_emails, as = "parsed", encoding = "UTF-8")
  
  company_all_users_emails <- lapply(result_all_users_emails$data$preon_op$users, function(x) x$email)
  
  #---
  
  # get the company_roles of the current user with the email address 'email'
  get_current_user_company_roles_query <- "
   query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          company_roles
        }
      }
    }
  "
  
  response_current_user_company_roles <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_current_user_company_roles_query,
      variables = email_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_current_user_company_roles <- httr::content(response_current_user_company_roles, as = "parsed", encoding = "UTF-8")
  
  company_current_user_roles <- result_current_user_company_roles$data$preon_op$users[[1]]$company_roles

  hasAdminRole <- any(sapply(company_current_user_roles, function(x) x == "admin"))

  if (hasAdminRole == FALSE) {
    message("You don't have the permission to delete user accounts.")
    message("Please contact admin in your organization.")
    stop("Deletion of user account has been interrupted.")
  }
  
  #---
  
  # get all users and their roles
  
  get_all_users_roles_company_id_query <- "
    query myQuery($company_id: Int!) {
      preon_op {
        users(where: {company_id: {_eq: $company_id}}) {
          id
          company_roles
        }
      }
    }
  "
  
  response_all_users_roles <- httr::POST(
    url = gql_api_url,
    encode = "json",
    body = list(
      query = get_all_users_roles_company_id_query,
      variables = list(company_id = company_id)
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_all_users_roles <- httr::content(response_all_users_roles, as = "parsed")
  
  
  all_users_roles <- do.call(
    rbind,
    lapply(
      result_all_users_roles$data$preon_op$users,
      function(user) {
        
        roles <- if (is.null(user$company_roles)) {
          character(0)
        } else {
          unlist(user$company_roles)
        }
        
        data.frame(
          id = user$id,
          company_roles = paste(roles, collapse = ","),
          stringsAsFactors = FALSE
        )
      }
    )
  )
  
  
  roles <- all_users_roles$company_roles
  
  # how often does the pattern "admin" occurs in roles
  admin_counts <- sum(grepl("admin", roles))
  
  # determing now the counts of user email addresses belonging to the company
  # without ai
  company_all_users_emails_without_ai_user <- company_all_users_emails[
    !grepl("-ai@", unlist(company_all_users_emails))
  ]
  
  email_account_to_be_deleted <- unlist(intersect(unlist(email_account_to_be_deleted), unlist(company_all_users_emails_without_ai_user)))
  
  if (length(email_account_to_be_deleted) == 0) {
    message("")
    message("No user account will be deleted.")
    message("")

    stop("Deleting user account terminated.")
  }
  else {
    message("")
    message("The following user account will be deteled:")
    message(email_account_to_be_deleted)
   
    answer <- tolower(
      readline("Do you want to continue to delete this account? (yes/no): ")
    )
   
    while (!answer %in% c("yes", "no")) {
      answer <- tolower(
        readline("Please enter yes or no: ")
      )
    }

    if (answer == "no") {
      stop("The deletion of user account is stopped/interrupted.")
    }
  }
  
  
  # is current user email (with admin role) equals the email_account_to_be_deleted
  current_admin_account_in_account_to_be_deleted = email %in% email_account_to_be_deleted
  
  users_count = length(company_all_users_emails_without_ai_user)
  
  user_getting_admin_privileges <- "notInUse"

  # if users_count == 1: only 1 (real) user belong to the company, and it must have admin privileges
  if (users_count == 1 && current_admin_account_in_account_to_be_deleted) {
    
    message("You're the only user of your company (and with admin privilges.")
    message("If you continue not only your user data, but")
    message("all data of your company will be deleted.")
    
    answer <- tolower(
      readline("Do you want to continue to delete your account? (yes/no): ")
    )
    while (!answer %in% c("yes", "no")) {
      answer <- tolower(
        readline("Please enter yes or no: ")
      )
      answer <- trimws(answer)
    }
    
    if (answer == "yes") {
      
      cat("Account deletion started...\n")
      cat("\n")
      
    } else {
      
      stop("Account deletion aborted")
      
    }
  } else if (users_count > 1 && current_admin_account_in_account_to_be_deleted && admin_counts == 1) {

    combined_information <- FALSE
    
    while (!combined_information) {
    
      message("The 2 following conditions must be fullfilled:")
      message("1.) Please assign the admin privileges to an already existing user of your company.")
      message("2.) This user account must not be equal the email account to be deleted.")
      
      user_getting_admin_privileges <- readline("Which user shall take over the admin privileges?:")
      user_getting_admin_privileges <- trimws(user_getting_admin_privileges)
    
      # check if assigned user_getting_admin_privileges exists already and is not one of the users
      # who shall be deleted
    
      is_new_admin_user_in_existing_user_accounts <- user_getting_admin_privileges %in% company_all_users_emails_without_ai_user
    
      is_new_admin_user_not_user_account_to_be_deleted <- (!user_getting_admin_privileges %in% email_account_to_be_deleted)
    
      combined_information = is_new_admin_user_in_existing_user_accounts && is_new_admin_user_not_user_account_to_be_deleted
    }
  } else if (users_count > 1 && current_admin_account_in_account_to_be_deleted && admin_counts > 1) {
    
    # nothing to be done: when the current user with admin privileges is deleted, there is at least still one
    # user available who has admin privileges
    
  } else if (users_count > 1 && !(current_admin_account_in_account_to_be_deleted) ) {
    
    # nothing to be done: the current user with admin privileges is not deleted
  }

  #--- start to delete user account with email address 'email'
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  endpoint <- paste0(api_url, "/userDelete")
  
  #  For security reasons, i.e., prevent company_id source code manipulation on client side:
  #  the admin company_id and for each email account the company_id will be determined on the backend
  #  => Only if they are identical the user email account will be deleted on the backend
  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(email = email, email_account = email_account_to_be_deleted, users_count = users_count, user_getting_admin_privileges = user_getting_admin_privileges)
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
