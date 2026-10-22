#!/usr/bin/env R

# Copyright © 2026 vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' UNDER DEVELOPMENT Share HTML and PDF outputs of rmd to Vedaly
#'
#' @param rmd_file path to .rmd rmarkdown document
#' @param project Project name where the report should be shared to. By default is it shared in active project (if set) or in personal workspace
#' @param title Report title. Used to display report in the list of reports. By default title from the header is used
#'
#' @return UUID of the report
#' @export
share_rmd_report <- function(rmd_file, project = "My Workspace", title = NULL) {

  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  
  vedaly.gql_api_url <- getOption(
    "vedaly.gql_api_url",
    default = "https://graphql-prod.omicschart.com/v1/graphql"
  )
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))

  # user who is currently logged in
  email <- auth_config$email
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
    url = vedaly.gql_api_url,
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
  
  # what is the current user id
  # get the company id of the current user
  get_user_id_query <- "
    query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          id
        }
      }
    }
  "
  
  response_user_id <- httr::POST(
    url = vedaly.gql_api_url,
    encode = "json",
    body = list(
      query = get_user_id_query,
      variables = email_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_user_id <- httr::content(response_user_id, as = "parsed", encoding = "UTF-8")
  user_id <- result_user_id$data$preon_op$users[[1]]$id
  

  stop("halt: below still bugs")
  
  # debugging
  # project_id and project_user_id under construction
  
  # get the project id and the user_id of the project
  # (== who is the owner of the project)
  get_project_id_and_project_user_id_query <- "
    query myQuery($projectl: String!) {
      preon_op {
        users(where: {name: {_eq: $project}}) {
          id
          user_id
        }
      }
    }
  "
  
  project_as_list <- list(project = project)
  
  response_project_id_and_project_user_id <- httr::POST(
    url = vedaly.gql_api_url,
    encode = "json",
    body = list(
      query = get_project_id_and_project_user_id_query,
      variables = project_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  result_project_id_and_project_user_id <- httr::content(response_user_id, as = "parsed", encoding = "UTF-8")
  project_id <- result_project_id_and_project_user_id$data$preon_op$company_projects[[1]]$id
  project_user_id <- result_project_id_and_project_user_id$data$preon_op$company_projects[[1]]$user_id
  
  
  message()
  message("project_id")
  print(project_id)
  message()
  
  stop("halt")
  
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  
  
  endpoint <- paste0(api_url, "/shareReportToPreon")
  
  message()
  message(rmd_file)
  message(project)
  message()
  
  stop("halten_1")

  response <- httr::POST(
    url = endpoint,
    encode = "json",
    body = list(
      pdf_report = ,
      html_report = ,
      ...
    ),
    httr::add_headers(Authorization = paste("Bearer", auth_config$access_token))
  )

  if (httr::http_error(response)) {
    msg <- tryCatch({
      httr::content(response, as = "text", encoding = "UTF-8")
    }, error = function(e) {
      response$status_code
    })
    stop("Report sharing failed: ", msg)
  }

  content <- jsonlite::fromJSON(httr::content(response))
  if (!content$success) stop("Report sharing failed. Try again later.")

  message("Report shared successfully. ", content$message)
  return(content$uuid)
}
