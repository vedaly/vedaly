#!/usr/bin/env R

# Copyright © 2026 vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' UNDER DEVELOPMENT Share HTML and PDF outputs of rmd to Vedaly
#'
#' @param rmd_file Path to .rmd rmarkdown document == directory containing exactl one PDF and one HTML report
#' @param prefix_s3 Parent directory of the s3 bucket
#' @param project Project name where the report should be shared to. By default is it shared in active project (if set) or in personal workspace
#' @param title Report title. Used to display report in the list of reports. By default title from the header is used
#'
#' @return UUID of the report
#' @export
share_rmd_report <- function(rmd_file, prefix_s3, project = "My Workspace", title = NULL) {
  
  # API emdpoint
  api_url <- getOption(
    "vedaly.api_url",
    default = "https://api.omicschart.com"
  )
  
  vedaly.gql_api_url <- getOption(
    "vedaly.gql_api_url",
    default = "https://graphql-prod.omicschart.com/v1/graphql"
  )
  
  # remove possible trailing white space
  api_url <- sub("/+$", "", api_url)
  prefix_s3 <- sub("/+$", "", prefix_s3)
  
  # validate the S3 bucket
  if (!grepl("^s3://[^/]+$", prefix_s3)) {
    stop("prefix_s3 must be an S3 bucket root, e.g. s3://preon-dev.iotk")
  }
  
  # validate the report directory
  if (length(rmd_file) != 1 || !dir.exists(rmd_file)) {
    stop("rmd_file must point to an existing directory.")
  }
  
  # read the current login session
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))
  
  # check if the email address and the necessary tokens are available
  if (is.null(auth_config$email) ||
      is.null(auth_config$id_token) ||
      is.null(auth_config$access_token)) {
    stop("The saved login session is missing required authentication data.")
  }

  # user who is currently logged in
  email <- auth_config$email
  email_as_list <- list(email = email)
  
  
  # -----------------------------
  # helper function: send a GraphQL query and validate its response
  # -----------------------------
  
  graphql_post <- function(query, variables) {
    
    response <- httr::POST(
      url = vedaly.gql_api_url,
      encode = "json",
      body = list(
        query = query,
        variables = variables
      ),
      httr::add_headers(
        Authorization = paste("Bearer", auth_config$id_token),
        `Content-Type` = "application/json"
      ),
      httr::timeout(60)
    )
    
    if (httr::http_error(response)) {
      stop(
        "GraphQL HTTP request failed: ",
        httr::content(response, as = "text", encoding = "UTF-8")
      )
    }
    
    result <- httr::content(
      response,
      as = "parsed",
      encoding = "UTF-8"
    )
    
    if (!is.null(result$errors)) {
      stop("GraphQL error: ", result$errors[[1]]$message)
    }
    
    # return value
    return(result)
  }
  
  # -- end of helper function
  
  #----------
  # company id and user id
  # -------
  
  # retrieve the company id and user id of the current user
  get_user_info_query <- "
    query myQuery($email: String!) {
      preon_op {
        users(where: {email: {_eq: $email}}) {
          company_id
          id
        }
      }
    }
  "
  
  result_user_info <- graphql_post(
    query = get_user_info_query,
    variables = list(email = auth_config$email)
  )

  users <- result_user_info$data$preon_op$users
  
  if (is.null(users) || length(users) != 1 ||
      is.null(users[[1]]$company_id) || is.null(users[[1]]$id)) {
    stop("Could not identify exactly one user with a company id.")
  }
  
  company_id <- users[[1]]$company_id
  user_id <- users[[1]]$id
  
  #-----------
  # project id
  #-----------
  
  # retrieve the project id of the current project / the project under consideration
  # its user_id and company_id must be given as well:
  # other users of the same company can own projects too, and 
  # project names in different companies might have the same name
  get_project_info_query <- "
    query myQuery(
      $name: String!,
      $company_id: Int!,
      $user_id: Int!
    ) {
      preon_op {
        company_projects(
          where: {
            name: {_eq: $name},
            company_id: {_eq: $company_id},
            user_id: {_eq: $user_id}
          }
        ) {
          id
        }
      }
    }
  "
  
  result_project_info <- graphql_post(
    query = get_project_info_query,
    variables = list(
      name = project,
      company_id = as.integer(company_id),
      user_id = as.integer(user_id)
    )
  )
  
  projects <- result_project_info$data$preon_op$company_projects
  
  if (is.null(projects) || length(projects) != 1 ||
      is.null(projects[[1]]$id)) {
    stop(
      "Could not identify exactly one project matching the project name, ",
      "company id, and user id."
    )
  }
  
  project_id <- projects[[1]]$id
  
  #-----------
  # report files' location
  #-----------
  
  # locate the report PDF and one HTML file
  report_files <- list.files(
    path = rmd_file,
    pattern = "\\.(pdf|html)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  pdf_files <- report_files[
    grepl("\\.pdf$", report_files, ignore.case = TRUE)
  ]
  
  html_files <- report_files[
    grepl("\\.html$", report_files, ignore.case = TRUE)
  ]
  
  # check if there is exactly one PDF and one HTML report file
  if (length(pdf_files) != 1) {
    stop(
      "Expected exactly one PDF report file, but found ",
      length(pdf_files),
      "."
    )
  }
  
  if (length(html_files) != 1) {
    stop(
      "Expected exactly one HTML report file, but found ",
      length(html_files),
      "."
    )
  }
  
  pdf_filename <- pdf_files[[1]]
  html_filename <- html_files[[1]]
  
  pdf_filename <- normalizePath(pdf_filename, mustWork = TRUE)
  html_filename <- normalizePath(html_filename, mustWork = TRUE)
  
  pdf_basename <- basename(pdf_filename)
  html_basename <- basename(html_filename)
  
  #---------------
  # send both files to Plumber. The backend, not this client function,
  # performs the AWS S3 uploads.
  #----------------
  
  endpoint <- paste0(api_url, "/shareReportToPreon")
  
  message("Uploading report files to the Vedaly backend...")
  message("PDF:  ", pdf_basename)
  message("HTML: ", html_basename)
  message("Project: ", project)
  
  
  request_body <- list(
    pdf_report = httr::upload_file(
      pdf_filename, type = "application/octet-stream"
    ),
    html_report = httr::upload_file(
      html_filename, type = "application/octet-stream"
    ),
    company_id = as.character(company_id),
    user_id = as.character(user_id),
    project_id = as.character(project_id),
    pdf_basename = pdf_basename,
    html_basename = html_basename,
    s3_bucket = sub("/+$", "", prefix_s3)
  )
  
  # ------------------
  #  report files upload
  # ------------------
  
  message("Uploading and sharing reports")
  
  response <- httr::POST(
    url = endpoint,
    body = request_body,
    encode = "multipart",
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$access_token)
    ),
    httr::timeout(600)
  )
  
  # ------------------------------------------------------------
  # read backend response as raw data
  # ------------------------------------------------------------
  
  response_raw <- httr::content(
    response,
    as = "raw"
  )
  
  response_text <- if (length(response_raw) > 0) {
    rawToChar(response_raw)
  } else {
    ""
  }
  
  # ------------------------------------------------------------
  # check HTTP status
  # ------------------------------------------------------------
  
  if (httr::http_error(response)) {
    
    backend_message <- if (nzchar(response_text)) {
      response_text
    } else {
      "The backend returned an empty response body."
    }
    
    error_content <- tryCatch(
      jsonlite::fromJSON(
        response_text,
        simplifyVector = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.list(error_content) &&
        !is.null(error_content$message)) {
      backend_message <- error_content$message
    }
    
    stop(
      "Report sharing failed (HTTP ",
      response$status_code,
      "): ",
      backend_message
    )
  }
  
  # ------------------------------------------------------------
  # check whether the backend returned a response body
  # ------------------------------------------------------------
  
  if (!nzchar(response_text)) {
    stop(
      "The backend returned an empty response body (HTTP ",
      response$status_code,
      ")."
    )
  }
  
  # ------------------------------------------------------------
  # parse JSON response
  # ------------------------------------------------------------
  
  content <- NULL
  
  content <- tryCatch(
    jsonlite::fromJSON(
      response_text,
      simplifyVector = FALSE
    ),
    error = function(e) NULL
  )
  
  #----------------------------------------------------------------
  # validate JSON response
  #-----------------------------------------------------------------
  
  if (is.null(content) || !is.list(content)) {
    
    if (httr::http_error(response)) {
      stop(
        "Report sharing failed (HTTP ",
        response$status_code,
        "): ",
        if (nzchar(response_text)) {
          response_text
        } else {
          "The backend returned an empty response body."
        }
      )
    }
    
    stop(
      "The backend returned an invalid JSON response: ",
      response_text
    )
  }
  
  # ------------------------------------------------------------
  # check explicit success flag
  # ------------------------------------------------------------
  
  if (!isTRUE(content$success)) {
    
    backend_message <- "Report sharing failed."
    
    if (!is.null(content$message)) {
      backend_message <- as.character(content$message)
    } else if (!is.null(content$error)) {
      backend_message <- as.character(content$error)
    }
    
    else if (!is.null(content$error)) {
      backend_message <- as.character(content$error)
    }
    
    stop(
      "Report sharing failed (HTTP ",
      response$status_code,
      "): ",
      return(backend_message)
    )
    
  }
  
  # ------------------------------------------------------------
  # validate and return report UUID
  # ------------------------------------------------------------
  
  if (is.null(content$uuid) ||
      length(content$uuid) != 1 ||
      is.na(content$uuid) ||
      !nzchar(as.character(content$uuid))) {
    stop(
      "The backend reported success, but did not return ",
      "a valid report UUID."
    )
  }
  
  report_uuid <- as.character(content$uuid)
  
  #-----------------------------------------
  # handle inconsistent HTTP status
  #------------------------------------------
  
  if (httr::http_error(response)) {
    
    warning(
      "The backend returned HTTP ",
      response$status_code,
      " but also reported success and returned a valid UUID. ",
      "The report upload appears to have succeeded; ",
      "the HTTP status should be investigated on the backend."
    )
    
  }
  
  #--------------------------------
  # success
  #-----------------
  
  message(
    "Report shared successfully. UUID: ",
    report_uuid
  )
  
  return(report_uuid)
}
 