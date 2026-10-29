#!/usr/bin/env R

# Copyright © 2026 vedaly Ltd <info@vedaly.io>
# Distributed under terms of the MIT license.

#' UNDER DEVELOPMENT Share HTML and PDF outputs of rmd to Vedaly
#'
#' @param rmd_file path to .rmd rmarkdown document
#' @param project Project name where the report should be shared to. By default is it shared in active project (if set) or in personal workspace
#' @param title Report title. Used to display report in the list of reports. By default title from the header is used
#' @param prefix_s3  parent directory of the s3 bucket
#'
#' @return UUID of the report
#' @export
share_rmd_report <- function(rmd_file, prefix_s3, project = "My Workspace", title = NULL) {
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  
  vedaly.gql_api_url <- getOption(
    "vedaly.gql_api_url",
    default = "https://graphql-prod.omicschart.com/v1/graphql"
  )
  
  auth_config = readRDS(file.path(tools::R_user_dir("vedaly", "config"), "session.rds"))

  # user who is currently logged in
  email <- auth_config$email
  email_as_list <- list(email = email)
  
  
  #----
  
  # get the company id and user id of the current user
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
  
  # send the GraphQL request for the user_info
  response_user_info <- httr::POST(
    url = vedaly.gql_api_url,
    encode = "json",
    body = list(
      query = get_user_info_query,
      variables = email_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )  
  
  # extract the user_info's response
  result_user_info <- httr::content(
    response_user_info,
    as = "parsed",
    encoding = "UTF-8"
  )
  
  # get the company id of the current user
  company_id <- result_user_info$data$preon_op$users[[1]]$company_id
  company_id_as_list <- list(company_id = company_id)
  
  # get the user id of the current user
  user_id <- result_user_info$data$preon_op$users[[1]]$id

  
  # get project id of the current project / the project under consideration
  # its user_id and company_id must be given as well:
  # other user of the same company can own projects too, and 
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

  # prepare the project name and company ID as named GraphQL variables
  project_name_and_company_id_user_id_as_list <- list(
    name = project,
    company_id = company_id,
    user_id = user_id
  )

  # send the GraphQL request for the project_info
  response_project_info <- httr::POST(
    url = vedaly.gql_api_url,
    encode = "json",
    body = list(
      query = get_project_info_query,
      variables = project_name_and_company_id_user_id_as_list
    ),
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$id_token),
      `Content-Type` = "application/json"
    )
  )
  
  # extract the project_info's response
  result_project_info <- httr::content(
    response_project_info,
    as = "parsed",
    encoding = "UTF-8"
  )
  
  # check whether a matching project was found
  if (
    is.null(result_project_info$data$preon_op$company_projects) ||
    length(result_project_info$data$preon_op$company_projects) == 0 ||
    is.null(result_project_info$data$preon_op$company_projects[[1]]$id)
  ) {
    stop(
      "Error: No project found matching the project name, company id, and user id."
    )
  }

  # extract the project id
  project_id <- result_project_info$data$preon_op$company_projects[[1]]$id
  
  # now the the report file base names (pdf and html) 
  # get all pdf and html files in the given directory "rmd_file"
  
  # get all pdf and html files in the given directory
  report_files <- list.files(
    path = rmd_file,
    pattern = "\\.(pdf|html)$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # get the full qualified html filename
  html_filename <- report_files[
    grepl("\\.html$", report_files, ignore.case = TRUE)
  ]
  
  # get the full qualified pdf filename
  pdf_filename <- report_files[
    grepl("\\.pdf$", report_files, ignore.case = TRUE)
  ]
  
  # check if there is exactly one pdf and one html file
  if (length(pdf_filename) != 1 || length(html_filename) != 1) {
    stop(
      "Error: The directory must contain exactly one pdf report file and exactly one html report file."
    )
  }
  
  # get basenames of pdf and html:
  pdf_basename <- basename(pdf_filename)
  html_basename <- basename(html_filename)
  
  # relative destinations s3 bucket:
  relative_s3 <- paste(company_id, user_id, "reports", sep="/")
  
  message()
  message("relatvive_s3")
  print(relative_s3)
  
  pdf_report <- paste(prefix_s3, relative_s3, pdf_basename, sep="/")
  html_report <- paste(prefix_s3, relative_s3, html_basename, sep="/")
  
  
  # # s3 bucket destination
  # 
  # # prefix s3:
  # prefix_s3 <- "s3://preon-dev.iotk/"
  # 
  # print(prefix_s3)
  # print(pdf_report)
  # print(html_report)
  # 
  # stop("halten_2")
  
  ###############
  
  ###############
  
  

  ###########
  ###########
  
  # stop("halt")
  
  message()
  print("hallo")
  message()
  
  
  api_url <- getOption("vedaly.api_url", default = "https://api.omicschart.com")
  
  
  endpoint <- paste0(api_url, "/shareReportToPreon")
  
  message()
  message(rmd_file)
  message(project)
  message()
  
  message("verlasse frontend")
  
  message()
  
  # stop("halten_1")

  # response <- httr::POST(
  #   url = endpoint,
  #   encode = "json",
  #   body = list(
  #     pdf_report_source = pdf_filename_source,
  #     htmtl_report_source = html_filename_source
  #     html_report = ,
  #     ...
  #   ),
  #   httr::add_headers(Authorization = paste("Bearer", auth_config$access_token))
  # )
  
  response <- httr::POST(
    url = paste0(endpoint),
    body = list(
      pdf_report = httr::upload_file(pdf_filename),
      html_report = httr::upload_file(html_filename),
      company_id = as.character(company_id),
      user_id = as.character(user_id),
      project_id = as.character(project_id),
      pdf_basename = pdf_basename,
      html_basename = html_basename
    ),
    encode = "multipart",
    httr::add_headers(
      Authorization = paste("Bearer", auth_config$access_token)
    )
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

