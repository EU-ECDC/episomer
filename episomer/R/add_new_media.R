
#' internal function for exporting code of necessary functions for creating templates for a new social media
#' @param file_content char, lines of the script to extract content from
keep_roxygen_and_function_declarations <- function(file_content) {
  text_to_keep <- character(0)
  function_body <- FALSE

  for (l in 1:length(file_content)) {
    l_clean <- trimws(file_content[l])
    is_function_start <- grepl("@function_def_start", l_clean)
    is_function_end <- grepl("@function_def_end", l_clean)

    if (function_body & !is_function_start &!is_function_end) {
      text_to_keep <- c(text_to_keep, l_clean)
    }

    if (is_function_start) {
      function_body <- TRUE
      text_to_keep <- c(text_to_keep, l_clean)
    }
    if (is_function_end) {
      function_body <- FALSE
      text_to_keep <- c(text_to_keep, l_clean)
      text_to_keep <- c(text_to_keep, "}\n")
    }    
  }
  return(text_to_keep)
}

#' internal function for producing the code template for a new social media
#' @param new_media char, name of the social media to add
#' @param ref_media char, name of the social media to use as a reference to produce template
#' @importFrom utils browseURL
create_api_and_plan_files_for_new_social_media <- function(
  new_media,
  ref_media = "bluesky"
) {
  is_r_folder_present()

  api_file <- paste0("R/sm-", ref_media, "-api.R")
  api_file_content <- readLines(api_file)
  api_file_cleaned <- keep_roxygen_and_function_declarations(
    api_file_content
  )
  api_file_cleaned <- gsub(ref_media, new_media, api_file_cleaned)
  writeLines(api_file_cleaned, paste0("R/sm-", new_media, "-api.R"))
  cli::cli_alert_success(paste0(
    "R file for api functions for ",
    new_media,
    " created - please look at the file ",
    paste0("R/sm-", new_media, "-api.R")
  ))

  plan_file <- paste0("R/sm-", ref_media, "-plan.R")
  plan_file_content <- readLines(plan_file)
  plan_file_cleaned <- keep_roxygen_and_function_declarations(
    plan_file_content
  )
  plan_file_cleaned <- gsub(ref_media, new_media, plan_file_cleaned)
  writeLines(plan_file_cleaned, paste0("R/sm-", new_media, "-plan.R"))
  cli::cli_alert_success(paste0(
    "R file for plan functions for ",
    new_media,
    " created - please look at the file ",
    paste0("R/sm-", new_media, "-plan.R")
  ))
  cli::cli_alert_info("To keep the documentation up to date, please run devtools::document()")

  return(invisible(TRUE))
}

#' Create new social media template files, so a developer can extend episomer and support a new social media.
#' this function has to be called within episomer subfolder containing thr R subfolder after executing devtools::load_all()
#' @param social_media Name of the new social media
#' @param social_media_ref Name of the reference social media 
#' @export
add_new_social_media <- function(
  social_media,
  social_media_ref = "bluesky"
) {
  create_api_and_plan_files_for_new_social_media(social_media, social_media_ref)
  cli::cli_alert_success("Added new social media files in the R/ folder successfully")
  cli::cli_alert_info("For next steps, please look at the vignette vignettes/add_new_social_media.Rmds")
  return(invisible(TRUE))
}

#' check whether the R folder is present in the current directory
is_r_folder_present <- function() {
  R_files <- list.files("R")
  if (length(R_files) == 0) {
    stop(
      "No R files found in the R directory - you should probably run devtools::load_all() first and set the current directory to the episomer folder (with the R subfolder)"
    )
  }
  return(TRUE)
}
