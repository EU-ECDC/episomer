test_that("keep_roxygen_and_function_declarations works", {
  function_body <-
    "
  # @function_def_start (do not delete)

#' Whether this social media is made of independent instances
#' @details Bluesky is served by a single API endpoint, so it is collected as one network.
#' @return FALSE
#' @export
sm_is_federated_bluesky <- function() {
# @function_def_end (do not delete)
  I should be deleted
}"
  temp_r_file <- tempfile()
  file.create(temp_r_file)
  writeLines(function_body, temp_r_file)

  content <- readLines(temp_r_file)

  function_without_content <- keep_roxygen_and_function_declarations(content) |>
    paste(collapse = " ")

  expect_false(
    grepl("I should be deleted", function_without_content)
  )
})


withr::with_tempdir({
  test_that("is_r_folder_present works", {
    dir.create(
      file.path(getwd(), "R/")
    )

    expect_error(
      is_r_folder_present(),
      "No R files found in the R directory"
    )

    file.create(
      file.path(getwd(), "R/", "toto.R")
    )

    expect_no_error(
      is_r_folder_present()
    )
  })
})


withr::with_tempdir({
  test_that("create_api_and_plan_files_for_new_social_media creates new files", {
    dir.create("R")

    # Create reference files with function declarations
    api_content <- c(
      "#' API function for bluesky",
      "# @function_def_start (do not delete)",
      "#' sm_get_bluesky",
      "sm_get_bluesky <- function() {",
      "  TRUE",
      "}",
      "# @function_def_end (do not delete)"
    )

    plan_content <- c(
      "#' Plan function for bluesky",
      "# @function_def_start (do not delete)",
      "#' sm_plan_bluesky",
      "sm_plan_bluesky <- function() {",
      "  TRUE",
      "}",
      "# @function_def_end (do not delete)"
    )

    writeLines(api_content, "R/sm-bluesky-api.R")
    writeLines(plan_content, "R/sm-bluesky-plan.R")

    expect_no_error(
      create_api_and_plan_files_for_new_social_media("mastodon")
    )

    expect_true(file.exists("R/sm-mastodon-api.R"))
    expect_true(file.exists("R/sm-mastodon-plan.R"))
  })
})


withr::with_tempdir({
  test_that("create_api_and_plan_files_for_new_social_media replaces media name correctly", {
    dir.create("R")

    api_content <- c(
      "# @function_def_start (do not delete)",
      "#' Get data from bluesky",
      "sm_get_bluesky <- function() {",
      "  # bluesky specific code",
      "  url <- 'https://bluesky.social'",
      "}",
      "# @function_def_end (do not delete)"
    )

    writeLines(api_content, "R/sm-bluesky-api.R")
    writeLines(api_content, "R/sm-bluesky-plan.R")

    create_api_and_plan_files_for_new_social_media("threads")

    new_api_content <- readLines("R/sm-threads-api.R")
    expect_true(any(grepl("sm_get_threads", new_api_content)))
    expect_false(any(grepl("sm_get_bluesky", new_api_content)))
  })
})


withr::with_tempdir({
  test_that("create_api_and_plan_files_for_new_social_media with custom reference", {
    dir.create("R")

    content <- c(
      "# @function_def_start (do not delete)",
      "#' Get data from twitter",
      "sm_get_twitter <- function() {}",
      "# @function_def_end (do not delete)"
    )

    writeLines(content, "R/sm-twitter-api.R")
    writeLines(content, "R/sm-twitter-plan.R")

    expect_no_error(
      create_api_and_plan_files_for_new_social_media("x", ref_media = "twitter")
    )

    expect_true(file.exists("R/sm-x-api.R"))
    expect_true(file.exists("R/sm-x-plan.R"))

    x_api_content <- readLines("R/sm-x-api.R")
    expect_true(any(grepl("sm_get_x", x_api_content)))
  })
})
