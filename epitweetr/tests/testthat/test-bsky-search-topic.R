unlink("bsky_session.rds")

conf <- list()
conf$data_dir <- tempdir()

test_that("bsky_search_topic works", {
  skip_on_ci()

  plan <- list(
    research_max_date = "2025-07-15T12:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    research_min_date = "2025-07-10T12:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    requests = 0
  )

  has_more <- TRUE
  expect_no_error(
    while (has_more) {
      plan <- bsky_search_topic(
        plan,
        "covid19",
        "covid19",
        output_in_scala = FALSE,
        conf
      )
      has_more <- plan$has_more
    }
  )
  expect_equal(plan$requests, 4)

  all_files <- list.files(
    conf$data_dir,
    full.names = TRUE,
    recursive = TRUE
  )
  json_files <- all_files[grepl("^\\d{4}.*\\.json$", basename(all_files))]

  expect_equal(length(json_files), 1)

  json_content <- jsonlite::read_json(json_files[1])
  expect_equal(
    lubridate::as_datetime(json_content[[1]]$created_from, tz = "UTC"),
    plan$boundaries_date_min,
    tolerance = 1
  )
  expect_equal(
    lubridate::as_datetime(json_content[[1]]$created_to, tz = "UTC"),
    plan$boundaries_date_max,
    tolerance = 1
  )
  expect_equal(json_content[[1]]$topic, "covid19")
  unlink(json_files, recursive = TRUE)
})

unlink("bsky_session.rds")


conf <- list()
conf$data_dir <- tempdir()

test_that("bsky_search_topic works when a previous search has been performed", {
  skip_on_ci()

  plan <- list(
    research_max_date = "2025-07-15T12:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    research_min_date = "2025-07-10T12:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    boundaries_date_max = "2025-07-13T12:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    requests = 0
  )

  has_more <- TRUE
  expect_no_error(
    while (has_more) {
      plan <- bsky_search_topic(
        plan,
        "covid19",
        "covid19",
        output_in_scala = FALSE,
        conf
      )
      has_more <- plan$has_more
      if (has_more) {
        expect_equal(plan$research_min_date, plan$boundaries_date_max)
      }
    }
  )
  all_files <- list.files(
    conf$data_dir,
    full.names = TRUE,
    recursive = TRUE
  )
  json_files <- all_files[grepl("^\\d{4}.*\\.json$", basename(all_files))]

  unlink(json_files, recursive = TRUE)
})

unlink("bsky_session.rds")


conf <- list()
conf$data_dir <- tempdir()

test_that("bsky_search_topic works with a fake topic", {
  skip_on_ci()

  plan <- list(
    research_max_date = "2025-07-15T00:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    research_min_date = "2025-07-10T00:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    requests = 0
  )

  has_more <- TRUE
  random_topic <- paste0(
    sample(LETTERS, 100, replace = TRUE),
    collapse = ""
  )
  expect_no_error(
    while (has_more) {
      plan <- bsky_search_topic(
        plan,
        random_topic,
        random_topic,
        output_in_scala = FALSE,
        conf
      )
      has_more <- plan$has_more
    }
  )
  expect_equal(plan$requests, 0)

  all_files <- list.files(
    conf$data_dir,
    full.names = TRUE,
    recursive = TRUE
  )
  json_files <- all_files[grepl("^\\d{4}.*\\.json$", basename(all_files))]

  expect_equal(length(json_files), 0)
  unlink(json_files, recursive = TRUE)
})


unlink("bsky_session.rds")
