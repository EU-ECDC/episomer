test_that("bsky_set_date_boundaries works", {
  plan <- list()

  boundaries <- bsky_set_date_boundaries(plan)
  plan_new <- boundaries$plan
  max_text <- boundaries$max_text
  min_text <- boundaries$min_text

  expect_equal(
    as.Date(plan_new$research_max_date),
    as.Date(Sys.time())
  )

  expect_true(is.null(plan$research_min_date))
  expect_equal(max_text, "last message")
  expect_equal(min_text, "first message")
})

test_that("bsky_set_date_boundaries works with a plan updated by a previous search in a given query", {
  # We have a plan updated by a previous search in a given query
  plan <- list(
    boundaries_date_min = lubridate::as_datetime(
      "2025-07-14T00:00:00",
      tz = "UTC"
    ),
    boundaries_date_max = lubridate::as_datetime(
      "2025-07-15T00:00:00",
      tz = "UTC"
    )
  )

  boundaries <- bsky_set_date_boundaries(plan)
  plan_new <- boundaries$plan

  expect_equal(
    plan_new$research_min_date,
    plan$boundaries_date_max
  )

  expect_equal(boundaries$max_text, "last message")
  expect_equal(boundaries$min_text, plan$boundaries_date_max)
})

test_that("bsky_set_date_boundaries fails when we want to retrieve data that we already have", {
  # We have a plan updated by a previous search in a given query
  plan <- list(
    boundaries_date_min = lubridate::as_datetime(
      "2025-07-14T00:00:00",
      tz = "UTC"
    ),
    boundaries_date_max = lubridate::as_datetime(
      "2025-07-15T00:00:00",
      tz = "UTC"
    ),
    research_max_date = lubridate::as_datetime(
      "2025-07-10T00:00:00",
      tz = "UTC"
    )
  )

  expect_error(
    bsky_set_date_boundaries(plan),
    "We already have all the data we need"
  )
})

test_that("bsky_update_plan works", {
  # We mimic a full search
  # Query 1: we have a full research to perform

  plan_initial <- list(
    boundaries_date_min = NULL,
    boundaries_date_max = NULL,
    research_max_date = "2025-07-14T12:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    research_min_date = NULL,
    requests = 0
  )

  ## We are retrieving 100 messages
  content <- list(
    newest_message_in_a_query = lubridate::as_datetime(
      "2025-07-13T23:00:00",
      tz = "UTC"
    ),
    oldest_message_in_a_query = lubridate::as_datetime(
      "2025-07-13T10:00:00",
      tz = "UTC"
    ),
    has_more = TRUE
  )

  newest_message_ever <- content$newest_message_in_a_query

  plan_new <- bsky_update_plan(plan_initial, content)

  # We expect the next search to start from the oldest message in the previous search
  expect_equal(
    plan_new$research_max_date,
    content$oldest_message_in_a_query
  )

  # We expect the boundaries date min to be updated to the oldest message in the previous search
  expect_equal(
    plan_new$boundaries_date_min,
    content$oldest_message_in_a_query
  )

  # We expect the has_more to be TRUE
  expect_equal(plan_new$has_more, content$has_more)

  # We expect the boundaries date max to be NULL, as it erased at the end of a full search
  expect_true(
    is.null(plan_new$boundaries_date_max)
  )

  # We expect no bound for the research min date since there are new messages to retrieve
  expect_true(is.null(plan_new$research_min_date))

  # We expect the requests to be 1
  expect_equal(plan_new$requests, 1)

  # We expect the start_on to be the current time
  expect_equal(as.Date(plan_new$start_on), as.Date(Sys.time()))
  previous_start_on <- plan_new$start_on

  # We expect the end_on to be NULL
  expect_true(is.null(plan_new$end_on))

  ## We are retrieving 100 messages more
  content <- list(
    newest_message_in_a_query = lubridate::as_datetime(
      "2025-07-13T09:00:00",
      tz = "UTC"
    ),
    oldest_message_in_a_query = lubridate::as_datetime(
      "2025-07-12T10:00:00",
      tz = "UTC"
    ),
    has_more = TRUE
  )

  plan_new <- bsky_update_plan(plan_new, content)
  # We expect the next search to start from the oldest message in the previous search
  expect_equal(
    plan_new$research_max_date,
    content$oldest_message_in_a_query
  )

  # We expect the boundaries date min to be updated to the oldest message in the previous search
  expect_equal(
    plan_new$boundaries_date_min,
    content$oldest_message_in_a_query
  )

  # We expect the has_more to be TRUE
  expect_equal(plan_new$has_more, content$has_more)

  # We expect the boundaries date max to be NULL, as it erased at the end of a full search
  expect_true(
    is.null(plan_new$boundaries_date_max)
  )

  # We expect no bound for the research min date since there are new messages to retrieve
  expect_true(is.null(plan_new$research_min_date))

  # We expect the requests to be 2
  expect_equal(plan_new$requests, 2)

  # We expect the start_on to be the previous start_on
  expect_equal(plan_new$start_on, previous_start_on)

  # We expect the end_on to be the current time
  expect_true(is.null(plan_new$end_on))

  ## We are retrieving 100 messages more and say there are no more messages to retrieve
  content <- list(
    newest_message_in_a_query = lubridate::as_datetime(
      "2025-07-12T10:00:00",
      tz = "UTC"
    ),
    oldest_message_in_a_query = lubridate::as_datetime(
      "2025-07-11T10:00:00",
      tz = "UTC"
    ),
    has_more = FALSE
  )

  plan_new <- bsky_update_plan(plan_new, content)

  # We expect the next search to start from the oldest message in the previous search
  expect_true(
    is.null(plan_new$research_max_date)
  )

  # We expect the boundaries date min to be updated to the oldest message in the previous search
  expect_equal(
    plan_new$boundaries_date_min,
    content$oldest_message_in_a_query
  )

  # We expect the has_more to be TRUE
  expect_equal(plan_new$has_more, content$has_more)

  # We expect the boundaries date max to be the newest message ever
  expect_equal(
    plan_new$boundaries_date_max,
    newest_message_ever
  )

  # We expect the research min date to be NULL as we are at the end of the research
  expect_true(is.null(plan_new$research_min_date))

  # We expect the research max date to be NULL as we are at the end of the research
  expect_true(is.null(plan_new$research_max_date))

  # We expect the requests to be 3
  expect_equal(plan_new$requests, 3)

  # We expect the end_on to be the current time
  expect_equal(as.Date(plan_new$end_on), as.Date(Sys.time()))
  expect_equal(plan_new$start_on, previous_start_on)
  expect_true(plan_new$end_on > previous_start_on)

  # What happens if we start a new research with the last plan
  plan_to_restart_with <- plan_new

  boundaries <- bsky_set_date_boundaries(plan_to_restart_with)
  plan_new <- boundaries$plan

  ## We want our future research to start from the last message retrieved
  expect_equal(
    plan_new$research_min_date,
    plan_to_restart_with$boundaries_date_max
  )

  expect_equal(
    boundaries$max_text,
    "last message"
  )

  expect_equal(
    boundaries$min_text,
    plan_to_restart_with$boundaries_date_max
  )
})
