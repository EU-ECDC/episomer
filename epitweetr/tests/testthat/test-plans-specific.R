test_that("bluesky_get_plan_elements works", {
  results <- bluesky_get_plan_elements(
    research_max_date = "2025-01-01 00:00:00",
    research_min_date = "2025-01-01 00:00:00",
    boundaries_date_min = "2025-01-01 00:00:00",
    boundaries_date_max = "2025-01-01 00:00:00",
    newest_messages_from_previous_queries = NULL,
    oldest_messages_from_previous_queries = "2025-01-01 00:00:00",
    has_more = TRUE,
    remove_me = "remove_me"
  )

  expect_equal(
    results$research_max_date,
    strptime("2025-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    results$research_min_date,
    strptime("2025-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    results$boundaries_date_min,
    strptime("2025-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    results$boundaries_date_max,
    strptime("2025-01-01 00:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_null(
    results$newest_messages_from_previous_queries
  )
  expect_equal(
    results$oldest_messages_from_previous_queries,
    "2025-01-01 00:00:00"
  )
  expect_equal(results$has_more, TRUE)
  expect_null(results$remove_me)
})
