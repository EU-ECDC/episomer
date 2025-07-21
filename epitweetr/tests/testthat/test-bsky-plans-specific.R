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


test_that("bluesky_new_plan_logic_if_has_passed works", {
  schedule_span <- 10
  plan <- list(
    list(
      expected_end = Sys.time(),
      boundaries_date_min = "2025-01-01 00:00:00"
    )
  )

  params <- list(
    expected_end = if (
      Sys.time() > plan[[1]]$expected_end + 60 * schedule_span
    ) {
      Sys.time() + 60 * schedule_span
    } else {
      plan[[1]]$expected_end + 60 * schedule_span
    },
    network = "bluesky"
  )

  bluesky_logic <- bluesky_new_plan_logic_if_previous_has_passed(plan)
  expect_equal(bluesky_logic$research_max_date, plan[[1]]$boundaries_date_min)

  params <- c(params, bluesky_logic)

  expect_no_error(do.call(get_plan, params))
})
