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

  bluesky_logic <- bluesky_new_plan_logic_if_previous_has_passed(plan)
  expect_equal(bluesky_logic$research_max_date, plan[[1]]$boundaries_date_min)
})

test_that("bluesky_new_plan_logic_if_has_passed integrates within update_plan", {
  schedule_span <- 10
  plan <- list(
    list(
      expected_end = Sys.time(),
      boundaries_date_min = "2025-01-01 00:00:00"
    )
  )

  bluesky_logic <- bluesky_new_plan_logic_if_previous_has_passed(plan)

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

  params <- c(params, bluesky_logic)

  expect_no_error(do.call(get_plan, params))
})


test_that("bluesky_finish_plan works", {
  plan <- list(
    list(
      boundaries_date_min = "2025-01-01 00:00:00",
      boundaries_date_max = "2025-01-01 00:00:00"
    )
  )

  results <- bluesky_finish_plan(plan[[1]])

  expect_equal(results$research_max_date, NULL)
  expect_equal(results$research_min_date, NULL)
  expect_equal(results$boundaries_date_min, "2025-01-01 00:00:00")
  expect_equal(results$boundaries_date_max, "2025-01-01 00:00:00")
  expect_null(results$newest_messages_from_previous_queries)
  expect_null(results$oldest_messages_from_previous_queries)
  expect_equal(results$has_more, FALSE)
})

test_that("bluesky_finish_plan integrates within finish_plans", {
  p <-
    list(
      boundaries_date_min = "2025-01-01 00:00:00",
      boundaries_date_max = "2025-01-01 00:00:00",
      network = "bluesky",
      scheduled_for = Sys.time(),
      start_on = Sys.time(),
      end_on = Sys.time(),
      requests = 10,
      progress = 1.0
    )

  conf <- list(
    schedule_span = 10
  )

  plan_common_elements <- list(
    network = "bluesky",
    expected_end = strftime(
      if (is.null(p$end_on)) {
        Sys.time() - conf$schedule_span * 60
      } else {
        p$end_on
      },
      "%Y-%m-%d %H:%M:%S"
    ),
    scheduled_for = strftime(p$scheduled_for, "%Y-%m-%d %H:%M:%S"),
    start_on = strftime(
      if (is.null(p$start_on)) {
        Sys.time() - conf$schedule_span * 60
      } else {
        p$start_on
      },
      "%Y-%m-%d %H:%M:%S"
    ),
    end_on = strftime(
      if (is.null(p$end_on)) {
        Sys.time() - conf$schedule_span * 60
      } else {
        p$end_on
      },
      "%Y-%m-%d %H:%M:%S"
    ),
    requests = p$requests,
    progress = 1.0
  )

  network_specific_logic <- bluesky_finish_plan(p)

  params <- c(plan_common_elements, network_specific_logic)

  expect_no_error(do.call(get_plan, params))
})
