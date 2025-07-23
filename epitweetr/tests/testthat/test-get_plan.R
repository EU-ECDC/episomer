test_that("get_plan works with basic parameters", {
  # Test with minimal required parameters
  plan <- get_plan(
    network = "bluesky",
    expected_end = "2025-01-01 12:00:00"
  )

  # Check that it's a get_plan object
  expect_true("get_plan" %in% class(plan))

  # Check common elements
  expect_equal(plan$network, "bluesky")
  expect_equal(
    plan$expected_end,
    strptime("2025-01-01 12:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(plan$requests, 0)
  expect_equal(plan$progress, 0)
  expect_null(plan$start_on)
  expect_null(plan$end_on)

  # Check that scheduled_for is set (should be close to current time)
  expect_true(!is.null(plan$scheduled_for))
  expect_true(abs(as.numeric(plan$scheduled_for) - as.numeric(Sys.time())) < 10)

  # Check we can update the plan and keep the rest as it is
  plan$research_max_date <- "2025-01-01 12:00:00"
  plan$research_min_date <- "2025-01-01 08:00:00"
  plan$boundaries_date_max <- "2025-01-01 11:00:00"
  plan$boundaries_date_min <- "2025-01-01 09:00:00"

  new_plan_network_specific_logic <- get(
    sprintf(
      "%s_new_plan_creation_based_on_previous_one_values",
      plan$network
    )
  )

  params <- list(
    network = plan$network,
    expected_end = plan$expected_end + 10
  )
  params <- c(
    params,
    new_plan_network_specific_logic(list(plan))
  )

  plan2 <- do.call(get_plan, params)

  expect_equal(
    as.character(plan2$research_max_date),
    as.character(plan$research_max_date)
  )
  expect_equal(
    as.character(plan2$research_min_date),
    as.character(plan$research_min_date)
  )
  expect_equal(
    as.character(plan2$boundaries_date_max),
    as.character(plan$boundaries_date_max)
  )
  expect_equal(
    as.character(plan2$boundaries_date_min),
    as.character(plan$boundaries_date_min)
  )

  expect_equal(plan2$requests, plan$requests)
  expect_equal(
    as.character(plan2$expected_end),
    as.character(plan$expected_end + 10)
  )
})

test_that("get_plan works with all parameters", {
  # Test with all parameters specified
  plan <- get_plan(
    network = "bluesky",
    expected_end = "2025-01-01 12:00:00",
    scheduled_for = "2025-01-01 10:00:00",
    start_on = "2025-01-01 09:00:00",
    end_on = "2025-01-01 11:00:00",
    results_span = 120,
    requests = 5,
    progress = 0.75,
    research_max_date = "2025-01-01 12:00:00",
    research_min_date = "2025-01-01 08:00:00",
    boundaries_date_min = "2025-01-01 09:00:00",
    boundaries_date_max = "2025-01-01 11:00:00",
    newest_messages_from_previous_queries = "msg1",
    oldest_messages_from_previous_queries = "msg2",
    has_more = TRUE
  )

  # Check common elements
  expect_equal(plan$network, "bluesky")
  expect_equal(
    plan$expected_end,
    strptime("2025-01-01 12:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$scheduled_for,
    strptime("2025-01-01 10:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$start_on,
    strptime("2025-01-01 09:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$end_on,
    strptime("2025-01-01 11:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(plan$requests, 5)
  expect_equal(plan$progress, 0.75)

  # Check specific elements (bluesky_get_plan_elements)
  expect_equal(
    plan$research_max_date,
    strptime("2025-01-01 12:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$research_min_date,
    strptime("2025-01-01 08:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$boundaries_date_min,
    strptime("2025-01-01 09:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$boundaries_date_max,
    strptime("2025-01-01 11:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(plan$newest_messages_from_previous_queries, "msg1")
  expect_equal(plan$oldest_messages_from_previous_queries, "msg2")
  expect_equal(plan$has_more, TRUE)
})

test_that("get_plan handles NULL values correctly", {
  # Test with NULL values for optional parameters
  plan <- get_plan(
    network = "bluesky",
    expected_end = NULL,
    scheduled_for = NULL,
    start_on = NULL,
    end_on = NULL,
    research_max_date = NULL,
    research_min_date = NULL
  )

  # Check that NULL values are preserved
  expect_null(plan$expected_end)
  expect_null(plan$scheduled_for)
  expect_null(plan$start_on)
  expect_null(plan$end_on)
  expect_null(plan$research_max_date)
  expect_null(plan$research_min_date)

  # Check that defaults are applied
  expect_equal(plan$requests, 0)
  expect_equal(plan$progress, 0)
  expect_equal(plan$network, "bluesky")
})

test_that("get_plan filters out unexpected parameters", {
  # Test that unexpected parameters are filtered out
  plan <- get_plan(
    network = "bluesky",
    expected_end = "2025-01-01 12:00:00",
    unexpected_param = "should_be_removed",
    research_max_date = "2025-01-01 12:00:00",
    another_unexpected = "also_removed"
  )

  # Check that expected parameters are present
  expect_equal(plan$network, "bluesky")
  expect_equal(
    plan$expected_end,
    strptime("2025-01-01 12:00:00", "%Y-%m-%d %H:%M:%S")
  )
  expect_equal(
    plan$research_max_date,
    strptime("2025-01-01 12:00:00", "%Y-%m-%d %H:%M:%S")
  )

  # Check that unexpected parameters are not present
  expect_null(plan$unexpected_param)
  expect_null(plan$another_unexpected)
})

test_that("get_plan handles different network types", {
  # Test that the function works with different network types
  # Note: This test assumes that other network-specific functions exist
  # For now, we'll test with bluesky since we know it exists

  plan <- get_plan(
    network = "bluesky",
    expected_end = "2025-01-01 12:00:00"
  )

  expect_equal(plan$network, "bluesky")
  expect_true("get_plan" %in% class(plan))
})


test_that("get_plan preserves numeric values correctly", {
  # Test that numeric values are preserved as expected
  plan <- get_plan(
    network = "bluesky",
    expected_end = "2025-01-01 12:00:00",
    requests = 42,
    progress = 0.5,
    results_span = 360
  )

  expect_equal(plan$requests, 42)
  expect_equal(plan$progress, 0.5)
  # Note: results_span is not in the common_elements, so it would be passed to specific_elements
  # The exact behavior depends on the network-specific function
})
