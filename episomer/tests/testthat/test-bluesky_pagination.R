# Tests for sm_api_update_plan_after_request_bluesky() (R/sm-bluesky-api.R).
#
# This is the state machine of the bluesky pagination: a pure function of
# (plan, results) that decides the pagination parameters for the next request.
#
# Unlike Mastodon, Bluesky uses time-based pagination (since/until dates) rather than
# cursor-based pagination. The plan tracks only the minimum (oldest) date collected so far,
# and the update function advances it as new data is discovered.

# results as bluesky_parse_response() builds them. Only $pagination is read here.
results_page <- function(
  min_created_at = NULL
) {
  list(
    count = 1L,
    posts = list(),
    pagination = list(
      min_created_at = min_created_at
    )
  )
}

# a plan that has performed at least one request: it has a current_min_date
walking_plan <- function(current_min_date = NULL, ...) {
  c(
    list(
      network = "bluesky"
    ),
    if (!is.null(current_min_date)) {
      list(current_min_date = current_min_date)
    },
    list(...)
  )
}

# a plan whose first request is about to be performed: no current_min_date yet
first_plan <- function(...) {
  c(list(network = "bluesky"), list(...))
}

update <- sm_api_update_plan_after_request_bluesky


# ---- seeding the current_min_date on the first request ----

test_that("the first request seeds current_min_date from the result", {
  plan <- update(
    first_plan(),
    results_page(min_created_at = "2026-03-04T05:06:07.123456Z")
  )

  expect_equal(
    plan$current_min_date,
    lubridate::as_datetime("2026-03-04T05:06:07.123456Z")
  )
})

test_that("a plan with no current_min_date gets one on the first request", {
  plan <- update(
    first_plan(plan_min_date = lubridate::as_datetime("2026-01-01T00:00:00Z")),
    results_page(min_created_at = "2026-03-04T05:06:07.123456Z")
  )

  expect_equal(
    plan$current_min_date,
    lubridate::as_datetime("2026-03-04T05:06:07.123456Z")
  )
})


# ---- the current_min_date moves backwards only ----

test_that("the current_min_date moves to a strictly older date", {
  older <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(min_created_at = "2026-03-01T00:00:00.000000Z")
  )

  expect_equal(older$current_min_date, lubridate::as_datetime("2026-03-01T00:00:00Z"))
})

test_that("the current_min_date stays the same if the result is newer", {
  newer <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(min_created_at = "2026-03-08T00:00:00.000000Z")
  )

  expect_equal(newer$current_min_date, lubridate::as_datetime("2026-03-04T00:00:00Z"))
})

test_that("the current_min_date stays the same if result is exactly equal", {
  equal <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(min_created_at = "2026-03-04T00:00:00.000000Z")
  )

  expect_equal(equal$current_min_date, lubridate::as_datetime("2026-03-04T00:00:00Z"))
})


# ---- a page with no result leaves the current_min_date alone ----

test_that("a page with NULL min_created_at leaves current_min_date untouched", {
  plan <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(min_created_at = NULL)
  )

  expect_equal(plan$current_min_date, lubridate::as_datetime("2026-03-04T00:00:00Z"))
})

test_that("an empty page on the first request leaves current_min_date absent", {
  plan <- update(
    first_plan(),
    results_page(min_created_at = NULL)
  )

  expect_null(plan$current_min_date)
})


# ---- missing pagination field is handled gracefully ----

test_that("a missing min_created_at is handled as NULL", {
  # defensive branch: pagination coming from an older plans file, without the field
  results <- results_page(min_created_at = "2026-03-04T05:06:07.123456Z")
  results$pagination$min_created_at <- NULL

  plan <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results
  )

  expect_equal(plan$current_min_date, lubridate::as_datetime("2026-03-04T00:00:00Z"))
})


# ---- the walk as a whole ----

test_that("a full walk steadily moves the window backwards", {
  # one plan driven through successive requests, advancing backwards in time
  plan <- first_plan(plan_min_date = lubridate::as_datetime("2026-01-01T00:00:00Z"))
  min_dates <- character(0)

  for (request in 1:9) {
    plan <- update(
      plan,
      results_page(
        min_created_at = sprintf("2026-03-%02dT12:00:00.000000Z", 10 - request)
      )
    )
    min_dates <- c(min_dates, format(plan$current_min_date, "%Y-%m-%d"))
  }

  # each step should move strictly backwards
  diffs_in_days <- diff(as.Date(min_dates))
  expect_true(all(diffs_in_days <= 0))
  # the earliest date should be strictly older than the first one
  expect_true(min_dates[length(min_dates)] < min_dates[1])
})
