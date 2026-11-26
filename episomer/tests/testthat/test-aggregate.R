# get_aggregates() keeps its results in the package level `cached` environment: each test starts
# from an empty one and puts back what was there.
local_cached <- function(.local_envir = parent.frame()) {
  saved <- as.list(cached, all.names = TRUE)
  withr::defer(
    {
      rm(list = ls(cached, all.names = TRUE), envir = cached)
      list2env(saved, envir = cached)
    },
    envir = .local_envir
  )
  rm(list = ls(cached, all.names = TRUE), envir = cached)
}

agg_fixture <- function() {
  data.frame(
    topic = c("a", "a", "b"),
    created_date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    count = 1:3,
    stringsAsFactors = FALSE
  )
}

mock_period <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_aggregated_period = function() list(last = as.Date("2026-01-03"), last_hour = 5L),
    get_scala_aggregate_url = function() "http://fs/aggregate",
    .package = "episomer",
    .env = .env
  )
}

test_that("get_aggregates builds the query url and derives week columns", {
  local_cached()
  local_conf(onthefly_api = TRUE)
  mock_period()
  urls <- character()
  testthat::local_mocked_bindings(
    stream_in = function(con, ...) {
      urls <<- c(urls, summary(con)$description)
      data.frame(topic = "a", created_date = "2026-01-05", count = 1L, stringsAsFactors = FALSE)
    },
    .package = "jsonlite"
  )
  df <- get_aggregates(
    "country_counts",
    filter = list(topic = c("a", "b"), period = as.Date(c("2026-01-01", "2026-01-31")), geo = "FR"),
    top_field = "topic", top_freq = "count"
  )
  expect_match(urls, "serie=country_counts", fixed = TRUE)
  expect_match(urls, "&topField=topic&topFrequency=count", fixed = TRUE)
  expect_match(urls, "&topic=a%3Bb", fixed = TRUE)
  expect_match(urls, "&from=2026-01-01&to=2026-01-31", fixed = TRUE)
  expect_match(urls, "&filter=geo:FR", fixed = TRUE)
  expect_false(grepl("last_aggregate", urls))
  expect_equal(df$created_week, "2026.02")
  expect_equal(df$created_weeknum, 202602L)
  expect_s3_class(df$created_date, "Date")
})

test_that("get_aggregates drops date columns on an empty answer", {
  local_cached()
  local_conf(onthefly_api = TRUE)
  mock_period()
  testthat::local_mocked_bindings(
    stream_in = function(con, ...) data.frame(),
    .package = "jsonlite"
  )
  df <- get_aggregates("country_counts")
  expect_equal(nrow(df), 0)
  expect_false("created_week" %in% names(df))
})

test_that("get_aggregates serves a repeated query from the cache", {
  local_cached()
  local_conf(onthefly_api = TRUE)
  mock_period()
  calls <- 0
  testthat::local_mocked_bindings(
    stream_in = function(con, ...) {
      calls <<- calls + 1
      data.frame(topic = "a", created_date = "2026-01-01", count = 1L, stringsAsFactors = FALSE)
    },
    .package = "jsonlite"
  )
  get_aggregates("country_counts")
  get_aggregates("country_counts")
  expect_equal(calls, 1)
  get_aggregates("country_counts", cache = FALSE)
  expect_equal(calls, 2)
})

test_that("get_aggregates filters the cache by period and values", {
  local_cached()
  local_conf(onthefly_api = TRUE)
  mock_period()
  cached$country_counts <- agg_fixture()
  cached$last_filter_country_counts <- list(last_aggregate = "2026-01-03 5")
  testthat::local_mocked_bindings(
    stream_in = function(...) stop("must not hit the database"),
    .package = "jsonlite"
  )
  df <- get_aggregates(
    "country_counts",
    filter = list(period = as.Date(c("2026-01-02", "2026-01-03")), topic = "b")
  )
  # cache holds no period restriction: all dates are cached, the filter is applied locally
  expect_equal(df$count, 3L)
  expect_equal(nrow(get_aggregates("country_counts")), 3)
})

test_that("get_aggregates refetches when a new aggregation has finished", {
  local_cached()
  local_conf(onthefly_api = TRUE)
  mock_period()
  cached$country_counts <- agg_fixture()
  cached$last_filter_country_counts <- list(last_aggregate = "2026-01-03 4")
  fetched <- FALSE
  testthat::local_mocked_bindings(
    stream_in = function(...) {
      fetched <<- TRUE
      data.frame()
    },
    .package = "jsonlite"
  )
  get_aggregates("country_counts")
  expect_true(fetched)
})

test_that("get_aggregates refetches when the cached period does not cover the query", {
  local_cached()
  local_conf(onthefly_api = TRUE)
  mock_period()
  cached$country_counts <- agg_fixture()
  cached$last_filter_country_counts <- list(
    last_aggregate = "2026-01-03 5",
    period = as.Date(c("2026-01-02", "2026-01-03"))
  )
  fetched <- FALSE
  testthat::local_mocked_bindings(
    stream_in = function(...) {
      fetched <<- TRUE
      data.frame()
    },
    .package = "jsonlite"
  )
  get_aggregates("country_counts", filter = list(period = as.Date(c("2026-01-01", "2026-01-03"))))
  expect_true(fetched)
})

test_that("get_aggregated_period parses the period and caches it", {
  local_cached()
  mock_url <- testthat::local_mocked_bindings(
    get_scala_period_url = function() "http://fs/period",
    .package = "episomer"
  )
  calls <- 0
  testthat::local_mocked_bindings(
    fromJSON = function(...) {
      calls <<- calls + 1
      list(first = "2026-01-01", last = "2026-01-09", last_hour = "7")
    },
    .package = "jsonlite"
  )
  p <- get_aggregated_period()
  expect_equal(p$first, as.Date("2026-01-01"))
  expect_equal(p$last, as.Date("2026-01-09"))
  expect_identical(p$last_hour, 7L)
  get_aggregated_period()
  expect_equal(calls, 1)
})

test_that("get_aggregated_period returns NAs and does not cache when the service warns", {
  local_cached()
  testthat::local_mocked_bindings(
    get_scala_period_url = function() "http://fs/period",
    .package = "episomer"
  )
  calls <- 0
  testthat::local_mocked_bindings(
    fromJSON = function(...) {
      calls <<- calls + 1
      warning("cannot open URL")
    },
    .package = "jsonlite"
  )
  p <- suppressWarnings(get_aggregated_period())
  expect_true(is.na(p$first))
  expect_true(is.na(p$last))
  suppressWarnings(get_aggregated_period())
  expect_equal(calls, 2)
})

test_that("get_aggregated_period returns NAs, with a warning, when the service fails", {
  local_cached()
  testthat::local_mocked_bindings(
    get_scala_period_url = function() "http://fs/period",
    .package = "episomer"
  )
  calls <- 0
  testthat::local_mocked_bindings(
    fromJSON = function(...) {
      calls <<- calls + 1
      stop("connection refused")
    },
    .package = "jsonlite"
  )
  expect_warning(p <- get_aggregated_period(), "connection refused")
  expect_true(is.na(p$first))
  expect_true(is.na(p$last))
  suppressWarnings(get_aggregated_period())
  expect_equal(calls, 2)
})

test_that("add_missing returns its input untouched", {
  df <- agg_fixture()
  expect_identical(add_missing(df, "country_counts"), df)
})
