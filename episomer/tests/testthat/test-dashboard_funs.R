test_that("line_chart_from_filters maps the filters to trend_line()", {
  got <- NULL
  testthat::local_mocked_bindings(
    trend_line = function(...) {
      got <<- list(...)
      "chart"
    },
    .package = "episomer"
  )
  out <- line_chart_from_filters(
    "bluesky", "covid", c("2", "3"), "created_date", as.Date(c("2026-01-01", "2026-01-31")),
    TRUE, 0.025, 0.05, 4, 7, FALSE, TRUE
  )
  expect_equal(out, "chart")
  expect_equal(got$sms, "bluesky")
  expect_equal(got$topic, "covid")
  expect_identical(got$countries, c(2L, 3L))
  expect_equal(got$date_type, "created_date")
  expect_equal(c(got$date_min, got$date_max), as.Date(c("2026-01-01", "2026-01-31")))
  expect_equal(got$no_historic, 7)
  expect_false(got$bonferroni_correction)
  expect_true(got$same_weekday_baseline)
})

test_that("line_chart_from_filters falls back to the first region when none is selected", {
  got <- NULL
  testthat::local_mocked_bindings(trend_line = function(...) got <<- list(...), .package = "episomer")
  line_chart_from_filters(
    "bluesky", "covid", NULL, "created_date", as.Date(c("2026-01-01", "2026-01-31")),
    TRUE, 0.025, 0.05, 4, 7, FALSE, TRUE
  )
  expect_equal(got$countries, 1)
})

test_that("map_chart_from_filters passes the regions disclaimer as caption", {
  local_conf(regions_disclaimer = "Borders are indicative")
  got <- NULL
  testthat::local_mocked_bindings(create_map = function(...) got <<- list(...), .package = "episomer")
  map_chart_from_filters("bluesky", "covid", character(0), as.Date(c("2026-01-01", "2026-01-31")), FALSE)
  expect_equal(got$countries, 1)
  expect_equal(got$caption, "Borders are indicative")
  expect_true(got$forplotly)
  expect_false(got$with_quotes)
  expect_equal(got$date_max, as.Date("2026-01-31"))
})

top_regions <- function() {
  list(
    list(name = "World", codes = c("FR", "DE", "ES")),
    list(name = "France", codes = "FR"),
    list(name = "Benelux", codes = c("BE", "NL", "LU")),
    list(name = "Germany", codes = c("DE", "FR"))
  )
}

top_chart_codes <- function(fcountries) {
  got <- NULL
  testthat::local_mocked_bindings(
    get_country_items = function(...) top_regions(),
    create_topchart = function(...) got <<- list(...),
    .package = "episomer",
    .env = parent.frame()
  )
  top_chart_from_filters(
    "bluesky", "covid", "topwords", fcountries, as.Date(c("2026-01-01", "2026-01-31")), TRUE, 10
  )
  got
}

test_that("top_chart_from_filters merges the country codes of the selected regions", {
  got <- top_chart_codes(c(3, 4))
  expect_equal(got$country_codes, c("BE", "NL", "LU", "DE", "FR"))
  expect_equal(got$serie, "topwords")
  expect_equal(got$date_min, as.Date("2026-01-01"))
  expect_equal(got[[length(got)]], 10)
})

test_that("top_chart_from_filters uses the first region when none or the world is selected", {
  expect_equal(top_chart_codes(NULL)$country_codes, c("FR", "DE", "ES"))
  expect_equal(top_chart_codes(c(1, 2))$country_codes, c("FR", "DE", "ES"))
})
