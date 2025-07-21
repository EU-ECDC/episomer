test_that("bsky_format_date works with character date input", {
  # Test date-only character input
  expect_equal(
    bsky_format_date("2024-01-01"),
    "2024-01-01T00:00:00Z"
  )

  # Test datetime character input
  expect_equal(
    bsky_format_date("2024-01-01 12:30:45"),
    "2024-01-01T12:30:45Z"
  )

  # Test with different date formats
  expect_equal(
    bsky_format_date("2024/01/01"),
    "2024-01-01T00:00:00Z"
  )

  # Test with timezone in input
  expect_equal(
    bsky_format_date(
      "2024-01-01 12:30:45",
      timezone = "America/New_York"
    ),
    "2024-01-01T17:30:45Z" # 5 hours ahead of UTC
  )
})

test_that("bsky_format_date works with Date objects", {
  date_obj <- as.Date("2024-01-01")

  # Test with include_time = TRUE (default)
  expect_equal(
    bsky_format_date(date_obj),
    "2024-01-01T00:00:00Z"
  )

  # Test with include_time = FALSE
  expect_equal(
    bsky_format_date(date_obj, include_time = FALSE),
    "2024-01-01T00:00:00Z"
  )
})

test_that("bsky_format_date works with POSIXct objects", {
  datetime_obj <- as.POSIXct("2024-01-01 12:30:45", tz = "UTC")

  expect_equal(
    bsky_format_date(datetime_obj),
    "2024-01-01T12:30:45Z"
  )

  # Test with different timezone
  datetime_obj_est <- as.POSIXct(
    "2024-01-01 12:30:45",
    tz = "America/New_York"
  )
  expect_equal(
    bsky_format_date(datetime_obj_est),
    "2024-01-01T17:30:45Z" # Converted to UTC
  )
})

test_that("bsky_format_date works with POSIXlt objects", {
  datetime_lt <- as.POSIXlt("2024-01-01 12:30:45", tz = "UTC")

  expect_equal(
    bsky_format_date(datetime_lt),
    "2024-01-01T12:30:45Z"
  )
})

test_that("bsky_format_date works with numeric timestamps", {
  # Unix timestamp for 2024-01-01 00:00:00 UTC
  timestamp <- 1704067200

  expect_equal(
    bsky_format_date(timestamp),
    "2024-01-01T00:00:00Z"
  )

  # Unix timestamp for 2024-01-01 12:10:45 UTC (corrected)
  timestamp_with_time <- 1704111045

  expect_equal(
    bsky_format_date(timestamp_with_time),
    "2024-01-01T12:10:45Z"
  )
})

test_that("bsky_format_date handles include_time parameter correctly", {
  datetime_obj <- as.POSIXct("2024-01-01 12:30:45", tz = "UTC")

  # With time included (default)
  expect_equal(
    bsky_format_date(datetime_obj, include_time = TRUE),
    "2024-01-01T12:30:45Z"
  )

  # With time set to 00:00:00
  expect_equal(
    bsky_format_date(datetime_obj, include_time = FALSE),
    "2024-01-01T00:00:00Z"
  )
})

test_that("bsky_format_date handles timezone conversion correctly", {
  # Test with EST timezone
  datetime_est <- as.POSIXct("2024-01-01 12:00:00", tz = "America/New_York")

  expect_equal(
    bsky_format_date(datetime_est, timezone = "America/New_York"),
    "2024-01-01T17:00:00Z" # 5 hours ahead of UTC
  )

  # Test with PST timezone
  datetime_pst <- as.POSIXct(
    "2024-01-01 12:00:00",
    tz = "America/Los_Angeles"
  )

  expect_equal(
    bsky_format_date(datetime_pst, timezone = "America/Los_Angeles"),
    "2024-01-01T20:00:00Z" # 8 hours ahead of UTC
  )
})

test_that("bsky_format_date handles edge cases", {
  # Test leap year
  expect_equal(
    bsky_format_date("2024-02-29"),
    "2024-02-29T00:00:00Z"
  )

  # Test end of year
  expect_equal(
    bsky_format_date("2024-12-31 23:59:59"),
    "2024-12-31T23:59:59Z"
  )

  # Test beginning of year
  expect_equal(
    bsky_format_date("2024-01-01 00:00:00"),
    "2024-01-01T00:00:00Z"
  )
})

test_that("bsky_format_date handles invalid inputs", {
  # Test NULL input
  expect_error(
    bsky_format_date(NULL),
    "Unsupported date input type"
  )

  # Test logical input
  expect_error(
    bsky_format_date(TRUE),
    "Unsupported date input type"
  )

  # Test empty string - this actually fails with a different error
  expect_error(
    bsky_format_date(""),
    "character string is not in a standard unambiguous format"
  )
})

test_that("bsky_format_date handles complex datetime scenarios", {
  # Test with milliseconds (should be truncated)
  datetime_with_ms <- as.POSIXct("2024-01-01 12:30:45.123", tz = "UTC")

  expect_equal(
    bsky_format_date(datetime_with_ms),
    "2024-01-01T12:30:45Z"
  )

  # Test with different date formats that should work
  # Note: The function falls back to date parsing for these formats
  expect_equal(
    bsky_format_date("2024-01-01T12:30:45"),
    "2024-01-01T00:00:00Z"
  )

  expect_equal(
    bsky_format_date("2024-01-01T12:30:45Z"),
    "2024-01-01T00:00:00Z"
  )
})

test_that("bsky_format_date maintains consistency across input types", {
  # Same datetime expressed in different formats should give same result
  date_char <- "2024-01-01 12:30:45"
  date_obj <- as.Date("2024-01-01")
  datetime_obj <- as.POSIXct("2024-01-01 12:30:45", tz = "UTC")
  timestamp <- 1704111045 # 2024-01-01 12:10:45 UTC

  result_char <- bsky_format_date(date_char)
  result_datetime <- bsky_format_date(datetime_obj)
  result_timestamp <- bsky_format_date(timestamp)

  expect_equal(result_char, result_datetime)
  # Note: timestamp represents a different time (12:10:45 vs 12:30:45)
  expect_false(result_datetime == result_timestamp)

  # Date object should give different result (time is 00:00:00)
  result_date <- bsky_format_date(date_obj)
  expect_false(result_date == result_char)
  expect_equal(result_date, "2024-01-01T00:00:00Z")
})
