test_that("get_alert_count_from goes back baseline plus the two days of the algorithm", {
  d <- as.Date("2026-03-20")
  expect_equal(get_alert_count_from(d, 7, FALSE), as.Date("2026-03-11"))
  expect_equal(get_alert_count_from(d, 7, TRUE), as.Date("2026-01-28"))
  expect_equal(get_alert_count_from(d, 0, FALSE), as.Date("2026-03-18"))
})

counts_df <- function() {
  data.frame(
    created_date = as.Date(rep(c("2026-03-01", "2026-03-02"), each = 2)),
    created_hour = c(10L, 23L, 10L, 23L),
    posts = c(5, 3, 7, 4),
    known_users = c(1, 0, 2, 1)
  )
}

test_that("get_reporting_date_counts returns an empty typed frame without usable data", {
  empty <- data.frame(
    reporting_date = as.Date(character()), count = numeric(), known_users = numeric(),
    stringsAsFactors = FALSE
  )
  expect_equal(get_reporting_date_counts(data.frame(), "t", NA, NA, Sys.Date()), empty)
  expect_equal(get_reporting_date_counts(counts_df()[0, ], "t", NA, NA, Sys.Date()), empty)
  expect_message(
    out <- suppressWarnings(capture.output(
      res <- get_reporting_date_counts(data.frame(x = 1), "t", NA, NA, Sys.Date())
    )),
    "incoherent"
  )
  expect_equal(res, empty)
})

test_that("a fully collected day is reported on its own date", {
  out <- get_reporting_date_counts(
    counts_df(), "t",
    start = as.Date("2026-03-01"), end = as.Date("2026-03-02"),
    last_day = as.Date("2026-03-05")
  )
  expect_equal(as.Date(out$reporting_date), as.Date(c("2026-03-01", "2026-03-02")))
  expect_equal(out$count, c(8, 11))
  expect_equal(out$known_users, c(1, 3))
})

test_that("hours after the last fully collected hour roll over to the next reporting date", {
  # last day: the last full hour is 22 (hour 23 is the last, still incomplete one)
  out <- get_reporting_date_counts(
    counts_df(), "t",
    start = NA, end = NA, last_day = as.Date("2026-03-02")
  )
  expect_equal(as.Date(out$reporting_date), as.Date(c("2026-03-01", "2026-03-02", "2026-03-03")))
  expect_equal(out$count, c(5, 10, 4))
  expect_equal(out$known_users, c(1, 2, 1))
})

test_that("start and end bound the reporting dates", {
  out <- get_reporting_date_counts(
    counts_df(), "t",
    start = as.Date("2026-03-02"), end = NA, last_day = as.Date("2026-03-05")
  )
  # hour 23 of the last collected day is not a full hour, it rolls over to the 3rd
  expect_equal(as.Date(out$reporting_date), as.Date(c("2026-03-02", "2026-03-03")))
  expect_equal(out$count, c(10, 4))
  out <- get_reporting_date_counts(
    counts_df(), "t",
    start = NA, end = as.Date("2026-03-01"), last_day = as.Date("2026-03-05")
  )
  expect_equal(as.Date(out$reporting_date), as.Date("2026-03-01"))
  expect_equal(out$count, 8) # end before last_day: the day is fully collected, hour 23 included
})

subscribers_sheet <- function(...) {
  extra <- list(...)
  base <- data.frame(
    User = "Ann", Email = "ann@x.org", Topics = "covid", Regions = "France",
    `Excluded Topics` = NA, `Real time Topics` = NA, `Alert Slots` = 1,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (n in names(extra)) base[[n]] <- extra[[n]]
  base
}

test_that("get_subscribers fills the columns missing from old subscriber files", {
  testthat::local_mocked_bindings(
    epiread_excel = function(...) subscribers_sheet(),
    get_subscribers_path = function() "subscribers.xlsx",
    .package = "episomer"
  )
  df <- get_subscribers()
  expect_true(is.na(df$`Alert category`))
  expect_true(df$`One post alerts`)
  expect_true(is.na(df$`Topics ignoring 1 post alerts`))
  expect_true(is.na(df$`Regions ignoring 1 post alerts`))
  expect_identical(df$`Alert Slots`, "1")
  expect_type(df$Topics, "character")
})

test_that("get_subscribers reads the one post alerts flag leniently", {
  sheet <- subscribers_sheet()[rep(1, 3), ]
  sheet$`One post alerts` <- c("no", "yes", "FALSE")
  testthat::local_mocked_bindings(
    epiread_excel = function(...) sheet,
    get_subscribers_path = function() "subscribers.xlsx",
    .package = "episomer"
  )
  expect_equal(get_subscribers()$`One post alerts`, c(FALSE, TRUE, FALSE))
})

test_that("get_reporting_date_counts does not depend on the timezone of the machine", {
  run <- function(tz) {
    withr::local_timezone(tz)
    get_reporting_date_counts(
      counts_df(), "t",
      start = NA, end = NA, last_day = as.Date("2026-03-02")
    )$count
  }
  expect_equal(run("UTC"), c(5, 10, 4))
  expect_equal(run("Europe/Paris"), c(5, 10, 4))
  expect_equal(run("America/Los_Angeles"), c(5, 10, 4))
})
