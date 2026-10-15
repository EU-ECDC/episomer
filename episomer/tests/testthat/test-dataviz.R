dataviz_mocks <- function(aggregates, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    stop_if_no_config = function(...) invisible(NULL),
    stop_if_no_fs = function(...) invisible(NULL),
    get_topics_labels = function() c(covid = "COVID-19", `dengue fever` = "Dengue fever"),
    get_aggregates = aggregates,
    .package = "episomer",
    .env = .env
  )
}

words_df <- function() {
  data.frame(
    topic = "covid",
    created_date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-02", "2026-01-03", "2026-02-15")),
    geo_country_code = c("FR", "FR", "DE", "FR", "FR"),
    token = c("mask", "mask", "mask", "vaccine", "mask"),
    original = c(5, 3, 4, 10, 100),
    quotes = c(1, NA, 2, 0, 50),
    stringsAsFactors = FALSE
  )
}

test_that("get_empty_chart returns a titled chart and no data", {
  res <- get_empty_chart("Nothing here")
  expect_s3_class(res$chart, "ggplot")
  expect_equal(res$chart$labels$title, "Nothing here")
  expect_true(inherits(res$data, "waiver"))
})

test_that("get_font_family is Helvetica", {
  skip_on_os("windows")
  expect_equal(get_font_family(), "Helvetica")
})

test_that("create_topchart sums the original frequency over the period and ranks it", {
  dataviz_mocks(function(...) words_df())
  res <- create_topchart(
    "bluesky", "covid", "topwords",
    date_min = as.Date("2026-01-01"), date_max = as.Date("2026-01-31")
  )
  expect_equal(as.character(res$data$top), c("mask", "vaccine"))
  expect_equal(res$data$frequency, c(12, 10))
  expect_equal(unique(res$data$topic), "COVID-19")
  expect_match(res$chart$labels$title, "Top words of messages mentioning COVID-19")
  expect_equal(res$chart$labels$subtitle, "from 2026-01-01 to 2026-01-31")
})

test_that("create_topchart can add quotes to the frequency", {
  dataviz_mocks(function(...) words_df())
  res <- create_topchart(
    "bluesky", "covid", "topwords",
    date_min = as.Date("2026-01-01"), date_max = as.Date("2026-01-31"), with_quotes = TRUE
  )
  expect_equal(res$data$frequency, c(15, 10))
})

test_that("create_topchart filters by country codes and limits the ranking", {
  dataviz_mocks(function(...) words_df())
  res <- create_topchart(
    "bluesky", "covid", "topwords", country_codes = "FR",
    date_min = as.Date("2026-01-01"), date_max = as.Date("2026-01-31"), top = 1
  )
  # FR over the period: mask 5 + 3 = 8, vaccine 10 (DE and the February row are out)
  expect_equal(as.character(res$data$top), "vaccine")
  expect_equal(res$data$frequency, 10)
})

test_that("create_topchart asks the aggregates with the filters of the query", {
  got <- NULL
  dataviz_mocks(function(...) {
    got <<- list(...)
    words_df()[0, ]
  })
  create_topchart(
    "bluesky", "Dengue", "tags", country_codes = c("FR", "DE"),
    date_min = as.Date("2026-01-01"), date_max = as.Date("2026-01-31")
  )
  expect_equal(got$dataset, "tags")
  expect_equal(got$top_field, "tag")
  expect_equal(got$top_freq, "frequency")
  expect_equal(got$filter$topic, "dengue")
  expect_equal(got$filter$geo_country_code, c("FR", "DE"))
  expect_equal(got$filter$network, "bluesky")
  expect_equal(got$filter$period, list(as.Date("2026-01-01"), as.Date("2026-01-31")))
})

test_that("create_topchart returns an empty chart when there is nothing to show", {
  dataviz_mocks(function(...) words_df()[0, ])
  res <- create_topchart("bluesky", "covid", "topwords")
  expect_match(res$chart$labels$title, "No data found")

  dataviz_mocks(function(...) stop("boom"))
  # try() prints the error it swallows on stderr
  utils::capture.output(res <- create_topchart("bluesky", "covid", "topwords"), type = "message")
  expect_match(res$chart$labels$title, "No data found")

  # data only outside of the requested period
  dataviz_mocks(function(...) words_df())
  res <- create_topchart(
    "bluesky", "covid", "topwords",
    date_min = as.Date("2030-01-01"), date_max = as.Date("2030-01-31")
  )
  expect_match(res$chart$labels$title, "No data found")
})

test_that("create_topchart rejects an unknown series", {
  dataviz_mocks(function(...) words_df())
  expect_error(create_topchart("bluesky", "covid", "bogus"))
})

test_that("create_topwords is create_topchart on the topwords series", {
  got <- NULL
  testthat::local_mocked_bindings(create_topchart = function(...) got <<- list(...), .package = "episomer")
  create_topwords("bluesky", "covid", country_codes = "FR", top = 5)
  expect_equal(got$serie, "topwords")
  expect_equal(got$country_codes, "FR")
  expect_equal(got$top, 5)
})

test_that("trend_line turns region names into indexes and asks the alerts", {
  got <- NULL
  testthat::local_mocked_bindings(
    stop_if_no_config = function(...) invisible(NULL),
    stop_if_no_fs = function(...) invisible(NULL),
    get_country_items = function(...) list(list(name = "World"), list(name = "France"), list(name = "Spain")),
    calculate_regions_alerts = function(...) {
      got <<- list(...)
      data.frame(number_of_posts = 0)
    },
    .package = "episomer"
  )
  res <- trend_line("bluesky", "Covid", countries = c("Spain", "World"))
  expect_equal(got$regions, c(1, 3))
  expect_equal(got$topic, "covid")
  expect_equal(got$sms, "bluesky")
  expect_match(res$chart$labels$title, "No data found for the selected topic")
})

test_that("trend_line plots the alerts it gets, with the label of the topic", {
  df <- data.frame(
    date = as.Date("2026-01-01") + 0:5, country = "France",
    number_of_posts = c(1, 2, 3, 20, 2, 1), baseline = 2, limit = 6,
    alert = c(0, 0, 0, 1, 0, 0), known_users = 1, known_ratio = 0.1,
    stringsAsFactors = FALSE
  )
  plotted <- NULL
  testthat::local_mocked_bindings(
    stop_if_no_config = function(...) invisible(NULL),
    stop_if_no_fs = function(...) invisible(NULL),
    get_topics_labels = function() c(`dengue fever` = "Dengue fever"),
    calculate_regions_alerts = function(..., logenv) {
      logenv$total_count <- 1234
      df
    },
    plot_trendline = function(...) {
      plotted <<- list(...)
      "chart"
    },
    .package = "episomer"
  )
  expect_equal(trend_line("bluesky", "dengue%20fever", countries = 2), "chart")
  expect_equal(plotted$topic, "Dengue fever")
  expect_equal(unique(plotted$df$topic), "Dengue fever")
  expect_equal(plotted$total_count, 1234)
})

trend_df <- function(countries = "France", n = 6) {
  df <- do.call(rbind, lapply(countries, function(country) {
    data.frame(
      date = as.Date("2026-01-01") + seq_len(n) - 1, country = country,
      number_of_posts = c(1, 2, 3, 20, 2, 1)[seq_len(n)], baseline = 2, limit = 6,
      alert = c(0, 0, 0, 1, 0, 0)[seq_len(n)], known_users = 1, known_ratio = 0.1,
      topic = "Covid", stringsAsFactors = FALSE
    )
  }))
  df
}

plot_it <- function(df, countries, date_type = "created_date") {
  testthat::local_mocked_bindings(
    get_country_items = function(...) list(list(name = "World"), list(name = "France")),
    .package = "episomer",
    .env = parent.frame()
  )
  plot_trendline(
    df, "bluesky", countries, "Covid", "2026-01-01", "2026-01-06", date_type,
    alpha = 0.025, alpha_outlier = 0.05, k_decay = 4, total_count = 12345
  )
}

test_that("plot_trendline titles a single region chart and hides the legend", {
  res <- plot_it(trend_df(), countries = 2)
  expect_equal(
    res$chart$labels$title,
    "Number of messages mentioning Covid from 2026-01-01 to 2026-01-06\n in France (n=29)"
  )
  expect_equal(res$chart$labels$x, "Posted date")
  expect_equal(res$chart$theme$legend.position, "none")
  expect_no_error(suppressWarnings(ggplot2::ggplot_build(res$chart)))
})

test_that("plot_trendline calls region 1 the world", {
  expect_match(plot_it(trend_df(), countries = 1)$chart$labels$title, "in the world")
})

test_that("plot_trendline handles several regions and weekly data", {
  res <- plot_it(trend_df(c("France", "Spain")), countries = c(1, 2), date_type = "created_weeknum")
  expect_match(res$chart$labels$title, "in multiples regions")
  expect_equal(res$chart$labels$x, "Posted week")
  expect_equal(res$chart$theme$legend.position, "right")
})

test_that("plot_trendline returns the data with display column names", {
  res <- plot_it(trend_df(), countries = 2)
  expect_true(all(c("Country", "Number of messages", "Message date", "Topic") %in% names(res$data)))
  # the rows come back sorted by alert, then date
  expect_match(res$data$Details[res$data$alert == 1], "Alert: yes", fixed = TRUE)
  expect_match(res$data$Details[res$data$alert == 0][[1]], "Alert: no", fixed = TRUE)
  expect_equal(res$data$lim_start[[1]], 0)
})

test_that("plot_trendline stacks the alert markers of the same date by country", {
  df <- trend_df(c("France", "Spain"))
  res <- plot_it(df, countries = c(1, 2))
  expect_equal(sort(res$data$rank[res$data$alert == 1]), c(1, 2))
  alert_layer <- res$chart$layers[[2]]$data
  expect_equal(nrow(alert_layer), 2)
  expect_match(alert_layer$Details[[1]], "Alert detected")
})

test_that("plot_trendline draws no alert marker when there is no alert", {
  df <- trend_df()
  df$alert <- 0
  res <- plot_it(df, countries = 2)
  expect_false(any(vapply(res$chart$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))))
})
