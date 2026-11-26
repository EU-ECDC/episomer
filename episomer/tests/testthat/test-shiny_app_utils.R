validation_message <- function(expr) {
  tryCatch(
    {
      force(expr)
      NULL
    },
    shiny.silent.error = function(e) conditionMessage(e)
  )
}

period_mocks <- function(first, last, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    get_topics_labels = function() c(covid = "COVID-19", flu = "Influenza"),
    get_country_items = function(...) list(list(name = "World"), list(name = "France")),
    active_social_media = function() c("bluesky", "mastodon.social", "mastodon.org"),
    get_aggregated_period = function() list(first = first, last = last, last_hour = 1L),
    .package = "episomer",
    .env = .env
  )
}

test_that("refresh_dashboard_data builds the filter choices", {
  local_conf(topics = list(list(topic = "covid"), list(topic = "flu"), list(topic = "covid")))
  period_mocks(as.Date("2026-01-01"), as.Date("2026-03-01"))
  e <- refresh_dashboard_data()
  expect_equal(unname(e$topics[e$topics != ""]), c("covid", "flu"))
  expect_setequal(names(e$topics), c("", "COVID-19", "Influenza"))
  expect_equal(e$countries, c(World = 1L, France = 2L))
  expect_equal(e$sms, c(Bluesky = "bluesky", Mastodon = "mastodon"))
  expect_equal(c(e$date_min, e$date_max), c("2026-01-01", "2026-03-01"))
  expect_true(inherits(e$topics_refresh_flag, "reactiveVal"))
})

test_that("refresh_dashboard_data copes with no topic configured", {
  local_conf(topics = list())
  period_mocks(as.Date("2026-01-01"), as.Date("2026-03-01"))
  e <- refresh_dashboard_data()
  expect_equal(e$topics, setNames("", ""))
})

test_that("refresh_dashboard_data defaults to the last 7 days on a long collection", {
  local_conf(topics = list(list(topic = "covid")))
  period_mocks(as.Date("2026-01-01"), as.Date("2026-03-01"))
  e <- refresh_dashboard_data()
  expect_equal(e$fixed_period, "last 7 days")
  expect_equal(e$date_start, "2026-02-22")
  expect_equal(e$date_end, "2026-03-01")
})

test_that("refresh_dashboard_data uses a custom period on a short collection", {
  local_conf(topics = list(list(topic = "covid")))
  period_mocks(as.Date("2026-03-01"), as.Date("2026-03-04"))
  e <- refresh_dashboard_data()
  expect_equal(e$fixed_period, "custom")
  expect_equal(e$date_start, "2026-03-01")
})

test_that("refresh_dashboard_data honours a fixed period only when enough data exists", {
  local_conf(topics = list(list(topic = "covid")))
  period_mocks(as.Date("2026-01-01"), as.Date("2026-03-01"))
  expect_equal(refresh_dashboard_data(fixed_period = "last 30 days")$date_start, "2026-01-30")
  # 59 days collected: last 60 days falls back to the first date
  expect_equal(refresh_dashboard_data(fixed_period = "last 60 days")$date_start, "2026-01-01")
  expect_equal(refresh_dashboard_data(fixed_period = "last 180 days")$date_start, "2026-01-01")
})

test_that("refresh_dashboard_data falls back to today when nothing was aggregated", {
  local_conf(topics = list(list(topic = "covid")))
  period_mocks(NA, NA)
  e <- refresh_dashboard_data()
  expect_equal(c(e$date_min, e$date_max), rep(strftime(Sys.Date(), "%Y-%m-%d"), 2))
  expect_equal(e$fixed_period, "custom")
})

test_that("refresh_dashboard_data reuses an existing environment and refresh flag", {
  local_conf(topics = list(list(topic = "covid")))
  period_mocks(as.Date("2026-01-01"), as.Date("2026-03-01"))
  e <- new.env()
  e$topics_refresh_flag <- "kept"
  expect_identical(refresh_dashboard_data(e), e)
  expect_equal(e$topics_refresh_flag, "kept")
})

can_render_mocks <- function(fs = TRUE, series = TRUE, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    is_fs_running = function() fs,
    check_series_present = function() series,
    epipath = function(...) "/data/fs",
    .package = "episomer",
    .env = .env
  )
}

valid_input <- function(...) {
  modifyList(
    list(period = as.Date(c("2026-01-01", "2026-01-31")), topics = "covid"),
    list(...)
  )
}

test_that("can_render passes on a healthy setup", {
  can_render_mocks()
  expect_null(validation_message(can_render(valid_input(), NULL)))
  expect_null(validation_message(can_render(NULL, NULL)))
})

test_that("can_render reports a stopped database first", {
  can_render_mocks(fs = FALSE, series = FALSE)
  expect_match(validation_message(can_render(valid_input(), NULL)), "Embedded database service is not running")
})

test_that("can_render names the location it looked for aggregated data", {
  can_render_mocks(series = FALSE)
  expect_match(validation_message(can_render(valid_input(), NULL)), "No aggregated data found on  /data/fs", fixed = TRUE)
})

test_that("can_render validates the period and the topic", {
  can_render_mocks()
  expect_match(
    validation_message(can_render(valid_input(period = as.Date(c("2026-02-01", "2026-01-01"))), NULL)),
    "start period must be a date earlier"
  )
  expect_match(
    validation_message(can_render(valid_input(period = as.Date(c(NA, "2026-01-01"))), NULL)),
    "select a start and end period"
  )
  expect_equal(validation_message(can_render(valid_input(topics = ""), NULL)), "Please select a topic")
})

test_that("chart_not_empty rejects a chart without data and shows its title", {
  empty <- list(data = ggplot2::waiver(), labels = list(title = "No data for chart"))
  full <- list(data = data.frame(x = 1), labels = list(title = "Chart"))
  expect_equal(validation_message(chart_not_empty(empty)), "No data for chart")
  expect_null(validation_message(chart_not_empty(full)))
})

test_that("update_config_from_input copies each input into its property", {
  local_conf()
  input <- list(
    conf_collect_span = 30, conf_schedule_span = 60, conf_spark_cores = 2, conf_spark_memory = "4g",
    conf_onthefly_api = TRUE, geolocation_threshold = 5, conf_regions_disclaimer = "txt",
    conf_alpha = 0.025, conf_alpha_outlier = 0.05, conf_k_decay = 4, conf_history = 7,
    conf_same_weekday_baseline = FALSE, conf_with_bonferroni_correction = TRUE,
    conf_with_quotes = FALSE, conf_smtp_host = "smtp", conf_smtp_port = 25,
    conf_smtp_from = "a@b.c", conf_smtp_login = "me", conf_smtp_insecure = TRUE,
    conf_smtp_password = "pw", conf_admin_email = "admin@b.c", conf_force_date_format = "%Y"
  )
  update_config_from_input(input)
  expect_equal(conf$collect_span, 30)
  expect_equal(conf$spark_memory, "4g")
  expect_equal(conf$alert_alpha, 0.025)
  expect_equal(conf$alert_history, 7)
  expect_false(conf$alert_with_quotes)
  expect_equal(conf$smtp_host, "smtp")
  expect_equal(conf$admin_email, "admin@b.c")
  expect_equal(conf$force_date_format, "%Y")
})

test_that("get_alertsdb_html renders top posts as escaped nested lists", {
  df <- data.frame(id = 1:2)
  df$topposts <- list(list(en = list("a <b> post", "second")), list())
  testthat::local_mocked_bindings(get_alert_training_df = function() df, .package = "episomer")
  out <- get_alertsdb_html()
  expect_match(out$topposts[[1]], "<UL> <LI> en <OL> <LI>a &lt;b&gt; post</LI><LI>second</LI> </OL> </LI> </UL>", fixed = TRUE)
  expect_equal(out$topposts[[2]], "")
})

test_that("get_alertsdb_html reports when there are no alerts", {
  testthat::local_mocked_bindings(get_alert_training_df = function() NULL, .package = "episomer")
  expect_equal(validation_message(get_alertsdb_html()), "No alerts generated for the selected period")
})

test_that("get_alertsdb_runs_html formats numbers and line breaks", {
  runs <- data.frame(
    f1score = c(0.123456, 0.9), accuracy = c(0.5, 0.987654),
    precision_by_class = c("a\nb", "c"), sensitivity_by_class = c("x\ny", "z"),
    fscore_by_class = c("1\n2", "3")
  )
  runs$custom_parameters <- list(list(alpha = 0.1, k = 4), list())
  testthat::local_mocked_bindings(get_alert_training_runs_df = function() runs, .package = "episomer")
  out <- get_alertsdb_runs_html()
  expect_equal(trimws(out$f1score), c("0.123", "0.900"))
  expect_equal(out$precision_by_class, c("a<BR>b", "c"))
  expect_equal(out$fscore_by_class, c("1<BR>2", "3"))
  expect_match(out$custom_parameters[[1]], "<LI> alpha : 0.1 </LI>", fixed = TRUE)
  expect_equal(out$custom_parameters[[2]], "")
})
