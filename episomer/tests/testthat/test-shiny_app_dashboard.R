# The dashboard "Run" flow: clicking Run computes every dataset once and stores
# the result in a snapshot (a reactiveVal) that the dashboard outputs read. The
# tests below pin the three properties that flow relies on:
#   - nothing renders before the first Run,
#   - the outputs read the snapshot, not the live filters,
#   - a can_render() failure still reaches the user (validate() raised inside an
#     observer would otherwise be swallowed silently).

dashboard_fake_chart <- function(title = "chart") {
  df <- data.frame(x = 1:3, y = 1:3)
  list(
    chart = ggplot2::ggplot(df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_line() +
      ggplot2::labs(title = title, subtitle = "subtitle", caption = "caption"),
    data = df
  )
}

# Builds the dashboard app with every external dependency mocked away, so the
# tests exercise the reactive wiring only. `fs_running = FALSE` makes
# can_render() fail on its first need().
local_dashboard_app <- function(fs_running = TRUE, .env = parent.frame()) {
  dir.create("R", showWarnings = FALSE)
  file.create(file.path(getwd(), "R", "dummy.R"))

  local_mocked_bindings(
    get_episomer_kr_password = function() "test_password_123",
    is_secret_set = function(secret) FALSE,
    get_secret = function(secret) "test_secret_value",
    set_secret = function(secret, value) TRUE,
    get_key_ring = function(backend = NULL) {
      list(
        keyring_list = function() data.frame(keyring = character()),
        keyring_create = function(...) TRUE,
        keyring_set_default = function(...) TRUE,
        keyring_is_locked = function(...) FALSE,
        keyring_unlock = function(...) TRUE,
        set_with_value = function(...) TRUE,
        list = function(...) data.frame(username = character())
      )
    },
    setup_config = function(...) TRUE,
    fs_answers_ping = function(...) TRUE,
    refresh_dashboard_data = function(e = new.env(), fixed_period = NULL) {
      e$sms <- c(Bluesky = "bluesky")
      e$topics <- c("", covid = "covid", flu = "flu")
      e$countries <- c(France = 1L)
      e$fixed_period <- "custom"
      e$date_min <- "2026-01-01"
      e$date_max <- "2026-01-31"
      e$date_start <- "2026-01-01"
      e$date_end <- "2026-01-31"
      if (!exists("topics_refresh_flag", envir = e)) {
        e$topics_refresh_flag <- shiny::reactiveVal(0)
      }
      e
    },
    get_topics_alphas = function() c(covid = 0.05, flu = 0.05),
    get_topics_alpha_outliers = function() c(covid = 0.05, flu = 0.05),
    get_topics_labels = function() c(covid = "Covid-19", flu = "Influenza"),
    is_fs_running = function() fs_running,
    check_series_present = function(...) TRUE,
    line_chart_from_filters = function(...) dashboard_fake_chart("line"),
    map_chart_from_filters = function(...) dashboard_fake_chart("map"),
    top_chart_from_filters = function(sms, topics, type, ...) {
      if (identical(type, "urls")) {
        df <- data.frame(top = c("http://a", "http://b"), frequency = c(2L, 1L))
        list(
          chart = ggplot2::ggplot(
            df,
            ggplot2::aes(x = .data$top, y = .data$frequency)
          ) +
            ggplot2::geom_col() +
            ggplot2::labs(title = "urls"),
          data = df
        )
      } else {
        dashboard_fake_chart(paste("top", type))
      }
    },
    .package = "episomer",
    .env = .env
  )

  local_conf(
    alert_alpha = .1,
    alert_alpha_outlier = .1,
    alert_k_decay = 1,
    .local_envir = .env
  )

  episomer_app(data_dir = getwd(), profile = "dashboard")
}

# Applies the filter values the dashboard reads when Run is clicked.
set_dashboard_filters <- function(session, topics = "covid") {
  session$setInputs(
    sms = "bluesky",
    topics = topics,
    countries = 1L,
    period_type = "day",
    fixed_period = "custom",
    period = c("2026-01-01", "2026-01-31"),
    with_quotes = FALSE,
    alpha_filter = .05,
    alpha_outlier_filter = .05,
    k_decay_filter = 1,
    history_filter = 7,
    bonferroni_correction = FALSE,
    same_weekday_baseline = FALSE
  )
}

withr::with_tempdir({
  test_that("dashboard outputs stay empty until Run is clicked", {
    app <- local_dashboard_app()

    shiny::testServer(app, {
      set_dashboard_filters(session)

      # req(snapshot) keeps every output silent, without erroring the session
      for (out in c("line_chart", "map_chart", "top_chart1", "top_chart2", "top_table")) {
        expect_error(session$getOutput(out), class = "shiny.silent.error")
      }
    })
  })
})

withr::with_tempdir({
  test_that("Run renders every dashboard output from a single snapshot", {
    app <- local_dashboard_app()

    shiny::testServer(app, {
      set_dashboard_filters(session)
      session$setInputs(run_dashboard = 1)

      expect_no_error(session$getOutput("line_chart"))
      expect_no_error(session$getOutput("map_chart"))
      expect_no_error(session$getOutput("top_chart1"))
      expect_no_error(session$getOutput("top_chart2"))
      # DT renders server-side, so the payload carries the columns, not the rows
      expect_match(session$getOutput("top_table"), "Url", fixed = TRUE)
      expect_match(session$getOutput("top_table"), "Frequency", fixed = TRUE)
      expect_match(session$getOutput("top_table_title"), "Covid-19", fixed = TRUE)
    })
  })
})

withr::with_tempdir({
  test_that("changing a filter without clicking Run does not desynchronise the table title", {
    app <- local_dashboard_app()

    shiny::testServer(app, {
      set_dashboard_filters(session, topics = "covid")
      session$setInputs(run_dashboard = 1)
      expect_match(session$getOutput("top_table_title"), "Covid-19", fixed = TRUE)

      # the title reads the snapshot, so it keeps describing the displayed table
      session$setInputs(topics = "flu")
      expect_match(session$getOutput("top_table_title"), "Covid-19", fixed = TRUE)
      expect_no_match(session$getOutput("top_table_title"), "Influenza", fixed = TRUE)

      # and follows the new topic once Run is clicked again
      session$setInputs(run_dashboard = 2)
      expect_match(session$getOutput("top_table_title"), "Influenza", fixed = TRUE)
    })
  })
})

withr::with_tempdir({
  test_that("can_render() failures are reported in the dashboard outputs", {
    app <- local_dashboard_app(fs_running = FALSE)

    shiny::testServer(app, {
      set_dashboard_filters(session)
      session$setInputs(run_dashboard = 1)

      # validate() raised inside the Run observer would be swallowed silently:
      # the message has to be replayed by the outputs themselves
      for (out in c("line_chart", "map_chart", "top_table", "top_table_title")) {
        expect_error(
          session$getOutput(out),
          "Embedded database service is not running",
          class = "shiny.silent.error"
        )
      }
    })
  })
})
