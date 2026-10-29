# refresh_config_data() with the readers of the app state mocked: what is under test is the
# bookkeeping (flags, change detection on file mtimes, tables built for the config page).

local_config_data <- function(limit_langs = list(), .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  local_conf(
    data_dir = dir, fs_remote = NULL,
    languages = list(
      list(name = "English", code = "en"),
      list(name = "French", code = "fr")
    ),
    .local_envir = .env
  )
  state <- new.env()
  state$topics_calls <- 0
  state$tasks <- list(
    alerts = list(
      task = "alerts", order = 1, status = "success",
      scheduled_for = as.POSIXct("2026-01-02 10:00:00", tz = "UTC"),
      started_on = as.POSIXct("2026-01-02 10:00:01", tz = "UTC"),
      end_on = as.POSIXct("2026-01-02 10:00:09", tz = "UTC"),
      message = "all good"
    ),
    geonames = list(
      task = "geonames", order = 0, status = "running",
      scheduled_for = NA, started_on = NA, end_on = NA
    )
  )
  testthat::local_mocked_bindings(
    setup_config = function(...) invisible(NULL),
    # system.file() cannot resolve the package name under load_all(), and the defaults are not wanted
    get_default_topics_path = function() file.path(dir, "no-default-topics.xlsx"),
    get_available_languages = function() {
      data.frame(
        Code = c("en", "fr"), Label = c("English", "French"),
        `Full Label` = c("English (en)", "French (fr)"), check.names = FALSE
      )
    },
    get_topics_df = function() {
      state$topics_calls <- state$topics_calls + 1
      data.frame(Topics = "covid")
    },
    is_detect_running = function() TRUE,
    is_fs_running = function() FALSE,
    is_search_running = function() TRUE,
    missing_search_jobs = function() character(0),
    last_search_time = function() Sys.time() - 5,
    runner_leases_df = function() data.frame(runner = "detect"),
    get_tasks = function(...) state$tasks,
    in_pending_status = function(task) task$task == "geonames",
    .package = "episomer",
    .env = .env
  )
  state
}

test_that("refresh_config_data creates the refresh flags once and keeps them", {
  local_config_data()
  e <- refresh_config_data(limit = list())
  flags <- c(
    "properties", "process", "subscribers", "users", "geotraining",
    "alert_training", "countries"
  )
  for (f in flags) {
    expect_true(inherits(e[[paste0(f, "_refresh_flag")]], "reactiveVal"), info = f)
  }
  kept <- e$users_refresh_flag
  expect_identical(refresh_config_data(e, limit = list())$users_refresh_flag, kept)
})

test_that("refresh_config_data only touches the requested blocks", {
  local_config_data()
  e <- refresh_config_data(limit = list("langs"))
  expect_true(exists("langs", e))
  expect_false(exists("tasks", e))
  expect_false(exists("topics_df", e))
})

test_that("the langs block exposes the language lookups and the configured languages", {
  local_config_data()
  e <- refresh_config_data(limit = list("langs"))
  expect_equal(e$lang_items, c(`English (en)` = "en", `French (fr)` = "fr"))
  expect_equal(e$lang_names, c(en = "English", fr = "French"))
  expect_equal(e$langs, data.frame(Language = c("English", "French"), Code = c("en", "fr")))
  expect_equal(shiny::isolate(e$langs_refresh_flag()), 0)
})

test_that("the langs flag follows the tasks file, then the properties file", {
  local_config_data()
  e <- refresh_config_data(limit = list("langs"))
  writeLines("{}", get_properties_path())
  props_time <- file.mtime(get_properties_path())
  refresh_config_data(e, limit = list("langs"))
  expect_equal(as.numeric(shiny::isolate(e$langs_refresh_flag())), as.numeric(props_time))

  writeLines("{}", get_tasks_path())
  Sys.setFileTime(get_tasks_path(), props_time + 100)
  refresh_config_data(e, limit = list("langs"))
  expect_equal(as.numeric(shiny::isolate(e$langs_refresh_flag())), as.numeric(props_time) + 100)
})

test_that("the topics block loads the topics once, then only when a file changes", {
  state <- local_config_data()
  e <- refresh_config_data(limit = list("topics"))
  expect_equal(state$topics_calls, 1)
  expect_equal(e$topics_df, data.frame(Topics = "covid"))

  refresh_config_data(e, limit = list("topics"))
  expect_equal(state$topics_calls, 1)

  writeLines("{}", get_user_topics_path())
  refresh_config_data(e, limit = list("topics"))
  expect_equal(state$topics_calls, 2)
  refresh_config_data(e, limit = list("topics"))
  expect_equal(state$topics_calls, 2)

  writeLines("{}", get_plans_path())
  refresh_config_data(e, limit = list("topics"))
  expect_equal(state$topics_calls, 3)
})

test_that("the tasks block publishes the services status and the runner table", {
  local_config_data()
  e <- refresh_config_data(limit = list("tasks"))
  expect_true(e$detect_running)
  expect_false(e$fs_running)
  expect_true(e$search_running)
  expect_s3_class(e$fs_since, "POSIXct")
  expect_equal(e$missing_search_jobs, character(0))
  expect_equal(as.numeric(e$search_diff), 5, tolerance = 2)
  expect_equal(e$runner_leases_df, data.frame(runner = "detect"))
})

test_that("the tasks table is sorted by order and formatted for display", {
  local_config_data()
  e <- refresh_config_data(limit = list("tasks"))
  expect_equal(names(e$tasks), c("geonames", "alerts"))
  expect_equal(row.names(e$tasks_df), c("0", "1"))
  expect_equal(e$tasks_df$Task, c("geonames", "alerts"))
  # a task the user asked to run shows as pending whatever its stored status is
  expect_equal(e$tasks_df$Status, c("pending", "success"))
  expect_equal(e$tasks_df$Message, c("", "all good"))
  expect_match(e$tasks_df$Scheduled[[2]], "^2026-01-02 \\d{2}:00:00")
  expect_true(is.na(e$tasks_df$Last.End[[1]]))
})

test_that("the tasks table is rebuilt only when tasks.json changes", {
  state <- local_config_data()
  writeLines("{}", get_tasks_path())
  e <- refresh_config_data(limit = list("tasks"))
  state$tasks$alerts$message <- "changed"
  refresh_config_data(e, limit = list("tasks"))
  expect_equal(e$tasks_df$Message, c("", "all good"))

  Sys.setFileTime(get_tasks_path(), Sys.time() + 100)
  refresh_config_data(e, limit = list("tasks"))
  expect_equal(e$tasks_df$Message, c("", "changed"))
})

test_that("the tasks block copes with tasks.json appearing after the first refresh", {
  state <- local_config_data()
  e <- refresh_config_data(limit = list("tasks"))
  writeLines("{}", get_tasks_path())
  state$tasks$alerts$message <- "after"
  expect_no_error(refresh_config_data(e, limit = list("tasks")))
  expect_equal(e$tasks_df$Message, c("", "after"))
})

test_that("refresh_config_data does not make its caller depend on the flags it writes", {
  local_config_data()
  e <- new.env()
  runs <- 0
  obs <- shiny::observe({
    runs <<- runs + 1
    refresh_config_data(e, limit = list("langs", "topics", "tasks"))
  })
  withr::defer(obs$destroy())
  shiny:::flushReact()
  expect_equal(runs, 1)
  # the files move: the flags are written, and the observer must not be invalidated by it
  writeLines("{}", get_tasks_path())
  writeLines("{}", get_user_topics_path())
  refresh_config_data(e, limit = list("langs", "topics", "tasks"))
  shiny:::flushReact()
  expect_equal(runs, 1)
})
