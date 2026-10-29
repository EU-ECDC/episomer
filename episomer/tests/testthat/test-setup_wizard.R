# ---- the prompt helpers -------------------------------------------------------------------------

menu <- function() list("1" = "first", "2" = "second", "3" = "third")

with_answers <- function(answers, code, .env = parent.frame()) {
  i <- 0
  testthat::local_mocked_bindings(
    readline = function(...) {
      i <<- i + 1
      answers[[i]]
    },
    .package = "base",
    .env = .env
  )
  testthat::local_mocked_bindings(pause = function(...) NULL, .package = "episomer", .env = .env)
  force(code)
}

ask <- function(..., silent = FALSE, default = NULL) {
  out <- NULL
  utils::capture.output(
    out <- suppressMessages(choose_between("Pick one", menu(), silent, default)),
    type = "output"
  )
  out
}

test_that("choose_between returns the default without asking when silent", {
  testthat::local_mocked_bindings(
    readline = function(...) stop("must not ask"),
    .package = "base"
  )
  testthat::local_mocked_bindings(pause = function(...) NULL, .package = "episomer")
  expect_equal(ask(silent = TRUE, default = "2"), "2")
})

test_that("choose_between returns the typed option", {
  with_answers("3", expect_equal(ask(default = "1"), "3"))
})

test_that("choose_between returns the default on an empty answer", {
  with_answers("", expect_equal(ask(default = "2"), "2"))
})

test_that("choose_between asks again after an unknown option", {
  with_answers(c("9", "x", "2"), expect_equal(ask(default = "1"), "2"))
})

test_that("choose_between works without a default", {
  with_answers(c("", "1"), expect_equal(ask(), "1"))
  with_answers("3", expect_equal(ask(), "3"))
})

test_that("choose_between has nothing to ask when there is no option", {
  testthat::local_mocked_bindings(
    readline = function(...) stop("must not ask"),
    .package = "base"
  )
  expect_null(suppressMessages(choose_between("Pick one", list(), FALSE, "1")))
})

test_that("choose_between marks the default option in the menu", {
  with_answers("", {
    msgs <- character()
    withCallingHandlers(
      utils::capture.output(choose_between("Pick one", menu(), FALSE, "2")),
      message = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
  })
  menu_msg <- grep("\\.\\.\\.\\. 1 <- first", msgs, value = TRUE)
  # the arrow itself is locale dependent: only the marker around it is checked
  expect_match(menu_msg, "\\.\\.\\.\\. 2 <- second \\(default")
  expect_false(grepl("first \\(default", menu_msg))
})

test_that("the text styling helpers keep the text", {
  expect_match(question("Why"), "Why", fixed = TRUE)
  expect_match(choice("a"), "a", fixed = TRUE)
  expect_match(choice("b", bold = TRUE), "b", fixed = TRUE)
  expect_match(alert("Careful"), "Careful", fixed = TRUE)
})

test_that("pause only sleeps when not silent", {
  slept <- 0
  testthat::local_mocked_bindings(Sys.sleep = function(t) slept <<- slept + t, .package = "base")
  pause(TRUE)
  expect_equal(slept, 0)
  pause(FALSE)
  expect_equal(slept, 0.5)
})

# ---- setup() ------------------------------------------------------------------------------------

# setup() reads the vector files of the configured languages before the menu
setup_conf <- function(..., .local_envir = parent.frame()) {
  local_conf(
    languages = list(list(code = "en", name = "English", vectors = "/nonexistent/en.txt.gz")),
    ...,
    .local_envir = .local_envir
  )
}

# Everything setup() reaches out to is replaced: the wizard is exercised on its own decisions, with
# `answers` giving the option picked for each question, matched on the start of the question text.
setup_mocks <- function(answers, .env = parent.frame()) {
  seen <- new.env()
  seen$config_dir <- NULL
  seen$saved <- 0
  seen$stopped <- 0
  seen$questions <- character()
  seen$app <- NULL
  seen$fs_registered <- 0
  testthat::local_mocked_bindings(
    setup_can_run = function() TRUE,
    setup_config = function(data_dir, ...) seen$config_dir <- data_dir,
    apply_env_languages = function() invisible(NULL),
    check_java_deps = function() TRUE,
    check_winutils = function() TRUE,
    check_geonames = function() TRUE,
    check_sm_auth = function() TRUE,
    get_lang_traing_need = function() FALSE,
    get_geonames_txt_path = function() "/nonexistent/allCountries.txt",
    get_geonames_parquet_path = function() "/nonexistent/geonames.parquet",
    save_config = function(...) seen$saved <- seen$saved + 1,
    stop_all_runner_tasks = function() seen$stopped <- seen$stopped + 1,
    # never the real one: it would start the database of whoever runs the tests
    register_fs_runner_task = function(...) seen$fs_registered <- seen$fs_registered + 1,
    is_fs_running = function() FALSE,
    choose_between = function(question, ops = list(), silent = FALSE, default = NULL) {
      seen$questions <- c(seen$questions, question)
      for (k in names(answers)) if (startsWith(question, k)) return(answers[[k]])
      default
    },
    episomer_app = function(...) {
      seen$app <- list(...)
      "app"
    },
    .package = "episomer",
    .env = .env
  )
  testthat::local_mocked_bindings(pause = function(...) NULL, .package = "episomer", .env = .env)
  seen
}

quiet_setup <- function(...) suppressMessages(setup(...))

test_that("setup stops when the requirements are not met", {
  setup_conf()
  seen <- setup_mocks(list())
  testthat::local_mocked_bindings(setup_can_run = function() FALSE, .package = "episomer")
  expect_error(quiet_setup(withr::local_tempdir()), "Missing requirements")
  expect_null(seen$config_dir)
})

test_that("setup creates the data directory and configures it without a remote fs", {
  setup_conf()
  seen <- setup_mocks(list("Which action" = "9"))
  dir <- file.path(withr::local_tempdir(), "new", "data")
  quiet_setup(dir)
  expect_true(dir.exists(dir))
  expect_equal(seen$config_dir, dir)
})

test_that("silent setup reuses the data dir of the session, then EPI_HOME, then fails", {
  setup_conf(data_dir = "/session/dir")
  seen <- setup_mocks(list("Which action" = "9"))
  testthat::local_mocked_bindings(dir.create = function(...) invisible(TRUE), .package = "base")
  withr::local_envvar(EPI_HOME = "/env/dir")
  quiet_setup(silent = TRUE)
  expect_equal(seen$config_dir, "/session/dir")

  rm("data_dir", envir = conf)
  quiet_setup(silent = TRUE)
  expect_equal(seen$config_dir, "/env/dir")

  withr::local_envvar(EPI_HOME = "")
  expect_error(quiet_setup(silent = TRUE), "cannot determine the data_folder")
})

test_that("setup offers to keep the current data folder and uses it", {
  setup_conf(data_dir = "/session/dir")
  seen <- setup_mocks(list("Do you want to keep" = "1", "Which action" = "9"))
  testthat::local_mocked_bindings(dir.create = function(...) invisible(TRUE), .package = "base")
  quiet_setup()
  expect_equal(seen$config_dir, "/session/dir")
  expect_equal(seen$questions[[1]], "Do you want to keep current data folder?")
})

test_that("option 9 exits without stopping the services but saves the config", {
  setup_conf()
  seen <- setup_mocks(list("Which action" = "9"))
  expect_null(quiet_setup(withr::local_tempdir()))
  expect_equal(seen$stopped, 0)
  expect_equal(seen$saved, 1)
  expect_equal(seen$questions, "Which action would you like perform?")
})

test_that("options 6, 7 and 8 do not stop the running services", {
  setup_conf()
  seen <- setup_mocks(list("Which action" = "8", "Do you want to open" = "4"))
  quiet_setup(withr::local_tempdir())
  expect_equal(seen$stopped, 0)
})

# interactive() is TRUE in Positron and FALSE under Rscript: the tests say which one they mean
local_interactive <- function(value, .env = parent.frame()) {
  testthat::local_mocked_bindings(is_interactive = function() value, .package = "episomer", .env = .env)
}

test_that("option 8 refuses to open the app when the database is not running and cannot be asked", {
  setup_conf()
  seen <- setup_mocks(list("Which action" = "8", "Do you want to open" = "1"))
  local_interactive(FALSE)
  expect_error(quiet_setup(withr::local_tempdir()), "database must be started")
  expect_equal(seen$fs_registered, 0)
})

test_that("option 8 refuses to open the app when the user declines to start the database", {
  setup_conf()
  seen <- setup_mocks(list(
    "Which action" = "8", "Do you want to open" = "1", "The episomer database must" = "2"
  ))
  local_interactive(TRUE)
  expect_error(quiet_setup(withr::local_tempdir()), "database must be started")
  expect_equal(seen$fs_registered, 0)
})

test_that("option 8 starts the database when the user agrees, then opens the app", {
  setup_conf()
  seen <- setup_mocks(list(
    "Which action" = "8", "Do you want to open" = "1", "The episomer database must" = "1"
  ))
  local_interactive(TRUE)
  running <- FALSE
  testthat::local_mocked_bindings(
    is_fs_running = function() running,
    register_fs_runner_task = function(...) {
      seen$fs_registered <- seen$fs_registered + 1
      running <<- TRUE
    },
    .package = "episomer"
  )
  expect_equal(quiet_setup(withr::local_tempdir()), "app")
  expect_equal(seen$fs_registered, 1)
  expect_gte(seen$saved, 2)
})

test_that("option 8 opens the app profile that was chosen", {
  for (case in list(
    list(ans = "1", profile = NULL, msg = "full mode"),
    list(ans = "2", profile = "dashboard", msg = "dashboard"),
    list(ans = "3", profile = "admin", msg = "admin")
  )) {
    setup_conf()
    seen <- setup_mocks(list("Which action" = "8", "Do you want to open" = case$ans))
    testthat::local_mocked_bindings(is_fs_running = function() TRUE, .package = "episomer")
    app <- quiet_setup(withr::local_tempdir())
    expect_equal(app, "app", info = case$ans)
    expect_equal(seen$app$profile, case$profile, info = case$ans)
  }
})

test_that("option 8 with the do nothing answer opens no app", {
  setup_conf()
  seen <- setup_mocks(list("Which action" = "8", "Do you want to open" = "4"))
  testthat::local_mocked_bindings(is_fs_running = function() TRUE, .package = "episomer")
  expect_null(quiet_setup(withr::local_tempdir()))
  expect_null(seen$app)
})

test_that("option 8 explains where it looked when the database does not answer after starting", {
  setup_conf(fs_port = 8080)
  seen <- setup_mocks(list(
    "Which action" = "8", "Do you want to open" = "1", "The episomer database must" = "1"
  ))
  local_interactive(TRUE)
  testthat::local_mocked_bindings(
    get_scala_ping_url = function() "http://localhost:8080/ping",
    .package = "episomer"
  )
  msgs <- character()
  app <- withCallingHandlers(
    setup(withr::local_tempdir()),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_null(app)
  expect_equal(seen$fs_registered, 1)
  expect_match(paste(msgs, collapse = "\n"), "did not answer on http://localhost:8080/ping")
  expect_match(paste(msgs, collapse = "\n"), "EPI_HOST=http://localhost:8080", fixed = TRUE)
})
