# Starting the database on an address that is deliberately not the configured one
# (register_fs_runner_task(bind_host =, bind_port =), R/runners.R).
#
# The language model retraining of setup() needs a running database, and that database must not be
# taken for the real service by whatever waits for the real service to become healthy before it
# starts - `depends_on: service_healthy` on search and detect in docker/compose.yml, a readiness
# probe in Kubernetes. So it is bound elsewhere, and the address travels as command line arguments
# all the way to the JVM instead of being written to the shared properties.json.

test_that("register_fs_runner_task passes the bind address to the runner script", {
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  withr::defer(clear_fs_endpoint_override())
  got <- NULL
  local_mocked_bindings(
    register_runner_task = function(task_name, runner_args = character(0)) {
      got <<- list(task_name = task_name, runner_args = runner_args)
      invisible(NULL)
    },
    # the service is not really starting, so the wait has to be answered
    is_fs_running = function() TRUE,
    .package = "episomer"
  )

  expect_message(
    register_fs_runner_task(
      wait_until_response = TRUE,
      bind_host = "localhost",
      bind_port = 9080
    ),
    "fs is running"
  )
  expect_equal(got$task_name, "fs")
  # 'key' 'value' pairs, the shape Episomer.main() reads its own arguments in. The scheme is
  # stripped: Http().newServerAt() takes an interface, not a URL.
  expect_equal(got$runner_args, c("bindHost", "localhost", "bindPort", "9080"))
})

test_that("register_fs_runner_task passes nothing when no bind address is asked for", {
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  got <- "not called"
  local_mocked_bindings(
    register_runner_task = function(task_name, runner_args = character(0)) {
      got <<- runner_args
      invisible(NULL)
    },
    is_fs_running = function() TRUE,
    .package = "episomer"
  )

  expect_message(
    register_fs_runner_task(wait_until_response = TRUE),
    "fs is running"
  )
  expect_equal(got, character(0))
  # and the endpoint resolution is left exactly as it was
  expect_null(fs_endpoint_override())
})

test_that("register_fs_runner_task looks for the service on the address it asked for", {
  # The order matters: the wait is is_fs_running(), i.e. a GET on get_scala_ping_url(). Asking the
  # configured address about a database bound somewhere else would wait for ever.
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  withr::defer(clear_fs_endpoint_override())
  seen <- character(0)
  local_mocked_bindings(
    register_runner_task = function(...) invisible(NULL),
    is_fs_running = function() {
      seen <<- c(seen, get_scala_ping_url())
      TRUE
    },
    .package = "episomer"
  )

  expect_message(
    register_fs_runner_task(
      wait_until_response = TRUE,
      bind_host = "localhost",
      bind_port = 9080
    ),
    "fs is running"
  )
  expect_equal(seen, "http://127.0.0.1:9080/ping")
})

test_that("register_fs_runner_task gives up with an error on a service that never answers", {
  # The wait used to be unbounded. That is survivable for an operator watching a console and not at
  # all for an unattended deployment: a database that cannot bind its address - a Kubernetes
  # service name resolving to a cluster IP the pod does not hold, see Settings.fsBindHost - never
  # answers at all, and setup() then hung on a growing row of dots with nothing to read.
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  withr::defer(clear_fs_endpoint_override())
  local_mocked_bindings(
    register_runner_task = function(...) invisible(NULL),
    is_fs_running = function() FALSE,
    .package = "episomer"
  )

  expect_error(
    suppressMessages(register_fs_runner_task(
      wait_until_response = TRUE,
      bind_host = "localhost",
      bind_port = 9080,
      timeout = 0
    )),
    "did not answer on http://127.0.0.1:9080/ping"
  )
})

test_that("register_runner_task appends the extra arguments after the data directory", {
  data_dir <- local_lease_conf(force_date_format = "")
  dir.create(file.path(data_dir, "jobs"), showWarnings = FALSE)
  got <- NULL
  local_mocked_bindings(
    # Only the command line is under test: nothing must be copied, scheduled or spawned.
    stop_if_no_config = function(...) invisible(NULL),
    .package = "episomer"
  )
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      got <<- list(command = command, args = args)
      invisible(0)
    },
    .package = "base"
  )
  skip_on_os("windows")

  suppressMessages(register_runner_task(
    "fs",
    runner_args = c("bindHost", "localhost", "bindPort", "9080")
  ))

  expect_equal(got$command, "Rscript")
  expect_equal(
    got$args,
    c(
      file.path(data_dir, "jobs", "fs.R"),
      data_dir,
      "bindHost",
      "localhost",
      "bindPort",
      "9080"
    )
  )
})

# --------------------------------------------------------------------------------------------------
# The guarantee that matters most: starting a database on another address must leave properties.json
# alone. What this replaced wrote the shifted port there and relied on a finally to put it back.
