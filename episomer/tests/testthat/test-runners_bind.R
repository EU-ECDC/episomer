# Starting the database on an address that is deliberately not the configured one
# (register_fs_runner_task(bind_host =, bind_port =), R/runners.R).
#
# The language model retraining of setup() needs a running database, and that database must not be
# taken for the real service by whatever waits for the real service to become healthy before it
# starts - `depends_on: service_healthy` on search and detect in docker/compose.yml, a readiness
# probe in Kubernetes. So it is bound elsewhere, and the address travels as command line arguments
# all the way to the JVM instead of being written to the shared properties.json.
#
# What this replaces shifted conf$fs_port by 1000, saved it, and undid it in the `finally` of a
# tryCatch(). A process killed outright never runs a finally, so the shifted port stayed in the
# configuration file and the next start shifted it again: 8080, 9080, 10080, 11080.

test_that("register_fs_runner_task passes the bind address to the runner script", {
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  withr::defer(clear_fs_endpoint_override())
  got <- NULL
  local_mocked_bindings(
    register_runner_task = function(task_name, runner_args = character(0), attached = FALSE) {
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
    register_runner_task = function(task_name, runner_args = character(0), attached = FALSE) {
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
  # service name resolving to a cluster IP the pod does not hold - never answers at all, and
  # setup() then hung on a growing row of dots with nothing to read.
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
  # shQuote()d: system2() quotes its `command` and not its `args`, and the first two are paths. The
  # default Windows data directory lives under "C:/Users/<given name> <family name>", so unquoted
  # they were split at the space and Rscript got "C:/Users/Yohann" as the script to run.
  expect_equal(
    got$args,
    shQuote(c(
      file.path(data_dir, "jobs", "fs.R"),
      data_dir,
      "bindHost",
      "localhost",
      "bindPort",
      "9080"
    ))
  )
  # every argument survives a directory holding a space, which is the case that failed
  expect_length(got$args, 6L)
})

test_that("register_runner_task quotes a data directory that holds a space", {
  data_dir <- file.path(withr::local_tempdir(), "Yohann Mansiaux", "EPI HOME")
  dir.create(file.path(data_dir, "jobs"), recursive = TRUE, showWarnings = FALSE)
  local_lease_conf(force_date_format = "")
  conf$data_dir <- data_dir
  got <- NULL
  local_mocked_bindings(
    stop_if_no_config = function(...) invisible(NULL),
    .package = "episomer"
  )
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      got <<- args
      invisible(0)
    },
    .package = "base"
  )
  skip_on_os("windows")

  suppressMessages(register_runner_task(
    "fs",
    runner_args = c("bindHost", "localhost", "bindPort", "9080"),
    attached = TRUE
  ))

  # six arguments, not the nine a split on the two spaces would have produced
  expect_length(got, 6L)
  # and the child sees the directory whole once the shell has stripped the quotes
  expect_equal(got[[2]], shQuote(data_dir))
})

test_that("rscript_exe_attached() is not quoted, because system2 quotes it", {
  # Quoting it here gets it quoted twice: measured on a default "C:/Program Files/R" install, that
  # fails with '"\"C:\PROGRA~1\...\Rscript.exe\""' not found and a return code of 127.
  exe <- rscript_exe_attached()
  expect_length(exe, 1L)
  expect_false(grepl('^"', exe))
  expect_false(grepl("'", exe, fixed = TRUE))
})

# The R the Scala side runs ------------------------------------------------------------------------
#
# The fs API starts the secret scripts of jobs/ through a ProcessBuilder, and it used to name the
# program "Rscript", i.e. whatever the PATH offered. Where that was another R installation the sub
# process died on "there is no package called 'episomer'" and the /secret endpoint answered 406,
# reporting a keyring failure for something that had not reached the keyring.

test_that("rscript_exe_for_scala() names the executable of this R", {
  exe <- rscript_exe_for_scala()

  expect_length(exe, 1L)
  expect_true(file.exists(exe))
  # this R, not one found on the PATH
  expect_equal(normalizePath(dirname(exe)), normalizePath(R.home("bin")))
})

test_that("rscript_exe_for_scala() is not quoted, because ProcessBuilder takes it literally", {
  # no shell sits between the JVM and the program, so a quote would become part of the file name
  exe <- rscript_exe_for_scala()

  expect_false(grepl('"', exe, fixed = TRUE))
  expect_false(grepl("'", exe, fixed = TRUE))
})

test_that("set_scala_r_env() hands the JVM the executable and the library paths", {
  # the JVM inherits this environment through system(), and ProcessBuilder copies it into the sub
  # process, so both reach the Rscript running get_secret.R two levels down
  withr::local_envvar(EPISOMER_RSCRIPT = NA, R_LIBS = NA)

  set_scala_r_env()

  expect_equal(Sys.getenv("EPISOMER_RSCRIPT"), rscript_exe_for_scala())
  expect_equal(
    Sys.getenv("R_LIBS"),
    paste(.libPaths(), collapse = .Platform$path.sep)
  )
})

test_that("an R sub process started with that environment gets those library paths", {
  skip_on_cran()
  # The point of the whole thing, and the reason it is R_LIBS rather than R_LIBS_USER: R_LIBS is
  # the one an R sub process prepends to .libPaths() whatever else it reads, so the Rscript the
  # JVM starts two levels down searches the libraries this session searched - which is where
  # episomer is. Run for real, because "which variable does R honour" is not something a mock can
  # answer.

  skip_on_ci()
  withr::local_envvar(EPISOMER_RSCRIPT = NA, R_LIBS = NA)
  set_scala_r_env()

  got <- suppressWarnings(system2(
    Sys.getenv("EPISOMER_RSCRIPT"),
    c("-e", shQuote("cat(.libPaths(), sep = '\n')")),
    stdout = TRUE,
    stderr = FALSE
  ))
  skip_if(!is.null(attr(got, "status")), "could not run the sub process")

  expect_true(all(normalizePath(.libPaths()) %in% normalizePath(got)))
})

# --------------------------------------------------------------------------------------------------
# The guarantee that matters most: starting a database on another address must leave properties.json
# alone. What this replaced wrote the shifted port there and relied on a finally to put it back.

test_that("register_fs_runner_task writes nothing to properties.json", {
  data_dir <- local_lease_conf(
    fs_url = "http://fs",
    fs_port = 8080,
    fs_setup_host = "localhost",
    fs_setup_port = 9080
  )
  withr::defer(clear_fs_endpoint_override())
  local_mocked_bindings(
    register_runner_task = function(...) invisible(NULL),
    is_fs_running = function() TRUE,
    .package = "episomer"
  )
  props <- file.path(data_dir, "properties.json")

  expect_message(
    register_fs_runner_task(
      wait_until_response = TRUE,
      bind_host = conf$fs_setup_host,
      bind_port = conf$fs_setup_port
    ),
    "fs is running"
  )

  # nothing saved at all, and the port in conf is untouched
  expect_false(file.exists(props))
  expect_identical(conf$fs_port, 8080)
  expect_identical(conf$fs_url, "http://fs")
})

test_that("save_config while an override is active keeps the configured port", {
  # The override lives in conf, next to fs_port, so the question is whether a save_config() called
  # by anything else during the retraining would carry it to disk. It cannot: save_config() writes
  # an explicit list of properties, and the override is not one of them.
  data_dir <- local_lease_conf()
  withr::defer(clear_fs_endpoint_override())
  suppressMessages(setup_config(data_dir = data_dir, fs_remote = NULL, ignore_keyring = TRUE))
  set_fs_endpoint_override("localhost", 9080)

  suppressMessages(save_config(data_dir = data_dir))
  saved <- jsonlite::read_json(file.path(data_dir, "properties.json"))

  # expect_equal, not identical: jsonlite reads the port back as an integer
  expect_equal(saved$fs_port, 8080)
  expect_identical(saved$fs_url, "http://localhost")
  expect_null(saved$fs_endpoint_override)
  # and the override is still in force for this process
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:9080")
})

# --------------------------------------------------------------------------------------------------
# wait_until_response used to be accepted and ignored: the function waited whatever the caller
# asked for, so "start it and let me carry on" could not be expressed.

test_that("register_fs_runner_task returns straight away when not asked to wait", {
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  asked <- 0L
  local_mocked_bindings(
    register_runner_task = function(...) invisible(NULL),
    is_fs_running = function() {
      asked <<- asked + 1L
      FALSE
    },
    .package = "episomer"
  )

  # is_fs_running() would answer FALSE for ever, so returning at all is the assertion
  expect_silent(register_fs_runner_task())
  expect_identical(asked, 0L)
})

test_that("register_fs_runner_task waits on the temporary port when asked to", {
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  withr::defer(clear_fs_endpoint_override())
  seen <- character(0)
  answers <- c(FALSE, FALSE, TRUE)
  local_mocked_bindings(
    register_runner_task = function(...) invisible(NULL),
    is_fs_running = function() {
      seen <<- c(seen, get_scala_ping_url())
      answers[length(seen)]
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
  # every probe went to the temporary port, never to the configured fs_url:fs_port
  expect_identical(unique(seen), "http://127.0.0.1:9080/ping")
  expect_length(seen, 3L)
})

# --------------------------------------------------------------------------------------------------
# A database bound somewhere else is a run-scoped service, so it never becomes a Windows scheduled
# task. Two independent reasons, both found on Windows:
#
#  * taskscheduler_create() registers an HOURLY task, so Windows went on starting a retraining
#    database on port 9080 every hour long after setup() had finished with it;
#  * schtasks caps the command line it stores. "cmd /c <Rexe> <script> <args> >> <log> 2>&1" names
#    the data directory three times, and adding "bindHost localhost bindPort 9080" took a default
#    Windows install from 221 to 254 characters. The task was stored ending in "2>&", which
#    cmd.exe cannot parse, so it exited 1 having created no log file at all - and setup() waited
#    for ever on a database nothing had started, with nothing to read.

test_that("register_fs_runner_task starts a bound database attached, never as a scheduled task", {
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  withr::defer(clear_fs_endpoint_override())
  got <- NULL
  local_mocked_bindings(
    register_runner_task = function(task_name, runner_args = character(0), attached = FALSE) {
      got <<- attached
      invisible(NULL)
    },
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
  expect_true(got)
})

test_that("register_fs_runner_task leaves the real service on the task scheduler", {
  # The reason this is not simply always attached: the real fs is meant to come back after a
  # reboot, which is the whole point of registering it with the scheduler on Windows.
  local_lease_conf(fs_url = "http://fs", fs_port = 8080)
  got <- "not called"
  local_mocked_bindings(
    register_runner_task = function(task_name, runner_args = character(0), attached = FALSE) {
      got <<- attached
      invisible(NULL)
    },
    is_fs_running = function() TRUE,
    .package = "episomer"
  )

  expect_message(register_fs_runner_task(wait_until_response = TRUE), "fs is running")
  expect_false(got)
})

test_that("register_runner_task(attached = TRUE) does not consult the task scheduler", {
  data_dir <- local_lease_conf(force_date_format = "")
  dir.create(file.path(data_dir, "jobs"), showWarnings = FALSE)
  got <- NULL
  local_mocked_bindings(stop_if_no_config = function(...) invisible(NULL), .package = "episomer")
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      got <<- args
      invisible(0)
    },
    .package = "base"
  )
  skip_on_os("windows")

  suppressMessages(register_runner_task(
    "fs",
    runner_args = c("bindHost", "localhost", "bindPort", "9080"),
    attached = TRUE
  ))

  # the arguments still reach the runner script, and no "Permanent Task scheduling" message is
  # printed because the scheduler branch is not entered at all
  expect_equal(got, shQuote(c(file.path(data_dir, "jobs", "fs.R"), data_dir,
                              "bindHost", "localhost", "bindPort", "9080")))
})

test_that("rscript_exe_attached() names an executable system2 can run", {
  exe <- rscript_exe_attached()
  expect_length(exe, 1L)
  expect_true(nchar(exe) > 0)
  if (.Platform$OS.type != "windows") expect_identical(exe, "Rscript")
})
