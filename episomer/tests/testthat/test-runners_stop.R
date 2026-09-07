# Stopping a runner on Windows (schtasks layer of stop_runner_task(), R/runners.R).
#
# A runner is only a scheduled task when taskscheduleR is installed: register_runner_task()
# otherwise runs it attached to the session, and there is then no "episomer_<name>_loop" task to
# /End nor to /Disable. Asking first is what keeps the two "ERROR:" lines schtasks writes for an
# unknown task out of the setup wizard output.

test_that("windows_scheduled_task_exists() is FALSE for a task nobody registered", {
  skip_on_os(c("mac", "linux", "solaris"))

  expect_false(windows_scheduled_task_exists("episomer_no_such_task_loop"))
})

test_that("stop_runner_task() leaves schtasks alone when the task is not registered", {
  local_lease_conf()
  skip_on_os(c("mac", "linux", "solaris"))
  called <- FALSE
  local_mocked_bindings(
    windows_scheduled_task_exists = function(taskname) {
      called <<- TRUE
      FALSE
    },
    # the PID layer is not what is under test here, and killing whatever the data dir records is
    # precisely what must not happen in a test
    kill_task = function(...) invisible(NULL),
    release_stopped_runner_leases = function(...) invisible(character(0)),
    .package = "episomer"
  )

  expect_message(stop_runner_task("fs"), "No Windows scheduled task named episomer_fs_loop")
  expect_true(called)
})

# Waiting for the database service to stop (stop_fs_runner_task(), R/runners.R).
#
# is_fs_running() asks an HTTP port, and a port says nothing about who owns it: a WSL distribution
# or a container publishing 8080 is served on the localhost of the Windows host too. The wait used
# to be unbounded, so such a fs - which no episomer command of this machine can stop - held the
# whole setup wizard, which calls stop_all_runner_tasks() before every step.

test_that("stop_fs_runner_task() does not wait for a fs no process of this machine holds", {
  local_lease_conf()
  local_mocked_bindings(
    stop_runner_task = function(...) invisible(NULL),
    is_fs_running = function() TRUE,
    get_scala_ping_url = function() "http://localhost:8080/ping",
    .package = "episomer"
  )

  expect_message(
    stop_fs_runner_task(timeout = 30),
    "no episomer process of this machine is registered for it"
  )
})

test_that("stop_fs_runner_task() gives up waiting for a fs of this machine that will not stop", {
  data_dir <- local_lease_conf()
  # What makes the fs ours: the JVM the loop spawned recorded itself here (Scala API.scala), in the
  # bare form that side writes when EPISOMER_INSTANCE is unset - no host, no start time.
  writeLines("4242", file.path(data_dir, "fs.java.PID"))
  local_mocked_bindings(
    stop_runner_task = function(...) invisible(NULL),
    is_fs_running = function() TRUE,
    get_scala_ping_url = function() "http://localhost:8080/ping",
    # The recorded JVM has to be *running* for the fs to be ours, and a test has no java process to
    # point at - the same reason local_live_this_process() mocks the R probe (helper-conf.R).
    is_java_pid_running = function(pid) identical(as.integer(pid), 4242L),
    .package = "episomer"
  )

  expect_message(
    expect_message(stop_fs_runner_task(timeout = 1), "Waiting until the fs runner stops"),
    "still answers on http://localhost:8080/ping after 1 seconds"
  )
})

test_that("stop_fs_runner_task() returns at once when the fs has stopped", {
  local_lease_conf()
  local_mocked_bindings(
    stop_runner_task = function(...) invisible(NULL),
    is_fs_running = function() FALSE,
    .package = "episomer"
  )

  expect_silent(stop_fs_runner_task())
})
