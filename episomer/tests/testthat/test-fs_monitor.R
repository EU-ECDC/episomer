# The fs monitor (R/fs.R).
#
# fs_loop() itself stays out of scope (while(TRUE) around spark_job(), see TESTING_ROADMAP.md) and
# so does the future worker. What is tested here is the one thing that went wrong twice on docker
# compose: the monitor dying on an error nobody could see. The monitor is the only R code of the fs
# service still running once fs_loop() has entered spark_job(), so its death takes the heartbeat of
# the fs lease and health_check() with it - and it happens in a future whose output nothing ever
# collects, which is why it writes to fs_mon.log and why fs_loop() checks whether it is still there.

test_that("fs_monitor_log() appends one timestamped line to fs_mon.log", {
  data_dir <- withr::local_tempdir()

  fs_monitor_log("Monitoring episomer", data_dir = data_dir)
  fs_monitor_log("stopping, the configuration could not be set up: boom", data_dir = data_dir)

  log <- readLines(file.path(data_dir, "fs_mon.log"))
  expect_length(log, 2)
  expect_match(log[1], "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9:]{8} \\[fs_mon\\]: Monitoring episomer$")
  expect_match(log[2], "could not be set up: boom$")
})

test_that("fs_monitor_log() does not raise when it cannot write", {
  # a monitor that cannot write its log still has a lease to renew, so the write is a try()
  expect_no_error(suppressWarnings(
    fs_monitor_log("nowhere", data_dir = file.path(tempdir(), "no-such-directory"))
  ))
})

test_that("fs_monitor_log_once() drops the repetitions of a message", {
  data_dir <- withr::local_tempdir()
  local_monitor_logs()

  expect_true(fs_monitor_log_once("registering the fs monitor failed: boom", data_dir = data_dir))
  # the monitor iterates every two seconds and the failures it survives are the lasting kind: the
  # same line thirty times a minute would bury the one that matters and grow fs_mon.log for nothing
  expect_false(fs_monitor_log_once("registering the fs monitor failed: boom", data_dir = data_dir))
  # a different failure is a different line, whatever the first one did
  expect_true(fs_monitor_log_once("renewing the fs lease failed: boom", data_dir = data_dir))
  # and a repetition is written again once the span has passed
  expect_true(fs_monitor_log_once(
    "registering the fs monitor failed: boom",
    span = 0,
    data_dir = data_dir
  ))

  expect_length(readLines(file.path(data_dir, "fs_mon.log")), 3)
})

test_that("fs_monitor_try() turns a failure into a log line instead of the end of the monitor", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir)
  local_monitor_logs()

  # this is register_fs_monitor() refusing to register, which is exactly what killed the monitor
  # after a container restart: the monitor has to survive it and try again on the next iteration
  expect_no_error(fs_monitor_try(
    stop("Runner  fs_mon PID: 119 is already running"),
    "registering the fs monitor"
  ))

  expect_match(
    readLines(file.path(data_dir, "fs_mon.log")),
    "registering the fs monitor failed: Runner  fs_mon PID: 119 is already running$"
  )
})

test_that("fs_monitor_try() gives the value back and logs nothing when the call succeeds", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir)
  local_monitor_logs()

  expect_identical(fs_monitor_try(21 * 2, "doing arithmetic"), 42)
  expect_false(file.exists(file.path(data_dir, "fs_mon.log")))
})

test_that("restart_fs_monitor_if_dead() reports the error of a dead monitor and starts a new one", {
  # sequential so that the future below is resolved - and errored - without spawning anything. The
  # restart itself is stubbed: the real one would run the monitor loop in this very process.
  previous_plan <- future::plan(future::sequential)
  withr::defer(future::plan(previous_plan))
  local_mocked_bindings(
    start_fs_monitor = function(data_dir) "a new monitor",
    .package = "episomer"
  )
  monitor <- future::future(stop("the configuration could not be set up"))

  expect_message(
    expect_identical(restart_fs_monitor_if_dead(monitor, "/episomer-data"), "a new monitor"),
    "The episomer fs monitor stopped with the following error"
  )
})
