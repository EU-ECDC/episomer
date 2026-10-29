# Where the services write their logs (logs_to_stdout(), R/config.R).
#
# One file per service under jobs/ is right for a machine an operator logs into, and unreadable in
# a container: `docker logs` and `kubectl logs` show the standard output of the process and nothing
# else, so a file written inside a pod exists and is lost. EPISOMER_LOGS_TO_STDOUT switches the
# three services over; what is tested here is the switch itself, and the two log writers that do
# not simply go through message() - the fs monitor, and the redirection of an attached runner.

test_that("logs_to_stdout() is FALSE unless the environment asks for it", {
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = NA))
  expect_false(logs_to_stdout())

  # An empty value is what an unset variable looks like in a compose file that declares it without
  # giving it a value, and it must not be read as an invalid boolean.
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = ""))
  expect_false(logs_to_stdout())

  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "FALSE"))
  expect_false(logs_to_stdout())
})

test_that("logs_to_stdout() accepts TRUE and 1", {
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "TRUE"))
  expect_true(logs_to_stdout())

  # writing a boolean as 1/0 in a compose file is common enough, and as.logical("1") is NA
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "1"))
  expect_true(logs_to_stdout())

  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "0"))
  expect_false(logs_to_stdout())
})

test_that("logs_to_stdout() refuses a value it cannot read", {
  # visible at startup rather than three hours later in the middle of a loop, as for the
  # EPISOMER_OPTION_* properties (coerce_env_props())
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "yes please"))
  expect_error(logs_to_stdout(), "EPISOMER_LOGS_TO_STDOUT")
})

test_that("fs_monitor_log() writes to the standard output and no file when asked to", {
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "TRUE"))
  data_dir <- withr::local_tempdir()

  out <- utils::capture.output(fs_monitor_log("Monitoring episomer", data_dir = data_dir))

  expect_match(out, "\\[fs_mon\\]: Monitoring episomer$")
  expect_false(file.exists(file.path(data_dir, "fs_mon.log")))
})

test_that("search_loop() logs to files unless the environment asks for the standard output", {
  # The default is computed at call time from the environment, which is what lets one data
  # directory be served by a container (stdout) and read by a desktop session (files).
  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = NA))
  expect_true(eval(formals(search_loop)$log_to_file))

  withr::local_envvar(c(EPISOMER_LOGS_TO_STDOUT = "TRUE"))
  expect_false(eval(formals(search_loop)$log_to_file))
})
