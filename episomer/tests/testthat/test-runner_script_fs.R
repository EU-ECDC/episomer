# The shipped runner script of the database, inst/extdata/fs.R, copied into <data_dir>/jobs/fs.R by
# register_runner_task() and started as a bare `Rscript`. Nothing else in the package sources it, so
# a mistake in it is invisible to every other test and shows up only as a service that will not
# start - which is how both of the mistakes below were found, in a container.
#
# The script is exercised here by evaluating everything it does *except* its final fs_loop() call,
# with commandArgs() shadowed to hand it the command line under test.

run_fs_runner_script <- function(...) {
  script <- system.file("extdata", "fs.R", package = "episomer")
  expect_true(file.exists(script))
  exprs <- parse(script)
  # the last expression is the episomer::fs_loop() call, which would start a database
  exprs <- exprs[-length(exprs)]
  env <- new.env(parent = globalenv())
  # shadows base::commandArgs for the duration of the eval, so the script reads this line
  env$commandArgs <- function(trailingOnly = FALSE) c(...)
  for (e in exprs) eval(e, envir = env)
  env
}

test_that("inst/extdata/fs.R parses", {
  # `named <- if (cond) a else b` written across several lines does not parse at the top level of a
  # script: R closes the statement at the end of the `if` line and then meets a bare `else`. The
  # error is "unexpected 'else'", and the only place it appeared was jobs/fs.log.
  expect_silent(parse(system.file("extdata", "fs.R", package = "episomer")))
})

test_that("inst/extdata/fs.R reads the bind arguments after the data directory", {
  env <- run_fs_runner_script("/episomer-data", "bindHost", "localhost", "bindPort", "9080")

  expect_identical(env$data_dir, "/episomer-data")
  expect_identical(env$named$bindHost, "localhost")
  expect_identical(env$named$bindPort, "9080")
})

test_that("inst/extdata/fs.R defines an empty argument list when given only the data directory", {
  # How the *real* service starts: register_fs_runner_task() with no bind address passes no extra
  # arguments at all. An `else` branch that builds a list without assigning it leaves `named`
  # undefined, and the script then dies on "object 'named' not found" - not in the retraining, which
  # does pass arguments, but in the one path that matters most.
  env <- run_fs_runner_script("/episomer-data")

  expect_identical(env$data_dir, "/episomer-data")
  expect_identical(env$named, list())
  expect_null(env$named$bindHost)
  expect_null(env$named$bindPort)
})

test_that("inst/extdata/fs.R refuses an argument that is not part of a pair", {
  expect_error(
    run_fs_runner_script("/episomer-data", "bindHost"),
    "'key' 'value' pairs"
  )
})

test_that("inst/extdata/fs.R refuses to start without a data directory", {
  expect_error(run_fs_runner_script(), "config file has not been provided")
})
