# Stopping a runner on Windows (schtasks layer of stop_runner_task(), R/runners.R).
#
# A runner is only a scheduled task when taskscheduleR is installed: register_runner_task()
# otherwise runs it attached to the session, and there is then no "episomer_<name>_loop" task to
# /End nor to /Disable. Asking first is what keeps the two "ERROR:" lines schtasks writes for an
# unknown task out of the setup wizard output.

test_that("windows_scheduled_task_exists() is FALSE for a task nobody registered", {
  skip_on_cran()
  skip_on_os(c("mac", "linux", "solaris"))

  expect_false(windows_scheduled_task_exists("episomer_no_such_task_loop"))
})

test_that("stop_runner_task() leaves schtasks alone when the task is not registered", {
  skip_on_cran()
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
  skip_on_cran()
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

# Stopping the search service (stop_runner_task("search"), R/runners.R).
#
# search_loop() does no collection itself: it claims the "search" runner and then blocks in
# parallel::parLapply() while one PSOCK worker per social media runs search_loop_worker(). Those
# workers are R processes of their own and the OS reparents them rather than killing them, so
# signalling the parent alone left every network collecting for ever - with search.PID gone, which
# is what made it look like a clean stop. Only the docker deployment was spared, its whole PID
# namespace going down with the container.

test_that("local_search_worker_runner_names() names the per-network workers only", {
  data_dir <- local_lease_conf()
  write_local_pid_file("search", 1L)
  write_local_pid_file("search.bluesky", 2L)
  write_local_pid_file("search.mastodon.piaille.fr", 3L)
  write_local_pid_file("detect", 4L)
  # The convention of the Scala side for the JVMs. There is no search JVM today, but these names are
  # signalled as R processes, so they must not end up in this list if one ever appears.
  write_local_pid_file("search.java", 5L)

  expect_setequal(
    local_search_worker_runner_names(),
    c("search.bluesky", "search.mastodon.piaille.fr")
  )
})

test_that("stop_runner_task() kills the search workers, not just the parent loop", {
  local_lease_conf()
  write_local_pid_file("search", 1L)
  write_local_pid_file("search.bluesky", 2L)
  write_local_pid_file("search.mastodon.piaille.fr", 3L)
  killed <- character(0)
  waited <- character(0)
  local_mocked_bindings(
    kill_task = function(pidfile, type = "R") {
      killed <<- c(killed, pidfile)
      invisible(TRUE)
    },
    # The leases the killed processes hold are the per-network ones, so the sweep has to be told to
    # wait for the workers too: signalling is not dying, and a sweep run while they are still in
    # the process table releases nothing (release_dead_local_runner_lease(), R/runners-lease.R).
    release_stopped_runner_leases = function(names, ...) {
      waited <<- names
      invisible(character(0))
    },
    .package = "episomer"
  )

  suppressMessages(stop_runner_task("search"))

  expect_setequal(
    killed,
    c("search", "search.bluesky", "search.mastodon.piaille.fr")
  )
  expect_setequal(
    waited,
    c("search", "search.bluesky", "search.mastodon.piaille.fr")
  )
})

test_that("search_has_local_process() sees a worker the parent loop has already left behind", {
  local_lease_conf()
  local_live_this_process()
  # The state an operator gets into by killing search.PID by hand, or by a parent that crashed: no
  # parent left to find, workers still collecting. Reading the parent alone had
  # stop_all_runner_tasks() decide there was nothing to stop.
  write_local_pid_file("search", dead_pid)
  write_local_pid_file("search.bluesky", Sys.getpid())

  expect_false(is_runner_running_locally("search"))
  expect_true(search_has_local_process())
})

test_that("search_has_local_process() is FALSE when every search PID file is stale", {
  local_lease_conf()
  local_live_this_process()
  write_local_pid_file("search", dead_pid)
  write_local_pid_file("search.bluesky", dead_pid)

  expect_false(search_has_local_process())
})

test_that("a search worker PID file of another PID namespace is never signalled", {
  local_lease_conf()
  local_live_this_process()
  # The state the `fs` container reads on every `make compose-up`: it mounts the whole of $EPI_HOME
  # and its startup setup(silent = TRUE) goes through stop_all_runner_tasks(), so it lists the
  # search PID files the *host* wizard left there. Their numbers mean nothing in its namespace, and
  # PIDs are handed out from the same small range at every container start, so believing one is how
  # a sweep would come to signal an unrelated R process of that container.
  #
  # The name is found - it is a search.<network>.PID like any other - and the record is what refuses
  # it: recorded_pid_is_running() compares the recorded host with conf$instance_host first.
  write_foreign_pid_file("search.bluesky", Sys.getpid(), host = "machine-B")
  killed <- character(0)
  local_mocked_bindings(
    kill_task = function(pidfile, type = "R") {
      killed <<- c(killed, pidfile)
      invisible(TRUE)
    },
    .package = "episomer"
  )

  expect_equal(local_search_worker_runner_names(), "search.bluesky")
  expect_false(search_has_local_process())
  # ...so "stop all services" finds nothing of its own to stop, and signals nothing.
  suppressMessages(stop_all_runner_tasks())
  expect_equal(killed, character(0))
})
