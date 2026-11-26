# Registering a runner (multi-instance services, plan issue 4).
#
# register_runner() is where the two exclusion layers meet: the PID file, which answers "is another
# process of *this machine* already doing this?", and the runner lease, which answers "am I allowed
# to be this runner in the whole cluster?".
#
# As in test-runners_lease.R, everything runs with fs_remote = NULL (epipath() resolves to a temp
# directory) and a "second instance" is just another conf$instance_id, borrowed with
# with_instance(). runner_claim_guard = 0 everywhere: the guard wait is tested in
# test-runners_lease.R, and sleeping five seconds per test would be pointless here.
#
# The PID *file* is exercised for real; what is deliberately not exercised is get_running_task_pid()
# finding a foreign live process, which needs a second OS process (see TESTING_ROADMAP.md, "hors
# perimetre").

pid_path <- function(name) file.path(conf$data_dir, paste0(name, ".PID"))

# Claiming ----------------------------------------------------------------------------------------

test_that("register_runner() writes the PID file, takes the lease and names the runner", {
  local_lease_conf(instance_id = "instance-A", runner_claim_guard = 0)

  register_runner("detect")

  # layer 1: the local PID file. The PID is alone on the first line - the Scala side writes
  # fs.java.PID in that form and kill_task() has to keep reading both - and the host and start time
  # below it are what make the file describe a process rather than a number (see
  # write_running_task_pid(), R/runners.R).
  expect_true(file.exists(pid_path("detect")))
  expect_identical(
    readLines(pid_path("detect"))[[1]],
    as.character(Sys.getpid())
  )
  expect_identical(
    read_running_task_pid_record("detect"),
    list(
      pid = Sys.getpid(),
      host = "machine-A",
      start_time = pid_start_time(Sys.getpid())
    )
  )
  # layer 2: the cluster-wide lease
  expect_true(i_hold_runner_lease("detect"))
  # session state, so the heartbeat helpers can be called with no argument
  expect_identical(conf$runner_name, "detect")
})

test_that("register_runner() lets the same process register twice without racing itself", {
  skip_on_cran()
  local_lease_conf(instance_id = "instance-A", runner_claim_guard = 0)
  register_runner("fs_mon")
  written_first <- jsonlite::read_json(runner_lease_path("fs_mon"))

  # the fs monitor re-registers on every iteration of its 2-second loop (R/fs.R): a re-registration
  # must be a renewal, not a fresh claim - no rewrite of the content, and above all no guard wait
  local_mocked_bindings(
    runner_claim_wait = function(guard) stop("a re-registration must not wait"),
    .package = "episomer"
  )
  expect_silent(register_runner("fs_mon"))

  expect_identical(jsonlite::read_json(runner_lease_path("fs_mon")), written_first)
  expect_true(i_hold_runner_lease("fs_mon"))
})

# Refusing ----------------------------------------------------------------------------------------

test_that("register_runner() refuses to start when another machine holds the runner", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  # this is the standby machine of a two-machine deployment discovering the other one collects:
  # the message has to name where it runs, so an operator knows which machine to go and look at
  expect_error(register_runner("search.bluesky"), "machine-B")
  expect_error(register_runner("search.bluesky"), "already held by another instance")

  # the holder is left strictly alone, and this machine does not pretend to run anything
  expect_identical(
    read_runner_lease("search.bluesky")$instance_id,
    "instance-B"
  )
  expect_false(file.exists(pid_path("search.bluesky")))
  expect_null(conf$runner_name)
})

test_that("register_runner() takes over a runner whose holder died", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_search = 300, runner_claim_guard = 0)
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )
  # B's host died: nothing renewed the lease for longer than the TTL. Ageing the file itself, so
  # the real mtime path is exercised - freshness never comes from the JSON timestamps.
  Sys.setFileTime(runner_lease_path("search.bluesky"), Sys.time() - 600)

  expect_silent(register_runner("search.bluesky"))
  expect_true(i_hold_runner_lease("search.bluesky"))
})

test_that("register_runner() takes over the runner a killed local process still holds", {
  skip_on_cran()
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_fs = 120,
    runner_claim_guard = 0
  )
  # The state stop_all_runner_tasks() used to leave behind: the fs was killed with SIGTERM, R ran no
  # exit handler, so the lease is still there and still fresh - and the PID file still names the
  # process that is gone. That PID file is the whole proof, and register_runner() used to overwrite
  # it with its own PID *before* claiming, destroying it: the claim then saw a lease it could not
  # recognise as dead, and starting the fs again was refused for a whole TTL.
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("fs"))
  set_lease_pid("fs", dead_pid)
  write_local_pid_file("fs", dead_pid)

  expect_silent(register_runner("fs"))

  expect_true(i_hold_runner_lease("fs"))
  expect_identical(read_running_task_pid_file("fs"), Sys.getpid())
})

test_that("register_runner() leaves the PID file of the previous holder alone when refused", {
  skip_on_cran()
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0
  )
  # a fresh lease of another machine: this instance is refused, and it has to leave the local disk
  # exactly as it found it. The PID file it finds belongs to the process that ran here before, and
  # it is what the *next* attempt will read to recognise a holder that died here (see above), so
  # deleting it - which the refusal used to do, because it had just written its own PID over it -
  # would throw away the evidence for good.
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search"))
  write_local_pid_file("search", dead_pid)

  expect_error(register_runner("search"), "already held by another instance")

  expect_identical(read_running_task_pid_file("search"), dead_pid)
  # and a PID file naming a process that no longer runs does not make this machine look busy
  expect_false(is_runner_running_locally("search"))
  expect_null(conf$runner_name)
})

test_that("runner_lease_holder_message() tells a live holder from one that just released", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_detect = 1800)
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))

  expect_match(runner_lease_holder_message("detect"), "instance-B")
  expect_match(runner_lease_holder_message("detect"), "host 'machine-B'")
  expect_match(runner_lease_holder_message("detect"), "1800 seconds")

  # a claim can also fail against a holder that releases right after: nothing is wrong then, and
  # the message must say so rather than send an operator hunting for a machine that stopped
  file.remove(runner_lease_path("detect"))
  expect_match(runner_lease_holder_message("detect"), "Starting again should now succeed")
})

# Unregistering ------------------------------------------------------------------------------------

test_that("unregister_runner() gives back both layers", {
  local_lease_conf(instance_id = "instance-A", runner_claim_guard = 0)
  register_runner("detect")

  unregister_runner("detect")

  # the lease is gone from runners/, so another machine may claim it at once instead of waiting out
  # the TTL, and it is kept as a trail under runners/old/
  expect_null(read_runner_lease("detect"))
  expect_true(epifile.exists(runner_lease_released_path("detect")))
  # and the PID file goes with it: it describes a process that has stopped. Leaving it behind is
  # the state observed after a search worker died - lease free, PID file naming a live process -
  # where the two layers disagreed about the same runner
  expect_false(file.exists(pid_path("detect")))
})

test_that("unregister_runner() leaves alone what is not ours", {
  local_lease_conf(instance_id = "instance-A", runner_claim_guard = 0)
  register_runner("detect")
  # taken over while we were busy: the file under runners/ is the new holder's now
  steal_lease("detect")
  # and the PID file names another local process
  write("1", file = pid_path("detect"))

  expect_message(unregister_runner("detect"), "not held by this instance anymore")

  expect_identical(read_runner_lease("detect")$instance_id, "instance-B")
  expect_true(file.exists(pid_path("detect")))
})

test_that("unregister_runner() does nothing when this process registered nothing", {
  local_lease_conf(instance_id = "instance-A")

  # the sandboxed loops and the Shiny app hold no runner: conf$runner_name is NULL and the on.exit
  # of every loop still calls this
  expect_false(unregister_runner(NULL))
})

# Stopping ----------------------------------------------------------------------------------------

test_that("stop_all_runner_tasks() ignores runners held by another machine", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_search = 300, runner_ttl_detect = 1800)
  with_instance("instance-B", host = "machine-B", code = {
    write_runner_lease("search")
    write_runner_lease("detect")
  })

  # stopping means killing a local process: a runner held elsewhere in the cluster is none of this
  # machine's business, and there is nothing here to kill. Without the local PID check this would
  # start killing processes that do not exist as soon as is_*_running() reads the lease (issue 7).
  stopped <- character()
  local_mocked_bindings(
    is_fs_running = function() FALSE,
    stop_search_runner_task = function() stopped <<- c(stopped, "search"),
    stop_detect_runner_task = function() stopped <<- c(stopped, "detect"),
    .package = "episomer"
  )

  stop_all_runner_tasks()

  expect_identical(stopped, character())
})
