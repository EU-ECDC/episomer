# The runner status table (multi-instance services, plan issue 8).
#
# What the Shiny configuration page shows next to the three "Running / Stopped" labels: one row per
# runner holding a lease, wherever in the cluster it runs, plus the wording that tells "running on
# another host" from "not running" when somebody clicks "activate".
#
# Same setup as test-runners_lease.R: fs_remote = NULL so epipath() resolves to a temp directory, and
# a "second instance" is just another conf$instance_id borrowed with with_instance().

# Listing the leases -------------------------------------------------------------------------------

test_that("runner_leases_df() is empty, with its columns, when nothing holds a lease", {
  local_lease_conf()

  df <- runner_leases_df()

  expect_identical(nrow(df), 0L)
  expect_identical(
    names(df),
    c("Runner", "Status", "Host", "Last seen", "Since")
  )
})

test_that("runner_leases_df() lists one row per live lease, in reading order", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A")
  write_runner_lease("search.bluesky")
  write_runner_lease("fs")
  with_instance("instance-B", host = "machine-B", code = {
    write_runner_lease("detect")
    write_runner_lease("search")
  })

  df <- runner_leases_df()

  # services in the order an operator thinks about them, and the parent "search" lease right before
  # its per-network children
  expect_identical(
    df$Runner,
    c("fs", "search", "search.bluesky", "detect")
  )
  expect_identical(unique(df$Status), "active")
  expect_identical(
    df$Host,
    c(
      "machine-A (this machine)",
      "machine-B",
      "machine-A (this machine)",
      "machine-B"
    )
  )
})

test_that("runner_leases_df() ignores the released leases and anything that is not one", {
  local_lease_conf(instance_id = "instance-A")
  write_runner_lease("detect")
  # a lease released by another instance: moved under runners/old/, which the table never lists -
  # a released runner is one whose row is simply gone (plan section 3.4)
  runner_lease_old_dir(create = TRUE)
  file.copy(
    runner_lease_path("detect"),
    file.path(runner_lease_old_dir(), "search.instance-B.json")
  )
  # and a stray file somebody dropped there
  writeLines("not a lease", file.path(runner_lease_dir(), "README.txt"))

  expect_identical(runner_lease_names(), "detect")
  expect_identical(runner_leases_df()$Runner, "detect")
})

# The wording of a status --------------------------------------------------------------------------

test_that("runner_leases_df() tells a lease nobody renews from a holder that crashed here", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 300,
    runner_ttl_search = 300
  )
  # a holder on another machine that stopped renewing: all we know is that we have not heard from
  # it for longer than the TTL
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))
  Sys.setFileTime(runner_lease_path("detect"), Sys.time() - 600)
  # a holder of *this* machine whose process is gone, its mtime still fresh: the two corroborating
  # signals of lease_holder_is_dead_locally() are the lease host and our own PID file
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("search"))
  set_lease_pid("search", dead_pid)
  write_local_pid_file("search", dead_pid)

  df <- runner_leases_df()

  expect_identical(
    df$Status[df$Runner == "detect"],
    "no activity for 5 min (probably stopped)"
  )
  expect_identical(df$Status[df$Runner == "search"], "stopped unexpectedly on this machine")
})

test_that("the parent search lease is named a supervisor rather than reported stale", {
  # search_loop() claims "search" and then blocks in parallel::parLapply() for the whole life of
  # the service, so that lease is never renewed and goes stale after runner_ttl_search on a
  # perfectly healthy machine (R/runners.R, is_search_running()). Wording it "stale (no heartbeat
  # for 5 min)" made the table report a dead search loop that was collecting normally; the workers
  # in the rows below are the ones that carry the truth, and they do renew.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300
  )
  # held elsewhere, so the local crash check has nothing to say about it
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search"))
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )
  Sys.setFileTime(runner_lease_path("search"), Sys.time() - 600)

  df <- runner_leases_df()

  expect_identical(
    df$Status[df$Runner == "search"],
    "coordinates collection, sends no activity signal by design: see the search.* rows for its real state"
  )
  # the worker is the row that reports whether anything is actually collecting...
  expect_identical(df$Status[df$Runner == "search.bluesky"], "active")
  # ... and it is displayed right under its parent, which is what the wording sends the reader to
  expect_identical(df$Runner, c("search", "search.bluesky"))
})

test_that("a per-network search worker nobody renews is still reported stale", {
  # The supervisor wording is for the parent runner only: a worker has an iteration to renew on
  # every heartbeat span, so its silence means what it says.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )
  Sys.setFileTime(runner_lease_path("search.bluesky"), Sys.time() - 600)

  expect_identical(
    runner_leases_df()$Status,
    "no activity for 5 min (probably stopped)"
  )
})

test_that("runner_leases_df() shows the age of the last heartbeat and the claim time", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_detect = 1800)
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))
  Sys.setFileTime(runner_lease_path("detect"), Sys.time() - 120)

  df <- runner_leases_df()

  # "last seen" is the mtime, the only quantity the expiry decision uses...
  expect_identical(df$`Last seen`, "2 min ago")
  # ... and "since" is claimed_at, which a renewal does not move: the lease was written now
  expect_match(
    df$Since,
    strftime(Sys.time(), format = "%Y-%m-%d %H:%M", tz = "UTC"),
    fixed = TRUE
  )
  expect_match(df$Since, "UTC$")
})

test_that("format_lease_duration() writes a duration for a human", {
  expect_identical(format_lease_duration(0), "0 s")
  expect_identical(format_lease_duration(42.4), "42 s")
  expect_identical(format_lease_duration(300), "5 min")
  expect_identical(format_lease_duration(5400), "1.5 h")
  expect_identical(format_lease_duration(172800), "2.0 days")
  # a negative age (the fs machine's clock being ahead of ours) reads as "now", not as the future
  expect_identical(format_lease_duration(-10), "0 s")
  expect_identical(format_lease_duration(NA), NA_character_)
})

# "Running on another host" is not "not running" ---------------------------------------------------

test_that("runner_running_elsewhere_message() names the machine holding a fresh lease", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300
  )
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search"))

  msg <- runner_running_elsewhere_message("search")

  expect_match(msg, "already running on host 'machine-B'")
  expect_match(msg, "TTL: 5 min")
})

test_that("runner_running_elsewhere_message() is silent when starting here is legitimate", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 300
  )

  # nothing holds it
  expect_identical(runner_running_elsewhere_message("detect"), "")

  # a lease of this very host: starting is a local matter, and the local PID check of
  # register_runner() is the one that must refuse a duplicate
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("detect"))
  expect_identical(runner_running_elsewhere_message("detect"), "")

  # a stale lease on another machine: taking over is exactly what this instance is for
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))
  Sys.setFileTime(runner_lease_path("detect"), Sys.time() - 600)
  expect_identical(runner_running_elsewhere_message("detect"), "")
})

test_that("loop_run_issues() refuses to start a loop already running on another host", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300
  )
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search"))

  # the check comes before everything the loop needs locally: no amount of fixing the tasks would
  # make a second collection start here
  expect_match(
    loop_run_issues("search"),
    "already running on host 'machine-B'"
  )
})

test_that("a stale fs lease is told apart by whether the database still answers", {
  # fs cannot renew its own lease, only its monitor does: a stale lease on a database that still
  # answers /ping means the monitor stopped reporting, not that the database is down.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_fs = 120
  )
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("fs"))
  Sys.setFileTime(runner_lease_path("fs"), Sys.time() - 600)

  testthat::local_mocked_bindings(is_fs_running = function() TRUE)
  expect_identical(
    runner_leases_df()$Status,
    "running, but its monitor stopped reporting"
  )

  testthat::local_mocked_bindings(is_fs_running = function() FALSE)
  expect_identical(
    runner_leases_df()$Status,
    "no activity for 2 min (probably stopped)"
  )
})
