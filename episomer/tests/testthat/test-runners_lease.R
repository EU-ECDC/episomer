# Runner leases (multi-instance services, plan issue 2).
#
# Everything here runs with fs_remote = NULL, so epipath() resolves to a temp directory and the
# lease primitives exercise their local branch: no network, no second machine. A "second instance"
# is just another conf$instance_id, which is what with_instance() (helper-conf.R) borrows.
#
# The remote branch of the same code paths (HTTP calls to the fs API) is not covered here: it needs
# a running server, and is checked manually on docker compose - see the end of section 11 of
# inst/schema/multi_services_plan.md.

# local_lease_conf() lives in helper-conf.R: the registration tests use it too.

# Paths and names ---------------------------------------------------------------------------------

test_that("runner_lease_path() puts one file per runner under runners/", {
  data_dir <- local_lease_conf()

  expect_identical(
    runner_lease_path("search.bluesky"),
    file.path(data_dir, "runners", "search.bluesky.json")
  )
  # a Mastodon runner name carries the instance host, dots included
  expect_identical(
    runner_lease_path("search.mastodon.piaille.fr"),
    file.path(data_dir, "runners", "search.mastodon.piaille.fr.json")
  )
})

test_that("runner_lease_path() creates the runners folder only when asked to", {
  data_dir <- local_lease_conf()
  runners <- file.path(data_dir, "runners")

  # reading a lease happens far more often than writing one, and must not create anything
  runner_lease_path("detect")
  expect_false(dir.exists(runners))

  runner_lease_path("detect", create_dir = TRUE)
  expect_true(dir.exists(runners))
})

test_that("runner_lease_released_path() keeps successive releases apart", {
  data_dir <- local_lease_conf(instance_id = "instance-A")

  expect_identical(
    runner_lease_released_path("search.bluesky"),
    file.path(data_dir, "runners", "old", "search.bluesky.instance-A.json")
  )
  # a second instance releasing the same runner writes next to it, it does not overwrite it
  expect_identical(
    with_instance("instance-B", code = runner_lease_released_path("search.bluesky")),
    file.path(data_dir, "runners", "old", "search.bluesky.instance-B.json")
  )
})

test_that("a runner name that could escape the runners folder is refused", {
  local_lease_conf()

  # part of a runner name comes from the configuration (the Mastodon instance host)
  expect_error(runner_lease_path("../../etc/passwd"), "Invalid runner name")
  expect_error(runner_lease_path("search/bluesky"), "Invalid runner name")
  expect_error(runner_lease_path(""), "Invalid runner name")
  expect_error(runner_lease_path(c("search", "detect")), "Invalid runner name")
})

# Per-service TTL ---------------------------------------------------------------------------------

test_that("runner_service() reads the service out of a runner name", {
  expect_identical(runner_service("search.mastodon.piaille.fr"), "search")
  expect_identical(runner_service("search.bluesky"), "search")
  expect_identical(runner_service("detect"), "detect")
  expect_identical(runner_service("fs"), "fs")
})

test_that("runner_lease_ttl() takes the TTL of the service from the configuration", {
  local_lease_conf(
    runner_ttl_search = 300,
    runner_ttl_detect = 1800,
    runner_ttl_fs = 120
  )

  # every search runner shares the search TTL, whatever the network
  expect_equal(runner_lease_ttl("search.bluesky"), 300)
  expect_equal(runner_lease_ttl("search.mastodon.piaille.fr"), 300)
  expect_equal(runner_lease_ttl("detect"), 1800)
  expect_equal(runner_lease_ttl("fs"), 120)
})

test_that("runner_lease_ttl() coerces a value coming from the environment", {
  # EPISOMER_OPTION_* properties arrive as strings and this value is used in arithmetic
  local_lease_conf(runner_ttl_search = "42")

  expect_identical(runner_lease_ttl("search.bluesky"), 42)
})

test_that("runner_lease_ttl() falls back on the defaults of the plan", {
  local_lease_conf()

  expect_equal(runner_lease_ttl("detect"), runner_lease_ttl_defaults()$detect)
  # an unknown service gets the fallback rather than an error: too short a TTL causes false
  # expiries, too long a one only delays failover
  expect_equal(runner_lease_ttl("whatever"), runner_lease_ttl_fallback)
})

# Reading a lease ---------------------------------------------------------------------------------

test_that("read_runner_lease() returns NULL when no lease file exists", {
  local_lease_conf()

  expect_null(read_runner_lease("search.bluesky"))
})

test_that("write_runner_lease() then read_runner_lease() round trips the identity", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A")

  write_runner_lease("search.bluesky")
  lease <- read_runner_lease("search.bluesky")

  expect_identical(lease$runner, "search.bluesky")
  expect_identical(lease$service, "search")
  expect_identical(lease$instance_id, "instance-A")
  expect_identical(lease$host, "machine-A")
  expect_equal(lease$pid, Sys.getpid())
  # seen_at is the file mtime, added by the reader - it is never stored in the JSON
  expect_s3_class(lease$seen_at, "POSIXct")
  expect_false("seen_at" %in% names(jsonlite::read_json(runner_lease_path("search.bluesky"))))
})

test_that("read_runner_lease() ignores a lease file that is not a lease", {
  data_dir <- local_lease_conf()
  dir.create(file.path(data_dir, "runners"))
  # e.g. the empty file epifile.touch() would create if renew_runner_lease() did not check first
  writeLines("", file.path(data_dir, "runners", "detect.json"))

  expect_null(read_runner_lease("detect"))
})

# Claiming ----------------------------------------------------------------------------------------

test_that("a free runner (no lease file) can be claimed", {
  local_lease_conf(instance_id = "instance-A")

  # guard = 0 everywhere in this file: the confirmation read is what is being tested, not the wait
  expect_true(claim_runner_lease("search.bluesky", guard = 0))
  expect_true(i_hold_runner_lease("search.bluesky"))
})

test_that("a fresh foreign lease cannot be claimed and is left untouched", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_search = 300)
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.bluesky"))

  expect_false(claim_runner_lease("search.bluesky", guard = 0))
  # no write at all when the runner is taken: the holder must not be disturbed
  expect_identical(read_runner_lease("search.bluesky")$instance_id, "instance-B")
  expect_false(i_hold_runner_lease("search.bluesky"))
  # and the holder is reported, which is what the caller displays ("already running on machine-B")
  status <- runner_lease_status("search.bluesky")
  expect_identical(status$status, "running")
  expect_identical(status$host, "machine-B")
  expect_false(status$mine)
})

test_that("an expired foreign lease can be claimed", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_search = 300)
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.bluesky"))
  # aging the file itself rather than mocking: this exercises the real mtime path, which is where
  # freshness comes from (the fs machine clock, never the JSON timestamps)
  path <- runner_lease_path("search.bluesky")
  Sys.setFileTime(path, Sys.time() - 600)

  expect_identical(runner_lease_status("search.bluesky")$status, "stale")
  expect_true(claim_runner_lease("search.bluesky", guard = 0))
  expect_identical(read_runner_lease("search.bluesky")$instance_id, "instance-A")
})

test_that("a released foreign lease can be claimed whatever its age", {
  local_lease_conf(instance_id = "instance-B", runner_ttl_search = 300)
  # B claims, then releases cleanly on exit
  expect_true(claim_runner_lease("search.bluesky", guard = 0))
  expect_true(release_runner_lease("search.bluesky"))

  released <- file.path(
    conf$data_dir,
    "runners",
    "old",
    "search.bluesky.instance-B.json"
  )

  # the live file is simply gone, which is already the "free" case - no released flag to handle
  expect_false(file.exists(runner_lease_path("search.bluesky")))
  # and the trail is kept: an operator can still read who held the runner and until when
  expect_true(file.exists(released))
  expect_identical(
    jsonlite::read_json(released, simplifyVector = TRUE)$instance_id,
    "instance-B"
  )

  # A takes over immediately, without waiting for the TTL
  with_instance("instance-A", host = "machine-A", code = {
    expect_true(claim_runner_lease("search.bluesky", guard = 0))
    expect_true(i_hold_runner_lease("search.bluesky"))
  })
})

test_that("the loser of a simultaneous claim backs off", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_search = 300)
  # both instances read an absent lease and both write; B's write lands last, so B wins. Simulated
  # by letting B write during A's guard wait, which is exactly what the confirmation read is for.
  local_mocked_bindings(
    runner_claim_wait = function(guard) {
      with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.bluesky"))
    },
    .package = "episomer"
  )

  expect_false(claim_runner_lease("search.bluesky", guard = 5))
  expect_identical(read_runner_lease("search.bluesky")$instance_id, "instance-B")
})

# Renewing ----------------------------------------------------------------------------------------

test_that("renew_runner_lease() moves the mtime forward without touching the content", {
  local_lease_conf(instance_id = "instance-A")
  write_runner_lease("search.bluesky")
  path <- runner_lease_path("search.bluesky")
  before <- jsonlite::read_json(path)
  Sys.setFileTime(path, Sys.time() - 600)

  expect_true(renew_runner_lease("search.bluesky"))

  # the heartbeat is a touch, so renewed_at (content) is unchanged while seen_at (mtime) is not
  expect_identical(jsonlite::read_json(path), before)
  expect_lt(
    as.numeric(difftime(Sys.time(), epifile.mtime(path), units = "secs")),
    60
  )
})

test_that("renew_runner_lease() does not recreate a lease that is gone", {
  local_lease_conf(instance_id = "instance-A")
  write_runner_lease("search.bluesky")
  release_runner_lease("search.bluesky")

  # epifile.touch() creates a missing file: renewing a released lease would leave an empty
  # non-JSON file under runners/ and pollute the listing the Shiny status table reads
  expect_false(renew_runner_lease("search.bluesky"))
  expect_false(file.exists(runner_lease_path("search.bluesky")))
})

# Releasing ---------------------------------------------------------------------------------------

test_that("release_runner_lease() leaves the lease alone when it was stolen meanwhile", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_search = 300)
  claim_runner_lease("search.bluesky", guard = 0)
  # A froze, its lease expired, B took over
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.bluesky"))

  # a release issued by an instance that lost its lease would take away the *new* holder's file
  expect_message(
    expect_false(release_runner_lease("search.bluesky")),
    "not held by this instance"
  )
  expect_identical(read_runner_lease("search.bluesky")$instance_id, "instance-B")
  expect_false(dir.exists(file.path(conf$data_dir, "runners", "old")))
})

# Releasing the lease of a stopped local process ---------------------------------------------------
#
# The reason this exists at all: R runs no exit handler on a signal, so a runner stopped by
# kill_task() (R/runners.R) dies without its on.exit(unregister_runner()) ever running. Somebody else
# has to give the lease back, otherwise starting the service again is refused until the TTL expires.

test_that("release_dead_local_runner_lease() gives back the lease of a killed local runner", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A", runner_ttl_fs = 120)
  # the fs that was running here until stop_all_runner_tasks() killed it: its lease is still fresh
  # (it was renewed seconds ago), and its PID file still names the process that is now gone
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("fs"))
  set_lease_pid("fs", dead_pid)
  write_local_pid_file("fs", dead_pid)

  expect_message(
    expect_true(release_dead_local_runner_lease("fs")),
    "not running on this machine anymore"
  )

  # gone from runners/, which is the single meaning of "this runner is free"...
  expect_null(read_runner_lease("fs"))
  # ... and kept as a trail under the name of the instance it describes, not of the one that swept
  # it away: runners/old/ is what an operator reads after the fact
  expect_true(epifile.exists(runner_lease_released_path("fs", instance_id = "instance-old")))
  expect_false(epifile.exists(runner_lease_released_path("fs", instance_id = "instance-A")))
})

test_that("release_dead_local_runner_lease() takes the PID file of the dead holder with it", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A")
  # the fs monitor of the previous run of a container: `docker compose down` killed it without
  # releasing anything, so both its lease and its PID file are still there when the next run starts
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("fs_mon"))
  set_lease_pid("fs_mon", dead_pid)
  write_local_pid_file("fs_mon", dead_pid)

  expect_true(suppressMessages(release_dead_local_runner_lease("fs_mon")))

  # Both layers go, and the PID file is the one that mattered here: PIDs are handed out from the
  # same small range at every start of a container, and `future::plan(multisession)` alone takes
  # some twenty of them for its idle workers, so the number left in that file is very likely to
  # belong to a live process of the new run. register_runner() would then read it, find it alive,
  # and refuse to register a runner nobody is running.
  expect_null(read_runner_lease("fs_mon"))
  expect_false(file.exists(get_running_task_pid_path("fs_mon")))
  expect_false(is_runner_running_locally("fs_mon"))
})

test_that("release_dead_local_runner_lease() refuses to touch a lease that is not this machine's", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A", runner_ttl_search = 300)
  # another machine of the cluster, collecting right now. Its PID happens to be dead *here*, which
  # is exactly the confusion the host signal exists to prevent
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search"))
  set_lease_pid("search", dead_pid)
  write_local_pid_file("search", dead_pid)

  expect_false(release_dead_local_runner_lease("search"))
  expect_identical(read_runner_lease("search")$instance_id, "instance-B")
})

test_that("release_dead_local_runner_lease() refuses without a local PID file to corroborate", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A", runner_ttl_search = 300)
  # Two containers of the same service share EPISOMER_INSTANCE but not their PID namespace, so the
  # host alone proves nothing: without our own PID file naming that PID, "it does not run here" does
  # not mean "the holder is dead". No PID file, and one naming another process, both fail closed.
  with_instance("instance-B", host = "machine-A", code = write_runner_lease("search"))
  set_lease_pid("search", dead_pid)

  expect_false(release_dead_local_runner_lease("search"))
  write_local_pid_file("search", dead_pid + 1L)
  expect_false(release_dead_local_runner_lease("search"))
  expect_identical(read_runner_lease("search")$instance_id, "instance-B")
})

test_that("release_dead_local_runner_lease() refuses while the holder is still alive", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A", runner_ttl_detect = 1800)
  # this process is the holder, and it is running: nothing to release
  local_live_this_process()
  claim_runner_lease("detect", guard = 0)
  write_local_pid_file("detect", Sys.getpid())

  expect_false(release_dead_local_runner_lease("detect"))
  expect_true(i_hold_runner_lease("detect"))
})

test_that("release_dead_local_runner_leases() sweeps runners/ and leaves the rest alone", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_fs = 120,
    runner_ttl_search = 300,
    runner_ttl_detect = 1800
  )
  # the whole point of sweeping rather than naming: "search.bluesky" is held by a worker whose name
  # the process doing the stopping cannot enumerate, and "detect" is the lease of an older crash
  # nobody ever released
  for (name in c("fs", "search.bluesky", "detect")) {
    with_instance("instance-old", host = "machine-A", code = write_runner_lease(name))
    set_lease_pid(name, dead_pid)
    write_local_pid_file(name, dead_pid)
  }
  # and one runner of another machine, which must come out of this untouched
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.mastodon.social"))

  released <- suppressMessages(release_dead_local_runner_leases())

  expect_setequal(released, c("fs", "search.bluesky", "detect"))
  expect_identical(runner_lease_names(), "search.mastodon.social")
})

test_that("release_dead_local_runner_leases() copes with a data directory holding no runners", {
  local_lease_conf(instance_id = "instance-A")

  # stopping services must not raise because nothing was ever started: runners/ does not exist yet
  expect_identical(release_dead_local_runner_leases(), character(0))
})

# Guarding the writes -----------------------------------------------------------------------------

test_that("stop_if_lease_lost() stays silent while the lease is mine", {
  local_lease_conf(instance_id = "instance-A")
  claim_runner_lease("search.bluesky", guard = 0)

  expect_silent(stop_if_lease_lost("search.bluesky"))
  # even a stale lease is still mine: nobody took it, I am only late renewing it
  Sys.setFileTime(runner_lease_path("search.bluesky"), Sys.time() - 600)
  expect_silent(stop_if_lease_lost("search.bluesky"))
})

test_that("stop_if_lease_lost() raises a typed condition when the lease was taken", {
  local_lease_conf(instance_id = "instance-A")
  claim_runner_lease("search.bluesky", guard = 0)
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.bluesky"))

  # a typed condition, so a loop can tell "I lost my spot" (exit quietly, release nothing) from a
  # real error
  expect_error(
    stop_if_lease_lost("search.bluesky"),
    class = "episomer_lease_lost"
  )
  condition <- tryCatch(
    stop_if_lease_lost("search.bluesky"),
    episomer_lease_lost = function(c) c
  )
  expect_identical(condition$runner, "search.bluesky")
})

test_that("stop_if_lease_lost() raises when the lease disappeared altogether", {
  local_lease_conf(instance_id = "instance-A")
  claim_runner_lease("search.bluesky", guard = 0)
  file.remove(runner_lease_path("search.bluesky"))

  expect_error(
    stop_if_lease_lost("search.bluesky"),
    class = "episomer_lease_lost"
  )
})

test_that("the lease helpers do nothing when no runner name is known", {
  # the Shiny app and the R console are not runners: conf$runner_name is unset there, and the
  # helpers called with no argument must not invent a lease
  local_lease_conf()

  expect_false(i_hold_runner_lease())
  expect_false(renew_runner_lease())
  expect_false(release_runner_lease())
  expect_silent(stop_if_lease_lost())
})

test_that("the helpers use conf$runner_name when called with no argument", {
  local_lease_conf(instance_id = "instance-A", runner_name = "detect")
  claim_runner_lease("detect", guard = 0)

  expect_true(i_hold_runner_lease())
  expect_true(renew_runner_lease())
  expect_silent(stop_if_lease_lost())
  expect_true(release_runner_lease())
})

# Status ------------------------------------------------------------------------------------------

test_that("runner_lease_status() reports a free runner", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_detect = 1800)

  status <- runner_lease_status("detect")

  expect_identical(status$status, "free")
  expect_identical(status$runner, "detect")
  expect_equal(status$ttl, 1800)
  expect_false(status$mine)
  expect_true(is.na(status$seen_at))
})

test_that("runner_lease_status() reports my own runner as running", {
  local_lease_conf(instance_id = "instance-A", instance_host = "machine-A")
  claim_runner_lease("detect", guard = 0)

  status <- runner_lease_status("detect")

  expect_identical(status$status, "running")
  expect_identical(status$host, "machine-A")
  expect_true(status$mine)
  expect_lt(status$age, 60)
})

# Heartbeat (plan issue 5) ------------------------------------------------------------------------
#
# What is tested here is the *cadence*: that a long wait keeps proving the runner is alive, that a
# short one costs nothing, and that a stolen lease interrupts the wait instead of being discovered
# forty minutes later. The wiring into the loops themselves (search_loop_worker(), detect_loop())
# is not unit tested - they are infinite loops needing a social media API and a running fs - it is
# checked manually on docker compose, like the rest of the remote branch.
#
# The waits below are tenths of seconds: the span is taken from the configuration, so a test can
# make a "long" wait last 0.3 second.

test_that("runner_heartbeat_span() reads the configuration", {
  local_lease_conf(runner_heartbeat_span = 10)
  expect_equal(runner_heartbeat_span(), 10)

  # as an EPISOMER_OPTION_RUNNER_HEARTBEAT_SPAN variable would give it, before coercion
  local_lease_conf(runner_heartbeat_span = "45")
  expect_equal(runner_heartbeat_span(), 45)
})

test_that("runner_heartbeat_span() refuses a value that would make a busy loop", {
  # zero, negative or unreadable: sleep_with_heartbeat() would spin instead of sleeping
  local_lease_conf(runner_heartbeat_span = 0)
  expect_equal(runner_heartbeat_span(), runner_heartbeat_span_default)

  local_lease_conf(runner_heartbeat_span = -5)
  expect_equal(runner_heartbeat_span(), runner_heartbeat_span_default)

  local_lease_conf(runner_heartbeat_span = "every 30 seconds")
  expect_equal(runner_heartbeat_span(), runner_heartbeat_span_default)

  # and a process whose configuration predates plan issue 3
  local_lease_conf()
  expect_equal(runner_heartbeat_span(), runner_heartbeat_span_default)
})

test_that("renew_runner_lease_if_due() renews at most once per heartbeat span", {
  local_lease_conf(instance_id = "instance-A", runner_heartbeat_span = 60)
  claim_runner_lease("detect", guard = 0)

  # nothing renewed yet in this process: the first call goes to the fs
  expect_true(renew_runner_lease_if_due("detect"))
  # the second one is what makes the helper callable from inside a loop
  expect_false(renew_runner_lease_if_due("detect"))

  # and it beats again once the span has passed
  assign("detect", Sys.time() - 120, envir = lease_last_renewal)
  expect_true(renew_runner_lease_if_due("detect"))
})

test_that("renew_runner_lease_if_due() does nothing when this process holds no lease", {
  # the Shiny app, a sandboxed loop, a function called from the console
  local_lease_conf(instance_id = "instance-A", runner_name = NULL)

  expect_false(renew_runner_lease_if_due())
  expect_false(heartbeat_is_due(NULL))
})

test_that("sleep_with_heartbeat() renews the lease while it waits", {
  local_lease_conf(instance_id = "instance-A", runner_heartbeat_span = 0.05)
  claim_runner_lease("detect", guard = 0)

  renewals <- 0
  local_mocked_bindings(
    renew_runner_lease = function(name = conf$runner_name) {
      renewals <<- renewals + 1
      # the real one records the attempt, and renew_runner_lease_if_due() reads that record
      assign(name, Sys.time(), envir = lease_last_renewal)
      TRUE
    }
  )

  sleep_with_heartbeat(0.3, name = "detect")

  # 0.3 second in slices of 0.05: the exact count depends on the machine, the point is that a wait
  # much longer than the span does not stay silent
  expect_gte(renewals, 2)
})

test_that("sleep_with_heartbeat() waits for the whole duration it was given", {
  local_lease_conf(runner_heartbeat_span = 0.05)

  # name = NULL: no lease at all, so it degrades to a plain, chopped Sys.sleep()
  elapsed <- system.time(sleep_with_heartbeat(0.3, name = NULL))[["elapsed"]]

  expect_gte(elapsed, 0.3 * 0.95) # We give an error margin
})

test_that("sleep_with_heartbeat() returns at once when there is nothing to wait for", {
  local_lease_conf(runner_heartbeat_span = 0.05)

  # do_next_alerts() calls it with wait_for = 0 on its first turn (R/alert.R)
  expect_null(sleep_with_heartbeat(0, name = NULL))
  expect_null(sleep_with_heartbeat(NA, name = NULL))
})

test_that("sleep_with_heartbeat() gives up as soon as the lease is taken by somebody else", {
  local_lease_conf(instance_id = "instance-A", runner_heartbeat_span = 0.05)
  claim_runner_lease("detect", guard = 0)

  # another instance takes the runner over while this one is asleep
  with_instance("instance-B", code = write_runner_lease("detect"))

  # asked for 30 seconds, back after the first slice: waking up half an hour later to discover
  # somebody else has been working is the outcome this prevents
  expect_error(
    sleep_with_heartbeat(30, name = "detect"),
    class = "episomer_lease_lost"
  )
})

test_that("heartbeat_before_retry() takes the long rate-limit waits over from httr2", {
  local_lease_conf(instance_id = "instance-A", runner_heartbeat_span = 0.05)
  claim_runner_lease("detect", guard = 0)
  conf$runner_name <- "detect"

  renewals <- 0
  local_mocked_bindings(
    renew_runner_lease = function(name = conf$runner_name) {
      renewals <<- renewals + 1
      assign(name, Sys.time(), envir = lease_last_renewal)
      TRUE
    }
  )

  elapsed <- system.time(waited <- heartbeat_before_retry(0.3))[["elapsed"]]

  # the wait happened here, with renewals...
  expect_gte(elapsed, 0.3 * 0.95)
  expect_gte(renewals, 2)
  # ...and httr2 is told it has nothing left to wait for
  expect_identical(waited, 0)
})

test_that("heartbeat_before_retry() leaves the httr2 backoff alone", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_heartbeat_span = 30,
    runner_name = "detect"
  )
  claim_runner_lease("detect", guard = 0)

  # NA: a transient 500/502/503, httr2 backs off on its own (60 seconds per try at most)
  expect_true(is.na(heartbeat_before_retry(NA)))
  # a wait shorter than the span is not worth chopping either
  expect_equal(heartbeat_before_retry(5), 5)
  # both beat on the way, since nothing had been renewed yet in this process
  expect_false(heartbeat_is_due("detect"))
})

test_that("renew_fs_runner_lease() beats for the fs while it answers /ping", {
  local_lease_conf(instance_id = "instance-A")
  claim_runner_lease("fs", guard = 0)

  local_mocked_bindings(is_fs_running = function() TRUE)

  expect_true(renew_fs_runner_lease())
  # the /ping call and the renewal only happen when a beat is due, not on every 2-second
  # iteration of the fs monitor loop
  expect_false(renew_fs_runner_lease())
})

test_that("renew_fs_runner_lease() lets the lease expire when the fs stops answering", {
  local_lease_conf(instance_id = "instance-A")
  claim_runner_lease("fs", guard = 0)

  # a dead or wedged fs must not be kept alive by its monitor: this is what tells the monitor
  # apart from the background beater the plan rejects
  local_mocked_bindings(is_fs_running = function() FALSE)

  expect_false(renew_fs_runner_lease())
})
