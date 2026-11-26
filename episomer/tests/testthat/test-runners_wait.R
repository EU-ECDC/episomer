# Waiting for a busy runner lease (multi-instance services, plan issue 14).
#
# The situation the whole file is about: a service starts, finds its runner still held by the
# instance that was running a minute ago, and has to do something better than exit. Since issue 14
# it waits, so the two questions asked here are "does it end up collecting?" and "does it still
# refuse everything it used to refuse?".
#
# Same setup as test-runners_lease.R and test-runners_register.R: fs_remote = NULL so epipath()
# resolves to a temp directory, a "second instance" is another conf$instance_id borrowed with
# with_instance(), and runner_claim_guard = 0 because the guard wait has its own tests elsewhere.
#
# The waits themselves are kept short on purpose. Two tests let the clock run for real - a second
# or so, against a one-second TTL - because that is the only way to see an expiry actually happen;
# everything else mocks claim_runner_lease() so that the retry loop, its logging and its return
# value are exercised without spending time.

# A claim that fails `n` times and then succeeds, standing in for a holder whose lease expires
# while we wait. Returns the counter so a test can check how many attempts it took.
failing_claims <- function(n) {
  attempts <- 0
  function(name, guard = runner_claim_guard()) {
    attempts <<- attempts + 1
    attempts > n
  }
}

# Configuration knobs -----------------------------------------------------------------------------

test_that("runner_wait_interval() reads the configuration", {
  local_conf(runner_wait_interval = 3)
  expect_identical(runner_wait_interval(), 3)

  # a value coming from EPISOMER_OPTION_RUNNER_WAIT_INTERVAL arrives as a string
  local_conf(runner_wait_interval = "7")
  expect_identical(runner_wait_interval(), 7)
})

test_that("runner_wait_interval() refuses a value that would make a busy loop", {
  # zero or negative would mean re-reading the lease file as fast as the fs answers
  for (bad in list(0, -5, "later", NA)) {
    local_conf(runner_wait_interval = bad)
    expect_identical(runner_wait_interval(), runner_wait_interval_default)
  }
  local_conf()
  expect_identical(runner_wait_interval(), runner_wait_interval_default)
})

test_that("runner_wait_timeout() defaults to waiting as long as it takes", {
  local_conf()
  expect_identical(runner_wait_timeout(), Inf)

  # Inf survives properties.json as the string "Inf" (jsonlite has no infinity), which is exactly
  # the round trip a deployment that never sets the property goes through
  local_conf(runner_wait_timeout = "Inf")
  expect_identical(runner_wait_timeout(), Inf)

  local_conf(runner_wait_timeout = 600)
  expect_identical(runner_wait_timeout(), 600)
})

test_that("runner_wait_timeout() falls back on Inf for a value it cannot use", {
  for (bad in list(-1, "soon", NA)) {
    local_conf(runner_wait_timeout = bad)
    expect_identical(runner_wait_timeout(), Inf)
  }
})

# Waiting -----------------------------------------------------------------------------------------

test_that("wait_for_runner_lease() claims a free runner at once, without waiting or logging", {
  local_lease_conf(runner_claim_guard = 0, runner_wait_interval = 30)

  attempts <- local_count_claims()
  expect_silent(expect_true(wait_for_runner_lease("detect")))

  # nothing held it, so not one sleep of the 30-second interval was paid
  expect_lte(attempts(), 1L)
  expect_true(i_hold_runner_lease("detect"))
})

test_that("wait_for_runner_lease() keeps trying until the runner is free", {
  skip_on_cran()
  local_lease_conf(runner_claim_guard = 0, runner_wait_interval = 0.01)
  claim <- failing_claims(3)
  local_mocked_bindings(claim_runner_lease = claim, .package = "episomer")

  expect_true(suppressMessages(wait_for_runner_lease("search.bluesky")))
  # the first attempt plus three refusals: the loop does not stop at the first "no"
  expect_identical(environment(claim)$attempts, 4)
})

test_that("wait_for_runner_lease() gives up when the timeout runs out", {
  skip_on_cran()
  local_lease_conf(runner_claim_guard = 0, runner_wait_interval = 0.01)
  local_mocked_bindings(
    claim_runner_lease = function(name, guard = runner_claim_guard()) FALSE,
    .package = "episomer"
  )

  expect_false(suppressMessages(wait_for_runner_lease("search.bluesky", timeout = 0.05)))
})

test_that("a timeout of zero is one attempt and no wait at all", {
  # This is what register_runner() does by default, and what the fs monitor - which re-registers on
  # every iteration of its 2-second loop - depends on: no sleep may ever happen there.
  local_lease_conf(
    instance_host = "machine-A",
    runner_ttl_fs = 120,
    runner_claim_guard = 0,
    runner_wait_interval = 30
  )
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("fs_mon"))

  attempts <- local_count_claims()
  expect_false(wait_for_runner_lease("fs_mon", timeout = 0))

  expect_lte(attempts(), 1L)
  expect_identical(read_runner_lease("fs_mon")$instance_id, "instance-B")
})

test_that("wait_for_runner_lease() takes over as soon as the foreign lease expires", {
  skip_on_cran()
  # The real thing, clock included: a `docker compose down` && `up` inside the TTL, in miniature.
  # One second rather than the 300 of the search service, but every step is the production one -
  # a foreign lease, no release, an expiry read from the file mtime.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 1,
    runner_claim_guard = 0,
    runner_wait_interval = 0.2
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )
  expect_false(claim_runner_lease("search.bluesky"))

  expect_true(suppressMessages(wait_for_runner_lease("search.bluesky")))
  expect_true(i_hold_runner_lease("search.bluesky"))
})

test_that("waiting never steals a lease that is still alive", {
  skip_on_cran()
  # The one thing the wait must not weaken: it only ever ends on a successful claim, so a holder
  # that keeps renewing is left strictly alone however long we sit there.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0,
    runner_wait_interval = 0.05
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  expect_false(suppressMessages(
    wait_for_runner_lease("search.bluesky", timeout = 0.2)
  ))

  expect_identical(read_runner_lease("search.bluesky")$instance_id, "instance-B")
  expect_false(i_hold_runner_lease("search.bluesky"))
})

test_that("wait_for_runner_lease() still takes over a runner its own machine left behind", {
  skip_on_cran()
  # The fast reclaim of section 3.6 happens inside claim_runner_lease(), so waiting must not delay
  # it: a holder that died on this very machine is dead now, and there is nothing to wait for.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 1800,
    runner_claim_guard = 0,
    runner_wait_interval = 60
  )
  with_instance("instance-B", host = "machine-A", code = write_runner_lease("detect"))
  set_lease_pid("detect", dead_pid)
  write_local_pid_file("detect", dead_pid)

  # Counted rather than timed: see local_count_claims().
  attempts <- local_count_claims()
  expect_true(wait_for_runner_lease("detect"))

  # not one 60-second interval was paid, and not one of the 30 minutes of the detect TTL
  expect_identical(attempts(), 1L)
  expect_true(i_hold_runner_lease("detect"))
})

# What the operator reads -------------------------------------------------------------------------

test_that("the waiting message names the holder, where it runs and when we try again", {
  local_lease_conf(
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  message <- runner_lease_wait_message("search.bluesky", timeout = Inf, interval = 15)

  # the same opening sentence as the refusal: one situation, one wording
  expect_match(message, "already held by another instance \\(instance-B\\)")
  expect_match(message, "running on host 'machine-B'")
  expect_match(message, "lease TTL: 300 seconds")
  # and the half the refusal does not say: something is going to happen, and when
  expect_match(message, "waiting for it to be released or to expire")
  expect_match(message, "every 15 seconds")
  # an unbounded wait announces no deadline
  expect_false(grepl("at most", message))
  expect_false(grepl("will not start", message))
})

test_that("a bounded wait says when it gives up", {
  skip_on_cran()
  local_lease_conf(
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  message <- runner_lease_wait_message("search.bluesky", timeout = 600, interval = 15)

  expect_match(message, "at most 10 min")
})

test_that("the waiting message copes with a lease that vanished between two attempts", {
  # Released, or taken over by a third instance whose write has not landed yet: there is no holder
  # to describe, only the retry.
  local_lease_conf(runner_claim_guard = 0)

  message <- runner_lease_wait_message("search.bluesky", timeout = Inf, interval = 15)

  expect_match(message, "lease is already gone")
  expect_match(message, "Trying again in 15 seconds")
})

test_that("a long wait does not write one line per attempt", {
  skip_on_cran()
  # runner_wait_log_span is a minute against an interval of seconds: an operator watching a
  # 30-minute detect TTL must not have to scroll through a thousand identical lines.
  local_lease_conf(
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0,
    runner_wait_interval = 0.01
  )
  # a real foreign lease, so the line written is the one describing a holder, and a claim that
  # refuses twenty times, so the loop has twenty chances to write it again
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )
  local_mocked_bindings(
    claim_runner_lease = failing_claims(20),
    .package = "episomer"
  )

  waiting <- testthat::capture_messages(wait_for_runner_lease("search.bluesky"))

  # one "still waiting" line for the whole wait, plus the line saying it was finally claimed
  expect_length(grep("waiting for it to be released", waiting), 1)
  expect_length(grep("has been claimed after waiting", waiting), 1)
})

# register_runner(wait = ) ------------------------------------------------------------------------

test_that("register_runner() waits for the lease when it is asked to", {
  skip_on_cran()
  local_lease_conf(runner_claim_guard = 0, runner_wait_interval = 0.01)
  local_mocked_bindings(
    claim_runner_lease = failing_claims(2),
    .package = "episomer"
  )

  suppressMessages(register_runner("search.bluesky", wait = TRUE))

  expect_identical(conf$runner_name, "search.bluesky")
  expect_true(file.exists(file.path(conf$data_dir, "search.bluesky.PID")))
})

test_that("register_runner() refuses at once by default", {
  # The behaviour every caller but the three service loops relies on: the Shiny app deciding
  # whether to spawn a task, and the fs monitor re-registering every two seconds.
  local_lease_conf(
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0,
    runner_wait_interval = 30
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  attempts <- local_count_claims()
  expect_error(register_runner("search.bluesky"), "will not start it")

  expect_lte(attempts(), 1L)
})

test_that("register_runner(wait = TRUE) still refuses a runner this machine already runs", {
  # A live process of *this* machine holding the runner is an operator mistake - the same service
  # started twice - not the standby situation, so it is never waited for: the answer is a PID to
  # kill, and it has to be given straight away.
  local_lease_conf(runner_claim_guard = 0, runner_wait_interval = 30)
  write_local_pid_file("detect", Sys.getpid() + 1L)
  local_mocked_bindings(
    is_r_pid_running = function(pid) TRUE,
    .package = "episomer"
  )

  attempts <- local_count_claims()
  expect_error(register_runner("detect", wait = TRUE), "is already running")

  expect_lte(attempts(), 1L)
})

test_that("register_runner() gives up with the historical message when the wait times out", {
  skip_on_cran()
  local_lease_conf(
    instance_host = "machine-A",
    runner_ttl_search = 300,
    runner_claim_guard = 0,
    runner_wait_timeout = 0.05,
    runner_wait_interval = 0.01
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  expect_error(
    suppressMessages(register_runner("search.bluesky", wait = TRUE)),
    "will not start it"
  )
  # refused means refused: the local disk is left exactly as it was found (issue 13)
  expect_false(file.exists(file.path(conf$data_dir, "search.bluesky.PID")))
  expect_null(conf$runner_name)
})
