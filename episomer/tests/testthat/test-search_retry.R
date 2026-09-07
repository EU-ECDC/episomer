# Surviving a failed request (R/search.R).
#
# Until this was written, one failed request ended the search worker of that network - and with it
# the collection of that network until somebody restarted the whole service. It was observed live:
# the bluesky worker stopped six seconds after its first request, its log file stopped mid-
# collection with no error in it, and mastodon kept collecting for hours next to it as if nothing
# had happened.
#
# Three things are tested here, in the order they matter:
#
#   * a failure is absorbed and the request is tried again;
#   * failing for ever is not absorbed: the worker gives the network up, so that the lease is
#     released and the network shows up as missing instead of looking healthy;
#   * the error text reaches the log, since that is the only place an operator will find it.
#
# Same conventions as test-runners_guard.R: fs_remote = NULL so nothing touches the network, and
# search_topic() is mocked - what is under test is the loop around it.

# The worker sleeps between two attempts. Real waits are five seconds and up (they double), so the
# tests replace them by a record of what would have been waited for.
local_no_retry_wait <- function(.env = parent.frame()) {
  waits <- new.env(parent = emptyenv())
  waits$seconds <- numeric(0)
  local_mocked_bindings(
    search_retry_wait = function(failures) {
      waits$seconds <- c(waits$seconds, failures)
      invisible(0)
    },
    .package = "episomer",
    .env = .env
  )
  waits
}

# A search_topic() that fails `times` times and then answers, counting its calls.
a_flaky_search_topic <- function(times, message = "HTTP 503 Service Unavailable") {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0
  list(
    calls = calls,
    fun = function(plan, query, topic, sandboxed = FALSE) {
      calls$n <- calls$n + 1
      if (calls$n <= times) {
        stop(message)
      }
      plan$requests <- plan$requests + 1
      plan
    }
  )
}

# try_search_topic() -----------------------------------------------------------------------------

test_that("try_search_topic() reports a successful request with the updated plan", {
  local_conf()
  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      plan$requests <- 42
      plan
    },
    .package = "episomer"
  )

  attempt <- try_search_topic(
    plan = list(network = "bluesky", requests = 0),
    query = "something",
    topic = "topic-1",
    sandboxed = TRUE,
    is_first_plan = TRUE
  )

  expect_identical(attempt$status, "done")
  expect_identical(attempt$plan$requests, 42)
})

test_that("try_search_topic() returns a failed request instead of raising", {
  local_conf()
  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      stop("HTTP 503 Service Unavailable")
    },
    .package = "episomer"
  )

  attempt <- try_search_topic(
    plan = list(network = "bluesky", requests = 0),
    query = "something",
    topic = "topic-1",
    sandboxed = TRUE,
    is_first_plan = TRUE
  )

  # the condition itself is kept, not just its text: the caller logs it and the loop above may
  # still want to tell one class of failure from another later on
  expect_identical(attempt$status, "failed")
  expect_s3_class(attempt$error, "error")
  expect_identical(conditionMessage(attempt$error), "HTTP 503 Service Unavailable")
})

test_that("try_search_topic() lets a lost lease through untouched", {
  local_lease_conf(instance_id = "instance-A", runner_name = "search.bluesky")
  claim_runner_lease("search.bluesky", guard = 0)
  steal_lease("search.bluesky")

  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) stop_if_lease_lost(),
    .package = "episomer"
  )

  # a lost lease is not a request that went wrong: retrying it would mean writing from an instance
  # that is not the runner anymore, so it must keep its class and reach run_until_lease_lost()
  expect_error(
    try_search_topic(
      plan = list(network = "bluesky", requests = 0),
      query = "something",
      topic = "topic-1",
      sandboxed = TRUE,
      is_first_plan = TRUE
    ),
    class = "episomer_lease_lost"
  )
})

test_that("try_search_topic() retries the first plan of a topic without its lower bound", {
  local_conf()
  seen <- list()
  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      seen[[length(seen) + 1]] <<- plan
      if (is.null(plan$since_id)) {
        return(plan)
      }
      stop("too-old")
    },
    .package = "episomer"
  )

  attempt <- suppressMessages(try_search_topic(
    plan = list(network = "bluesky", requests = 0, since_id = "42", since_target = "42"),
    query = "something",
    topic = "topic-1",
    sandboxed = TRUE,
    is_first_plan = TRUE
  ))

  expect_identical(attempt$status, "done")
  expect_length(seen, 2)
  expect_null(seen[[2]]$since_id)
  expect_null(seen[[2]]$since_target)
})

test_that("try_search_topic() runs the too-old retry with the sandboxed flag it was given", {
  local_conf()
  sandboxes <- logical(0)
  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      sandboxes <<- c(sandboxes, sandboxed)
      if (is.null(plan$since_id)) {
        return(plan)
      }
      stop("too-old")
    },
    .package = "episomer"
  )

  suppressMessages(try_search_topic(
    plan = list(network = "bluesky", requests = 0, since_id = "42"),
    query = "something",
    topic = "topic-1",
    sandboxed = TRUE,
    is_first_plan = TRUE
  ))

  # the recovery used to call search_topic() without passing `sandboxed` on, so a sandboxed run
  # recovering from a too-old request uploaded posts after all
  expect_identical(sandboxes, c(TRUE, TRUE))
})

test_that("try_search_topic() reports too-old on any plan but the first", {
  local_conf()
  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) stop("too-old"),
    .package = "episomer"
  )

  attempt <- try_search_topic(
    plan = list(network = "bluesky", requests = 0),
    query = "something",
    topic = "topic-1",
    sandboxed = TRUE,
    is_first_plan = FALSE
  )

  # the caller ends the plan on this: only the first plan of a topic is the one walking back in
  # time, so only that one is worth asking again without its lower bound
  expect_identical(attempt$status, "too-old")
})

# The worker -------------------------------------------------------------------------------------

test_that("search_loop_worker() retries a failed request instead of giving the network up", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = a_topic_with_one_plan()
  )
  claim_runner_lease("search.bluesky", guard = 0)
  waits <- local_no_retry_wait()

  flaky <- a_flaky_search_topic(times = 2)
  local_mocked_bindings(search_topic = flaky$fun, .package = "episomer")

  suppressMessages(outcome <- search_loop_worker(
    "bluesky",
    sandboxed = TRUE,
    max_requests = 3
  ))

  # two failures, then the request goes through: the worker is still there, and it never stopped
  # collecting this network
  expect_gt(flaky$calls$n, 2)
  expect_identical(outcome, "ended")
  # and it waited between the attempts, longer the second time
  expect_identical(waits$seconds, c(1, 2))
})

test_that("search_loop_worker() writes down why a request failed", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = a_topic_with_one_plan()
  )
  claim_runner_lease("search.bluesky", guard = 0)
  local_no_retry_wait()

  flaky <- a_flaky_search_topic(times = 1, message = "HTTP 401 Unauthorized")
  local_mocked_bindings(search_topic = flaky$fun, .package = "episomer")

  # the log of the network is the only place this text ever appears: the worker runs in a PSOCK
  # node whose errors the parent process cannot read while the other networks are still looping
  expect_message(
    search_loop_worker("bluesky", sandboxed = TRUE, max_requests = 2),
    "HTTP 401 Unauthorized"
  )
})

test_that("search_loop_worker() gives the network up after too many consecutive failures", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = a_topic_with_one_plan()
  )
  claim_runner_lease("search.bluesky", guard = 0)
  local_no_retry_wait()

  flaky <- a_flaky_search_topic(times = Inf, message = "HTTP 401 Unauthorized")
  local_mocked_bindings(
    search_topic = flaky$fun,
    search_max_request_failures = function() 3,
    .package = "episomer"
  )

  # retrying for ever would be worse than stopping: the worker would keep renewing its lease and
  # the service would look healthy while collecting nothing. Stopping releases the lease, which is
  # what puts the network in missing_search_jobs()
  suppressMessages(expect_error(
    search_loop_worker("bluesky", sandboxed = TRUE, max_requests = 0),
    "3 consecutive requests failed on bluesky.*HTTP 401 Unauthorized"
  ))
  expect_identical(flaky$calls$n, 3)
})

test_that("search_loop_worker() counts consecutive failures only", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = a_topic_with_one_plan()
  )
  claim_runner_lease("search.bluesky", guard = 0)
  local_no_retry_wait()

  # fail, succeed, fail, succeed...: a flaky network must never reach the cap, however long it
  # stays flaky
  n <- 0
  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      n <<- n + 1
      if (n %% 2 == 1) {
        stop("HTTP 503 Service Unavailable")
      }
      plan$requests <- plan$requests + 1
      plan
    },
    search_max_request_failures = function() 2,
    .package = "episomer"
  )

  suppressMessages(outcome <- search_loop_worker(
    "bluesky",
    sandboxed = TRUE,
    max_requests = 6
  ))

  expect_identical(outcome, "ended")
})

# The backoff ------------------------------------------------------------------------------------

test_that("search_retry_wait() backs off exponentially and stops growing at five minutes", {
  local_conf()
  slept <- numeric(0)
  local_mocked_bindings(
    sleep_with_heartbeat = function(seconds, ...) {
      slept <<- c(slept, seconds)
      invisible(NULL)
    },
    .package = "episomer"
  )

  suppressMessages(for (failures in 1:9) search_retry_wait(failures))

  # doubling, then capped: an API that is down must not be asked every five seconds for twenty
  # minutes, and a collection must not stay down for an hour because of one bad minute
  expect_identical(slept, c(5, 10, 20, 40, 80, 160, 300, 300, 300))
})

test_that("search_retry_wait() waits through sleep_with_heartbeat(), never Sys.sleep()", {
  # a heartbeat every tenth of a second so that the wait is chopped, renewed and checked at once:
  # the point is where the check happens, not how long the test takes to get there
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    runner_heartbeat_span = 0.1
  )
  claim_runner_lease("search.bluesky", guard = 0)
  steal_lease("search.bluesky")

  # a five-minute backoff is longer than the search TTL: waiting with a plain Sys.sleep() would
  # hand the network to a standby machine, and waking up to collect after somebody else took over
  # is exactly what the lease is there to prevent
  suppressMessages(expect_error(
    search_retry_wait(1),
    class = "episomer_lease_lost"
  ))
})
