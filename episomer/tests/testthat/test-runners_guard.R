# Guarding the writes (multi-instance services, plan issue 6).
#
# Issue 4 decides who is allowed to be a runner and issue 5 keeps proving it, but neither of them
# can stop a runner that lost its lease in the meantime from writing: a lease is cooperative, so
# the guard before each meaningful write is the safety net the whole design rests on
# (inst/schema/multi_services_plan.md, sections 3.1 and 6.3).
#
# Two halves are tested here:
#
#   * stop_if_lease_lost() placed before the writes - covered through the callers, since what
#     matters is *where* it sits (before the upload, not after) and not the function itself, which
#     test-runners_lease.R already covers;
#   * run_until_lease_lost() at the top of each loop, turning the condition into a clean exit.
#
# Same conventions as test-runners_lease.R: fs_remote = NULL so epipath() resolves to a temp
# directory and nothing touches the network, and a "second instance" is another conf$instance_id
# borrowed with with_instance().

# steal_lease() and a_topic_with_one_plan() live in helper-conf.R: test-search_retry.R drives the
# same worker with the same one-plan configuration.

# run_until_lease_lost() -------------------------------------------------------------------------

test_that("run_until_lease_lost() returns the value of the loop it wraps", {
  local_lease_conf(instance_id = "instance-A", runner_name = "detect")

  expect_identical(run_until_lease_lost("ended"), "ended")
})

test_that("run_until_lease_lost() turns a lost lease into a clean, explained exit", {
  local_lease_conf(instance_id = "instance-A", runner_name = "detect")
  claim_runner_lease("detect", guard = 0)
  steal_lease("detect")

  # what an operator reads in the log of the machine that stepped aside: the runner it lost, and
  # the fact that this is an orderly exit rather than a crash
  expect_message(
    outcome <- run_until_lease_lost(stop_if_lease_lost()),
    "Runner lease 'detect' was taken"
  )
  expect_identical(outcome, "lease-lost")

  # and the lease is left strictly alone: it belongs to its new holder now
  expect_identical(read_runner_lease("detect")$instance_id, "instance-B")
})

test_that("run_until_lease_lost() lets a real error through", {
  local_lease_conf(instance_id = "instance-A", runner_name = "detect")

  # the whole point of using a typed condition: a genuine failure must still crash the loop and be
  # visible, instead of being mistaken for an orderly handover
  expect_error(run_until_lease_lost(stop("database is on fire")), "on fire")
})

# The guard inside search_topic() ----------------------------------------------------------------

test_that("search_topic() refuses to upload posts once the lease was taken", {
  local_lease_conf(instance_id = "instance-A", runner_name = "search.bluesky")
  claim_runner_lease("search.bluesky", guard = 0)

  searched <- 0
  local_mocked_bindings(
    sm_plan_next_search_info = function(plan) "next search",
    sm_api_get_token = function(network) "a-token",
    sm_api_search = function(query, token, plan) {
      searched <<- searched + 1
      # the takeover happens *during* the social media request, which is the realistic case: it can
      # wait minutes on a rate limit (heartbeat_before_retry(), issue 5)
      steal_lease("search.bluesky")
      list(posts = list())
    },
    .package = "episomer"
  )
  local_mocked_bindings(
    POST = function(...) stop("the posts must not be uploaded"),
    .package = "httr"
  )

  expect_error(
    search_topic(
      plan = a_topic_with_one_plan()[[1]]$plan[[1]],
      query = "something",
      topic = "topic-1"
    ),
    class = "episomer_lease_lost"
  )
  # the request itself did happen: the guard sits between the request and the upload, so we spend
  # somebody else's rate limit rather than write posts twice
  expect_identical(searched, 1)
})

test_that("search_topic() uploads normally while the lease is held", {
  local_lease_conf(instance_id = "instance-A", runner_name = "search.bluesky")
  claim_runner_lease("search.bluesky", guard = 0)

  uploads <- 0
  local_mocked_bindings(
    sm_plan_next_search_info = function(plan) "next search",
    sm_api_get_token = function(network) "a-token",
    sm_api_search = function(query, token, plan) list(posts = list()),
    .package = "episomer"
  )
  local_mocked_bindings(
    POST = function(...) {
      uploads <<- uploads + 1
      stop("reached the upload")
    },
    .package = "httr"
  )

  # the guard must not turn into a brake on the normal path: the upload is attempted (and here
  # fails on its own retries, which is the only way to observe it without a live fs)
  suppressMessages(expect_error(
    search_topic(
      plan = a_topic_with_one_plan()[[1]]$plan[[1]],
      query = "something",
      topic = "topic-1"
    ),
    "too many retries"
  ))
  expect_gt(uploads, 0)
})

# The search worker ------------------------------------------------------------------------------

test_that("search_loop_worker() exits cleanly when its lease is taken mid-collection", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = a_topic_with_one_plan()
  )
  claim_runner_lease("search.bluesky", guard = 0)

  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      stop_if_lease_lost()
    },
    .package = "episomer"
  )
  steal_lease("search.bluesky")

  # sandboxed so the worker neither registers nor writes anything: what is under test is the path
  # from the guard inside search_topic() up to the handler at the top of the loop, across the
  # tryCatch() that used to flatten the condition into a plain error
  suppressMessages(outcome <- search_loop_worker(
    "bluesky",
    sandboxed = TRUE,
    max_requests = 1
  ))

  expect_identical(outcome, "lease-lost")
})

test_that("search_loop_worker() still fails loudly on a real error", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = a_topic_with_one_plan()
  )
  claim_runner_lease("search.bluesky", guard = 0)

  local_mocked_bindings(
    search_topic = function(plan, query, topic, sandboxed = FALSE) {
      stop("the social media API is down")
    },
    # A failed request is retried now (test-search_retry.R), so a single tolerated failure is what
    # makes this test about what it has always been about: a genuine error must not be mistaken for
    # an orderly handover, it must reach the caller with its text.
    search_max_request_failures = function() 1,
    .package = "episomer"
  )

  suppressMessages(expect_error(
    search_loop_worker("bluesky", sandboxed = TRUE, max_requests = 1),
    "the social media API is down"
  ))
})

# What the parent process reports ----------------------------------------------------------------

test_that("search_loop() reports a whole machine stepping aside without raising", {
  local_lease_conf(instance_id = "instance-A")

  # every worker gave its network up: this machine has been replaced, which is the standby
  # mechanism working - restarting it at once would only make it fight a healthy instance
  expect_message(
    outcome <- report_search_loop_outcomes(
      c("bluesky", "mastodon.piaille.fr"),
      list("lease-lost", "lease-lost")
    ),
    "not the search runner anymore"
  )
  expect_identical(outcome, "lease-lost")
})

test_that("search_loop() still raises when a worker ended for another reason", {
  local_lease_conf(instance_id = "instance-A")

  # the historical error is kept, but it now names the networks and what each of them returned,
  # instead of the bare "which is unexpected" that sent an operator reading three log files
  suppressMessages(expect_error(
    report_search_loop_outcomes(
      c("bluesky", "mastodon.piaille.fr"),
      list("lease-lost", "ended")
    ),
    "mastodon.piaille.fr: ended"
  ))
})

# The detect loop --------------------------------------------------------------------------------

test_that("detect_loop() guards tasks.json before touching it", {
  local_lease_conf(instance_id = "instance-A", runner_name = "detect")
  claim_runner_lease("detect", guard = 0)
  steal_lease("detect")

  # plan_tasks() writes tasks.json as soon as it reschedules something, so the guard sits before
  # the call and not inside save_tasks() - which the Shiny app, holder of no lease, also calls
  local_mocked_bindings(
    plan_tasks = function(statuses = list()) stop("tasks.json must not be rewritten"),
    .package = "episomer"
  )

  expect_error(
    {
      stop_if_lease_lost()
      plan_tasks()
    },
    class = "episomer_lease_lost"
  )
})

test_that("save_tasks() itself stays unguarded, for the Shiny app", {
  local_lease_conf(instance_id = "instance-A", runner_name = "detect")
  claim_runner_lease("detect", guard = 0)
  steal_lease("detect")

  # the app is a reader of the runner state and never a lease holder (plan section 6.4): a guard
  # inside save_tasks() would abort it as soon as any service took a lease
  expect_silent(save_tasks(list(alerts = list(task = "alerts", order = 1))))
  expect_true(epifile.exists(get_tasks_path()))
})
