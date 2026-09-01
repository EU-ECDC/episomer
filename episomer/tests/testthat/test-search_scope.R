# Keeping each search worker to the topics of its own network (R/config.R, R/search.R).
#
# conf$topics is a flat list: one entry per (topic, query, network) triple, every activated network
# together. Three Excel topics, one of which holds three synonyms, over three networks is fifteen
# entries - and each search worker must make requests for exactly the five that carry its network.
#
# It did not. The worker looped over the whole list and tested `plan$network`, not the entry's
# network - and on a fresh start, where no entry has a plan yet, the plan is created by
# update_plans_schedule() with *the worker's own* network in it (R/plans.R), so the test passed for
# every entry. Observed live on 2026-08-31, right after the multi-instance Mastodon setup went up:
# three activated networks meant every topic was searched three times per network, with the second
# and third request of each triple being a byte-for-byte repeat of the first.
#
# It stayed invisible on disk because save_config() has always filtered on the entry's network, so
# the extra plans were never written: topics.bluesky.json looked perfectly normal while the bluesky
# log showed "searching for topic trump" three times in a row.
#
# Same conventions as test-search_retry.R, which drives the same worker: fs_remote = NULL so
# nothing touches the network, sandboxed = TRUE so nothing is written, and search_topic() mocked -
# what is under test is which topics the loop hands it.

# The fifteen-entry shape of the bug, in miniature: one topic per network plus a topic with two
# queries on the worker's own network, so that a test can tell "the right network" from "the right
# number of entries".
topics_of_three_networks <- function() {
  entry <- function(topic, query, network) {
    list(topic = topic, query = query, network = network, plan = list())
  }
  list(
    entry("trump", "trump", "bluesky"),
    entry("trump", "trump", "mastodon.mastodon.social"),
    entry("trump", "trump", "mastodon.mstdn.party"),
    entry("covid-19", "coronavirus", "bluesky"),
    entry("covid-19", "covid-19", "bluesky"),
    entry("covid-19", "coronavirus", "mastodon.mstdn.party")
  )
}

# A search_topic() that records the (network, topic, query) of every request instead of making one.
a_recording_search_topic <- function() {
  calls <- new.env(parent = emptyenv())
  calls$seen <- character(0)
  list(
    calls = calls,
    fun = function(plan, query, topic, sandboxed = FALSE) {
      calls$seen <- c(calls$seen, paste(plan$network, topic, query))
      plan$requests <- plan$requests + 1
      plan
    }
  )
}

# topics_of_network() ----------------------------------------------------------------------------

test_that("topics_of_network() selects on the entry network, not on its plans", {
  local_conf(topics = topics_of_three_networks())

  expect_identical(topics_of_network("bluesky"), c(1L, 4L, 5L))
  expect_identical(topics_of_network("mastodon.mstdn.party"), c(3L, 6L))
  # an instance that is configured but has no topic yet is not an error: the worker waits
  expect_identical(topics_of_network("mastodon.piaille.fr"), integer(0))
})

test_that("topics_of_network() is not fooled by a plan belonging to another network", {
  topics <- topics_of_three_networks()
  # exactly what the bug produced in memory: a mastodon entry carrying a bluesky plan, because the
  # bluesky worker got there first and created it
  topics[[2]]$plan <- list(list(network = "bluesky", requests = 0))
  local_conf(topics = topics)

  expect_false(2L %in% topics_of_network("bluesky"))
  expect_identical(topics_of_network("mastodon.mastodon.social"), 2L)
})

# The worker -------------------------------------------------------------------------------------

test_that("search_loop_worker() only searches the topics of its own network", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = topics_of_three_networks()
  )
  claim_runner_lease("search.bluesky", guard = 0)

  recorded <- a_recording_search_topic()
  local_mocked_bindings(search_topic = recorded$fun, .package = "episomer")

  suppressMessages(outcome <- search_loop_worker(
    "bluesky",
    sandboxed = TRUE,
    max_requests = 2
  ))

  expect_identical(outcome, "ended")
  # three entries carry "bluesky", and the round robin visits them in order: nothing from the two
  # mastodon instances, and no repeat of "trump" within a pass
  expect_identical(
    unique(recorded$calls$seen),
    c(
      "bluesky trump trump",
      "bluesky covid-19 coronavirus",
      "bluesky covid-19 covid-19"
    )
  )
})

test_that("search_loop_worker() creates a plan only for the topics of its own network", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.bluesky",
    collect_span = 60,
    topics = topics_of_three_networks()
  )
  claim_runner_lease("search.bluesky", guard = 0)

  recorded <- a_recording_search_topic()
  local_mocked_bindings(search_topic = recorded$fun, .package = "episomer")

  suppressMessages(search_loop_worker("bluesky", sandboxed = TRUE, max_requests = 1))

  # the plan is the bookmark of a collection, so a worker fabricating one for somebody else's
  # entry is what made the entry collectable by the wrong network in the first place
  planned <- vapply(conf$topics, function(t) length(t$plan) > 0, logical(1))
  expect_identical(which(planned), c(1L, 4L, 5L))
  expect_true(all(vapply(
    conf$topics[planned],
    function(t) identical(t$plan[[1]]$network, "bluesky"),
    logical(1)
  )))
})

test_that("search_loop_worker() waits instead of spinning when it has no topic of its own", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_name = "search.mastodon.piaille.fr",
    collect_span = 60,
    topics = topics_of_three_networks()
  )
  claim_runner_lease("search.mastodon.piaille.fr", guard = 0)

  recorded <- a_recording_search_topic()
  local_mocked_bindings(
    search_topic = recorded$fun,
    # the branch sleeps and reloads the configuration before looping; both are the point of the
    # branch and neither belongs in a test, so they are replaced by a way out of the loop
    sleep_with_heartbeat = function(seconds) invisible(seconds),
    setup_config = function(...) stop("no topic appeared"),
    .package = "episomer"
  )

  # an instance added to mastodon_instances before topics.xlsx is touched again reaches exactly
  # this state: activated, no translated topic. It must not make requests, and it must not spin on
  # a `1:0` loop over an empty topic list either
  suppressMessages(expect_error(
    search_loop_worker("mastodon.piaille.fr", sandboxed = TRUE, max_requests = 1),
    "no topic appeared"
  ))
  expect_identical(recorded$calls$seen, character(0))
})
