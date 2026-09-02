# Tests for R/plans.R (roadmap item T1-2).
#
# The whole file is pure: the only configuration it reads is conf$collect_history_days (through
# sm_plan_first_attributes) and conf$schedule_span (in finish_plans), both provided with
# local_conf(). Network-specific attributes are delegated to sm_plan_*_<family>, so the tests below
# exercise the real bluesky and mastodon implementations rather than mocks: the round-trip
# invariant is only meaningful end to end.

# a plan as it exists in conf, i.e. already parsed (dates as POSIXlt)
a_plan <- function(network = "bluesky", ...) {
  args <- list(
    network = network,
    expected_end = "2026-01-02 03:04:05",
    scheduled_for = "2026-01-01 08:30:00",
    requests = 1,
    got_rows = TRUE,
    progress = 0.5
  )
  overrides <- list(...)
  args[names(overrides)] <- overrides
  do.call(parse_plan_attributes, args)
}

# a bare list standing in for a plan, when only a couple of fields matter (scheduling branches)
a_stub <- function(id, ended = FALSE, requests = 0, ...) {
  c(
    list(
      network = "bluesky",
      id = id,
      requests = requests,
      end_on = if (ended) Sys.time() else NULL
    ),
    list(...)
  )
}

# reproduces the real persistence pipeline: conf -> save_config (format_plan + write_json) ->
# setup_config (read_json + copy_plans_from -> parse_plan_attributes). Anything that does not
# survive this is lost on every restart, so this is the round-trip that matters, not
# parse(format(x)) in memory.
save_load_plan <- function(plan) {
  json <- jsonlite::toJSON(format_plan(plan), auto_unbox = TRUE)
  do.call(
    parse_plan_attributes,
    jsonlite::fromJSON(json, simplifyVector = FALSE)
  )
}


# ---- parse_plan_attributes ---------------------------------------------------

test_that("parse_plan_attributes parses the common dates as POSIXlt", {
  p <- a_plan()

  expect_s3_class(p$expected_end, "POSIXlt")
  expect_s3_class(p$scheduled_for, "POSIXlt")
  expect_equal(format(p$expected_end), "2026-01-02 03:04:05")
  expect_equal(format(p$scheduled_for), "2026-01-01 08:30:00")
})

test_that("parse_plan_attributes keeps the optional dates as NULL", {
  p <- a_plan()

  # the names are present, the values are NULL: format_plan and merge_plans both rely on it
  expect_true(all(c("start_on", "end_on") %in% names(p)))
  expect_null(p$start_on)
  expect_null(p$end_on)
})

test_that("parse_plan_attributes applies the documented defaults", {
  p <- parse_plan_attributes(
    network = "bluesky",
    expected_end = "2026-01-02 03:04:05"
  )

  expect_equal(p$requests, 0)
  expect_false(p$got_rows)
  expect_equal(p$progress, 0)
})

test_that("parse_plan_attributes treats an empty json object as an absent date", {
  # jsonlite serialises a NULL as {} and reads it back as an empty list, not as NULL: the
  # is.null(unlist(x)) guards are what make a saved plan reloadable (same trap as
  # mastodon_is_present)
  p <- a_plan(start_on = list(), end_on = list())

  expect_null(p$start_on)
  expect_null(p$end_on)
})

test_that("parse_plan_attributes delegates the network-specific attributes to the family", {
  bsky <- a_plan(
    "bluesky",
    plan_max_date = "2026-01-02 03:04:05.123456Z"
  )
  expect_s3_class(bsky$plan_max_date, "POSIXlt")
  expect_equal(attr(bsky$plan_max_date, "tzone"), "UTC")
  # mastodon-only attributes have no reason to exist on a bluesky plan
  expect_false("current_offset" %in% names(bsky))

  masto <- a_plan(
    "mastodon.mstdn.party",
    plan_max_date = "2026-01-02 03:04:05.000000Z",
    current_offset = 120,
    current_min_id = "113456789012345678",
    consecutive_empty = 2,
    seen_ids = list("113456789012345678", "113456789012345677")
  )
  expect_type(masto$current_offset, "integer")
  expect_equal(masto$current_offset, 120L)
  expect_type(masto$current_min_id, "character")
  expect_equal(masto$consecutive_empty, 2L)
  expect_equal(
    masto$seen_ids,
    c("113456789012345678", "113456789012345677")
  )
})

test_that("parse_plan_attributes routes on the family, not on the full network name", {
  # every mastodon instance is a network of its own but shares the sm_*_mastodon implementations
  p <- a_plan(
    "mastodon.fosstodon.org",
    plan_max_date = "2026-01-02 03:04:05.000000Z",
    current_offset = 40
  )

  expect_equal(p$network, "mastodon.fosstodon.org")
  expect_equal(p$current_offset, 40L)
})


# ---- format_plan ------------------------------------------------------------

test_that("format_plan turns the network-specific dates into strings", {
  p <- a_plan("bluesky", plan_max_date = "2026-01-02 03:04:05.123456Z")

  f <- format_plan(p)

  expect_type(f$plan_max_date, "character")
  expect_equal(f$plan_max_date, "2026-01-02 03:04:05.123456Z")
})

test_that("format_plan leaves an absent network-specific date absent", {
  f <- format_plan(a_plan("bluesky"))

  expect_null(f$plan_max_date)
  expect_null(f$current_min_date)
})

test_that("format_plan dispatches to the mastodon implementation", {
  p <- a_plan(
    "mastodon.mstdn.party",
    plan_min_date = "2025-12-26 00:00:00.000000Z",
    current_offset = 80,
    seen_ids = list("113456789012345678")
  )

  f <- format_plan(p)

  expect_equal(f$plan_min_date, "2025-12-26 00:00:00.000000Z")
  # documented as serialising as-is
  expect_equal(f$current_offset, 80L)
  expect_equal(f$seen_ids, "113456789012345678")
})


# ---- round trip through the plan file ---------------------------------------

test_that("a bluesky plan survives a save and reload", {
  p <- a_plan(
    "bluesky",
    start_on = "2025-12-31 23:00:00",
    plan_max_date = "2026-01-02 03:04:05.123456Z",
    plan_min_date = "2025-12-26 10:00:00.000000Z",
    current_min_date = "2025-12-30 12:00:00.500000Z"
  )

  back <- save_load_plan(p)

  expect_equal(back$network, p$network)
  expect_equal(back$requests, p$requests)
  expect_equal(back$got_rows, p$got_rows)
  expect_equal(back$progress, p$progress)
  expect_equal(format(back$expected_end), format(p$expected_end))
  expect_equal(format(back$start_on), format(p$start_on))
  expect_null(back$end_on)
  expect_equal(format(back$plan_max_date), format(p$plan_max_date))
  expect_equal(format(back$plan_min_date), format(p$plan_min_date))
  expect_equal(format(back$current_min_date), format(p$current_min_date))
})

test_that("a mastodon plan survives a save and reload, pagination cursor included", {
  p <- a_plan(
    "mastodon.mstdn.party",
    plan_max_date = "2026-01-02 03:04:05.000000Z",
    plan_min_date = "2025-12-26 10:00:00.000000Z",
    current_min_date = "2025-12-30 12:00:00.000000Z",
    current_offset = 120,
    current_min_id = "113456789012345678",
    consecutive_empty = 2,
    seen_ids = list("113456789012345678", "113456789012345677")
  )

  back <- save_load_plan(p)

  expect_equal(back$current_offset, 120L)
  expect_equal(back$current_min_id, "113456789012345678")
  expect_equal(back$consecutive_empty, 2L)
  expect_equal(
    back$seen_ids,
    c("113456789012345678", "113456789012345677")
  )
  expect_equal(format(back$current_min_date), format(p$current_min_date))
})

test_that("a single seen id survives the auto_unbox of the plan file", {
  # auto_unbox = TRUE writes a length-1 vector as a json scalar, not as an array: the reload has
  # to give a character vector back, otherwise the de-duplication memory silently breaks
  p <- a_plan("mastodon.mstdn.party", seen_ids = list("113456789012345678"))

  back <- save_load_plan(p)

  expect_type(back$seen_ids, "character")
  expect_length(back$seen_ids, 1)
  expect_equal(back$seen_ids, "113456789012345678")
})

test_that("the sub-second precision of the network dates survives a save and reload", {
  p <- a_plan("bluesky", current_min_date = "2025-12-30 12:00:00.750000Z")

  back <- save_load_plan(p)

  expect_equal(
    format(back$current_min_date, "%OS3"),
    format(p$current_min_date, "%OS3")
  )
})

test_that("a common date falling exactly on midnight is LOST on reload", {
  # KNOWN BUG, behaviour frozen here on purpose (roadmap T1-2).
  # format_plan only stringifies the network-specific dates; the common ones (expected_end,
  # scheduled_for, start_on, end_on) reach the plan file as POSIXlt and are stringified by
  # jsonlite, whose format.POSIXlt drops the time part when it is exactly 00:00:00. The reload
  # then feeds "2026-01-01" to strptime("%Y-%m-%d %H:%M:%S"), which yields NA.
  # Consequence: update_plans_schedule() compares expected_end with Sys.time(), so an NA there
  # raises "missing value where TRUE/FALSE needed" and the search loop stops for that topic.
  # When this is fixed, this test must be replaced by the expect_equal below.
  p <- a_plan("bluesky", scheduled_for = "2026-01-01 00:00:00")

  back <- save_load_plan(p)

  expect_true(is.na(back$scheduled_for))
  # expect_equal(format(back$scheduled_for), format(p$scheduled_for))
})


# ---- merge_plans ------------------------------------------------------------

test_that("merge_plans gives precedence to the second plan", {
  merged <- merge_plans(
    list(requests = 1, progress = 0.2),
    list(requests = 9, progress = 1.0)
  )

  expect_equal(merged$requests, 9)
  expect_equal(merged$progress, 1.0)
})

test_that("merge_plans falls back on the first plan when the second holds a NULL", {
  merged <- merge_plans(
    list(network = "bluesky", start_on = "kept"),
    list(network = "mastodon", start_on = NULL)
  )

  expect_equal(merged$network, "mastodon")
  expect_equal(merged$start_on, "kept")
})

test_that("merge_plans keeps the keys present on one side only", {
  merged <- merge_plans(
    list(only_in_first = 1, shared = "first"),
    list(only_in_second = 2, shared = "second")
  )

  expect_setequal(
    names(merged),
    c("only_in_first", "shared", "only_in_second")
  )
  expect_equal(merged$shared, "second")
})

test_that("merge_plans coerces the values to a single type when they are all scalars", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-2).
  # merge_plans builds its result with mapply() and leaves SIMPLIFY at its TRUE default, so when
  # every merged value is a length-1 vector the result is simplified to an atomic vector and the
  # types are coerced to the widest one: a numeric `requests` comes back as the string "4".
  # It is latent rather than active today because finish_plans, the only caller, always merges
  # plans holding at least one POSIXlt or NULL, which blocks the simplification (see the test
  # below). It would bite the moment a plan is merged without any date.
  # The fix is SIMPLIFY = FALSE in mapply(); when applied, expect the types to be preserved here.
  merged <- merge_plans(
    list(network = "bluesky", requests = 1),
    list(network = "bluesky", requests = 4)
  )

  expect_type(merged$requests, "character")
  expect_equal(merged$requests, "4")
})

test_that("merge_plans preserves the types when a plan holds a date or a NULL", {
  # the realistic case, and the reason the coercion above never fires in production
  merged <- merge_plans(
    list(network = "bluesky", requests = 1, end_on = NULL),
    list(
      network = "bluesky",
      requests = 4,
      end_on = strptime("2026-01-02 03:04:05", "%Y-%m-%d %H:%M:%S")
    )
  )

  expect_type(merged$requests, "double")
  expect_equal(merged$requests, 4)
  expect_s3_class(merged$end_on, "POSIXlt")
})

test_that("merge_plans keeps a key that is NULL on both sides", {
  merged <- merge_plans(list(a = 1, b = NULL), list(a = 2))

  expect_true("b" %in% names(merged))
  expect_null(merged$b)
})


# ---- update_plans_schedule --------------------------------------------------

test_that("update_plans_schedule creates a first plan when there is none", {
  local_conf(collect_history_days = 7)

  plans <- update_plans_schedule(
    plans = list(),
    network = "bluesky",
    schedule_span = 60
  )

  expect_length(plans, 1)
  expect_equal(plans[[1]]$network, "bluesky")
  expect_equal(plans[[1]]$requests, 0)
  expect_equal(plans[[1]]$progress, 0)
  # the expected end is one span away, and the window backfills collect_history_days
  expect_equal(
    as.numeric(
      difftime(plans[[1]]$expected_end, Sys.time(), units = "mins")
    ),
    60,
    tolerance = 0.05
  )
  expect_equal(
    as.Date(plans[[1]]$plan_min_date),
    Sys.Date() - 7
  )
})

test_that("update_plans_schedule keeps the current plan untouched while it is running", {
  running <- a_plan(
    "bluesky",
    expected_end = strftime(Sys.time() + 3600, "%Y-%m-%d %H:%M:%S"),
    requests = 3
  )

  plans <- update_plans_schedule(
    plans = list(running),
    network = "bluesky",
    schedule_span = 60
  )

  expect_length(plans, 1)
  expect_equal(plans[[1]]$requests, 3)
  expect_equal(format(plans[[1]]$expected_end), format(running$expected_end))
})

test_that("update_plans_schedule does not renew a plan that has never run", {
  # requests == 0 means the plan never got its chance, an elapsed expected_end is not enough
  never_ran <- a_plan(
    "bluesky",
    expected_end = strftime(Sys.time() - 3600, "%Y-%m-%d %H:%M:%S"),
    requests = 0
  )

  plans <- update_plans_schedule(
    plans = list(never_ran),
    network = "bluesky",
    schedule_span = 60
  )

  expect_length(plans, 1)
  expect_equal(plans[[1]]$requests, 0)
  expect_equal(format(plans[[1]]$expected_end), format(never_ran$expected_end))
})

test_that("update_plans_schedule prepends a new plan once the expected end has passed", {
  local_conf(collect_history_days = 7)
  elapsed <- a_plan(
    "bluesky",
    expected_end = strftime(Sys.time() - 60, "%Y-%m-%d %H:%M:%S"),
    requests = 3,
    plan_max_date = strftime(
      Sys.time() - 60,
      "%Y-%m-%d %H:%M:%OS6Z",
      tz = "UTC"
    )
  )

  plans <- update_plans_schedule(
    plans = list(elapsed),
    network = "bluesky",
    schedule_span = 60
  )

  expect_length(plans, 2)
  # the new plan comes first, the unfinished one is kept behind it
  expect_equal(plans[[1]]$requests, 0)
  expect_equal(plans[[2]]$requests, 3)
  # the new window starts where the previous one stopped
  expect_equal(
    format(plans[[1]]$plan_min_date),
    format(elapsed$plan_max_date)
  )
})

test_that("update_plans_schedule chains the expected end on the previous one when it is recent", {
  local_conf(collect_history_days = 7)
  previous_end <- Sys.time() - 60
  elapsed <- a_plan(
    "bluesky",
    expected_end = strftime(previous_end, "%Y-%m-%d %H:%M:%S"),
    requests = 3,
    plan_max_date = strftime(previous_end, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
  )

  plans <- update_plans_schedule(
    plans = list(elapsed),
    network = "bluesky",
    schedule_span = 60
  )

  # previous expected end + one span, so the schedule does not drift
  expect_equal(
    as.numeric(difftime(plans[[1]]$expected_end, previous_end, units = "mins")),
    60,
    tolerance = 0.05
  )
})

test_that("update_plans_schedule restarts from now when the previous end is more than a span old", {
  local_conf(collect_history_days = 7)
  # more than one span in the past: chaining would put the new expected end in the past too
  long_gone <- Sys.time() - 3 * 3600
  elapsed <- a_plan(
    "bluesky",
    expected_end = strftime(long_gone, "%Y-%m-%d %H:%M:%S"),
    requests = 3,
    plan_max_date = strftime(long_gone, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
  )

  plans <- update_plans_schedule(
    plans = list(elapsed),
    network = "bluesky",
    schedule_span = 60
  )

  expect_equal(
    as.numeric(difftime(plans[[1]]$expected_end, Sys.time(), units = "mins")),
    60,
    tolerance = 0.05
  )
})

test_that("update_plans_schedule drops the finished plans", {
  plans <- update_plans_schedule(
    plans = list(
      a_stub(1),
      a_stub(2, ended = TRUE),
      a_stub(3),
      a_stub(4, ended = TRUE)
    ),
    network = "bluesky",
    schedule_span = 60
  )

  expect_equal(vapply(plans, function(p) p$id, numeric(1)), c(1, 3))
})

test_that("update_plans_schedule keeps the current plan even when it is the only one", {
  plans <- update_plans_schedule(
    plans = list(a_stub(1)),
    network = "bluesky",
    schedule_span = 60
  )

  expect_length(plans, 1)
})

test_that("update_plans_schedule never keeps a finished plan behind the current one", {
  plans <- update_plans_schedule(
    plans = list(a_stub(1), a_stub(2, ended = TRUE)),
    network = "bluesky",
    schedule_span = 60
  )

  expect_length(plans, 1)
  expect_equal(plans[[1]]$id, 1)
})

test_that("update_plans_schedule caps the kept plans at 101, not 100", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-2).
  # The intent, per the comment in the source, is "a maximum of 100 plans are kept", but the cap
  # is applied to the TAIL only and the current plan is then appended on top, so the ceiling is
  # really 101. Note that `< 100` vs `<= 100` in the source makes no difference: at a tail of
  # exactly 100 both branches return the same 100 elements.
  # When this is fixed, the expected length below becomes 100.
  with_tail <- function(n) {
    update_plans_schedule(
      plans = c(list(a_stub(0)), replicate(n, a_stub(1), simplify = FALSE)),
      network = "bluesky",
      schedule_span = 60
    )
  }

  # below the cap everything is kept: current plan + tail
  expect_length(with_tail(98), 99)
  expect_length(with_tail(99), 100)
  # from a tail of 100 on, the result saturates at 101
  expect_length(with_tail(100), 101)
  expect_length(with_tail(101), 101)
  expect_length(with_tail(250), 101)
})


# ---- finish_plans -----------------------------------------------------------

test_that("finish_plans returns an empty list for no plans", {
  expect_equal(finish_plans(list()), list())
})

test_that("finish_plans closes a running plan", {
  local_conf(schedule_span = 60)
  running <- a_plan("bluesky", requests = 4, progress = 0.3)

  finished <- finish_plans(list(running))

  expect_length(finished, 1)
  expect_equal(finished[[1]]$progress, 1.0)
  expect_false(is.null(finished[[1]]$end_on))
  expect_false(is.null(finished[[1]]$start_on))
  # the request count is carried over, it is history
  expect_equal(finished[[1]]$requests, 4)
})

test_that("finish_plans backdates a plan that never started by one span", {
  local_conf(schedule_span = 120)
  running <- a_plan("bluesky", requests = 0)

  finished <- finish_plans(list(running))

  expect_equal(
    as.numeric(difftime(Sys.time(), finished[[1]]$start_on, units = "mins")),
    120,
    tolerance = 0.05
  )
  expect_equal(
    as.numeric(difftime(Sys.time(), finished[[1]]$end_on, units = "mins")),
    120,
    tolerance = 0.05
  )
})

test_that("finish_plans keeps the end date of an already finished plan", {
  local_conf(schedule_span = 60)
  ended_at <- "2026-01-02 03:04:05"
  already <- a_plan("bluesky", requests = 4, end_on = ended_at)

  finished <- finish_plans(list(already))

  expect_equal(format(finished[[1]]$end_on), ended_at)
})

test_that("finish_plans preserves the network-specific attributes", {
  local_conf(schedule_span = 60)
  # finish_plans rebuilds the plan without the network attributes and merges it back over the
  # original, so what merge_plans keeps here is what survives a "finish all plans" action
  running <- a_plan(
    "mastodon.mstdn.party",
    requests = 4,
    plan_min_date = "2025-12-26 10:00:00.000000Z",
    current_min_id = "113456789012345678",
    current_offset = 120
  )

  finished <- finish_plans(list(running))

  expect_equal(finished[[1]]$current_min_id, "113456789012345678")
  expect_equal(finished[[1]]$current_offset, 120L)
  expect_equal(
    format(finished[[1]]$plan_min_date),
    format(running$plan_min_date)
  )
})

test_that("finish_plans handles several plans at once", {
  local_conf(schedule_span = 60)

  finished <- finish_plans(list(
    a_plan("bluesky", requests = 1),
    a_plan("bluesky", requests = 2)
  ))

  expect_length(finished, 2)
  expect_true(all(vapply(finished, function(p) p$progress, numeric(1)) == 1))
  expect_equal(vapply(finished, function(p) p$requests, numeric(1)), c(1, 2))
})


# ---- update_plan_after_request ----------------------------------------------

test_that("update_plan_after_request counts the request and stamps the start", {
  local_mocked_bindings(
    sm_api_update_plan_after_request = function(plan, results) plan,
    sm_plan_get_progress = function(plan) 0.4
  )
  plan <- a_plan("bluesky", requests = 2, got_rows = FALSE, progress = 0)

  updated <- update_plan_after_request(plan, list(count = 10))

  expect_equal(updated$requests, 3)
  expect_false(is.null(updated$start_on))
  expect_true(updated$got_rows)
  expect_equal(updated$progress, 0.4)
})

test_that("update_plan_after_request keeps the original start date", {
  local_mocked_bindings(
    sm_api_update_plan_after_request = function(plan, results) plan,
    sm_plan_get_progress = function(plan) 0.4
  )
  started <- "2025-12-31 23:00:00"
  plan <- a_plan("bluesky", requests = 2, start_on = started)

  updated <- update_plan_after_request(plan, list(count = 10))

  expect_equal(format(updated$start_on), started)
})

test_that("update_plan_after_request ends the plan when a request brings nothing back", {
  plan <- a_plan("bluesky", requests = 2, got_rows = FALSE, progress = 0.3)

  updated <- update_plan_after_request(plan, list(count = 0))

  expect_equal(updated$requests, 3)
  expect_false(is.null(updated$end_on))
  expect_equal(updated$progress, 1.0)
  # an empty request does not turn got_rows on
  expect_false(updated$got_rows)
})

test_that("update_plan_after_request delegates the cursor update to the family", {
  local_mocked_bindings(
    sm_api_update_plan_after_request = function(plan, results) {
      plan$current_min_id <- "113456789012345600"
      plan
    },
    sm_plan_get_progress = function(plan) 0.7
  )
  plan <- a_plan("mastodon.mstdn.party", requests = 2)

  updated <- update_plan_after_request(plan, list(count = 40))

  expect_equal(updated$current_min_id, "113456789012345600")
  expect_equal(updated$progress, 0.7)
})

test_that("update_plan_after_request keeps got_rows on after an empty request", {
  # got_rows is the memory of the whole plan, not of the last request
  plan <- a_plan("bluesky", requests = 2, got_rows = TRUE)

  updated <- update_plan_after_request(plan, list(count = 0))

  expect_true(updated$got_rows)
})
