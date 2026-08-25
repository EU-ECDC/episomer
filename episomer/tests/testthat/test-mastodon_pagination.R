# Tests for sm_api_update_plan_after_request_mastodon() (R/sm-mastodon-api.R, roadmap item T3-1).
#
# This is the state machine of the mastodon pagination cursor: a pure function of
# (plan, results) that decides where the next request will read from. It is documented at length
# in the @details of sm_api_search_mastodon; every rule stated there gets a test here.
#
# Recap of the cursor, because the assertions below are unreadable without it:
#   plan$current_min_id   the pagination anchor, sent as max_id. The server turns it into an
#                         inclusive `before:<second>` filter, so it selects a WINDOW (that second
#                         and everything older), never a position. It moves at exactly two
#                         moments: the first request of the plan, and each rebase.
#   plan$current_offset   walks inside that window, one page at a time (0, 40, 80, ...). NULL
#                         means "no request performed yet" -- it is only ever reset to 0L, never
#                         back to NULL, so it is the unambiguous marker of the first request.
#   plan$consecutive_empty  streak of consecutive requests that brought back no in-window post.
#                         mastodon_parse_response reads it to end the plan; the two must stay in
#                         sync, hence the tests on the streak here.
#   plan$seen_ids         sliding memory of the ids already returned, used by
#                         mastodon_parse_response to drop the duplicates the API hands back.
#
# Everything the function reads comes from results$pagination, which mastodon_parse_response
# computes on the RAW statuses (before any client-side filtering), never on the posts kept.

# results as mastodon_parse_response() builds them. Only $pagination is read here.
results_page <- function(
  oldest_id = NULL,
  posts_after_mindate = 0L,
  new_ids = character(0),
  min_created_at = NULL
) {
  list(
    count = 1L,
    posts = list(),
    pagination = list(
      returned = length(new_ids),
      oldest_id = oldest_id,
      min_created_at = min_created_at,
      posts_after_mindate = posts_after_mindate,
      new_ids = new_ids
    )
  )
}

# a plan that has already performed at least one request: it has an anchor and a non-NULL offset
walking_plan <- function(offset = 0L, anchor = "111000000000000000", ...) {
  c(
    list(
      network = "mastodon.mstdn.party",
      current_min_id = anchor,
      current_offset = offset
    ),
    list(...)
  )
}

# a plan whose first request has just been performed: no offset yet
first_plan <- function(...) {
  c(list(network = "mastodon.mstdn.party"), list(...))
}

update <- sm_api_update_plan_after_request_mastodon


# ---- the consecutive-empty streak -------------------------------------------
#
# The streak is the stop criterion of the plan. It cannot be an "ended on the first below-window
# request" rule because mastodon orders by id (ingestion time) while the window is on created_at,
# and federation makes the two axes diverge.

test_that("a request bringing back an in-window post resets the streak", {
  plan <- update(
    walking_plan(consecutive_empty = 3L),
    results_page(oldest_id = "111000000000000000", posts_after_mindate = 2L)
  )

  expect_identical(plan$consecutive_empty, 0L)
})

test_that("a request bringing back no in-window post extends the streak", {
  plan <- update(
    walking_plan(consecutive_empty = 3L),
    results_page(oldest_id = "111000000000000000", posts_after_mindate = 0L)
  )

  expect_identical(plan$consecutive_empty, 4L)
})

test_that("the streak starts at 1 on a plan that has none yet", {
  plan <- update(
    walking_plan(),
    results_page(oldest_id = "111000000000000000", posts_after_mindate = 0L)
  )

  expect_identical(plan$consecutive_empty, 1L)
})

test_that("a missing posts_after_mindate is read as an empty request", {
  # defensive branch: pagination coming from an older plans file, without the field
  results <- results_page(oldest_id = "111000000000000000")
  results$pagination$posts_after_mindate <- NULL

  plan <- update(walking_plan(consecutive_empty = 2L), results)

  expect_identical(plan$consecutive_empty, 3L)
})

test_that("the streak is advanced even when the page holds no status at all", {
  # the empty-page branch returns early, so the streak has to be advanced before it
  plan <- update(
    walking_plan(offset = 40L, consecutive_empty = 1L),
    results_page(oldest_id = NULL, posts_after_mindate = 0L)
  )

  expect_identical(plan$consecutive_empty, 2L)
})


# ---- seeding the anchor on the first request --------------------------------

test_that("the first request seeds the anchor onto the oldest id of its page", {
  plan <- update(
    first_plan(),
    results_page(oldest_id = "111000000000000000", posts_after_mindate = 5L)
  )

  expect_identical(plan$current_min_id, "111000000000000000")
  # the offset walk starts inside the window the anchor has just selected
  expect_identical(plan$current_offset, 0L)
})

test_that("the first request is recognised by a NULL offset, not by a NULL anchor", {
  # a plan whose offset is back to 0 after a rebase is NOT a first request: it must step forward,
  # not re-seed its anchor onto the page it just read
  plan <- update(
    walking_plan(offset = 0L, anchor = "111000000000000000"),
    results_page(oldest_id = "110000000000000000", posts_after_mindate = 5L)
  )

  expect_identical(plan$current_min_id, "111000000000000000")
  expect_identical(plan$current_offset, 40L)
})


# ---- walking the offset inside a window -------------------------------------

test_that("the offset advances by the page size, not by the number of rows returned", {
  # the server drops DB-less rows (.objects.compact) so a page usually holds fewer than the
  # requested 40 statuses. offset addresses the API's own pre-compact positions, so advancing it
  # by the number of rows kept would silently skip results.
  plan <- update(
    walking_plan(offset = 40L),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 12L,
      new_ids = as.character(seq_len(12))
    )
  )

  expect_identical(plan$current_offset, 80L)
})

test_that("the anchor does not move while the offset walks inside its window", {
  plan <- update(
    walking_plan(offset = 120L, anchor = "111000000000000000"),
    results_page(oldest_id = "110000000000000000", posts_after_mindate = 5L)
  )

  expect_identical(plan$current_min_id, "111000000000000000")
  expect_identical(plan$current_offset, 160L)
})

test_that("the offset stays strictly below the elasticsearch ceiling", {
  # mastodon_max_offset is 1000: the last offset ever sent is 960, never 1000
  plan <- update(
    walking_plan(offset = 920L),
    results_page(oldest_id = "110000000000000000", posts_after_mindate = 5L)
  )

  expect_identical(plan$current_offset, 960L)
  expect_true(plan$current_offset < mastodon_max_offset)
})


# ---- rebasing the anchor at the ceiling -------------------------------------

test_that("reaching the ceiling rebases the anchor onto the oldest id and restarts the offset", {
  plan <- update(
    walking_plan(offset = 960L, anchor = "111000000000000000"),
    results_page(oldest_id = "110000000000000000", posts_after_mindate = 5L)
  )

  # the new window drops every second newer than that id, so the bounded offset has room again
  expect_identical(plan$current_min_id, "110000000000000000")
  expect_identical(plan$current_offset, 0L)
})

test_that("the rebase happens exactly once the next offset would reach the ceiling", {
  below <- update(
    walking_plan(offset = mastodon_max_offset - 2L * mastodon_page_size),
    results_page(oldest_id = "110000000000000000", posts_after_mindate = 5L)
  )
  at <- update(
    walking_plan(offset = mastodon_max_offset - mastodon_page_size),
    results_page(oldest_id = "110000000000000000", posts_after_mindate = 5L)
  )

  expect_identical(below$current_min_id, "111000000000000000")
  expect_identical(at$current_min_id, "110000000000000000")
  expect_identical(at$current_offset, 0L)
})


# ---- pages holding no status at all -----------------------------------------

test_that("a page with no status steps the offset and leaves the anchor alone", {
  # nothing to anchor onto, so the only move available is one page forward inside the current
  # window. Repeated empty pages extend the streak above and end the plan, so this cannot loop.
  plan <- update(
    walking_plan(offset = 80L, anchor = "111000000000000000"),
    results_page(oldest_id = NULL)
  )

  expect_identical(plan$current_offset, 120L)
  expect_identical(plan$current_min_id, "111000000000000000")
})

test_that("an empty page on the very first request steps the offset without seeding an anchor", {
  # KNOWN QUIRK: the empty-page branch is checked before the first-request branch, so the plan
  # comes out with an offset but no anchor -- it would paginate with offset alone. Harmless in
  # practice: a first request with no in-window post makes mastodon_parse_response return
  # count = 0, and the generic update_plan_after_request ends the plan without ever calling this
  # function again.
  plan <- update(first_plan(), results_page(oldest_id = NULL))

  expect_identical(plan$current_offset, mastodon_page_size)
  expect_null(plan$current_min_id)
})


# ---- the sliding memory of seen ids -----------------------------------------

test_that("the ids of the request are pushed in front of the sliding memory", {
  plan <- update(
    walking_plan(seen_ids = c("old1", "old2")),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 2L,
      new_ids = c("new1", "new2")
    )
  )

  # newest first, so the oldest ones fall off the end once the memory is full
  expect_identical(plan$seen_ids, c("new1", "new2", "old1", "old2"))
})

test_that("an id already in the memory is not stored twice", {
  plan <- update(
    walking_plan(seen_ids = c("a", "b")),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 2L,
      new_ids = c("b", "c")
    )
  )

  expect_identical(plan$seen_ids, c("b", "c", "a"))
})

test_that("the sliding memory is capped at mastodon_seen_ids_size", {
  previous <- sprintf("old%03d", seq_len(mastodon_seen_ids_size))
  fresh <- sprintf("new%03d", seq_len(mastodon_page_size))

  plan <- update(
    walking_plan(seen_ids = previous),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 40L,
      new_ids = fresh
    )
  )

  expect_length(plan$seen_ids, mastodon_seen_ids_size)
  # the whole page is kept, the oldest ids are the ones dropped
  expect_identical(plan$seen_ids[seq_along(fresh)], fresh)
  expect_identical(
    plan$seen_ids[(length(fresh) + 1L):mastodon_seen_ids_size],
    previous[seq_len(mastodon_seen_ids_size - length(fresh))]
  )
})

test_that("the memory read back from a plans file as a list is flattened", {
  # jsonlite hands seen_ids back as a list of length-1 strings
  plan <- update(
    walking_plan(seen_ids = list("a", "b")),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 1L,
      new_ids = list("c")
    )
  )

  expect_identical(plan$seen_ids, c("c", "a", "b"))
})

test_that("a request that kept no post leaves the memory untouched", {
  plan <- update(
    walking_plan(seen_ids = c("a", "b")),
    results_page(oldest_id = "111000000000000000", posts_after_mindate = 3L)
  )

  expect_identical(plan$seen_ids, c("a", "b"))
})


# ---- the oldest date collected, used for progress ---------------------------

test_that("the oldest collected date is recorded on a plan that has none", {
  plan <- update(
    first_plan(),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 3L,
      min_created_at = "2026-03-04T05:06:07.123456Z"
    )
  )

  expect_equal(
    plan$current_min_date,
    lubridate::as_datetime("2026-03-04T05:06:07.123456Z")
  )
})

test_that("the oldest collected date moves backwards only", {
  older <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 3L,
      min_created_at = "2026-03-01T00:00:00.000000Z"
    )
  )
  newer <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(
      oldest_id = "111000000000000000",
      posts_after_mindate = 3L,
      min_created_at = "2026-03-08T00:00:00.000000Z"
    )
  )

  expect_equal(older$current_min_date, lubridate::as_datetime("2026-03-01T00:00:00Z"))
  expect_equal(newer$current_min_date, lubridate::as_datetime("2026-03-04T00:00:00Z"))
})

test_that("a page without any kept post leaves the oldest collected date alone", {
  plan <- update(
    walking_plan(current_min_date = lubridate::as_datetime("2026-03-04T00:00:00Z")),
    results_page(oldest_id = "111000000000000000")
  )

  expect_equal(plan$current_min_date, lubridate::as_datetime("2026-03-04T00:00:00Z"))
})


# ---- the walk as a whole ----------------------------------------------------

test_that("a full walk seeds, digs and rebases without ever leaving the window", {
  # one plan driven through the sequence the search loop would produce, to check the pieces
  # compose: first request -> offsets 40, 80, ... -> rebase at the ceiling -> offsets again
  plan <- first_plan(plan_min_date = lubridate::as_datetime("2026-01-01T00:00:00Z"))
  anchors <- character(0)
  offsets <- integer(0)

  for (request in 1:30) {
    plan <- update(
      plan,
      results_page(
        oldest_id = sprintf("11100000000000%04d", 1000 - request),
        posts_after_mindate = 5L
      )
    )
    anchors <- c(anchors, plan$current_min_id)
    offsets <- c(offsets, plan$current_offset)
  }

  # offsets: 0 (seeding) then 40, 80 ... 960, then back to 0 at the rebase, then 40 again
  expect_identical(offsets[1:3], c(0L, 40L, 80L))
  expect_true(all(offsets < mastodon_max_offset))
  expect_identical(offsets[which(offsets == 960L) + 1L], 0L)
  # the anchor changes only at the seeding and at each rebase, i.e. exactly when the offset is 0
  expect_length(unique(anchors), sum(offsets == 0L))
})
