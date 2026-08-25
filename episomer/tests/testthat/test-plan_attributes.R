# Tests for the per-family plan attributes (roadmap item T1-6):
# sm_plan_parse_attributes_bluesky() <-> sm_plan_format_bluesky() (R/sm-bluesky-plan.R) and the
# mastodon pair (R/sm-mastodon-plan.R).
#
# These two functions are the two halves of the plan file persistence, for the attributes that
# belong to a network family rather than to every plan:
#
#   conf (dates as POSIXlt)
#     `- save_config -> format_plan -> sm_plan_format_<family>   : dates -> "%Y-%m-%d %H:%M:%OS6Z"
#          `- jsonlite::write_json(auto_unbox = TRUE)
#               `- read_json + parse_plan_attributes -> sm_plan_parse_attributes_<family>
#                    `- strptime("%Y-%m-%d %H:%M:%OSZ", tz = "UTC") -> POSIXlt, back in conf
#
# Unlike the COMMON dates (expected_end, scheduled_for, start_on, end_on), which reach the file as
# POSIXlt and are stringified by jsonlite -- so an in-memory parse(format(x)) tests jsonlite, not
# the package, and misses B-1 -- the network dates are stringified by the family formatter itself.
# parse(format(x)) is therefore the real unit here, and the JSON leg only matters where jsonlite
# reshapes a value (empty object, auto_unbox of a length-1 array): those cases are exercised
# explicitly below. The end-to-end version through the plan file lives in test-plans.R (T1-2).
#
# The date halves of the two families are character-for-character identical, so the same battery is
# run against both and their equivalence is frozen by a test at the bottom: fixing one side and
# forgetting the other shows up. The mastodon pagination cursor, which has no bluesky equivalent,
# is tested on its own afterwards.

families <- list(
  bluesky = list(
    parse = sm_plan_parse_attributes_bluesky,
    format = sm_plan_format_bluesky,
    first = sm_plan_first_attributes_bluesky,
    next_attrs = sm_plan_next_attributes_bluesky
  ),
  mastodon = list(
    parse = sm_plan_parse_attributes_mastodon,
    format = sm_plan_format_mastodon,
    first = sm_plan_first_attributes_mastodon,
    next_attrs = sm_plan_next_attributes_mastodon
  )
)

# the three window dates every family carries
window_dates <- c("plan_max_date", "plan_min_date", "current_min_date")

# a date as it sits in conf once a plan is parsed
a_date <- function(x, tz = "UTC") as.POSIXlt(x, tz = tz)

# format then parse, i.e. one save and one reload of a plan
round_trip <- function(family, plan) do.call(family$parse, family$format(plan))

# the plan file leg, for the values jsonlite reshapes on the way
via_json <- function(family, plan) {
  json <- jsonlite::toJSON(family$format(plan), auto_unbox = TRUE)
  do.call(family$parse, jsonlite::fromJSON(json, simplifyVector = FALSE))
}


for (network in names(families)) {
  family <- families[[network]]

  # ---- format ---------------------------------------------------------------

  test_that(sprintf("[%s] format turns the window dates into strings", network), {
    f <- family$format(list(
      plan_max_date = a_date("2026-01-02 03:04:05.123456"),
      plan_min_date = a_date("2025-12-26 10:00:00"),
      current_min_date = a_date("2025-12-30 12:00:00.5")
    ))

    expect_type(f$plan_max_date, "character")
    expect_equal(f$plan_max_date, "2026-01-02 03:04:05.123456Z")
    expect_equal(f$plan_min_date, "2025-12-26 10:00:00.000000Z")
    expect_equal(f$current_min_date, "2025-12-30 12:00:00.500000Z")
  })

  test_that(sprintf("[%s] format leaves an absent date absent", network), {
    f <- family$format(list(
      network = network,
      plan_max_date = NULL,
      plan_min_date = NULL,
      current_min_date = NULL
    ))

    expect_null(f$plan_max_date)
    expect_null(f$plan_min_date)
    expect_null(f$current_min_date)
  })

  test_that(sprintf("[%s] format leaves the rest of the plan untouched", network), {
    # the formatter is handed the whole plan, not just its own attributes: everything it does not
    # own has to come out of it unchanged, the common dates included (they are stringified by
    # jsonlite, which is the root of B-1)
    plan <- list(
      network = network,
      expected_end = a_date("2026-01-02 03:04:05", tz = ""),
      requests = 4,
      got_rows = TRUE,
      progress = 0.5,
      plan_max_date = a_date("2026-01-02 03:04:05")
    )

    f <- family$format(plan)

    expect_equal(f$network, network)
    expect_s3_class(f$expected_end, "POSIXlt")
    expect_equal(f$requests, 4)
    expect_true(f$got_rows)
    expect_equal(f$progress, 0.5)
  })

  test_that(sprintf("[%s] format is idempotent on an already formatted date", network), {
    # not academic: sm_plan_first_attributes_<family>() returns the dates ALREADY as strings, so
    # the first save of a brand new plan formats a string, not a POSIXlt
    once <- family$format(list(plan_max_date = a_date("2026-01-02 03:04:05.123456")))

    twice <- family$format(once)

    expect_equal(twice$plan_max_date, once$plan_max_date)
  })

  # ---- parse ----------------------------------------------------------------

  test_that(sprintf("[%s] parse returns the window dates as UTC POSIXlt", network), {
    p <- family$parse(
      plan_max_date = "2026-01-02 03:04:05.123456Z",
      plan_min_date = "2025-12-26 10:00:00.000000Z",
      current_min_date = "2025-12-30 12:00:00.500000Z"
    )

    for (d in window_dates) {
      expect_s3_class(p[[d]], "POSIXlt")
      expect_equal(attr(p[[d]], "tzone"), "UTC")
    }
    expect_equal(
      format(p$plan_max_date, "%Y-%m-%d %H:%M:%OS6"),
      "2026-01-02 03:04:05.123456"
    )
    expect_equal(
      format(p$current_min_date, "%Y-%m-%d %H:%M:%OS6"),
      "2025-12-30 12:00:00.500000"
    )
  })

  test_that(sprintf("[%s] parse keeps the window dates as NULL when they are absent", network), {
    p <- family$parse()

    # the names must be there with a NULL value: format and merge_plans both rely on it
    expect_true(all(window_dates %in% names(p)))
    for (d in window_dates) expect_null(p[[d]])
  })

  test_that(sprintf("[%s] parse reads an empty json object as an absent date", network), {
    # jsonlite writes a NULL as {} and reads it back as an empty list, not as a NULL: the
    # is.null(unlist(x)) guards are what make a saved plan reloadable
    p <- family$parse(
      plan_max_date = list(),
      plan_min_date = list(),
      current_min_date = list()
    )

    for (d in window_dates) expect_null(p[[d]])
  })

  test_that(sprintf("[%s] parse reads the length-1 list jsonlite hands back", network), {
    # with simplifyVector = FALSE every scalar comes back wrapped in a list
    p <- family$parse(plan_max_date = list("2026-01-02 03:04:05.123456Z"))

    expect_s3_class(p$plan_max_date, "POSIXlt")
    expect_equal(
      format(p$plan_max_date, "%Y-%m-%d %H:%M:%OS6"),
      "2026-01-02 03:04:05.123456"
    )
  })

  test_that(sprintf("[%s] parse forces UTC whatever the session timezone", network), {
    withr::local_timezone("Europe/Paris")

    p <- family$parse(plan_max_date = "2026-07-02 03:04:05.000000Z")

    expect_equal(attr(p$plan_max_date, "tzone"), "UTC")
    expect_equal(format(p$plan_max_date, "%H:%M:%S"), "03:04:05")
  })

  test_that(sprintf("[%s] parse yields NA on a date it cannot read", network), {
    # KNOWN TRAP, behaviour frozen here on purpose (roadmap T1-6).
    # The plan file format and the API wire format differ by one character: the plan uses
    # "%Y-%m-%d %H:%M:%OS6Z" and the API "%Y-%m-%dT%H:%M:%OS6Z" (mastodon_format_date /
    # bluesky_format_date). Feeding the parser an API-shaped date -- or any date without the
    # trailing Z -- yields NA rather than an error, and the NA only surfaces much later, as
    # "missing value where TRUE/FALSE needed" in the window comparisons of the search (same
    # failure mode as B-1).
    p <- family$parse(
      plan_max_date = "2026-01-02T03:04:05.123456Z",
      plan_min_date = "2026-01-02 03:04:05.123456",
      current_min_date = "not a date"
    )

    for (d in window_dates) expect_true(is.na(p[[d]]))
  })

  # ---- round trip -----------------------------------------------------------

  test_that(sprintf("[%s] the window dates survive a save and reload", network), {
    plan <- list(
      network = network,
      plan_max_date = a_date("2026-01-02 03:04:05.123456"),
      plan_min_date = a_date("2025-12-26 10:00:00"),
      current_min_date = a_date("2025-12-30 12:00:00.5")
    )

    back <- round_trip(family, plan)

    for (d in window_dates) {
      expect_equal(
        format(back[[d]], "%Y-%m-%d %H:%M:%OS6"),
        format(plan[[d]], "%Y-%m-%d %H:%M:%OS6")
      )
      expect_equal(as.numeric(back[[d]]), as.numeric(plan[[d]]))
    }
  })

  test_that(sprintf("[%s] absent window dates survive a save and reload as NULL", network), {
    back <- round_trip(family, list(
      network = network,
      plan_max_date = a_date("2026-01-02 03:04:05"),
      plan_min_date = NULL,
      current_min_date = NULL
    ))

    # current_min_date is NULL for the whole first request of every plan, and both search
    # functions branch on it: turning it into anything else would change the first query
    expect_null(back$plan_min_date)
    expect_null(back$current_min_date)
    expect_false(is.null(back$plan_max_date))
  })

  test_that(sprintf("[%s] a window date falling exactly on midnight survives", network), {
    # the contrast with B-1: the format string is explicit here, so the time part is written even
    # when it is 00:00:00 and the reload gets a full datetime back. The common dates, left to
    # jsonlite, are the ones that lose it.
    back <- round_trip(family, list(
      plan_min_date = a_date("2026-01-01 00:00:00")
    ))

    expect_false(is.na(back$plan_min_date))
    expect_equal(
      format(back$plan_min_date, "%Y-%m-%d %H:%M:%S"),
      "2026-01-01 00:00:00"
    )
  })

  test_that(sprintf("[%s] a date given in local time comes back as the same instant", network), {
    withr::local_timezone("Europe/Paris")
    # the plan holds whatever the API parser produced; only the instant has to be preserved
    plan <- list(current_min_date = as.POSIXct(
      "2026-07-02 03:04:05",
      tz = "Europe/Paris"
    ))

    back <- round_trip(family, plan)

    expect_equal(as.numeric(back$current_min_date), as.numeric(plan$current_min_date))
    expect_equal(format(back$current_min_date, "%H:%M:%S"), "01:04:05")
  })

  test_that(sprintf("[%s] the microsecond precision does not drift over saves", network), {
    # a plan is saved after every request, so the round trip runs hundreds of times on the same
    # date: a formatter that rounded down would walk the window boundary backwards
    plan <- list(current_min_date = a_date("2026-01-02 03:04:05.999999"))

    for (i in 1:10) plan <- round_trip(family, plan)

    expect_equal(
      format(plan$current_min_date, "%Y-%m-%d %H:%M:%OS6"),
      "2026-01-02 03:04:05.999999"
    )
  })

  test_that(sprintf("[%s] a reloaded date still supports sub-second arithmetic", network), {
    # sm_api_search_bluesky asks for `until = current_min_date - 0.000001` to exclude the oldest
    # post already collected: a date that came back from the file with a second-level resolution
    # would make that decrement a no-op and the search would loop on the same post
    back <- round_trip(family, list(
      current_min_date = a_date("2026-01-02 03:04:05.123456")
    ))

    expect_equal(
      strftime(back$current_min_date - 0.000001, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC"),
      "2026-01-02 03:04:05.123455Z"
    )
  })

  # ---- the attributes of a new plan are readable by the parser ---------------

  test_that(sprintf("[%s] the first plan attributes are readable by the parser", network), {
    local_conf(collect_history_days = 7)

    p <- do.call(family$parse, family$first())

    expect_s3_class(p$plan_max_date, "POSIXlt")
    expect_equal(
      as.numeric(p$plan_max_date),
      as.numeric(Sys.time()),
      tolerance = 1e-6
    )
    # the first plan backfills collect_history_days, from midnight
    expect_equal(as.Date(p$plan_min_date), Sys.Date() - 7)
    expect_equal(format(p$plan_min_date, "%H:%M:%S"), "00:00:00")
    expect_null(p$current_min_date)
  })

  test_that(sprintf("[%s] the next plan attributes chain on the previous window", network), {
    previous <- list(list(plan_max_date = a_date("2026-01-02 03:04:05.123456")))

    p <- do.call(family$parse, family$next_attrs(previous))

    # the new window starts exactly where the previous one stopped: any loss here would either
    # re-collect or skip the posts of the gap
    expect_equal(
      format(p$plan_min_date, "%Y-%m-%d %H:%M:%OS6"),
      "2026-01-02 03:04:05.123456"
    )
    expect_equal(
      as.numeric(p$plan_max_date),
      as.numeric(Sys.time()),
      tolerance = 1e-6
    )
    expect_null(p$current_min_date)
  })
}


# ---- equivalence of the two implementations ---------------------------------

test_that("both families format and parse the window dates the same way", {
  # the date halves of R/sm-bluesky-plan.R and R/sm-mastodon-plan.R are the same code twice: this
  # freezes their equivalence so that a fix applied to one file only fails here
  plan <- list(
    plan_max_date = a_date("2026-01-02 03:04:05.123456"),
    plan_min_date = a_date("2025-12-26 00:00:00"),
    current_min_date = a_date("2025-12-30 12:00:00.5")
  )

  expect_identical(
    sm_plan_format_bluesky(plan)[window_dates],
    sm_plan_format_mastodon(plan)[window_dates]
  )
  expect_identical(
    round_trip(families$bluesky, plan)[window_dates],
    round_trip(families$mastodon, plan)[window_dates]
  )
})


# ---- bluesky: no cursor attributes ------------------------------------------

test_that("the bluesky parser carries the window dates and nothing else", {
  # bluesky paginates on the dates alone (until / since), so an attribute it does not know about
  # is dropped on reload -- a mastodon cursor written on a bluesky plan does not come back
  p <- sm_plan_parse_attributes_bluesky(
    plan_max_date = "2026-01-02 03:04:05.000000Z",
    current_offset = 120,
    current_min_id = "113456789012345678"
  )

  expect_named(p, window_dates)
  expect_false("current_offset" %in% names(p))
})


# ---- mastodon: the pagination cursor ----------------------------------------

test_that("the mastodon parser types the cursor attributes", {
  p <- sm_plan_parse_attributes_mastodon(
    current_offset = 120,
    current_min_id = "113456789012345678",
    consecutive_empty = 2,
    seen_ids = list("113456789012345678", "113456789012345677")
  )

  expect_type(p$current_offset, "integer")
  expect_equal(p$current_offset, 120L)
  # ids stay characters: as a double, two neighbouring snowflakes collapse onto the same value
  expect_type(p$current_min_id, "character")
  expect_equal(p$current_min_id, "113456789012345678")
  expect_type(p$consecutive_empty, "integer")
  expect_equal(p$consecutive_empty, 2L)
  expect_equal(p$seen_ids, c("113456789012345678", "113456789012345677"))
})

test_that("the mastodon parser keeps the cursor attributes as NULL when absent", {
  p <- sm_plan_parse_attributes_mastodon()

  expect_named(
    p,
    c(window_dates, "current_offset", "current_min_id", "consecutive_empty", "seen_ids")
  )
  expect_null(p$current_offset)
  expect_null(p$current_min_id)
  expect_null(p$consecutive_empty)
  expect_null(p$seen_ids)
})

test_that("a zero offset is NOT read as an absent offset", {
  # sm_api_update_plan_after_request_mastodon tells a first request from a resumed one by
  # current_offset being NULL (see T3-1): a plan rebased to offset 0 must not look like a first
  # request after a restart, or it would seed its anchor again and re-collect the same page
  p <- sm_plan_parse_attributes_mastodon(current_offset = 0, consecutive_empty = 0)

  expect_equal(p$current_offset, 0L)
  expect_equal(p$consecutive_empty, 0L)
})

test_that("the mastodon formatter leaves the cursor attributes as they are", {
  plan <- list(
    current_offset = 120L,
    current_min_id = "113456789012345678",
    consecutive_empty = 2L,
    seen_ids = c("113456789012345678", "113456789012345677")
  )

  f <- sm_plan_format_mastodon(plan)

  expect_identical(f[names(plan)], plan)
})

test_that("the mastodon cursor survives a save and reload", {
  back <- round_trip(families$mastodon, list(
    plan_max_date = a_date("2026-01-02 03:04:05.123456"),
    current_min_date = a_date("2025-12-30 12:00:00"),
    current_offset = 120L,
    current_min_id = "113456789012345678",
    consecutive_empty = 2L,
    seen_ids = c("113456789012345678", "113456789012345677")
  ))

  expect_equal(back$current_offset, 120L)
  expect_equal(back$current_min_id, "113456789012345678")
  expect_equal(back$consecutive_empty, 2L)
  expect_equal(back$seen_ids, c("113456789012345678", "113456789012345677"))
})

test_that("a single seen id survives the auto_unbox of the plan file", {
  # the only place where the json leg changes a value of this family: auto_unbox = TRUE writes a
  # length-1 vector as a scalar, so the reload has to widen it back to a character vector,
  # otherwise the de-duplication memory silently breaks on the plan that has seen exactly one post
  back <- via_json(families$mastodon, list(
    seen_ids = "113456789012345678",
    current_min_id = "113456789012345678",
    current_offset = 120L
  ))

  expect_type(back$seen_ids, "character")
  expect_length(back$seen_ids, 1)
  expect_equal(back$seen_ids, "113456789012345678")
  # the anchor is a scalar on both sides, it must not become a list
  expect_equal(back$current_min_id, "113456789012345678")
  expect_equal(back$current_offset, 120L)
})

test_that("an offset written as a string is read back as an integer", {
  # jsonlite normally gives numbers back as numbers, but a hand-edited plan file is a real
  # debugging practice and the coercion covers it
  p <- sm_plan_parse_attributes_mastodon(current_offset = "120")

  expect_equal(p$current_offset, 120L)
})

test_that("numeric seen ids are MANGLED by the parser", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-6).
  # as.character() looks like it makes the ids safe whatever their type, but an 18-digit snowflake
  # given as a number has already lost its low digits to the double it was stored in: the two
  # distinct ids below come back as ONE AND THE SAME string, and neither is an id that exists.
  # The de-duplication memory then stops matching the real ids and silently keeps the duplicates.
  # The ids must be characters from end to end (same quirk as mastodon_id_lt, T1-4).
  p <- sm_plan_parse_attributes_mastodon(
    seen_ids = list(113456789012345678, 113456789012345677)
  )

  expect_equal(p$seen_ids, c("113456789012345680", "113456789012345680"))
  # expect_equal(p$seen_ids, c("113456789012345678", "113456789012345677"))
})
