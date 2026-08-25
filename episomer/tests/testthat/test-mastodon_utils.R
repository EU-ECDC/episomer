# Tests for the isolated helpers of R/sm-mastodon-api.R (roadmap item T1-4).
#
# All of them are pure: no network, no configuration, no token. The parsers that consume them
# (mastodon_parse_response, mastodon_parse_quoted, sm_api_update_plan_after_request_mastodon) are
# roadmap items T3-1 to T3-3 and are not touched here.


# ---- mastodon_id_lt ---------------------------------------------------------

test_that("mastodon_id_lt compares snowflake ids beyond the 32-bit range", {
  # a real mastodon id is a 18-digit snowflake, i.e. far past .Machine$integer.max
  expect_true(mastodon_id_lt("113000000000000001", "113000000000000002"))
  expect_false(mastodon_id_lt("113000000000000002", "113000000000000001"))
})

test_that("mastodon_id_lt compares the ids numerically, not as strings", {
  # the whole point of the helper: a plain `<` on the characters would order these the other way
  # round, and the pagination anchor would walk backwards
  expect_true(mastodon_id_lt("999999999999", "1000000000000"))
  expect_false("999999999999" < "1000000000000")
})

test_that("mastodon_id_lt is strict, equal ids are not lower", {
  expect_false(mastodon_id_lt("113000000000000001", "113000000000000001"))
})

test_that("mastodon_id_lt is vectorised", {
  expect_equal(
    mastodon_id_lt(c("1", "3", "2"), c("2", "2", "2")),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("mastodon_id_lt returns NA on a missing id", {
  # documented rather than desirable: it is up to the callers to filter the NAs, which is exactly
  # what mastodon_min_id does before reducing
  expect_true(is.na(mastodon_id_lt(NA, "113000000000000001")))
  expect_true(is.na(mastodon_id_lt("113000000000000001", NA)))
})

test_that("mastodon_id_lt returns an empty logical on an absent id", {
  expect_identical(mastodon_id_lt(NULL, "113000000000000001"), logical(0))
  expect_identical(mastodon_id_lt(character(0), character(0)), logical(0))
})

test_that("mastodon_id_lt needs the ids as characters to keep their precision", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-4).
  # bit64 only preserves the 18 digits when it reads them from a string. Passing the same ids as
  # numbers first funnels them through a double, whose spacing at 1e17 is 16, so the two distinct
  # ids collapse onto the same value and the comparison silently answers FALSE.
  # jsonlite gives mastodon ids as strings, so the callers are safe; this locks the requirement.
  expect_false(mastodon_id_lt(113000000000000001, 113000000000000002))
  expect_true(mastodon_id_lt("113000000000000001", "113000000000000002"))
})


# ---- mastodon_min_id --------------------------------------------------------

test_that("mastodon_min_id returns the numerically smallest id", {
  ids <- list(
    "113000000000000002",
    "113000000000000001",
    "113000000000000003"
  )

  expect_equal(mastodon_min_id(ids), "113000000000000001")
})

test_that("mastodon_min_id does not pick the lexicographically smallest id", {
  expect_equal(mastodon_min_id(c("999999999999", "1000000000000")), "999999999999")
})

test_that("mastodon_min_id flattens a nested list of ids", {
  # it is fed the ids collected from a parsed json page, so they arrive as a list of lists
  expect_equal(mastodon_min_id(list(list("500", "400"), "600")), "400")
})

test_that("mastodon_min_id ignores the missing ids", {
  expect_equal(mastodon_min_id(c("500", NA, "400")), "400")
})

test_that("mastodon_min_id returns NULL when there is no usable id", {
  expect_null(mastodon_min_id(character(0)))
  expect_null(mastodon_min_id(list()))
  expect_null(mastodon_min_id(NULL))
  expect_null(mastodon_min_id(c(NA, NA)))
})

test_that("mastodon_min_id returns a single id unchanged", {
  expect_equal(mastodon_min_id("42"), "42")
})

test_that("mastodon_min_id returns a character, not an integer64", {
  # the result is stored in the plan and written to the plans json: an integer64 would be
  # serialised as its double bit pattern, so the id has to come back as one of the input strings
  res <- mastodon_min_id(list("113000000000000002", "113000000000000001"))

  expect_type(res, "character")
  expect_identical(res, "113000000000000001")
})


# ---- mastodon_is_present ----------------------------------------------------

test_that("mastodon_is_present is FALSE on the {} the API sends for an absent object", {
  # the documented trap: mastodon serialises an absent object as {}, not as null, and jsonlite
  # turns that into an empty list, on which is.null() answers FALSE
  status <- jsonlite::fromJSON(
    '{"id": "1", "quote": {}, "reblog": null}',
    simplifyVector = FALSE
  )

  expect_false(is.null(status$quote))
  expect_false(mastodon_is_present(status$quote))
  expect_false(mastodon_is_present(status$reblog))
})

test_that("mastodon_is_present is TRUE on an object that carries something", {
  status <- jsonlite::fromJSON(
    '{"quote": {"state": "accepted", "quoted_status": {"id": "2"}}}',
    simplifyVector = FALSE
  )

  expect_true(mastodon_is_present(status$quote))
  expect_true(mastodon_is_present(status$quote$quoted_status))
})

test_that("mastodon_is_present is FALSE on NULL and on any empty vector", {
  expect_false(mastodon_is_present(NULL))
  expect_false(mastodon_is_present(list()))
  expect_false(mastodon_is_present(character(0)))
})

test_that("mastodon_is_present only looks at the length, an empty string is present", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-4).
  # The test is `length(x) > 0`, so a present-but-empty scalar passes: "" and NA are reported as
  # present. Harmless on the objects it is used for ($quote, $reblog, $card, $poll, which are
  # either {} or a populated object), but it is not a truthiness test and must not be used as one.
  expect_true(mastodon_is_present(""))
  expect_true(mastodon_is_present(NA))
})


# ---- mastodon_extract_urls --------------------------------------------------

test_that("mastodon_extract_urls returns an empty list when the content has no link", {
  expect_identical(mastodon_extract_urls("<p>a status without any link</p>"), list())
})

test_that("mastodon_extract_urls extracts the target of a link and drops the href markup", {
  content <- '<p>see <a href="https://www.ecdc.europa.eu/en/flu" rel="nofollow">this</a></p>'

  expect_equal(
    mastodon_extract_urls(content),
    list("https://www.ecdc.europa.eu/en/flu")
  )
})

test_that("mastodon_extract_urls matches http as well as https", {
  content <- '<a href="http://legacy.example.org/a">a</a>'

  expect_equal(mastodon_extract_urls(content), list("http://legacy.example.org/a"))
})

test_that("mastodon_extract_urls deduplicates and keeps the first occurrence order", {
  content <- paste0(
    '<a href="https://b.example.org/2">b</a>',
    '<a href="https://a.example.org/1">a</a>',
    '<a href="https://b.example.org/2">b again</a>'
  )

  expect_equal(
    mastodon_extract_urls(content),
    list("https://b.example.org/2", "https://a.example.org/1")
  )
})

test_that("mastodon_extract_urls returns an empty list on an absent or empty content", {
  expect_identical(mastodon_extract_urls(NULL), list())
  expect_identical(mastodon_extract_urls(character(0)), list())
  expect_identical(mastodon_extract_urls(""), list())
})

test_that("mastodon_extract_urls accepts the length-1 list jsonlite produces", {
  expect_equal(
    mastodon_extract_urls(list('<a href="https://a.example.org">a</a>')),
    list("https://a.example.org")
  )
})

test_that("mastodon_extract_urls leaves the url exactly as the html carries it", {
  # the html entities are NOT decoded: whatever is between the quotes is returned verbatim, so a
  # query string comes back with its &amp; intact
  content <- '<a href="https://a.example.org/s?x=1&amp;y=2">a</a>'

  expect_equal(
    mastodon_extract_urls(content),
    list("https://a.example.org/s?x=1&amp;y=2")
  )
})

test_that("mastodon_extract_urls ignores a single-quoted href", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-4).
  # The regexp hardcodes double quotes, which is what the mastodon API serves; a status rendered
  # with single quotes would have its links silently dropped.
  expect_identical(mastodon_extract_urls("<a href='https://a.example.org'>a</a>"), list())
})

test_that("mastodon_extract_urls only accepts one content at a time", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-4).
  # The guard is `nchar(content) == 0` inside a `||`, so a content vector of length > 1 raises
  # "'length = 2' in coercion to 'logical(1)'" instead of scanning both. All the callers pass a
  # single status content, so this only documents that the helper is scalar-only.
  expect_error(
    mastodon_extract_urls(c('<a href="https://a.example.org">a</a>', "<p>b</p>")),
    "coercion to 'logical\\(1\\)'"
  )
})


# ---- mastodon_format_date / mastodon_parse_date -----------------------------

test_that("mastodon_format_date writes ISO 8601 UTC with microseconds", {
  expect_equal(
    mastodon_format_date(as.POSIXct("2026-01-02 03:04:05.123456", tz = "UTC")),
    "2026-01-02T03:04:05.123456Z"
  )
})

test_that("mastodon_format_date keeps the time part at exactly midnight", {
  # unlike the default format.POSIXlt that loses it (see B-1), the explicit format string here
  # always writes the whole timestamp
  expect_equal(
    mastodon_format_date(as.POSIXct("2026-01-01 00:00:00", tz = "UTC")),
    "2026-01-01T00:00:00.000000Z"
  )
})

test_that("mastodon_format_date converts to UTC whatever the local timezone is", {
  withr::local_timezone("Europe/Paris")
  # 14:30 in Paris in June is 12:30 UTC: the API only ever gets the UTC instant, so a collector
  # running outside UTC cannot shift its search window
  paris_noon <- as.POSIXct("2026-06-15 14:30:00", tz = "Europe/Paris")

  expect_equal(mastodon_format_date(paris_noon), "2026-06-15T12:30:00.000000Z")
})

test_that("mastodon_parse_date returns a POSIXct anchored on UTC", {
  withr::local_timezone("Europe/Paris")

  res <- mastodon_parse_date("2026-06-15T12:30:00.000Z")

  expect_s3_class(res, "POSIXct")
  expect_equal(attr(res, "tzone"), "UTC")
  expect_equal(format(res, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "2026-06-15 12:30:00")
})

test_that("mastodon_parse_date accepts the list shape jsonlite produces", {
  expect_equal(
    mastodon_parse_date(list("2026-01-02T03:04:05Z")),
    mastodon_parse_date("2026-01-02T03:04:05Z")
  )
})

test_that("mastodon_parse_date parses a whole page of dates at once", {
  res <- mastodon_parse_date(list("2026-01-02T03:04:05Z", "2026-01-03T00:00:00Z"))

  expect_length(res, 2)
  expect_equal(
    format(res, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    c("2026-01-02T03:04:05Z", "2026-01-03T00:00:00Z")
  )
})

test_that("mastodon_parse_date brings an explicit offset back to UTC", {
  res <- mastodon_parse_date("2026-01-02T03:04:05+02:00")

  expect_equal(format(res, "%H:%M:%S", tz = "UTC"), "01:04:05")
})

test_that("the format / parse round-trip keeps the sub-second precision", {
  # the pagination compares a plan anchor with the created_at of a status, and two statuses of the
  # same second have to stay distinguishable
  iso <- "2026-01-02T03:04:05.123456Z"

  expect_equal(mastodon_format_date(mastodon_parse_date(iso)), iso)
})

test_that("the parse / format round-trip preserves the instant from a local timezone", {
  withr::local_timezone("Europe/Paris")
  local_time <- as.POSIXct("2026-06-15 14:30:00.500000", tz = "Europe/Paris")

  back <- mastodon_parse_date(mastodon_format_date(local_time))

  expect_equal(as.numeric(back), as.numeric(local_time))
})
