# Tests for the isolated helpers of R/sm-bluesky-api.R (roadmap item T1-5).
#
# Same protocol as test-mastodon_utils.R (roadmap T1-4): pure helpers only, no network. The
# responses handed to bluesky_rate_limited_check() are fabricated with httr2::response(), which
# needs no server. bluesky_parse_response() / bluesky_parse_quoted() are roadmap item T3-4.

# a post carrying a single facet holding the given features
a_faceted_post <- function(...) {
  list(
    record = list(text = "a post", facets = list(list(features = list(...))))
  )
}

a_feature <- function(type, ...) {
  c(list(`$type` = type), list(...))
}


# ---- bluesky_rate_limited_check ---------------------------------------------
#
# It is passed to httr2::req_retry() as `is_transient`, so TRUE means "retry after
# bluesky_rerun_after_rate_limit() seconds" and FALSE means "give up and raise on this status".

test_that("bluesky_rate_limited_check reports a 429 with an exhausted quota as transient", {
  resp <- httr2::response(
    status_code = 429,
    headers = list("RateLimit-Remaining" = "0")
  )

  expect_true(bluesky_rate_limited_check(resp))
})

test_that("bluesky_rate_limited_check reads the quota header case-insensitively", {
  # httr2 normalises the header names, so the casing the server chose does not matter
  resp <- httr2::response(
    status_code = 429,
    headers = list("ratelimit-remaining" = "0")
  )

  expect_true(bluesky_rate_limited_check(resp))
})

test_that("bluesky_rate_limited_check does not retry a 429 that still has quota left", {
  resp <- httr2::response(
    status_code = 429,
    headers = list("RateLimit-Remaining" = "5")
  )

  expect_false(bluesky_rate_limited_check(resp))
})

test_that("bluesky_rate_limited_check reports the transient server errors", {
  for (status in c(420, 500, 502, 503)) {
    resp <- httr2::response(status_code = status)

    expect_message(
      expect_true(bluesky_rate_limited_check(resp)),
      sprintf("Error %s encountered", status)
    )
  }
})

test_that("bluesky_rate_limited_check reports a successful response as final", {
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 200)))
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 201)))
})

test_that("bluesky_rate_limited_check reports the client errors as final", {
  # a 400 or a 401 is not going to fix itself by waiting: the query or the token is wrong
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 400)))
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 401)))
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 404)))
})

test_that("bluesky_rate_limited_check does not retry a 429 without the quota header", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-5).
  # A 429 IS "too many requests", but only the one carrying `RateLimit-Remaining: 0` is treated as
  # transient. A 429 emitted by something else than the bluesky API itself (a gateway, a proxy)
  # carries no such header, so req_retry() gives up and the search errors out instead of waiting.
  # httr2's own default is_transient, which this function replaces, retries any 429.
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 429)))
})

test_that("bluesky_rate_limited_check does not retry a 504", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-5).
  # 420, 500, 502 and 503 are listed as transient but 504 (gateway timeout) is not, although it is
  # just as retriable. When this is fixed, 504 joins the list above.
  expect_false(bluesky_rate_limited_check(httr2::response(status_code = 504)))
})

test_that("bluesky_rate_limited_check compares the quota header as an exact string", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-5).
  # The test is identical(header, "0"), so anything but the bare "0" reads as quota remaining --
  # padding included. Harmless with the current API, which sends "0", but it makes the retry
  # depend on the exact spelling rather than on the value.
  resp <- httr2::response(
    status_code = 429,
    headers = list("RateLimit-Remaining" = " 0")
  )

  expect_false(bluesky_rate_limited_check(resp))
})


# ---- bluesky_parse_features -------------------------------------------------

test_that("bluesky_parse_features returns the three empty lists on a post without facets", {
  empty <- list(tags = list(), urls = list(), categories = list())

  expect_equal(bluesky_parse_features(list(cid = "x")), empty)
  expect_equal(
    bluesky_parse_features(list(record = list(text = "a post"))),
    empty
  )
  expect_equal(
    bluesky_parse_features(list(
      record = list(text = "a post", facets = list())
    )),
    empty
  )
})

test_that("bluesky_parse_features collects the hashtags", {
  post <- a_faceted_post(a_feature("app.bsky.richtext.facet#tag", tag = "flu"))

  expect_equal(bluesky_parse_features(post)$tags, list("flu"))
})

test_that("bluesky_parse_features accepts both spellings of the tag facet", {
  # the lexicon renamed #hashtag into #tag, older posts still carry the former
  post <- a_faceted_post(
    a_feature("app.bsky.richtext.facet#hashtag", tag = "covid")
  )

  expect_equal(bluesky_parse_features(post)$tags, list("covid"))
})

test_that("bluesky_parse_features collects the link targets", {
  post <- a_faceted_post(
    a_feature(
      "app.bsky.richtext.facet#link",
      uri = "https://www.ecdc.europa.eu/en/flu"
    )
  )

  res <- bluesky_parse_features(post)

  expect_equal(res$urls, list("https://www.ecdc.europa.eu/en/flu"))
  expect_equal(res$tags, list())
})

test_that("bluesky_parse_features accumulates the features in order", {
  post <- a_faceted_post(
    a_feature("app.bsky.richtext.facet#tag", tag = "flu"),
    a_feature("app.bsky.richtext.facet#link", uri = "https://a.example.org"),
    a_feature("app.bsky.richtext.facet#tag", tag = "vaccine")
  )

  res <- bluesky_parse_features(post)

  expect_equal(res$tags, list("flu", "vaccine"))
  expect_equal(res$urls, list("https://a.example.org"))
})

test_that("bluesky_parse_features walks all the facets of a post", {
  # one facet per span of the text, each with its own features
  post <- list(
    record = list(
      text = "a post",
      facets = list(
        list(
          features = list(a_feature("app.bsky.richtext.facet#tag", tag = "flu"))
        ),
        list(
          features = list(
            a_feature(
              "app.bsky.richtext.facet#link",
              uri = "https://a.example.org"
            )
          )
        )
      )
    )
  )

  res <- bluesky_parse_features(post)

  expect_equal(res$tags, list("flu"))
  expect_equal(res$urls, list("https://a.example.org"))
})

test_that("bluesky_parse_features skips a facet that carries no feature", {
  post <- list(
    record = list(
      text = "a post",
      facets = list(list(index = list(byteStart = 0, byteEnd = 4)))
    )
  )

  expect_equal(
    bluesky_parse_features(post),
    list(tags = list(), urls = list(), categories = list())
  )
})

test_that("bluesky_parse_features keeps a repeated tag twice", {
  # no deduplication here, unlike the urls of bluesky_parse_response, which unique()s them
  post <- a_faceted_post(
    a_feature("app.bsky.richtext.facet#tag", tag = "flu"),
    a_feature("app.bsky.richtext.facet#tag", tag = "flu")
  )

  expect_equal(bluesky_parse_features(post)$tags, list("flu", "flu"))
})

test_that("bluesky_parse_features silently ignores the known irrelevant facet types", {
  # mentions, formatting, the twitter bridge marker and the poll options carry nothing to search
  post <- a_faceted_post(
    a_feature("app.bsky.richtext.facet#mention", did = "did:plc:abc"),
    a_feature("app.bsky.richtext.facet#format", format = "bold"),
    a_feature("app.twitter.source#id", id = "1"),
    a_feature("blue.poll.post.facet#option", option = "yes")
  )

  expect_silent(res <- bluesky_parse_features(post))
  expect_equal(res, list(tags = list(), urls = list(), categories = list()))
})

test_that("bluesky_parse_features warns and dumps the post on an unknown facet type", {
  # the dump is how a new lexicon type gets noticed, so it has to land in the data dir
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir)
  post <- a_faceted_post(a_feature("app.unknown.lexicon#thing", value = 1))

  expect_warning(
    res <- bluesky_parse_features(post),
    "Unexpected feature type app.unknown.lexicon#thing"
  )

  expect_equal(res, list(tags = list(), urls = list(), categories = list()))
  expect_true(file.exists(file.path(data_dir, "badpost.json")))
  # the dump is the whole post; note it is written without auto_unbox, so every scalar comes back
  # as a length-1 array and it cannot be fed straight back to the parser
  dumped <- jsonlite::fromJSON(
    file.path(data_dir, "badpost.json"),
    simplifyVector = FALSE
  )
  expect_equal(
    unlist(dumped$record$facets[[1]]$features[[1]]$`$type`),
    "app.unknown.lexicon#thing"
  )
})

test_that("bluesky_parse_features overwrites the previous dump", {
  # a single file name, so only the last unexpected post is kept: it is a debugging aid, not a log
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir)

  suppressWarnings({
    bluesky_parse_features(a_faceted_post(a_feature("app.unknown#first")))
    bluesky_parse_features(a_faceted_post(a_feature("app.unknown#second")))
  })

  dumped <- jsonlite::fromJSON(
    file.path(data_dir, "badpost.json"),
    simplifyVector = FALSE
  )
  expect_equal(
    unlist(dumped$record$facets[[1]]$features[[1]]$`$type`),
    "app.unknown#second"
  )
})

test_that("bluesky_parse_features never fills categories", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-5).
  # `categories` is initialised and returned but no branch ever writes to it, so the standardised
  # post format documented in sm_api_search_bluesky always carries an empty categories list
  # (R/sm-bluesky-api.R:306). Nothing downstream depends on it being filled.
  post <- a_faceted_post(
    a_feature("app.bsky.richtext.facet#tag", tag = "flu"),
    a_feature("app.bsky.richtext.facet#link", uri = "https://a.example.org")
  )

  expect_equal(bluesky_parse_features(post)$categories, list())
})

test_that("bluesky_parse_features fails on a feature without a $type", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T1-5).
  # The type is read with [[ ]] and compared without any presence check, so a feature missing its
  # `$type` raises "missing value where TRUE/FALSE needed" instead of being dumped as unexpected,
  # which is the branch meant to catch a malformed feature. The dump does not happen either, so
  # there is nothing left to debug with.
  post <- a_faceted_post(list(tag = "flu"))

  expect_error(bluesky_parse_features(post))
})


# ---- bluesky_format_date / bluesky_parse_date -------------------------------

test_that("bluesky_format_date writes ISO 8601 UTC with microseconds", {
  expect_equal(
    bluesky_format_date(as.POSIXct("2026-01-02 03:04:05.123456", tz = "UTC")),
    "2026-01-02T03:04:05.123456Z"
  )
})

test_that("bluesky_format_date keeps the time part at exactly midnight", {
  expect_equal(
    bluesky_format_date(as.POSIXct("2026-01-01 00:00:00", tz = "UTC")),
    "2026-01-01T00:00:00.000000Z"
  )
})

test_that("bluesky_format_date converts to UTC whatever the local timezone is", {
  # the formatted date is what goes into the `since` / `until` query parameters of the search, so
  # a collector running outside UTC must not shift its own window
  withr::local_timezone("Europe/Paris")
  paris_afternoon <- as.POSIXct("2026-06-15 14:30:00", tz = "Europe/Paris")

  expect_equal(
    bluesky_format_date(paris_afternoon),
    "2026-06-15T12:30:00.000000Z"
  )
})

test_that("bluesky_parse_date returns a POSIXct anchored on UTC", {
  withr::local_timezone("Europe/Paris")

  res <- bluesky_parse_date("2026-06-15T12:30:00.000Z")

  expect_s3_class(res, "POSIXct")
  expect_equal(attr(res, "tzone"), "UTC")
  expect_equal(
    format(res, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2026-06-15 12:30:00"
  )
})

test_that("bluesky_parse_date accepts the list shape jsonlite produces", {
  expect_equal(
    bluesky_parse_date(list("2026-01-02T03:04:05Z")),
    bluesky_parse_date("2026-01-02T03:04:05Z")
  )
})

test_that("bluesky_parse_date parses a whole page of createdAt at once", {
  res <- bluesky_parse_date(list(
    "2026-01-02T03:04:05Z",
    "2026-01-03T00:00:00Z"
  ))

  expect_length(res, 2)
  expect_equal(
    format(res, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    c("2026-01-02T03:04:05Z", "2026-01-03T00:00:00Z")
  )
})

test_that("bluesky_parse_date brings an explicit offset back to UTC", {
  # posts carry the timezone of the client that wrote them, not always a Z
  res <- bluesky_parse_date("2026-01-02T03:04:05+02:00")

  expect_equal(format(res, "%H:%M:%S", tz = "UTC"), "01:04:05")
})

test_that("bluesky_parse_date parses the millisecond precision bluesky actually sends", {
  res <- bluesky_parse_date("2024-11-05T14:23:41.123Z")

  expect_equal(bluesky_format_date(res), "2024-11-05T14:23:41.123000Z")
})

test_that("the format / parse round-trip keeps the sub-second precision", {
  # sm_api_search_bluesky pages backwards with `until = current_min_date - 0.000001`, so losing
  # the fraction would either re-read or skip a post
  iso <- "2026-01-02T03:04:05.123456Z"

  expect_equal(bluesky_format_date(bluesky_parse_date(iso)), iso)
})

test_that("the parse / format round-trip preserves the instant from a local timezone", {
  withr::local_timezone("Europe/Paris")
  local_time <- as.POSIXct("2026-06-15 14:30:00.500000", tz = "Europe/Paris")

  back <- bluesky_parse_date(bluesky_format_date(local_time))

  expect_equal(as.numeric(back), as.numeric(local_time))
})


# ---- sm_api_has_credentials_bluesky -------------------------------------------
#
# Purely local: it must never call the Bluesky API, unlike sm_api_get_token_bluesky.

test_that("sm_api_has_credentials_bluesky is TRUE when a session is already stored", {
  local_mocked_bindings(
    is_secret_set = function(secret) identical(secret, "bluesky_session")
  )

  expect_true(sm_api_has_credentials_bluesky())
})

test_that("sm_api_has_credentials_bluesky is TRUE when handle and password are stored", {
  local_mocked_bindings(
    is_secret_set = function(secret) secret %in% c("bluesky_user", "bluesky_password")
  )

  expect_true(sm_api_has_credentials_bluesky())
})

test_that("sm_api_has_credentials_bluesky is FALSE when only the handle is stored", {
  local_mocked_bindings(
    is_secret_set = function(secret) identical(secret, "bluesky_user")
  )

  expect_false(sm_api_has_credentials_bluesky())
})

test_that("sm_api_has_credentials_bluesky is FALSE when nothing is stored", {
  local_mocked_bindings(
    is_secret_set = function(secret) FALSE
  )

  expect_false(sm_api_has_credentials_bluesky())
})
