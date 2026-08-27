# Tests for the mastodon response parser (roadmap items T3-2 and T3-3):
# mastodon_parse_response() and mastodon_parse_quoted(). Pure functions, no network, no token.
#
# Two kinds of input:
#   - fixtures/mastodon_page_covid_anon.json, the anonymised copy of a real /api/v2/search page
#     (5 statuses, 3 languages, 2 link cards, 1 media with an alt text, 2 with hashtags), and its
#     empty counterpart: they say what the parser does on a whole realistic page;
#   - minimal statuses built by mstdn_status(), one behaviour at a time. The captured page carries
#     neither a boost nor a quote post, so those two cases (the only ones that set is_quote) are
#     covered by hand-made statuses until a fixture holding them is captured.
#
# The helpers mastodon_is_present(), mastodon_extract_urls(), mastodon_min_id() and the date
# functions are already covered by T1-4 (test-mastodon_utils.R); the pagination update function by
# T3-1 (test-mastodon_pagination.R). Only the parsers are exercised here.

FIXTURE <- "mastodon_page_covid_anon.json"

# the 12 fields of the standardised post format, in order, shared with bluesky
STANDARD_FIELDS <- c(
  "id", "uri", "created_at", "user_name", "text", "lang", "is_quote", "quoted_text",
  "quoted_lang", "tags", "urls", "categories"
)

# a status as /api/v2/search serves it, reduced to what the parsers read. An absent object comes
# back from the API as {}, i.e. an empty list once parsed -- not as null: `card` and `poll` are
# built that way on purpose, since that is the trap mastodon_is_present() exists for.
mstdn_status <- function(...,
                         id = "110000000000000001",
                         created_at = "2026-08-27T08:00:00.000Z",
                         language = "en",
                         content = "<p>a status about covid</p>",
                         acct = "user001@instance.example") {
  override(
    list(
      id = id,
      uri = sprintf("https://mastodon.example/users/user001/statuses/%s", id),
      url = sprintf("https://mastodon.example/@user001/%s", id),
      created_at = created_at,
      language = language,
      content = content,
      account = list(acct = acct),
      tags = list(),
      media_attachments = list(),
      card = list(),
      poll = list(),
      reblog = NULL,
      quote = NULL
    ),
    list(...)
  )
}

mstdn_page <- function(...) list(accounts = list(), statuses = list(...), hashtags = list())

mstdn_plan <- function(...) override(list(network = "mastodon.mstdn.party"), list(...))

# the standardised post of a one-status page
parse_one <- function(status, plan = mstdn_plan()) {
  mastodon_parse_response(mstdn_page(status), plan)$posts[[1]]
}


# ---- mastodon_parse_response, on the captured page --------------------------

test_that("mastodon_parse_response turns the captured page into the standardised format", {
  page <- read_fixture(FIXTURE)

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_length(page$statuses, 5)
  expect_length(res$posts, 5)
  expect_equal(res$stored, 5)
  expect_named(res$posts[[1]], STANDARD_FIELDS)
  expect_true(all(vapply(
    res$posts,
    function(p) identical(names(p), STANDARD_FIELDS),
    logical(1)
  )))
})

test_that("the network is the one of the plan, not a constant", {
  # unlike bluesky, mastodon is federated: the network name carries the instance the plan collects
  # from, and it is what identifies the messages downstream
  res <- mastodon_parse_response(read_fixture(FIXTURE), mstdn_plan(network = "mastodon.piaille.fr"))

  expect_equal(res$network, "mastodon.piaille.fr")
})

test_that("mastodon_parse_response parses the captured page without a warning", {
  page <- read_fixture(FIXTURE)

  expect_silent(mastodon_parse_response(page, mstdn_plan()))
})

test_that("the text of the captured statuses is stripped of its html and of its urls", {
  # the API serves the status as rendered html; clean_messages() is what the keyword matching and
  # the database see
  res <- mastodon_parse_response(read_fixture(FIXTURE), mstdn_plan())

  texts <- vapply(res$posts, `[[`, character(1), "text")

  expect_false(any(grepl("<", texts, fixed = TRUE)))
  expect_false(any(grepl("http", texts, fixed = TRUE)))
  expect_true(all(nchar(texts) > 0))
})

test_that("the languages of the captured page are kept as they are", {
  res <- mastodon_parse_response(read_fixture(FIXTURE), mstdn_plan())

  expect_equal(
    vapply(res$posts, `[[`, character(1), "lang"),
    c("fr", "de", "en", "en", "en")
  )
})

test_that("min_created_at is the oldest creation date among the kept posts", {
  res <- mastodon_parse_response(read_fixture(FIXTURE), mstdn_plan())

  kept <- vapply(res$posts, `[[`, character(1), "created_at")

  expect_equal(res$pagination$min_created_at, min(kept))
  expect_equal(res$pagination$min_created_at, "2026-08-27T05:34:03.000000Z")
})

test_that("the pagination cursor of the captured page reads the raw statuses", {
  page <- read_fixture(FIXTURE)

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_equal(res$pagination$returned, 5L)
  expect_equal(res$pagination$oldest_id, mastodon_min_id(lapply(page$statuses, `[[`, "id")))
  expect_equal(
    res$pagination$new_ids,
    vapply(page$statuses, function(s) unlist(s$uri), character(1))
  )
})

test_that("mastodon_parse_response reports an empty page as no post and no cursor", {
  res <- mastodon_parse_response(read_fixture("mastodon_page_empty_anon.json"), mstdn_plan())

  expect_identical(res$posts, list())
  expect_equal(res$stored, 0)
  expect_equal(res$pagination$returned, 0L)
  expect_null(res$pagination$oldest_id)
  expect_null(res$pagination$min_created_at)
  expect_identical(res$pagination$new_ids, character(0))
})

test_that("the captured page feeds the attachment branches it was captured for", {
  # a guard on the fixture itself: a re-capture that loses these cases must be noticed
  page <- read_fixture(FIXTURE)

  expect_equal(sum(vapply(page$statuses, function(s) mastodon_is_present(s$card), logical(1))), 2)
  expect_equal(
    sum(vapply(
      page$statuses,
      function(s) {
        any(vapply(
          s$media_attachments,
          function(m) nchar(unlist(m$description)) > 0,
          logical(1)
        ))
      },
      logical(1)
    )),
    1
  )
  expect_equal(sum(vapply(page$statuses, function(s) length(s$tags) > 0, logical(1))), 2)
  # still missing, hence the hand-made statuses below (see the header)
  expect_equal(sum(vapply(page$statuses, function(s) mastodon_is_present(s$reblog), logical(1))), 0)
  expect_equal(sum(vapply(page$statuses, function(s) mastodon_is_present(s$quote), logical(1))), 0)
})


# ---- the standardised post --------------------------------------------------

test_that("id is the uri of the status, not its id on the instance", {
  # a federated status has a different id on every instance that stores it: the uri is the only
  # identifier that is stable across instances, so it is what deduplicates the messages
  post <- parse_one(mstdn_status(id = "110000000000000042"))

  expect_equal(post$id, "https://mastodon.example/users/user001/statuses/110000000000000042")
})

test_that("id falls back on the instance id when the status has no uri", {
  post <- parse_one(mstdn_status(uri = NULL))

  expect_equal(post$id, "110000000000000001")
})

test_that("uri is the public url of the status", {
  post <- parse_one(mstdn_status(id = "110000000000000042"))

  expect_equal(post$uri, "https://mastodon.example/@user001/110000000000000042")
})

test_that("uri falls back on the federation uri when the status has no url", {
  post <- parse_one(mstdn_status(url = NULL))

  expect_equal(post$uri, "https://mastodon.example/users/user001/statuses/110000000000000001")
})

test_that("created_at is reformatted to ISO 8601 UTC with microseconds", {
  post <- parse_one(mstdn_status(created_at = "2026-08-27T08:00:00.123Z"))

  expect_equal(post$created_at, "2026-08-27T08:00:00.123000Z")
})

test_that("user_name is the webfinger acct, instance included for a remote account", {
  expect_equal(parse_one(mstdn_status(acct = "user001@other.example"))$user_name, "user001@other.example")
  # a local account has no instance part in its acct
  expect_equal(parse_one(mstdn_status(acct = "user001"))$user_name, "user001")
})

test_that("text is the content stripped of its html, its urls and its newlines", {
  content <- "<p>weekly <strong>covid</strong> report</p>\n<p>see <a href=\"https://example.org/a\">here</a></p>"

  expect_equal(
    parse_one(mstdn_status(content = content))$text,
    "weekly covid reportsee here"
  )
})

test_that("lang is the language declared by the instance", {
  expect_equal(parse_one(mstdn_status(language = "de"))$lang, "de")
})

test_that("tags are the names of the hashtags, without their urls", {
  post <- parse_one(mstdn_status(tags = list(
    list(name = "covid19", url = "https://mastodon.example/tags/covid19"),
    list(name = "grippe", url = "https://mastodon.example/tags/grippe")
  )))

  expect_equal(post$tags, list("covid19", "grippe"))
})

test_that("urls merge the links of the content and those of the embedded material", {
  post <- parse_one(mstdn_status(
    content = "<p>see <a href=\"https://example.org/a\">here</a></p>",
    card = list(url = "https://example.org/b", title = "a title", description = "a description")
  ))

  # content first, embedded material after
  expect_equal(post$urls, list("https://example.org/a", "https://example.org/b"))
})

test_that("a url present both in the content and in the card is only kept once", {
  # the usual shape of a link status: the card previews the link written in the text
  post <- parse_one(mstdn_status(
    content = "<p>see <a href=\"https://example.org/a\">here</a></p>",
    card = list(url = "https://example.org/a", title = "a title", description = "a description")
  ))

  expect_equal(post$urls, list("https://example.org/a"))
})

test_that("categories is always empty at this stage", {
  # symmetric with bluesky: the field only exists to keep one post format across networks
  expect_identical(parse_one(mstdn_status())$categories, list())
})

test_that("a plain status is not a quote and carries no quoted text", {
  post <- parse_one(mstdn_status())

  expect_false(post$is_quote)
  expect_null(post$quoted_text)
  expect_null(post$quoted_lang)
})

test_that("the quoted text is cleaned like the text of the status", {
  # mastodon_parse_quoted() returns the raw html of the quoted status on purpose; the response
  # parser is the one that cleans it
  post <- parse_one(mstdn_status(
    reblog = mstdn_status(content = "<p>the <em>boosted</em> status</p>")
  ))

  expect_true(post$is_quote)
  expect_equal(post$quoted_text, "the boosted status")
})


# ---- the client side filters ------------------------------------------------

test_that("a status created in the future is dropped with a warning", {
  page <- mstdn_page(mstdn_status(created_at = mastodon_format_date(Sys.time() + 3600)))

  expect_warning(res <- mastodon_parse_response(page, mstdn_plan()), "creation date in future")
  expect_identical(res$posts, list())
  expect_equal(res$stored, 0)
})

test_that("a status older than plan_min_date is dropped, and silently", {
  # reaching the lower bound is the normal end of a plan, not an anomaly: unlike the bluesky upper
  # bound, this filter does not warn
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002", created_at = "2026-08-27T08:00:00.000Z"),
    mstdn_status(id = "110000000000000001", created_at = "2026-08-26T08:00:00.000Z")
  )
  plan <- mstdn_plan(plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"))

  res <- expect_silent(mastodon_parse_response(page, plan))

  expect_length(res$posts, 1)
  expect_equal(res$posts[[1]]$created_at, "2026-08-27T08:00:00.000000Z")
})

test_that("a status exactly on plan_min_date is kept", {
  # the comparison is strict (created < plan_min_date), so the bound itself belongs to the window
  bound <- lubridate::as_datetime("2026-08-27T00:00:00Z")
  page <- mstdn_page(mstdn_status(created_at = "2026-08-27T00:00:00.000Z"))

  res <- mastodon_parse_response(page, mstdn_plan(plan_min_date = bound))

  expect_length(res$posts, 1)
})

test_that("a plan without plan_min_date keeps every past status", {
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002", created_at = "2026-08-27T08:00:00.000Z"),
    mstdn_status(id = "110000000000000001", created_at = "2020-01-01T00:00:00.000Z")
  )

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_length(res$posts, 2)
})

test_that("a status without language is dropped, like on bluesky", {
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002"),
    mstdn_status(id = "110000000000000001", language = NULL)
  )

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_length(res$posts, 1)
  expect_equal(res$posts[[1]]$id, "https://mastodon.example/users/user001/statuses/110000000000000002")
})

test_that("a status already returned by a previous request is dropped", {
  # the search API is not stable across requests: an anchor change re-reads the head of the second
  # it lands on, so a status can come back. plan$seen_ids is the sliding memory of what was kept.
  already <- "https://mastodon.example/users/user001/statuses/110000000000000001"
  page <- mstdn_page(
    mstdn_status(id = "110000000000000001"),
    mstdn_status(id = "110000000000000002")
  )

  res <- mastodon_parse_response(page, mstdn_plan(seen_ids = list(already)))

  expect_length(res$posts, 1)
  expect_equal(res$posts[[1]]$id, "https://mastodon.example/users/user001/statuses/110000000000000002")
  expect_equal(res$stored, 1)
})

test_that("the deduplication compares the uris, not the ids of the instance", {
  # what is remembered in seen_ids is the standardised id, i.e. the uri: the numeric id of the
  # instance would not match the same status seen through another instance
  page <- mstdn_page(mstdn_status(id = "110000000000000001"))

  res <- mastodon_parse_response(page, mstdn_plan(seen_ids = list("110000000000000001")))

  expect_length(res$posts, 1)
})

test_that("new_ids only lists what the request actually kept", {
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002"),
    mstdn_status(id = "110000000000000001", language = NULL)
  )

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_equal(
    res$pagination$new_ids,
    "https://mastodon.example/users/user001/statuses/110000000000000002"
  )
})


# ---- the pagination cursor, computed on the RAW statuses --------------------

test_that("returned counts the statuses the API sent, not the ones kept", {
  # it is the page size of the request, and the update function walks the offset with it: counting
  # the kept statuses instead would shift the cursor by the client side filtering
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002"),
    mstdn_status(id = "110000000000000001", language = NULL)
  )

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_equal(res$pagination$returned, 2L)
  expect_equal(res$stored, 1)
})

test_that("oldest_id is the numerically smallest id of the page", {
  # the ids are 18-digit snowflakes handed over as strings: a lexicographic minimum would pick the
  # wrong anchor as soon as two ids have a different number of digits
  page <- mstdn_page(
    mstdn_status(id = "1000000000000"),
    mstdn_status(id = "999999999999")
  )

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_equal(res$pagination$oldest_id, "999999999999")
})

test_that("oldest_id is read from the raw statuses, filtered ones included", {
  # the anchor must reflect where the API stopped, not where the window did
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002", created_at = "2026-08-27T08:00:00.000Z"),
    mstdn_status(id = "110000000000000001", created_at = "2020-01-01T00:00:00.000Z")
  )
  plan <- mstdn_plan(plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"))

  res <- mastodon_parse_response(page, plan)

  expect_equal(res$pagination$oldest_id, "110000000000000001")
  expect_length(res$posts, 1)
})

test_that("posts_after_mindate counts the raw statuses inside the window", {
  page <- mstdn_page(
    mstdn_status(id = "110000000000000003", created_at = "2026-08-27T08:00:00.000Z"),
    mstdn_status(id = "110000000000000002", created_at = "2026-08-27T07:00:00.000Z"),
    mstdn_status(id = "110000000000000001", created_at = "2020-01-01T00:00:00.000Z")
  )
  plan <- mstdn_plan(plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"))

  res <- mastodon_parse_response(page, plan)

  expect_equal(res$pagination$posts_after_mindate, 2L)
})

test_that("posts_after_mindate survives a page emptied by the deduplication", {
  # the reason it is computed on the raw statuses: a rebase page re-reads an already seen head, so
  # res$posts can be empty while older statuses are still reachable at the next offset. Counting the
  # kept posts here would end the plan too early.
  page <- mstdn_page(mstdn_status(id = "110000000000000001"))
  plan <- mstdn_plan(
    plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"),
    current_offset = 40,
    seen_ids = list("https://mastodon.example/users/user001/statuses/110000000000000001")
  )

  res <- mastodon_parse_response(page, plan)

  expect_identical(res$posts, list())
  expect_equal(res$stored, 0)
  expect_equal(res$pagination$posts_after_mindate, 1L)
  expect_equal(res$count, 1L)
})

test_that("without a lower bound every returned status counts as in the window", {
  page <- mstdn_page(mstdn_status(created_at = "2020-01-01T00:00:00.000Z"))

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_equal(res$pagination$posts_after_mindate, 1L)
})


# ---- count, the end of plan signal ------------------------------------------
# update_plan_after_request() ends the plan when count == 0. count is NOT the number of stored
# posts: it is the number of raw statuses still inside the window, or a positive sentinel while the
# consecutive-empty streak has not reached conf$mastodon_max_empty_requests.

test_that("count is the number of in-window statuses, not the number stored", {
  page <- mstdn_page(
    mstdn_status(id = "110000000000000002"),
    mstdn_status(id = "110000000000000001", language = NULL)
  )

  res <- mastodon_parse_response(page, mstdn_plan())

  expect_equal(res$count, 2L)
  expect_equal(res$stored, 1)
})

test_that("a FIRST request with nothing in the window ends the plan at once", {
  # the first request has neither anchor nor offset, so it returns the newest matching statuses: if
  # none of them is in the window, no later request could get closer to it
  page <- mstdn_page(mstdn_status(created_at = "2020-01-01T00:00:00.000Z"))
  plan <- mstdn_plan(plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"))

  expect_null(plan$current_offset)
  expect_equal(mastodon_parse_response(page, plan)$count, 0L)
})

test_that("mid-walk, a request with nothing in the window keeps the plan alive", {
  # created_at is noisy: walked by id, a supposedly older request can still surface newer statuses,
  # so one empty request is not enough to conclude
  page <- mstdn_page(mstdn_status(created_at = "2020-01-01T00:00:00.000Z"))
  plan <- mstdn_plan(
    plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"),
    current_offset = 40,
    consecutive_empty = 0
  )

  expect_equal(mastodon_parse_response(page, plan)$count, 1L)
})

test_that("the request completing the streak ends the plan", {
  local_conf(mastodon_max_empty_requests = 3L)
  page <- mstdn_page(mstdn_status(created_at = "2020-01-01T00:00:00.000Z"))
  plan <- mstdn_plan(
    plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"),
    current_offset = 40
  )

  # one short of the threshold: the plan goes on
  expect_equal(mastodon_parse_response(page, override(plan, list(consecutive_empty = 1)))$count, 1L)
  # this request is the third in a row without an in-window status
  expect_equal(mastodon_parse_response(page, override(plan, list(consecutive_empty = 2)))$count, 0L)
})

test_that("the streak threshold defaults to 5 when the configuration says nothing", {
  local_conf()
  expect_null(conf$mastodon_max_empty_requests)
  page <- mstdn_page(mstdn_status(created_at = "2020-01-01T00:00:00.000Z"))
  plan <- mstdn_plan(
    plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"),
    current_offset = 40
  )

  expect_equal(mastodon_parse_response(page, override(plan, list(consecutive_empty = 3)))$count, 1L)
  expect_equal(mastodon_parse_response(page, override(plan, list(consecutive_empty = 4)))$count, 0L)
})

test_that("an in-window status resets the reasoning, whatever the streak", {
  local_conf(mastodon_max_empty_requests = 3L)
  page <- mstdn_page(mstdn_status(created_at = "2026-08-27T08:00:00.000Z"))
  plan <- mstdn_plan(
    plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"),
    current_offset = 40,
    consecutive_empty = 2
  )

  expect_equal(mastodon_parse_response(page, plan)$count, 1L)
})

test_that("an empty page ends a first request and keeps a walking plan alive", {
  page <- mstdn_page()
  plan <- mstdn_plan(plan_min_date = lubridate::as_datetime("2026-08-27T00:00:00Z"))

  expect_equal(mastodon_parse_response(page, plan)$count, 0L)
  expect_equal(
    mastodon_parse_response(page, override(plan, list(current_offset = 40)))$count,
    1L
  )
})


# ---- mastodon_parse_quoted --------------------------------------------------

test_that("mastodon_parse_quoted finds nothing on a plain status", {
  quote <- mastodon_parse_quoted(mstdn_status())

  expect_null(quote$text)
  expect_null(quote$lang)
  expect_identical(quote$urls, list())
  expect_false(quote$is_quote)
})

test_that("a boost is a quote and brings the content of the boosted status", {
  quote <- mastodon_parse_quoted(mstdn_status(
    reblog = mstdn_status(content = "<p>the boosted status</p>", language = "de")
  ))

  expect_true(quote$is_quote)
  # the html is deliberately kept here, the response parser cleans it
  expect_equal(quote$text, "<p>the boosted status</p>")
  expect_equal(quote$lang, "de")
})

test_that("an accepted quote post is a quote", {
  quote <- mastodon_parse_quoted(mstdn_status(quote = list(
    state = "accepted",
    quoted_status = mstdn_status(content = "<p>the quoted status</p>", language = "fr")
  )))

  expect_true(quote$is_quote)
  expect_equal(quote$text, "<p>the quoted status</p>")
  expect_equal(quote$lang, "fr")
})

test_that("a quote that was not accepted carries nothing to harvest", {
  # pending, rejected, revoked, deleted, unauthorized: the API sends the state without any
  # quoted_status, so there is simply nothing to read
  quote <- mastodon_parse_quoted(mstdn_status(quote = list(state = "pending")))

  expect_null(quote$text)
  expect_false(quote$is_quote)
})

test_that("the empty object the API sends for an absent quote is not mistaken for one", {
  # the {} vs null trap: is.null() is not enough, which is what mastodon_is_present() is for
  status <- jsonlite::fromJSON(
    '{"language": "en", "content": "<p>c</p>", "quote": {}, "reblog": null, "card": {}}',
    simplifyVector = FALSE
  )

  quote <- mastodon_parse_quoted(status)

  expect_false(quote$is_quote)
  expect_null(quote$text)
})

test_that("a link card gives its title and description, but is not a quote", {
  quote <- mastodon_parse_quoted(mstdn_status(card = list(
    url = "https://example.org/a",
    title = "a title",
    description = "a description"
  )))

  expect_equal(quote$text, "a title\na description")
  expect_equal(quote$urls, list("https://example.org/a"))
  expect_false(quote$is_quote)
})

test_that("the language of the card wins over the language of the status", {
  # the card language is the language of the linked page, a better guess for its title than the
  # language of whoever posted the link
  quote <- mastodon_parse_quoted(mstdn_status(
    language = "en",
    card = list(url = "https://example.org/a", title = "un titre", language = "fr")
  ))

  expect_equal(quote$lang, "fr")
})

test_that("a card missing one of its texts keeps the other one alone", {
  # the counterpart of the bluesky bug on the same case (B-14, frozen in test-bluesky_parse.R):
  # here the absent and empty values are filtered out properly
  quote <- mastodon_parse_quoted(mstdn_status(card = list(
    url = "https://example.org/a",
    description = "a description"
  )))

  expect_equal(quote$text, "a description")
})

test_that("the alternative texts of the medias are collected", {
  quote <- mastodon_parse_quoted(mstdn_status(media_attachments = list(
    list(description = "first chart"),
    list(description = "second chart")
  )))

  expect_equal(quote$text, "first chart\nsecond chart")
  expect_false(quote$is_quote)
})

test_that("a media without alternative text is skipped", {
  quote <- mastodon_parse_quoted(mstdn_status(media_attachments = list(
    list(description = ""),
    list(description = "the only alt text")
  )))

  expect_equal(quote$text, "the only alt text")
})

test_that("the card and the medias of the quoted status are harvested too", {
  # the case the function exists for: the keyword only appears in what the quoted status carries.
  # A quoted status is a full status of its own, with its own card and its own attachments.
  quote <- mastodon_parse_quoted(mstdn_status(quote = list(
    state = "accepted",
    quoted_status = mstdn_status(
      content = "<p>look at this</p>",
      card = list(url = "https://example.org/quoted", title = "covid figures"),
      media_attachments = list(list(description = "the quoted chart"))
    )
  )))

  expect_true(quote$is_quote)
  expect_equal(quote$text, "<p>look at this</p>\ncovid figures\nthe quoted chart")
  expect_equal(quote$urls, list("https://example.org/quoted"))
})

test_that("the urls written in the quoted content are collected as well", {
  quote <- mastodon_parse_quoted(mstdn_status(
    reblog = mstdn_status(content = "<p>see <a href=\"https://example.org/boosted\">here</a></p>")
  ))

  expect_equal(quote$urls, list("https://example.org/boosted"))
})

test_that("a poll is acknowledged but its options are not collected", {
  # symmetric with the bluesky poll facets and videos: no text harvested, and no dump either
  quote <- mastodon_parse_quoted(mstdn_status(poll = list(
    id = "1",
    options = list(list(title = "yes", votes_count = 3), list(title = "no", votes_count = 1))
  )))

  expect_null(quote$text)
  expect_false(quote$is_quote)
})

test_that("quoted_lang is the majority language of the collected texts, on two letters", {
  # mode() over the collected texts: two medias in the language of the status outvote the card
  quote <- mastodon_parse_quoted(mstdn_status(
    language = "en",
    card = list(url = "https://example.org/a", title = "un titre", language = "fr"),
    media_attachments = list(list(description = "first"), list(description = "second"))
  ))

  expect_equal(quote$lang, "en")
})

test_that("quoted_lang is cut to the two first letters of the language tag", {
  quote <- mastodon_parse_quoted(mstdn_status(
    language = "pt-BR",
    media_attachments = list(list(description = "uma imagem"))
  ))

  expect_equal(quote$lang, "pt")
})
