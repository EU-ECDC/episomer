# Tests for the bluesky response parser (roadmap item T3-4): bluesky_parse_response() and
# bluesky_parse_quoted(). Pure functions, no network, no token.
#
# Two kinds of input:
#   - fixtures/bluesky_page_covid_anon.json, the anonymised copy of a real searchPosts page
#     (100 posts, 56 with an embed, 10 languages, 7 posts without a usable language), and its empty
#     counterpart: they say what the parser does on a whole realistic page;
#   - minimal posts built by bsky_post(), one behaviour at a time.
#
# bluesky_parse_features() is already covered by T1-5 (test-bluesky_utils.R); only the way its
# result lands in the standardised post is checked here.

FIXTURE <- "bluesky_page_covid_anon.json"

# the 12 fields of the standardised post format, in order (the vignette documents them)
STANDARD_FIELDS <- c(
  "id", "uri", "created_at", "user_name", "text", "lang", "is_quote", "quoted_text",
  "quoted_lang", "tags", "urls", "categories"
)

# a post as searchPosts serves it, reduced to what the parser reads. The `...` overrides the
# top-level fields (`embed` mostly); `facets` lands inside `record`, where the parser looks for it.
bsky_post <- function(...,
                      cid = "bafyreiaaa",
                      did = "did:plc:aaaaaaaaaaaaaaaa",
                      handle = "user001.bsky.social",
                      rkey = "3aaaaaaaaaaaa",
                      created_at = "2026-08-27T08:00:00.000Z",
                      text = "a post about covid",
                      langs = list("en"),
                      facets = NULL) {
  post <- list(
    uri = sprintf("at://%s/app.bsky.feed.post/%s", did, rkey),
    cid = cid,
    author = list(did = did, handle = handle),
    record = list(
      `$type` = "app.bsky.feed.post",
      createdAt = created_at,
      text = text,
      langs = langs
    )
  )
  if (!is.null(facets)) {
    post$record$facets <- facets
  }
  override(post, list(...))
}

bsky_page <- function(...) list(posts = list(...), cursor = "cursor1", hitsTotal = 42)

# the standardised posts of a one-post page, without the pagination wrapper
parse_one <- function(post, current_min_date = NULL) {
  bluesky_parse_response(bsky_page(post), current_min_date)$posts[[1]]
}

link_facet <- function(uri) {
  list(list(features = list(list(`$type` = "app.bsky.richtext.facet#link", uri = uri))))
}


# ---- bluesky_parse_response, on the captured page ---------------------------

test_that("bluesky_parse_response turns the captured page into the standardised format", {
  page <- read_fixture(FIXTURE)

  res <- bluesky_parse_response(page, NULL)

  expect_equal(res$network, "bluesky")
  expect_length(page$posts, 100)
  expect_length(res$posts, 93)
  expect_equal(res$count, 93)
  expect_named(res$posts[[1]], STANDARD_FIELDS)
  # the contract holds for every post of the page, not just the first one
  expect_true(all(vapply(
    res$posts,
    function(p) identical(names(p), STANDARD_FIELDS),
    logical(1)
  )))
})

test_that("bluesky_parse_response parses the captured page without a warning", {
  # the two dump-and-warn paths (an unknown facet type in bluesky_parse_features(), an embed shaped
  # like nothing known in bluesky_parse_quoted()) would also try to write badpost.json into
  # conf$data_dir, which is not set here: a warning-free parse proves the page hits neither.
  page <- read_fixture(FIXTURE)

  expect_silent(bluesky_parse_response(page, NULL))
})

test_that("bluesky_parse_response drops the posts without a usable language", {
  page <- read_fixture(FIXTURE)
  # 6 posts carry no langs at all and 1 an empty langs array: first() answers NULL for both
  no_lang <- Filter(function(p) is.null(unlist(p$record$langs)), page$posts)
  expect_length(no_lang, 7)

  res <- bluesky_parse_response(page, NULL)

  expect_length(res$posts, 93)
  expect_false(any(vapply(res$posts, function(p) is.null(p$lang), logical(1))))
})

test_that("bluesky_parse_response keeps the whole language variety of the page", {
  # the language is what the collector filters topics on downstream: a page is multilingual and the
  # parser must not normalise or drop anything but the missing values
  res <- bluesky_parse_response(read_fixture(FIXTURE), NULL)

  langs <- vapply(res$posts, `[[`, character(1), "lang")

  expect_equal(sort(unique(langs)), sort(c("en", "de", "fr", "pt", "es", "it", "ja", "nl", "pl", "sv")))
})

test_that("min_created_at is the oldest creation date among the KEPT posts", {
  res <- bluesky_parse_response(read_fixture(FIXTURE), NULL)

  kept <- vapply(res$posts, `[[`, character(1), "created_at")

  expect_equal(res$pagination$min_created_at, min(kept))
  # the dates are reformatted before being compared, so the string minimum is the chronological one
  expect_equal(
    bluesky_parse_date(res$pagination$min_created_at),
    min(bluesky_parse_date(kept))
  )
})

test_that("bluesky_parse_response reports an empty page as no post and no date", {
  res <- bluesky_parse_response(read_fixture("bluesky_page_empty_anon.json"), NULL)

  expect_equal(res$count, 0)
  expect_identical(res$posts, list())
  expect_null(res$pagination$min_created_at)
  expect_equal(res$network, "bluesky")
})

test_that("the captured page feeds the quoted branches it was captured for", {
  # a guard on the fixture itself: if a re-capture loses these cases, the tests below still pass on
  # hand-made posts while the realistic coverage is silently gone
  res <- bluesky_parse_response(read_fixture(FIXTURE), NULL)

  expect_equal(sum(vapply(res$posts, function(p) isTRUE(p$is_quote), logical(1))), 13)
  expect_equal(sum(vapply(res$posts, function(p) !is.null(p$quoted_text), logical(1))), 44)
  expect_gt(sum(vapply(res$posts, function(p) length(p$urls) > 0, logical(1))), 0)
  expect_gt(sum(vapply(res$posts, function(p) length(p$tags) > 0, logical(1))), 0)
})


# ---- the standardised post --------------------------------------------------

test_that("id is the cid of the post, not its uri nor its record key", {
  # the cid is the content hash: it is what the collector stores as the message id
  post <- parse_one(bsky_post(cid = "bafyreicid", rkey = "3rkey"))

  expect_equal(post$id, "bafyreicid")
})

test_that("uri is rebuilt as a web url from the author did and the record key", {
  # the at:// uri the API returns is not clickable: the standardised uri is the bsky.app permalink,
  # built from the did (not the handle, which can change) and the last segment of the at:// uri
  post <- parse_one(bsky_post(did = "did:plc:zzz", rkey = "3rkey"))

  expect_equal(post$uri, "https://bsky.app/profile/did:plc:zzz/post/3rkey")
})

test_that("created_at is reformatted to ISO 8601 UTC with microseconds", {
  # the API sends milliseconds, the plans and the database expect the episomer format
  post <- parse_one(bsky_post(created_at = "2026-08-27T08:00:00.123Z"))

  expect_equal(post$created_at, "2026-08-27T08:00:00.123000Z")
})

test_that("created_at is read from the record, not from indexedAt", {
  # indexedAt is when the relay saw the post; the collection window is about when it was written
  post <- bsky_post(created_at = "2026-08-20T08:00:00.000Z")
  post$indexedAt <- "2026-08-27T08:00:00.000Z"

  expect_equal(parse_one(post)$created_at, "2026-08-20T08:00:00.000000Z")
})

test_that("user_name is the handle without its .bsky.social suffix", {
  expect_equal(parse_one(bsky_post(handle = "user001.bsky.social"))$user_name, "user001")
})

test_that("user_name keeps a custom domain handle whole", {
  expect_equal(
    parse_one(bsky_post(handle = "user001.example.org"))$user_name,
    "user001.example.org"
  )
})

test_that("KNOWN QUIRK: user_name strips the bsky suffix as an unanchored regexp", {
  # KNOWN QUIRK, behaviour frozen here on purpose (roadmap T3-4).
  # gsub(".bsky.social", "", handle) is a regexp: the dots match any character and nothing anchors
  # the pattern to the end of the handle, so a custom domain that merely contains something shaped
  # like it loses that part of its name.
  post <- parse_one(bsky_post(handle = "abskyisocial.example.org"))

  expect_equal(post$user_name, ".example.org")
  # expect_equal(post$user_name, "abskyisocial.example.org")  # once fixed
})

test_that("text is the record text, kept verbatim", {
  # unlike mastodon, bluesky serves plain text: nothing to clean, and the newlines are meaningful
  text <- "line one\nline two #covid https://example.org/a"

  expect_equal(parse_one(bsky_post(text = text))$text, text)
})

test_that("lang is the first declared language of the record", {
  expect_equal(parse_one(bsky_post(langs = list("de", "en")))$lang, "de")
})

test_that("tags come from the facets of the record", {
  post <- parse_one(bsky_post(
    facets = list(list(features = list(
      list(`$type` = "app.bsky.richtext.facet#tag", tag = "covid19"),
      list(`$type` = "app.bsky.richtext.facet#tag", tag = "flu")
    )))
  ))

  expect_equal(post$tags, list("covid19", "flu"))
})

test_that("urls merge the link facets and the urls of the embed, deduplicated", {
  post <- parse_one(bsky_post(
    facets = link_facet("https://example.org/a"),
    embed = list(external = list(
      uri = "https://example.org/b",
      title = "a title",
      description = "a description"
    ))
  ))

  # facet urls first, embed urls after, and unique() over the two
  expect_equal(post$urls, list("https://example.org/a", "https://example.org/b"))
})

test_that("a url present both in a facet and in the embed is only kept once", {
  # the usual shape of a link post: the card and the text point at the same page
  post <- parse_one(bsky_post(
    facets = link_facet("https://example.org/a"),
    embed = list(external = list(
      uri = "https://example.org/a",
      title = "a title",
      description = "a description"
    ))
  ))

  expect_equal(post$urls, list("https://example.org/a"))
})

test_that("categories is always empty at this stage", {
  # bluesky_parse_features() never fills categories (T1-5): they are computed later, by the topic
  # matching, and the field only exists to keep one post format across networks
  expect_identical(parse_one(bsky_post())$categories, list())
})

test_that("a post without embed is not a quote and carries no quoted text", {
  post <- parse_one(bsky_post())

  expect_false(post$is_quote)
  expect_null(post$quoted_text)
  expect_null(post$quoted_lang)
})

test_that("is_quote needs a quoted text that is actually there", {
  # bluesky_parse_quoted() reports is_quote as soon as a quoted record is present, even one whose
  # text is empty; the response parser requires both, so an empty quote is not a quote
  post <- parse_one(bsky_post(embed = list(record = list(value = list(text = "")))))

  expect_true(bluesky_parse_quoted(bsky_post(embed = list(record = list(value = list(text = "")))))$is_quote)
  expect_false(post$is_quote)
})


# ---- the two date filters ---------------------------------------------------

test_that("a post created in the future is dropped with a warning", {
  # seen in production: a client with a wrong clock, or a bridged post, dates a post ahead of now.
  # Keeping it would make the collection window walk forward for ever.
  page <- bsky_page(bsky_post(created_at = bluesky_format_date(Sys.time() + 3600)))

  expect_warning(res <- bluesky_parse_response(page, NULL), "creation date in future")
  expect_equal(res$count, 0)
  expect_identical(res$posts, list())
})

test_that("a post created after current_min_date is dropped", {
  # current_min_date is the oldest date already collected by the plan: the search API sometimes
  # hands back newer posts than that, which would move the cursor backwards and loop for ever
  bound <- lubridate::as_datetime("2026-08-27T07:00:00Z")
  page <- bsky_page(
    bsky_post(cid = "newer", created_at = "2026-08-27T08:00:00.000Z"),
    bsky_post(cid = "at-the-bound", created_at = "2026-08-27T07:00:00.000Z"),
    bsky_post(cid = "older", created_at = "2026-08-27T06:00:00.000Z")
  )

  expect_warning(res <- bluesky_parse_response(page, bound), "after the upper bound")

  # the comparison is strict, so a post exactly on the bound is kept
  expect_equal(vapply(res$posts, `[[`, character(1), "id"), c("at-the-bound", "older"))
  expect_equal(res$count, 2)
})

test_that("min_created_at ignores the posts the bound dropped", {
  bound <- lubridate::as_datetime("2026-08-27T07:00:00Z")
  page <- bsky_page(
    bsky_post(cid = "newer", created_at = "2026-08-27T08:00:00.000Z"),
    bsky_post(cid = "older", created_at = "2026-08-27T06:00:00.000Z")
  )

  suppressWarnings(res <- bluesky_parse_response(page, bound))

  expect_equal(res$pagination$min_created_at, "2026-08-27T06:00:00.000000Z")
})

test_that("a NULL current_min_date keeps every past post", {
  # the first request of a plan has no lower bound yet
  page <- bsky_page(
    bsky_post(cid = "a", created_at = "2026-08-27T08:00:00.000Z"),
    bsky_post(cid = "b", created_at = "2020-01-01T00:00:00.000Z")
  )

  res <- bluesky_parse_response(page, NULL)

  expect_equal(res$count, 2)
})

test_that("KNOWN BUG: the out-of-window warning names the two dates the wrong way round (B-15)", {
  # KNOWN BUG, behaviour frozen here on purpose (roadmap T3-4).
  # The sprintf arguments are swapped: the message announces the creation date where it prints the
  # bound, and the bound where it prints the creation date. Cosmetic, but it makes the collection
  # logs read backwards exactly where one goes to understand a stuck plan.
  bound <- lubridate::as_datetime("2026-08-27T07:00:00Z")
  page <- bsky_page(bsky_post(created_at = "2026-08-27T08:00:00.000Z"))

  expect_warning(
    bluesky_parse_response(page, bound),
    "creation \\(2026-08-27 07:00:00\\) after the upper bound: \\(2026-08-27T08:00:00\\.000Z\\)"
  )
  # expect_warning(..., "creation \\(2026-08-27T08:00:00\\.000Z\\) after the upper bound: \\(2026-08-27 07:00:00\\)")
})


# ---- bluesky_parse_quoted ---------------------------------------------------

test_that("bluesky_parse_quoted reports nothing at all on a post without embed", {
  # the early return is a one-field list: the callers must not expect text / lang / urls
  expect_identical(bluesky_parse_quoted(bsky_post()), list(is_quote = FALSE))
})

test_that("an external card gives its title and description, but is not a quote", {
  # same rule as mastodon's card: the linked page is quoted material for the keyword matching, yet
  # the post is not a quote post
  quote <- bluesky_parse_quoted(bsky_post(embed = list(external = list(
    uri = "https://example.org/a",
    title = "a title",
    description = "a description"
  ))))

  expect_equal(quote$text, "a title\na description")
  expect_equal(quote$lang, "en")
  expect_equal(quote$urls, list("https://example.org/a"))
  expect_false(quote$is_quote)
})

test_that("KNOWN BUG: an external card missing one of its texts injects the string NULL (B-14)", {
  # KNOWN BUG, behaviour frozen here on purpose (roadmap T3-4).
  # The filter is `texts[!is.null(texts)]`: is.null() looks at the whole vector, which is never
  # NULL, so it always answers FALSE and the subsetting keeps everything -- including the absent
  # field, which paste() renders as the literal "NULL". A card carrying only a description (or only
  # a title) therefore feeds "NULL\na description" to the keyword matching and to the database.
  # The intended filter is texts[!sapply(texts, is.null)]; the mastodon counterpart does it right
  # (card_texts[!is.na(card_texts) & nchar(card_texts) > 0]).
  quote <- bluesky_parse_quoted(bsky_post(embed = list(external = list(
    uri = "https://example.org/a",
    description = "a description"
  ))))

  expect_equal(quote$text, "NULL\na description")
  # expect_equal(quote$text, "a description")  # once fixed
})

test_that("a quoted post is a quote and brings its own language", {
  quote <- bluesky_parse_quoted(bsky_post(
    langs = list("en"),
    embed = list(record = list(value = list(text = "the quoted post", langs = list("de"))))
  ))

  expect_true(quote$is_quote)
  expect_equal(quote$text, "the quoted post")
  expect_equal(quote$lang, "de")
})

test_that("a quoted post without language falls back on the quoting post", {
  quote <- bluesky_parse_quoted(bsky_post(
    langs = list("fr"),
    embed = list(record = list(value = list(text = "the quoted post")))
  ))

  expect_equal(quote$lang, "fr")
})

test_that("a quoted feed or list is read from its description", {
  # embed$record has no `value` when what is quoted is not a post (feed generator, list, starter
  # pack): the human readable text is its description
  quote <- bluesky_parse_quoted(bsky_post(
    embed = list(record = list(description = "a feed about outbreaks"))
  ))

  expect_true(quote$is_quote)
  expect_equal(quote$text, "a feed about outbreaks")
})

test_that("a quoted post nested one level deeper is found too", {
  # recordWithMedia puts the quoted post at embed$record$record$value instead of embed$record$value
  quote <- bluesky_parse_quoted(bsky_post(
    embed = list(record = list(record = list(value = list(text = "the nested quote"))))
  ))

  expect_true(quote$is_quote)
  expect_equal(quote$text, "the nested quote")
})

test_that("the card of the quoted post is harvested as well", {
  # the case the function exists for: the keyword only appears in the card of the quoted post
  quote <- bluesky_parse_quoted(bsky_post(embed = list(record = list(
    value = list(text = "look at this"),
    embeds = list(list(external = list(
      uri = "https://example.org/quoted",
      title = "covid figures",
      description = "weekly report"
    )))
  ))))

  expect_true(quote$is_quote)
  expect_equal(quote$text, "covid figures\nweekly report\nlook at this")
  expect_equal(quote$urls, list("https://example.org/quoted"))
})

test_that("the alternative texts of the images are collected", {
  quote <- bluesky_parse_quoted(bsky_post(embed = list(images = list(
    list(alt = "first chart"),
    list(alt = "second chart")
  ))))

  expect_equal(quote$text, "first chart\nsecond chart")
  expect_false(quote$is_quote)
})

test_that("an image without alternative text is skipped", {
  quote <- bluesky_parse_quoted(bsky_post(embed = list(images = list(
    list(alt = ""),
    list(alt = "the only alt text")
  ))))

  expect_equal(quote$text, "the only alt text")
})

test_that("the images of a recordWithMedia embed are collected", {
  quote <- bluesky_parse_quoted(bsky_post(embed = list(media = list(
    images = list(list(alt = "the media alt text"))
  ))))

  expect_equal(quote$text, "the media alt text")
})

test_that("a quoted post that is gone is recognised and yields nothing", {
  # notFound / blocked / detached are the three ways a quote disappears: the branch exists so that
  # the post is not reported as unexpected and dumped to badpost.json
  for (state in c("notFound", "blocked", "detached")) {
    embed <- list(record = stats::setNames(list(TRUE), state))

    quote <- expect_silent(bluesky_parse_quoted(bsky_post(embed = embed)))

    expect_null(quote$text)
    expect_false(quote$is_quote)
  }
})

test_that("a video embed is recognised and yields nothing", {
  # videos have no text to harvest, they only need to be known so the post is not dumped
  quote <- expect_silent(bluesky_parse_quoted(bsky_post(
    embed = list(playlist = "https://video.example.org/playlist.m3u8")
  )))

  expect_null(quote$text)
  expect_false(quote$is_quote)
})

test_that("quoted_lang is the majority language of the quoted texts, on two letters", {
  # mode() over the collected texts, not the language of the quoted post: two images in the
  # language of the quoting post outvote the quoted record
  quote <- bluesky_parse_quoted(bsky_post(
    langs = list("en"),
    embed = list(
      record = list(value = list(text = "the quoted post", langs = list("de"))),
      images = list(list(alt = "first chart"), list(alt = "second chart"))
    )
  ))

  expect_equal(quote$lang, "en")
})

test_that("quoted_lang is cut to the two first letters of the language tag", {
  quote <- bluesky_parse_quoted(bsky_post(
    langs = list("pt-BR"),
    embed = list(images = list(list(alt = "uma imagem")))
  ))

  expect_equal(quote$lang, "pt")
})

test_that("an embed shaped like nothing known warns and dumps the post", {
  # the escape hatch of the parser: bluesky keeps adding embed types, and this is how a new one is
  # noticed. The dump is written to conf$data_dir, so the parser CRASHES on an unset data_dir --
  # hence local_conf() here, and hence the warning-free assertion on the fixture above.
  local_conf(data_dir = withr::local_tempdir())
  post <- bsky_post(embed = list(`$type` = "app.bsky.embed.unknown#view", widget = list(a = 1)))

  expect_warning(quote <- bluesky_parse_quoted(post), "Embed information expected but not found")

  expect_true(file.exists(file.path(conf$data_dir, "badpost.json")))
  expect_null(quote$text)
  expect_false(quote$is_quote)
})
