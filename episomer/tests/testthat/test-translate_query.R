# Tests for translate_query() (R/config.R, roadmap item T1-3).
#
# translate_query does two things: it parses the query language written in the topics Excel file
# into the (ors, neg) structure documented on sm_api_translate_query_mastodon, then hands that
# structure to the network translator. Only the parser is tested here, so
# sm_api_translate_query is mocked away and the tests read the structure it receives -- the
# native query expansion is roadmap item T3-5.
#
# Reminder of the contract being checked, from the roxygen of sm_api_translate_query_mastodon:
#   ors : subqueries to be OR'ed
#     `- each subquery : terms to be AND'ed
#          `- each term : list of synonyms for one literal
#   neg : terms to exclude from the results of any of the subqueries

# Runs the parser only, and returns the (ors, neg) structure that would have been passed on to
# the network translator. The mock lives for the duration of this helper, which covers the
# translate_query call below.
parsed_query <- function(q, sm = "bluesky") {
  local_mocked_bindings(
    sm_api_translate_query = function(network, parsed) parsed,
    .package = "episomer"
  )
  translate_query(sm, q)
}

# ors as plain nested character vectors, easier to read in the assertions below than the
# list-of-list-of-list the parser builds
plain <- function(parsed) {
  lapply(parsed$ors, function(subquery) lapply(subquery, unlist))
}


# ---- routing to the network translator --------------------------------------

test_that("translate_query hands the parsed query to the translator of the given network", {
  seen <- NULL
  local_mocked_bindings(
    sm_api_translate_query = function(network, parsed) {
      seen <<- list(network = network, parsed = parsed)
      c("native query 1", "native query 2")
    },
    .package = "episomer"
  )

  result <- translate_query("mastodon.mstdn.party", "a or b")

  expect_equal(seen$network, "mastodon.mstdn.party")
  expect_length(seen$parsed$ors, 2)
  # the return value is the translator's, untouched
  expect_equal(result, c("native query 1", "native query 2"))
})


# ---- operators --------------------------------------------------------------

test_that("or splits the query into as many subqueries", {
  parsed <- parsed_query("a or b")

  expect_equal(plain(parsed), list(list("a"), list("b")))
  expect_equal(parsed$neg, list())
})

test_that("and puts the terms in a single subquery", {
  parsed <- parsed_query("a and b")

  expect_equal(plain(parsed), list(list("a", "b")))
  expect_equal(parsed$neg, list())
})

test_that("the operators are recognised in upper case too", {
  expect_equal(parsed_query("a OR b"), parsed_query("a or b"))
  expect_equal(parsed_query("a AND b"), parsed_query("a and b"))
})

test_that("the native operators & and | are accepted, spaces or not", {
  expect_equal(plain(parsed_query("a|b")), list(list("a"), list("b")))
  expect_equal(plain(parsed_query("a | b")), list(list("a"), list("b")))
  expect_equal(plain(parsed_query("a&b")), list(list("a", "b")))
  expect_equal(plain(parsed_query("a & b")), list(list("a", "b")))
})

test_that("and binds tighter than or", {
  parsed <- parsed_query("a and b or c and d")

  expect_equal(plain(parsed), list(list("a", "b"), list("c", "d")))
})

test_that("extra spaces around the operators are absorbed", {
  expect_equal(parsed_query("a  or  b"), parsed_query("a or b"))
  expect_equal(parsed_query("a   and   b"), parsed_query("a and b"))
})

test_that("a query with no operator is a single term", {
  expect_equal(plain(parsed_query("covid")), list(list("covid")))
})

test_that("a term keeps its inner spaces", {
  parsed <- parsed_query("long covid or post covid syndrome")

  expect_equal(plain(parsed), list(list("long covid"), list("post covid syndrome")))
})


# ---- synonyms ---------------------------------------------------------------

test_that("slashes declare synonyms inside one term", {
  parsed <- parsed_query("flu/influenza and vaccine")

  # one subquery, two terms, the first one holding two synonyms
  expect_equal(plain(parsed), list(list(c("flu", "influenza"), "vaccine")))
})

test_that("a term can hold more than two synonyms", {
  parsed <- parsed_query("flu/influenza/grippe")

  expect_equal(plain(parsed), list(list(c("flu", "influenza", "grippe"))))
})

test_that("the synonyms are trimmed", {
  expect_equal(
    parsed_query("flu / influenza"),
    parsed_query("flu/influenza")
  )
})


# ---- negations --------------------------------------------------------------

test_that("a term prefixed with - goes to neg, not to the subquery", {
  parsed <- parsed_query("vaccine and -covid")

  expect_equal(plain(parsed), list(list("vaccine")))
  expect_equal(parsed$neg, list("covid"))
})

test_that("a negation at the start of the query is recognised", {
  parsed <- parsed_query("-covid and vaccine")

  expect_equal(plain(parsed), list(list("vaccine")))
  expect_equal(parsed$neg, list("covid"))
})

test_that("several negations are all collected, duplicates included", {
  parsed <- parsed_query("vaccine and -covid and -flu and -covid")

  expect_equal(plain(parsed), list(list("vaccine")))
  expect_equal(parsed$neg, list("covid", "flu", "covid"))
})

test_that("a hyphen inside a word is not a negation", {
  parsed <- parsed_query("covid-19 or post-covid")

  expect_equal(plain(parsed), list(list("covid-19"), list("post-covid")))
  expect_equal(parsed$neg, list())
})

test_that("the negations are global, they are not scoped to the subquery they appear in", {
  # Documented behaviour rather than a quirk: `neg` is a single flat list in the parsed
  # structure, and the translators append it to every native query they build. So a negation
  # written in the first branch of an `or` excludes the term from all the branches.
  parsed <- parsed_query("vaccine and -covid or flu")

  expect_equal(plain(parsed), list(list("vaccine"), list("flu")))
  expect_equal(parsed$neg, list("covid"))
})

test_that("an all-negative query yields an empty subquery", {
  # Not a crash, but nothing to search either: an empty subquery makes expand.grid return zero
  # rows in sm_api_translate_query_mastodon/_bluesky, so such a topic produces no native query
  # at all and is silently never collected.
  parsed <- parsed_query("-covid")

  expect_length(parsed$ors, 1)
  expect_equal(parsed$ors[[1]], list())
  expect_equal(parsed$neg, list("covid"))
})


# ---- known quirks, behaviour frozen on purpose -------------------------------

test_that("the operators are recognised in mixed case", {
  expect_equal(plain(parsed_query("flu Or influenza")), list(list("flu"), list("influenza")))
  expect_equal(plain(parsed_query("flu And influenza")), list(list("flu", "influenza")))
})

test_that("an operator at the very end of the query is now correctly handled", {
  expect_equal(plain(parsed_query("flu or")), list(list("flu")))
})

test_that("a lone hyphen surrounded by spaces is trimmed correctly", {
  parsed <- parsed_query("vaccine - covid")

  expect_equal(plain(parsed), list(list("vaccine")))
  expect_equal(parsed$neg, list("covid"))
})

test_that("a negation right after an or no longer swallows the or", {
  expect_equal(plain(parsed_query("flu or -covid")), list(list("flu")))
  expect_equal(parsed_query("flu or -covid")$neg, list("covid"))
  # The or is now preserved, creating two subqueries like it should
  parsed <- parsed_query("flu or -covid or vaccine")
  expect_equal(plain(parsed), list(list("flu"), list("vaccine")))
  expect_equal(parsed$neg, list("covid"))
})

test_that("an empty query returns an empty ors list", {
  parsed <- parsed_query("")
  expect_length(parsed$ors, 1)
  expect_equal(parsed$ors[[1]], list())
  expect_equal(parsed$neg, list())

  parsed_spaces <- parsed_query("   ")
  expect_length(parsed_spaces$ors, 1)
  expect_equal(parsed_spaces$ors[[1]], list())
  expect_equal(parsed_spaces$neg, list())
})
