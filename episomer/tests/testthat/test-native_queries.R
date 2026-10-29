# Tests for sm_api_translate_query_mastodon() and sm_api_translate_query_bluesky()
# (R/sm-mastodon-api.R, R/sm-bluesky-api.R, roadmap item T3-5).
#
# These are the second half of the query pipeline: translate_query() (tested in
# test-translate_query.R, item T1-3) parses the query language of the topics Excel file into the
# (ors, neg) structure, and these two translate that structure into the queries actually sent to
# each API.
#
#   ors : subqueries to be OR'ed
#     `- each subquery : terms to be AND'ed
#          `- each term : list of synonyms for one literal
#   neg : terms to exclude from the results of any of the subqueries
#
# Neither API supports parentheses or an explicit OR, so a subquery is expanded into ONE NATIVE
# QUERY PER COMBINATION of synonyms, and the search loop performs one request per query returned.
# The number of queries produced is therefore the number of requests per plan: the cartesian
# product below is a cost, not just a shape.
#
# The two implementations are character-for-character the same apart from their comments. They are
# run through the same battery here, and their equivalence is frozen by a test at the bottom so
# that fixing one and forgetting the other shows up.

# the structure translate_query() hands to the translators
parsed <- function(ors, neg = list()) list(ors = ors, neg = neg)
# a term, i.e. the list of synonyms of one literal
term <- function(...) list(...)

translators <- list(
  mastodon = sm_api_translate_query_mastodon,
  bluesky = sm_api_translate_query_bluesky
)

# runs one expectation against both implementations, naming the network in the test label
for (network in names(translators)) {
  translate <- translators[[network]]

  # ---- one query per subquery ------------------------------------------------

  test_that(
    sprintf("[%s] a single literal becomes a single native query", network),
    {
      expect_identical(translate(parsed(list(list(term("flu"))))), list("flu"))
    }
  )

  test_that(
    sprintf("[%s] the terms of a subquery are joined by a space", network),
    {
      # both APIs read a space as an implicit AND
      expect_identical(
        translate(parsed(list(list(term("flu"), term("vaccine"))))),
        list("flu vaccine")
      )
    }
  )

  test_that(
    sprintf("[%s] each subquery produces its own native query", network),
    {
      # no native OR operator: the OR is performed by issuing several requests
      expect_identical(
        translate(parsed(list(list(term("flu")), list(term("covid"))))),
        list("flu", "covid")
      )
    }
  )

  test_that(
    sprintf(
      "[%s] the queries come out in the order of the subqueries",
      network
    ),
    {
      expect_identical(
        translate(parsed(list(
          list(term("a")),
          list(term("b")),
          list(term("c"))
        ))),
        list("a", "b", "c")
      )
    }
  )

  # ---- synonyms are expanded into a cartesian product ------------------------

  test_that(
    sprintf(
      "[%s] the synonyms of a term are expanded into one query each",
      network
    ),
    {
      expect_identical(
        translate(parsed(list(list(term("flu"), term("covid", "corona"))))),
        list("flu covid", "flu corona")
      )
    }
  )

  test_that(
    sprintf("[%s] synonyms of several terms are combined pairwise", network),
    {
      # expand.grid varies the FIRST term fastest, which is the order the requests are performed in
      expect_identical(
        translate(parsed(list(list(term("a1", "a2"), term("b1", "b2"))))),
        list("a1 b1", "a2 b1", "a1 b2", "a2 b2")
      )
    }
  )

  test_that(
    sprintf(
      "[%s] the product grows multiplicatively with the synonyms",
      network
    ),
    {
      # 3 x 2 x 2 = 12 requests for a single subquery: worth knowing before writing the topic
      queries <- translate(parsed(list(list(
        term("a1", "a2", "a3"),
        term("b1", "b2"),
        term("c1", "c2")
      ))))

      expect_length(queries, 12)
      expect_length(unique(unlist(queries)), 12)
    }
  )

  # ---- negations -------------------------------------------------------------

  test_that(
    sprintf("[%s] a negated term is appended with a leading dash", network),
    {
      expect_identical(
        translate(parsed(list(list(term("flu"))), neg = list("vaccine"))),
        list("flu -vaccine")
      )
    }
  )

  test_that(
    sprintf(
      "[%s] the negations apply to every query of every subquery",
      network
    ),
    {
      # the negations are global to the query, see the scope test in test-translate_query.R
      expect_identical(
        translate(parsed(
          list(
            list(term("flu")),
            list(term("covid", "corona"), term("vaccine"))
          ),
          neg = list("trump", "election")
        )),
        list(
          "flu -trump -election",
          "covid vaccine -trump -election",
          "corona vaccine -trump -election"
        )
      )
    }
  )

  # ---- degenerate structures -------------------------------------------------

  test_that(
    sprintf("[%s] a subquery with no term produces no query", network),
    {
      # what an all-negative query ("-covid") parses to: nothing positive to search for, so the
      # topic is simply never collected -- no request, no warning
      expect_identical(
        translate(parsed(list(list()), neg = list("covid"))),
        list()
      )
    }
  )

  test_that(
    sprintf("[%s] a term with no synonym drops its whole subquery", network),
    {
      # a zero-length column makes the cartesian product empty
      expect_identical(
        translate(parsed(list(list(term("flu"), term()), list(term("covid"))))),
        list("covid")
      )
    }
  )

  test_that(
    sprintf("[%s] the result is a flat list of single strings", network),
    {
      queries <- translate(parsed(list(list(term("a", "b")), list(term("c")))))

      expect_type(queries, "list")
      expect_true(all(vapply(
        queries,
        function(q) is.character(q) && length(q) == 1L,
        logical(1)
      )))
    }
  )

  # ---- bugs and quirks frozen here -------------------------------------------

  test_that(
    sprintf("[%s] an empty ors list produces no query (B-8)", network),
    {
      # regression test for B-8: the loop used to be written `for (i in 1:length(parsed$ors))`, and
      # 1:0 is c(1, 0), so it ran once on an out-of-bounds index instead of not running at all.
      # Same pattern as B-4 in translate_query, which is still open.
      expect_identical(translate(parsed(list())), list())
    }
  )

  test_that(
    sprintf(
      "[%s] a term alone in its subquery is expanded, not ANDed (B-6)",
      network
    ),
    {
      # regression test for B-6. The subquery used to be passed to expand.grid through do.call, i.e.
      # one argument per term -- and expand.grid unwraps a single list argument, taking it as its
      # list of factors. A one-term subquery therefore had its SYNONYMS turned into one column each,
      # i.e. AND'ed: a topic written "covid/corona" was searched as `covid corona` and only collected
      # the posts holding both words.
      expect_identical(
        translate(parsed(list(list(term("covid", "corona"))))),
        list("covid", "corona")
      )
      expect_identical(
        translate(parsed(list(list(term("covid", "corona", "sars"))))),
        list("covid", "corona", "sars")
      )
      # adding a second term always worked, which is why the bug went unnoticed
      expect_identical(
        translate(parsed(list(list(term("covid", "corona"), term("vaccine"))))),
        list("covid vaccine", "corona vaccine")
      )
    }
  )

  test_that(
    sprintf("[%s] KNOWN QUIRK: a multi-word literal is not quoted", network),
    {
      # translate_query preserves the spaces inside a literal, but nothing quotes it here, so both
      # APIs read it as an implicit AND of its words: "covid vaccine" matches a post saying "covid"
      # in one sentence and "vaccine" in another. There is no way to express a phrase search.
      expect_identical(
        translate(parsed(list(list(term("covid vaccine"))))),
        list("covid vaccine")
      )
    }
  )

  test_that(
    sprintf(
      "[%s] KNOWN BUG: only the first word of a negation is negated",
      network
    ),
    {
      # "flu -covid vaccine" parses to neg = "covid vaccine", which is emitted as `-covid vaccine`:
      # the dash binds to the first word only, so "vaccine" becomes a REQUIRED term. The topic then
      # collects strictly fewer posts than the plain "flu", silently.
      expect_identical(
        translate(parsed(list(list(term("flu"))), neg = list("covid vaccine"))),
        list("flu -covid vaccine")
      )
      # expected behaviour would be to negate each word, or to quote: `-"covid vaccine"`
    }
  )
}

# ---- the two implementations agree ------------------------------------------

test_that("both networks translate a parsed query identically", {
  # duplicated implementations: freeze the equivalence so that fixing one and forgetting the other
  # fails here rather than in production on one network only
  query <- parsed(
    list(
      list(term("flu"), term("covid", "corona")),
      list(term("outbreak"))
    ),
    neg = list("trump")
  )

  expect_identical(
    sm_api_translate_query_mastodon(query),
    sm_api_translate_query_bluesky(query)
  )
})

# ---- end to end, from the Excel query to the native queries ------------------

test_that("a query written in the topics file reaches the API as the expected requests", {
  # the composition translate_query() -> sm_api_translate_query_<family>() is what the search loop
  # actually calls, and is where the two halves can disagree
  expect_identical(
    translate_query(
      "mastodon.mstdn.party",
      "flu or covid/corona and vaccine -trump"
    ),
    list("flu -trump", "covid vaccine -trump", "corona vaccine -trump")
  )
  expect_identical(
    translate_query("bluesky", "flu or covid/corona and vaccine -trump"),
    list("flu -trump", "covid vaccine -trump", "corona vaccine -trump")
  )
})

test_that("a topic made of a single synonym list searches each synonym (B-6)", {
  # end-to-end view of B-6, on the query a user is most likely to write in the Excel file: a list
  # of the ways of naming one thing. It used to come out as `covid corona`, the intersection.
  expect_identical(
    translate_query("bluesky", "covid/corona"),
    list("covid", "corona")
  )
  expect_identical(
    translate_query("mastodon.mstdn.party", "covid/corona/sars"),
    list("covid", "corona", "sars")
  )
  # each branch of an OR is a subquery of its own, so each is expanded independently
  expect_identical(
    translate_query("bluesky", "covid/corona or grippe/flu"),
    list("covid", "corona", "grippe", "flu")
  )
})

test_that("an all-negative query reaches the API as no request at all", {
  # the topic is configured, the plans are created, and nothing is ever collected
  expect_identical(translate_query("bluesky", "-covid"), list())
})
