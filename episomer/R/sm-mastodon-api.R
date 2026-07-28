# @function_def_start (do not delete)

#' Get social media token
#' @details this function implements the authethication on the respective social media and stores it securely.
#' It is called by the search loop before starting message collection
#' @return Token
#' @export
sm_api_get_token_mastodon <- function() {
  # @function_def_end (do not delete)
  # Mastodon uses a personal access token (Preferences > Development > New application).
  if (is_secret_set("mastodon_token")) {
    return(get_secret("mastodon_token"))
  }
  if (Sys.getenv("MASTODON_TOKEN") != "") {
    return(Sys.getenv("MASTODON_TOKEN"))
  }
  stop("You have to provide a Mastodon access token to collect Mastodon posts")
}

# @function_def_start (do not delete)

#' Translate a parsed query to the social media query format
#' @details This function receive as a parameter a parsed query from the a topic in the list of topics.
#' It can return one or many queries that matches the expected results using the underlying social media API
#' @param parsed list. Named list containing two attributes 'ors' and 'neg'.
#' 'ors' is a list containing subqueries that should be contatenated with an OR logic
#'   - each subquery in 'ors' is a list containing terms that should be concatecated with an AND logic
#'       - each term in 'ors subqueries' is a list of synonyms for a particular literal
#' 'neg' is a list of terms that should be removed from any result of the query
sm_api_translate_query_mastodon <- function(parsed) {
# @function_def_end (do not delete)
  # Mastodon does not support parenthesis nor an explicit OR operator, so like Bluesky we
  # expand the OR subqueries into several native queries. Within a subquery, terms are joined
  # by a space (implicit AND) and negated terms are appended with a leading '-'.
  ret = list()
  for (i in 1:length(parsed$ors)) {
    comb = do.call(expand.grid, parsed$ors[[i]])
    if (nrow(comb) > 0) {
      r <- paste(lapply(
        1:nrow(comb),
        function(i) do.call(paste, as.list(comb[i, ]))
      ))
      if (length(parsed$neg) > 0)
        r <- paste(r, do.call(paste, as.list(paste0("-", parsed$neg))))
      ret = c(ret, r)
    }
  }
  ret
}

# @function_def_start (do not delete)

#' Search posts for a given query and produce standardised results
#'
#' @param query query to search for
#' @param token Access token
#' @param plan Plan object
#' @param max_retries Maximum number of retries
#' @param verbose Whether to print progress messages
#' @details This function impleents the search of messages for a given query and plan.
#' The query has been already transformed by sm_api_translate_query_xxx so it is expected to be already in the native social media format
#' The token has been produced by the function sm_api_get_token_xxx and shoulb be used to autenticate the request
#' The plan should be used to extract the precise time-span to request messages
#'
#' - Response attributes
#'   - network: string
#'   - posts: [...]
#'   - count: number,
#'   - pagination:\{...\}
#' - post in posts
#'     - id: string
#'     - uri: string
#'     - created_at: datetime
#'     - user_name: string
#'     - text: string
#'     - lang: string
#'     - quoted_text: string
#'     - quoted_lang: string
#'     - tags: [...]
#'     - urls: [...]
#'     - categories: [...]
#' - tag in post.tags: string
#' - url in post.urls: string
#' - category in post.categories: string
#' - pagination: attributes used for paginating requests
#'
#' Pagination combines `max_id` and `offset`, because `max_id` alone is not enough on Mastodon's
#' status search: server-side `max_id` is converted to an inclusive, SECOND-resolution
#' `before:<timestamp>` filter, so it can only address a whole second, never a position inside
#' it. A popular keyword can have far more than one page (40) of matches in a single second, and
#' `max_id` on its own would keep returning that second's newest 40 forever. `offset` alone is not
#' enough either: the search is backed by Elasticsearch, whose `max_result_window` caps the usable
#' offset (~10000) — past it the API errors out.
#'
#' The strategy (results are ordered id DESC) walks a bounded offset under a moving `max_id`:
#' - `max_id` (`plan$current_min_id`) is the anchor selecting the "window": all posts in its second
#'   and older. It is null on the first request, which simply starts from the newest posts, and is
#'   then seeded onto the oldest id of that first page.
#' - `offset` (`plan$current_offset`) walks *inside* that window, one page at a time (0, 40, ...).
#' - As long as the offset stays below `mastodon_max_offset` we keep the same anchor and grow the
#'   offset by one page, digging deeper into the current window.
#' - When the offset would reach `mastodon_max_offset` we rebase the anchor onto the oldest id seen
#'   so far and reset the offset to 0. Because the rebased window drops every second newer than
#'   that id, the oldest id sits near the top of the new window, so the bounded offset has room to
#'   dig past it. See sm_api_update_plan_after_request_mastodon for the logic.
#'
#' This keeps the offset permanently below the Elasticsearch cap while the anchor gives unlimited
#' time depth. Two consequences of the server dropping DB-less rows (`.objects.compact`) shape the
#' details: (1) a page usually returns fewer than the requested 40 rows even when more exist, so we
#' advance the offset by the requested page size (pre-compact positions), never by the number of
#' rows kept; (2) each anchor change re-reads the head of the second it lands on, producing some
#' duplicate posts at the boundary. The client-side filters on the returned posts are the lower time
#' bound (`plan_min_date`, posts older than it are discarded) and a de-duplication against
#' `plan$seen_ids`, a sliding memory of the last `mastodon_seen_ids_size` ids returned by this plan
#' (the API is not stable enough to guarantee a status is served only once). Neither filter touches
#' the pagination bookkeeping, which is computed on the raw statuses.
#'
#' Stop criterion: Mastodon orders results by `id` (ingestion time) while the lower bound is on
#' `created_at`, and the two axes diverge because of federation, so a single request walked by id
#' can return only posts older than `plan_min_date` and yet be followed by newer ones. We therefore
#' do not end the plan on the first below-window request; instead we count CONSECUTIVE requests that
#' bring back no in-window post (`created_at >= plan_min_date`) in `plan$consecutive_empty`, and end
#' the plan once that streak reaches `conf$mastodon_max_empty_requests` (any in-window post resets
#' it). The one exception is the very FIRST request of the plan (no anchor, no offset, i.e. the
#' newest statuses matching the query): if it brings back no in-window post the plan ends
#' immediately, since every following request walks strictly older ids and could only move further
#' away from the window. Residual limitation: a single second holding more matches than
#' `mastodon_max_offset` cannot be fully drained.
#'
#' This is the implementation of the mastodon search. You can find more information about the mastodon search API here: \url{https://docs.joinmastodon.org/methods/search/#v2}
#' Additionnaly more information about the "Status" object is available here: \url{https://docs.joinmastodon.org/entities/Status/}
#' @return List with posts int the standard post format defined in episomer
#' @export
#' @importFrom httr2 request req_url_query req_headers req_perform resp_body_json is_online resp_status resp_header last_response req_retry
#' @importFrom magrittr %>%
#' @importFrom lubridate as_datetime
sm_api_search_mastodon <- function(
  query,
  token,
  plan,
  max_retries = 10,
  verbose = TRUE
) {
  # @function_def_end (do not delete)
  instance <- if (
    !is.null(conf$mastodon_instance) && nchar(conf$mastodon_instance) > 0
  ) {
    conf$mastodon_instance
  } else {
    "mastodon.social"
  }
  search_url <- sprintf("https://%s/api/v2/search", instance)

  req <- httr2::request(search_url) %>%
    httr2::req_url_query(
      q = query,
      type = "statuses",
      limit = mastodon_page_size # v2 search caps limit at 40
    )

  # current_min_id is the pagination anchor: it is sent as max_id and selects the window (posts in
  # its second and older, see @details). It is null on the very first request of a plan, where we
  # simply start from the newest posts, and is then advanced onto an older id at each rebase.
  if (!is.null(plan$current_min_id)) {
    req <- req %>%
      httr2::req_url_query(
        max_id = plan$current_min_id
      )
  }
  # offset walks inside the current max_id window, page by page (0, 40, 80, ...). It is reset
  # every time we rebase the anchor onto an older second, so it never grows beyond one dense second.
  if (!is.null(plan$current_offset) && plan$current_offset > 0) {
    req <- req %>%
      httr2::req_url_query(
        offset = plan$current_offset
      )
  }
  resp <- req %>%
    httr2::req_headers(Authorization = paste("Bearer", token)) %>%
    httr2::req_retry(
      max_tries = max_retries,
      retry_on_failure = TRUE,
      is_transient = mastodon_rate_limited_check,
      after = mastodon_rerun_after_rate_limit
    ) %>%
    httr2::req_perform()

  json <- httr2::resp_body_json(resp)
  mastodon_parse_response(json, plan)
}

# @function_def_start (do not delete)

#' @title Save mastodon credentials (access token and instance) and store them securely
#' @description Update configuration object and underlying storage with given mastodon access token and instance. The token is encrypted.
#' @param shiny_input_list List of shiny input values containing the mastodon specific credentials if not provided the input will be requested interactively
#' @details Update authentication tokens in configuration object
#' @examples
#' \dontrun{
#'  #Setting the configuration values
#'    sm_api_set_auth_mastodon(
#'      shiny_input_list = list(mastodon_token = "your token here", mastodon_instance = "mastodon.social"),
#'    )
#' }
#' @seealso
#' \code{\link{save_config}}
#' @return Nothing. Called for side effects (saving credentials)
#' @export
sm_api_set_auth_mastodon <- function(shiny_input_list=NULL) {
# @function_def_end (do not delete)
  if (!is.null(shiny_input_list)) {
    if (!"mastodon_token" %in% names(shiny_input_list)) {
      stop("mastodon_token is not in the shiny_input_list")
    }
    mastodon_token <- shiny_input_list$mastodon_token
    # instance is optional: keep the currently configured one if not provided
    mastodon_instance <- if ("mastodon_instance" %in% names(shiny_input_list)) {
      shiny_input_list$mastodon_instance
    } else {
      conf$mastodon_instance
    }
  } else {
    mastodon_instance <- readline("Mastodon instance (e.g. mastodon.social):")
    mastodon_token <- askpass::askpass("Mastodon access token:")
  }
  set_secret("mastodon_token", mastodon_token)
  # the instance is not sensitive: store it as a normal configuration property
  if (!is.null(mastodon_instance) && nchar(mastodon_instance) > 0) {
    conf$mastodon_instance <- mastodon_instance
    save_config(data_dir = conf$data_dir, sm_topics = list())
  }
}

# @function_def_start (do not delete)

#' Takes a plan after a request has been performed and the standardised results provided as returned by the function sm_api_search_xxx
#' it should return a plan with necessary information updated so next request will continue paginating as expected to obtain
#' targeted messages
#' @param plan list, the plan to update
#' @param results list, the obtained results of the plan
sm_api_update_plan_after_request_mastodon <- function(plan, results) {
  # @function_def_end (do not delete)
  # track the oldest collected date (used for progress computation, like Bluesky)
  if (!is.null(results$pagination$min_created_at)) {
    min_obtained <- lubridate::as_datetime(results$pagination$min_created_at)
    if (
      is.null(plan$current_min_date) || min_obtained < plan$current_min_date
    ) {
      plan$current_min_date <- min_obtained
    }
  }

  # sliding memory of the ids returned by the recent requests, used by mastodon_parse_response to
  # drop the duplicates the API hands back (see mastodon_seen_ids_size). Newest ids first, so the
  # oldest ones fall off the end once the memory is full.
  new_ids <- unlist(results$pagination$new_ids)
  if (length(new_ids) > 0) {
    seen_ids <- unique(c(new_ids, unlist(plan$seen_ids)))
    plan$seen_ids <- if (length(seen_ids) > mastodon_seen_ids_size) {
      seen_ids[1:mastodon_seen_ids_size]
    } else {
      seen_ids
    }
  }

  # --- bounded-offset + moving max_id pagination cursor (see @details in sm_api_search_mastodon) --
  # We reason on the RAW statuses the API returned (its own result positions), not on the posts
  # we kept, because offset addresses the API's ordering and must not be shifted by our
  # client-side filtering (language, window bounds).

  # advance the consecutive-empty streak used as the plan's stop criterion. A request that brought
  # back at least one in-window post (created_at >= plan_min_date) resets the streak; a request
  # with none extends it. mastodon_parse_response reads this streak to decide when to end the plan
  # (once it reaches conf$mastodon_max_empty_requests), so the two must stay in sync.
  in_window <- if (is.null(results$pagination$in_window)) {
    0L
  } else {
    results$pagination$in_window
  }
  if (in_window > 0) {
    plan$consecutive_empty <- 0L
  } else {
    plan$consecutive_empty <- (if (is.null(plan$consecutive_empty)) {
      0L
    } else {
      plan$consecutive_empty
    }) +
      1L
  }

  # current_min_id is the single pagination anchor sent as max_id (see @details). It moves only
  # when the offset walk restarts, i.e. at exactly two moments: the very first request of the plan
  # (it has no anchor yet), and each rebase. While walking offsets inside a window it stays fixed,
  # so the offset always addresses the same max_id window.
  # A NULL offset means the request just performed was the very first one of the plan (offset is
  # only ever reset to 0L, never back to NULL, so this is unambiguous).
  is_first_request <- is.null(plan$current_offset)
  previous_offset <- if (is.null(plan$current_offset)) {
    0L
  } else {
    plan$current_offset
  }

  if (is.null(results$pagination$oldest_id)) {
    # Empty page (no status at all, hence no id to anchor onto): we can only step the offset
    # forward inside the current window. Repeated empty pages keep extending the streak above and
    # will end the plan once it reaches the threshold, so this cannot loop forever.
    plan$current_offset <- previous_offset + mastodon_page_size
    return(plan)
  }

  if (is_first_request) {
    # First (max_id-less) page done: seed the anchor onto its oldest id and restart the offset, so
    # the next request paginates inside `before:<that second>`. The oldest id's second is re-read
    # at the boundary; the resulting duplicates are dropped by the plan$seen_ids filter in
    # mastodon_parse_response, and never affect the cursor (computed on the raw statuses).
    plan$current_min_id <- results$pagination$oldest_id
    plan$current_offset <- 0L
    return(plan)
  }

  # advance the bounded offset, rebasing the anchor when it would reach the Elasticsearch ceiling.
  next_offset <- previous_offset + mastodon_page_size

  if (next_offset < mastodon_max_offset) {
    # Still under the offset ceiling: keep the same anchor and dig one page deeper.
    plan$current_offset <- next_offset
  } else {
    # Reached the ceiling: rebase the anchor onto the oldest id seen and restart the offset. Since
    # we have walked deep into `before:<current anchor>`, this oldest id is well older than the
    # anchor, so the new `before:<second>` window drops every second newer than it and the bounded
    # offset has room to dig past it. Its second is re-read at the boundary (duplicates dropped by
    # the plan$seen_ids filter, see above).
    plan$current_min_id <- results$pagination$oldest_id
    plan$current_offset <- 0L
  }

  return(plan)
}

# @function_def_start (do not delete)

#' Receive the results provided by a request and return a logical value indicating whether the request returned results.
#' @details This function needs to be implemented for each social media since it is interpreted as 'No more messages are available for this plan'
#' When FALSE is returned the underlying plan will be ended
#' @param results list, results of the request in the standardised episomer format
sm_api_got_rows_mastodon <- function(results) {
# @function_def_end (do not delete)
  (exists("posts", results) & length(results$posts) > 0)
}


## The rest of the functions in this script are used by sm_api_***_mastodon functions and are internal

#' function to check if there has been a rate limit error returned by the mastodon API
#' @param resp, the httr2 response object
mastodon_rate_limited_check <- function(resp) {
  if (httr2::resp_status(resp) == 429) {
    TRUE
  } else if (httr2::resp_status(resp) %in% c(500, 502, 503)) {
    message(sprintf("Error %s encountered.", httr2::resp_status(resp)))
    TRUE
  } else {
    FALSE
  }
}

#' function to extract the number of seconds to wait after a rate limit has been reached
#' @param resp, the httr2 response object
mastodon_rerun_after_rate_limit <- function(resp) {
  if (httr2::resp_status(resp) == 429) {
    message("Rate limit exceeded. Waiting for reset.")
    reset <- httr2::resp_header(resp, "X-RateLimit-Reset")
    if (is.null(reset)) {
      return(60)
    }
    reset_after <- as.numeric(difftime(
      lubridate::as_datetime(reset),
      Sys.time(),
      units = "secs"
    ))
    reset_after <- max(reset_after, 1)
    message(sprintf("Rate limit reset in %s seconds.", reset_after))
    return(reset_after)
  } else {
    return(NA)
  }
}

#' formats a date in the expected mastodon format (ISO 8601 UTC)
#' @param datetime, the datetime to format
mastodon_format_date <- function(datetime) {
  format(datetime, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

#' parse a date as provided by mastodon into a date_time object
#' @param date_input char, the text of a date (ISO 8601)
mastodon_parse_date <- function(date_input) {
  lubridate::as_datetime(unlist(date_input))
}

#' compares two mastodon snowflake ids and returns TRUE if a < b.
#' ids exceed 32-bit integers so they are compared with bit64.
#' @param a char, first id
#' @param b char, second id
#' @importFrom bit64 as.integer64
mastodon_id_lt <- function(a, b) {
  bit64::as.integer64(a) < bit64::as.integer64(b)
}

#' returns the smallest of a list/vector of mastodon snowflake ids
#' @param ids character vector of ids
mastodon_min_id <- function(ids) {
  ids <- unlist(ids)
  ids <- ids[!is.na(ids)]
  if (length(ids) == 0) {
    return(NULL)
  }
  Reduce(function(x, y) if (mastodon_id_lt(x, y)) x else y, ids)
}

# page size requested to the v2 search (capped at 40 by the API). It is also the number of
# PRE-compact result positions each request consumes, so the offset advances by this amount
# inside a window (see the update function).
mastodon_page_size <- 40L

# number of recently returned post ids kept in `plan$seen_ids` to drop duplicates (see
# mastodon_parse_response). The API is not stable across requests (an anchor change re-reads the
# head of the second it lands on, and Elasticsearch positions can shift between requests), so the
# same status can come back several times. A short sliding memory is enough: duplicates only ever
# show up a few requests apart, and it keeps the plan file small.
mastodon_seen_ids_size <- 100L

# maximum offset walked inside a single max_id window before we rebase max_id and restart the
# offset at 0. It must stay well under the Elasticsearch `max_result_window` (~10000), past which
# the search API errors out. Smaller values rebase more often (a little more re-read overhead);
# larger values rebase less often but get closer to the cap. See the pagination @details.
mastodon_max_offset <- 400L


#' extracts http(s) urls from the html content of a mastodon status
#' @param content char, the html content of a status
mastodon_extract_urls <- function(content) {
  if (is.null(content) || length(content) == 0 || nchar(content) == 0) {
    return(list())
  }
  m <- regmatches(
    content,
    gregexpr('href="(https?://[^"]+)"', content, perl = TRUE)
  )[[1]]
  if (length(m) == 0) {
    return(list())
  }
  urls <- sub('^href="', "", sub('"$', "", m))
  as.list(unique(urls))
}

#' tells if an optional mastodon object is actually present.
#' the API serialises absent objects as `{}` (and not as `null`), which jsonlite turns into an
#' empty list: `is.null()` is therefore NOT enough to test for absence (`status$quote`,
#' `status$reblog`, `status$card`, `status$poll`, `status$in_reply_to_id`... are all `{}` on a
#' plain status).
#' @param x the value read from the parsed json
mastodon_is_present <- function(x) {
  !is.null(x) && length(x) > 0
}

#' extracts the quoted / embedded texts of a mastodon status, in the same spirit as
#' bluesky_parse_quoted(): everything that carries text around the status itself is collected and
#' concatenated into a single `quoted_text` used downstream for keyword matching.
#'
#' The mastodon cases and their bluesky counterparts:
#' \itemize{
#'   \item quote post (`status$quote$quoted_status`, mastodon >= 4.4) and boost (`status$reblog`)
#'         ~ bluesky `embed$record`: the content of the quoted / boosted status. These are the only
#'         cases that set `is_quote`.
#'   \item link preview card (`card`, title + description) ~ bluesky `embed$external`.
#'         Like bluesky, a card alone does NOT make the status a quote.
#'   \item media alternative texts (`media_attachments[[i]]$description`)
#'         ~ bluesky `embed$images[[i]]$alt`.
#'   \item polls (`poll`) are acknowledged but their options are not collected, like
#'         bluesky videos / poll facets.
#' }
#' A quoted / boosted status is a full Status object of its own: it carries its own `card` and its
#' own `media_attachments`, and those are harvested too (bluesky does the same by reading
#' `post$embed$record$embeds[[i]]$external`). A quote post whose card is the only place where the
#' keyword appears would otherwise be missed.
#'
#' Texts coming from a status content are kept as raw html, consistently with the `text` field of
#' mastodon_parse_response() (see the html question in the vignette).
#' @param status list, the status to extract data from, in mastodon format
mastodon_parse_quoted <- function(status) {
  # utilitary functions, same semantics as in bluesky_parse_quoted()
  first <- function(x) {
    x <- x[!sapply(x, is.null)]
    unlist(x[min(1, length(x))])
  }
  mode <- function(x) first(names(sort(table(unlist(x)), decreasing = TRUE)))

  quoted_texts <- list()
  urls <- list()
  is_quote <- FALSE

  status_lang <- unlist(status$language)

  # attachments of a status: link preview card and media alternative texts. Collected both on the
  # status itself and on every quoted / boosted status (which are full Status objects), hence the
  # local function. `default_lang` is the fallback when the attachment carries no language.
  parse_attachments <- function(s, default_lang) {
    texts <- list()
    found_urls <- list()
    # link preview card -> title and description of the linked page
    if (mastodon_is_present(s$card)) {
      card_texts <- unlist(lapply(
        c("title", "description"),
        function(attr) unlist(s$card[[attr]])
      ))
      card_texts <- card_texts[!is.na(card_texts) & nchar(card_texts) > 0]
      if (length(card_texts) > 0) {
        texts <- c(
          texts,
          list(list(
            text = paste(card_texts, collapse = "\n"),
            # the card language is the language of the linked page, a better guess than the
            # language of the status when both are available
            lang = first(c(unlist(s$card$language), default_lang))
          ))
        )
      }
      if (mastodon_is_present(s$card$url)) {
        found_urls <- c(found_urls, unlist(s$card$url))
      }
    }
    # alternative texts of the attached medias
    for (media in s$media_attachments) {
      description <- unlist(media$description)
      if (!is.null(description) && nchar(description) > 0) {
        texts <- c(texts, list(list(text = description, lang = default_lang)))
      }
    }
    # polls: options not considered
    list(texts = texts, urls = found_urls)
  }

  # case 1: quote post and boost -> content of the quoted / boosted status, plus its own
  # attachments (the quoted status is a full Status object).
  quoted_statuses <- list()
  if (
    mastodon_is_present(status$quote) &&
      mastodon_is_present(status$quote$quoted_status)
  ) {
    quoted_statuses <- c(quoted_statuses, list(status$quote$quoted_status))
  }
  # a quote whose state is not "accepted" (pending, rejected, revoked, deleted, unauthorized)
  # carries no quoted_status at all: there is simply nothing to extract.
  if (mastodon_is_present(status$reblog)) {
    quoted_statuses <- c(quoted_statuses, list(status$reblog))
  }
  for (quoted in quoted_statuses) {
    quoted_lang <- first(c(unlist(quoted$language), status_lang))
    content <- unlist(quoted$content)
    if (!is.null(content) && nchar(content) > 0) {
      is_quote <- TRUE
      quoted_texts <- c(
        quoted_texts,
        list(list(text = content, lang = quoted_lang))
      )
      urls <- c(urls, mastodon_extract_urls(content))
    }
    nested <- parse_attachments(quoted, quoted_lang)
    quoted_texts <- c(quoted_texts, nested$texts)
    urls <- c(urls, nested$urls)
  }

  # case 2: attachments of the status itself
  own <- parse_attachments(status, status_lang)
  quoted_texts <- c(quoted_texts, own$texts)
  urls <- c(urls, own$urls)

  text <- paste(sapply(quoted_texts, `[[`, "text"), collapse = "\n")
  lang <- paste(
    substr(mode(lapply(quoted_texts, `[[`, "lang")), 1, 2),
    collapse = "\n"
  )
  list(
    text = if (nchar(text) == 0) NULL else text,
    lang = if (nchar(lang) == 0) NULL else lang,
    urls = unique(urls),
    is_quote = is_quote
  )
}

#' parses the response of the mastodon search API into the standardised episomer format. For more details see the vignette.
#' @param response list, the raw parsed json response (contains accounts, statuses, hashtags)
#' @param plan list, the current plan, used to discard posts out of the targeted window
mastodon_parse_response <- function(response, plan) {
  res <- list(
    network = "mastodon",
    # pagination info used by sm_api_update_plan_after_request_mastodon to advance the
    # max_id + offset cursor. Everything here is computed on the RAW statuses (before any
    # client-side filtering) because it must reflect the API's own result positions/ordering.
    pagination = list(
      returned = 0L, # number of statuses the API returned (its page size for this request)
      oldest_id = NULL, # smallest (oldest) id of the page -> next anchor (max_id) at first req/rebase
      min_created_at = NULL, # oldest creation date kept (for progress), set further down
      in_window = 0L, # raw statuses with created_at >= plan_min_date -> consecutive-empty streak
      new_ids = character(0) # ids kept by this request -> appended to plan$seen_ids by the update fn
    )
  )

  statuses <- response$statuses
  if (is.null(statuses)) {
    statuses <- list()
  }

  # raw pagination bookkeeping (see comment on res$pagination above)
  res$pagination$returned <- length(statuses)
  if (length(statuses) > 0) {
    raw_ids <- unlist(lapply(statuses, `[[`, "id"))
    res$pagination$oldest_id <- mastodon_min_id(raw_ids)
  }

  res[["posts"]] <- lapply(statuses, function(status) {
    created <- mastodon_parse_date(status$created_at)
    quote <- mastodon_parse_quoted(status)
    tags <- if (!is.null(status$tags)) {
      lapply(status$tags, function(t) unlist(t$name))
    } else {
      list()
    }
    # urls of the status itself + those of the embedded content (card, quoted status)
    urls <- unique(c(
      mastodon_extract_urls(unlist(status$content)),
      unlist(quote$urls)
    ))

    ret <- list(
      id = unlist(status$id),
      uri = if (!is.null(status$url)) {
        unlist(status$url)
      } else {
        unlist(status$uri)
      },
      created_at = mastodon_format_date(created),
      user_name = unlist(status$account$acct),
      text = unlist(status$content),
      lang = unlist(status$language),
      # like bluesky: a status is a quote only if some quoted text was actually extracted
      is_quote = quote$is_quote &&
        !is.null(quote$text) &&
        nchar(quote$text) > 0,
      quoted_text = quote$text,
      quoted_lang = quote$lang,
      tags = tags,
      urls = urls,
      categories = list()
    )

    # discard posts with a creation date in the future
    if (created > Sys.time()) {
      warning(sprintf(
        "Ignoring post with creation date in future :%s (%s)",
        status$created_at,
        ret$uri
      ))
      ret <- NULL
    }

    # lower bound of the window: discard posts older than plan_min_date. Reaching this bound is
    # what ends the plan (see the end-of-plan signal computed below, on the raw statuses).
    if (
      !is.null(ret) &&
        !is.null(plan$plan_min_date) &&
        created < plan$plan_min_date
    ) {
      ret <- NULL
    }
    ret
  })

  # removing null posts (out of window or in the future)
  if (length(res$posts) > 0) {
    res$posts <- res$posts[sapply(res$posts, function(p) !is.null(p))]
  }
  # aligning with Bluesky: posts without a detected language are discarded
  if (length(res$posts) > 0) {
    res$posts <- res$posts[sapply(res$posts, function(p) !is.null(p$lang))]
  }

  # de-duplication against the ids returned by the last `mastodon_seen_ids_size` posts of this plan
  # (`plan$seen_ids`). The search API is not stable: each anchor change re-reads the head of the
  # second it lands on (max_id's `before:<second>` is inclusive) and Elasticsearch positions can
  # shift between requests, so a status we already returned can come back. Only res$posts is
  # filtered here: res$pagination is deliberately computed on the RAW statuses (positions, oldest
  # id, in_window streak), so dropping duplicates never disturbs the pagination cursor nor the
  # stop criterion.
  if (length(res$posts) > 0 && length(unlist(plan$seen_ids)) > 0) {
    seen <- unlist(plan$seen_ids)
    res$posts <- res$posts[sapply(res$posts, function(p) !(p$id %in% seen))]
  }
  # ids kept by this request: sm_api_update_plan_after_request_mastodon pushes them onto
  # plan$seen_ids so the following requests can filter them out.
  if (length(res$posts) > 0) {
    res$pagination$new_ids <- unlist(lapply(res$posts, `[[`, "id"))
  }

  if (length(res$posts) > 0) {
    res$pagination$min_created_at <- Reduce(
      function(x, y) if (x < y) x else y,
      lapply(res$posts, `[[`, "created_at")
    )
  }

  # number of NEW posts to store this request (after de-duplication and every client-side filter)
  res$stored <- length(res$posts)

  # number of RAW statuses still inside the lower time bound (created_at >= plan_min_date). We
  # reason on the raw statuses, not on res$posts: the seen_ids de-duplication can empty res$posts on
  # a rebase page that only re-reads the already-seen head of a window (max_id's before:<second> is
  # inclusive), even though older posts are still reachable at the next offset.
  # When there is no lower bound every returned status counts as in-window.
  if (length(statuses) == 0) {
    in_window <- 0L
  } else if (is.null(plan$plan_min_date)) {
    in_window <- length(statuses)
  } else {
    raw_created <- lapply(statuses, function(s) {
      mastodon_parse_date(s$created_at)
    })
    in_window <- sum(sapply(raw_created, function(d) d >= plan$plan_min_date))
  }
  # expose it so sm_api_update_plan_after_request_mastodon can advance the consecutive-empty streak
  res$pagination$in_window <- in_window

  # end-of-plan signal (res$count), consumed by the generic update_plan_after_request(): it ends
  # the plan when count == 0. We do NOT end on the first below-window request, because Mastodon's
  # created_at is noisy (a "supposedly older" request, walked by id, can still surface more recent
  # posts than an earlier one, see @details in sm_api_search_mastodon). Instead we only end once
  # `conf$mastodon_max_empty_requests` CONSECUTIVE requests have brought back no in-window post.
  # `plan$consecutive_empty` holds the streak so far (updated in the update function); here we look
  # one step ahead to decide whether THIS request closes the plan.
  threshold <- if (!is.null(conf$mastodon_max_empty_requests)) {
    conf$mastodon_max_empty_requests
  } else {
    5L
  }
  prev_streak <- if (is.null(plan$consecutive_empty)) {
    0L
  } else {
    plan$consecutive_empty
  }
  # the very first request of the plan (no offset set yet, see the update function) has neither
  # anchor nor offset: it returns the newest statuses matching the query. If none of them is inside
  # the window there is nothing to collect at all -- every following request walks strictly older
  # ids, so it could only go further away from the window. We end the plan straight away instead of
  # spending `threshold` requests on it: the noisy-created_at argument below only justifies
  # persevering *while paginating*, not on a head request that already comes back empty.
  is_first_request <- is.null(plan$current_offset)
  if (in_window > 0) {
    # in-window posts found: keep paginating, streak resets in the update function.
    res$count <- in_window
  } else if (is_first_request || prev_streak + 1L >= threshold) {
    # first request with nothing in the window, or this request completes the streak of empty
    # requests -> end the plan.
    res$count <- 0L
  } else {
    # still below the streak threshold: keep paginating (positive sentinel so the plan continues
    # and the update function runs to bump the streak and advance the cursor).
    res$count <- 1L
  }
  res
}
