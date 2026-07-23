# @function_def_start (do not delete)

#' Get social media token
#' @details this function implements the authethication on the respective social media and stores it securely.
#' It is called by the search loop before starting message collection
#' @return Token
#' @export
sm_api_get_token_mastodon <- function() {
# @function_def_end (do not delete)
  # Mastodon uses a personal access token (Preferences > Development > New application).
  # Unlike Bluesky there is no login/password session to create: the token is used directly.
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
#' Pagination is done backwards in time using the mastodon snowflake ids: the plan attribute
#' `current_min_id` is sent as `max_id` (return results older than this id). The lower bound of
#' the window (`plan_min_date`) is enforced client-side by discarding returned posts older than
#' it (mastodon ids are monotonic in time, so pagination by id decreases created_at reliably).
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
  # instance <- if (!is.null(conf$mastodon_instance) && nchar(conf$mastodon_instance) > 0) {
  #   conf$mastodon_instance
  # } else {
  #   "mastodon.social"
  # }
  instance <- "mastodon.social"
  search_url <- sprintf("https://%s/api/v2/search", instance)
  number_of_posts_per_request <- 40 # v2 search caps limit at 40

  req <- httr2::request(search_url) %>%
    httr2::req_url_query(
      q = query,
      type = "statuses",
      limit = number_of_posts_per_request
    )

  # walk backwards in time: after the first request current_min_id holds the smallest id
  # obtained so far, used as an exclusive upper bound (max_id) for the next page.
  if (!is.null(plan$current_min_id)) {
    # NOTE: on Mastodon's status search, max_id is NOT an exact id cursor. It is converted
    # server-side to an inclusive, second-resolution "before:<timestamp>" filter (snowflake
    # ids sharing the same second are indistinguishable). So the boundary post and its
    # same-second neighbours keep coming back on every page. We de-duplicate client-side (see plan$seen_ids).
    req <- req %>%
      httr2::req_url_query(
        max_id = plan$current_min_id
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
  # explicit stop condition: if the smallest id obtained this round is the same as the
  # previous cursor, the pagination did not advance and there is nothing new to fetch,
  # so we end the plan (belt-and-suspenders on top of the client-side de-duplication).
  if (
    !is.null(results$pagination$min_id) &&
      !is.null(plan$current_min_id) &&
      results$pagination$min_id == plan$current_min_id
  ) {
    plan$end_on <- Sys.time()
    return(plan)
  }

  # record every id collected this round so already-seen posts are dropped on later pages.
  # Only ids in the boundary second (that of current_min_id) can ever come back, because
  # max_id only decreases and never re-includes a second once we have paginated past it.
  # We therefore keep only the most recently collected ids: seen_ids grows as
  # c(existing, new_ids) and each round's new_ids are the older posts, so the tail holds
  # the ids nearest the cursor. A page is capped at 40 posts, so this bound is far larger
  # than one second's worth of matches while keeping seen_ids from growing without limit.
  max_seen_ids <- 500L
  new_ids <- unlist(lapply(results$posts, `[[`, "id"))
  if (length(new_ids) > 0) {
    plan$seen_ids <- utils::tail(
      unique(c(plan$seen_ids, new_ids)),
      max_seen_ids
    )
  }

  # advance the pagination cursor: the smallest id obtained becomes the next max_id
  if (!is.null(results$pagination$min_id)) {
    if (
      is.null(plan$current_min_id) ||
        mastodon_id_lt(results$pagination$min_id, plan$current_min_id)
    ) {
      plan$current_min_id <- results$pagination$min_id
    }
  }
  # track the oldest collected date (used for progress computation, like Bluesky)
  if (!is.null(results$pagination$min_created_at)) {
    min_obtained <- lubridate::as_datetime(results$pagination$min_created_at)
    if (
      is.null(plan$current_min_date) || min_obtained < plan$current_min_date
    ) {
      plan$current_min_date <- min_obtained
    }
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

#' parses the response of the mastodon search API into the standardised episomer format. For more details see the vignette.
#' @param response list, the raw parsed json response (contains accounts, statuses, hashtags)
#' @param plan list, the current plan, used to discard posts out of the targeted window
mastodon_parse_response <- function(response, plan) {
  res <- list(
    network = "mastodon",
    pagination = list(min_id = NULL, min_created_at = NULL)
  )

  statuses <- response$statuses
  if (is.null(statuses)) {
    statuses <- list()
  }

  res[["posts"]] <- lapply(statuses, function(status) {
    created <- mastodon_parse_date(status$created_at)
    is_boost <- !is.null(status$reblog)
    tags <- if (!is.null(status$tags)) {
      lapply(status$tags, function(t) unlist(t$name))
    } else {
      list()
    }
    urls <- mastodon_extract_urls(unlist(status$content))
    if (is_boost) {
      urls <- unique(c(
        urls,
        mastodon_extract_urls(unlist(status$reblog$content))
      ))
    }

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
      is_quote = is_boost,
      quoted_text = if (is_boost) unlist(status$reblog$content) else NULL,
      quoted_lang = if (is_boost) unlist(status$reblog$language) else NULL,
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
    # anti-loop safety: discard posts newer than the current pagination position
    # if (
    #   !is.null(ret) &&
    #     !is.null(plan$current_min_date) &&
    #     created > plan$current_min_date
    # ) {
    #   warning(sprintf(
    #     "Ignoring post with creation (%s) after the upper bound (%s), uri: (%s)",
    #     status$created_at,
    #     plan$current_min_date,
    #     ret$uri
    #   ))
    #   ret <- NULL
    # }
    # lower bound of the window: discard posts older than plan_min_date. When a whole page
    # falls below this bound the resulting count is 0 and the plan is considered finished.
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
  # client-side de-duplication: because max_id is a coarse second-resolution filter (see the
  # note in sm_api_search_mastodon), already-seen posts are returned again on later pages.
  # Drop any post whose id has already been collected. When a whole page contains only
  # already-seen posts the resulting count is 0 and the plan is considered finished, which
  # also protects against an infinite pagination loop.
  if (length(res$posts) > 0 && !is.null(plan$seen_ids)) {
    res$posts <- res$posts[
      sapply(res$posts, function(p) !(p$id %in% plan$seen_ids))
    ]
  }

  if (length(res$posts) > 0) {
    res$pagination$min_id <- mastodon_min_id(lapply(res$posts, `[[`, "id"))
    res$pagination$min_created_at <- Reduce(
      function(x, y) if (x < y) x else y,
      lapply(res$posts, `[[`, "created_at")
    )
  }
  res$count <- length(res$posts)
  res
}
