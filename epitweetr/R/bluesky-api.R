#' Search posts for a given query
#'
#' @param query query to search for
#' @param token Access token
#' @param plan Plan object
#' @param max_retries Maximum number of retries
#' @param errors_for_retries Errors for retries
#' @param verbose Whether to print progress messages
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
#' Additionnaly more information about the "post" object is available here: \url{https://atproto.blue/en/latest/atproto/atproto_client.models.app.bsky.feed.defs.html#atproto_client.models.app.bsky.feed.defs.PostView}
#' @return List with posts and next cursor for resumption
#' @export
#' @importFrom httr2 request req_url_query req_headers req_perform resp_body_json is_online resp_status last_response req_retry
#' @importFrom lubridate as_datetime
bluesky_search <- function(
  query,
  token,
  plan,
  max_retries = 20,
  errors_for_retries = c(420, 500, 503),
  verbose = TRUE
) {
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
  sort <- "latest"
  number_of_posts_per_request <- 100

  # Make a single request and return results
  req <- httr2::request(search_url) |>
    httr2::req_url_query(q = query, limit = number_of_posts_per_request, sort = sort)

  if (!is.null(plan$plan_min_date)) {
    req <- req |> httr2::req_url_query(since = bluesky_format_date(plan$plan_min_date))
  }
  if (!is.null(plan$current_min_date)) {
    req <- req |> httr2::req_url_query(until = bluesky_format_date(plan$current_min_date))
  } else {
    req <- req |> httr2::req_url_query(until = bluesky_format_date(plan$plan_max_date))
  }

  # Must find a way to check for invalid token
  resp <- req |>
    httr2::req_headers(Authorization = paste("Bearer", token)) |>
    httr2::req_retry(
      max_tries = max_retries,
      retry_on_failure = TRUE,
      is_transient = bluesky_rate_limited_check,
      after = bluesky_rerun_after_rate_limit
    ) %>%
    httr2::req_perform()

  # Get results
  results <- httr2::resp_body_json(resp)
  posts <- results$posts
  created_at <- bluesky_extract_many_posts_created_at(posts)
  if (length(created_at) == 0) {
    min_created_at <- NULL
  } else {
    min_created_at <- min(unlist(created_at)) %>% bluesky_parse_date()
  }

  if (verbose) {
    cat("Retrieved", length(posts), "posts.\n")
  }

  # Return both posts and next cursor for potential resumption
  return(list(
    posts = posts,
    query_min_date = min_created_at
  ))
}

#' @noRd
bluesky_rate_limited_check <- function(resp) {
  if (httr2::resp_status(resp) == 429) {
    identical(httr2::resp_header(resp, "RateLimit-Remaining"), "0")
  } else if (httr2::resp_status(resp) == 503) {
    TRUE
  } else {
    FALSE
  }
}

#' @noRd
bluesky_rerun_after_rate_limit <- function(resp) {
  if (httr2::resp_status(resp) == 429) {
    time <- as.numeric(resp_header(resp, "RateLimit-Reset"))
    time - unclass(Sys.time())
  } else {
    return(NA)
  }
}

#' @noRd
bluesky_format_date <- function(datetime) {
  timezone = "UTC"
  # Full ISO 8601 format with time
  format(datetime, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

#' @noRd
bluesky_parse_date <-function(date_input) {
   lubridate::as_datetime(date_input)
   # utc="UTC"
   # curDigits <- options("digits.secs")
   # on.exit(options(digits.secs = unlist(curDigits)))
   # options(digits.secs = 6)
   # localtz=Sys.timezone()
   # format="%Y-%m-%dT%H:%M:%OSZ"
   # as.POSIXct(format(as.POSIXct(date_input, tz=utc, format=format), format, tz=localtz), format=format)
}
