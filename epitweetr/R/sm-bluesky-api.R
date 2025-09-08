#' Get token
#'
#' @return Token
#' @export
#' @rdname session
sm_api_get_token_bluesky <- function() {
  if (is_secret_set("bsky_session")) {
    session <- jsonlite::fromJSON(get_secret("bsky_session"))
    access_jwt <- bluesky_check_token_validity(session$access_jwt)
    if (session$created < strptime(Sys.time() - 3600, "%Y-%m-%d %H:%M:%S")) {
      session <- bluesky_create_session()
      access_jwt <- session$access_jwt
    }
    return(access_jwt)
  }
  return(bluesky_create_session()$access_jwt)
}

sm_api_translate_query_bluesky <- function(parts, excluded) {
    queries = list()
    comb = do.call(expand.grid, parts)
    if(nrow(comb) > 0) {
        ret <- lapply(1:nrow(comb), function(i) do.call(paste, as.list(comb[i,])))
        if(length(excluded) > 0)
          ret <- paste(ret, do.call(paste, as.list(paste0("-", excluded))))
	ret
    } else {
      list()
    }
}

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
sm_api_search_bluesky <- function(
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
    req <- req |> httr2::req_url_query(until = bluesky_format_date(plan$current_min_date - 0.000001))
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
#' Get bearer token
#'
#' @param handle Bluesky handle
#' @param password Bluesky password
#' @param login_url Bluesky login URL
#'
#' @return List with access_jwt and did
#' @export
#' @importFrom httr2 request req_body_json req_perform resp_check_status resp_body_json is_online req_headers
#' @details You can find more information about the Bluesky API here: \url{https://docs.bsky.app/docs/api/com-atproto-server-create-session}
#' @rdname session
#' @examples
#' \dontrun{
#' bluesky_create_session()
#' }
#'
bluesky_create_session <- function(handle = NULL, password = NULL) {
  if (is.null(handle) && is_secret_set("bsky_user")) {
      handle = get_secret("bsky_user")
  } else if(is.null(handle) && Sys.getenv("bluesky_id") != "")
      handle = Sys.getenv("bluesky_id")
  else {
    stop("You have to provide a user_name to collect Bluesky posts")
  }

  if (is.null(password) && is_secret_set("bsky_password")) {
      password = get_secret("bsky_password")
  } else if(is.null(handle) && Sys.getenv("bluesky_pwd") != "")
      password = Sys.getenv("bluesky_pwd")
  else {
    stop("You have to provide a password to collect Bluesky posts")
  }
  login_url = "https://bsky.social/xrpc/com.atproto.server.createSession"
  
  if (!httr2::is_online()) {
    stop("No internet connection")
  }

  resp <- httr2::request(login_url) |>
    httr2::req_body_json(list(identifier = handle, password = password)) |>
    httr2::req_perform()

  httr2::resp_check_status(resp)

  session <- httr2::resp_body_json(resp)
  session <- list(
    handle = session$handle,
    access_jwt = session$accessJwt,
    did = session$did,
    refreshJwt = session$refreshJwt,
    created = strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
  json_session = jsonlite::toJSON(session)
  set_secret("bsky_session",json_session)
  return(jsonlite::fromJSON(json_session))
}


#' @noRd
#' @rdname session
#' @importFrom httr2 request req_url_query req_headers req_error req_perform resp_status
bluesky_check_token_validity <- function(
  access_jwt,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
) {
  simple_request <- httr2::request(search_url) |>
    httr2::req_url_query(q = "covid19", limit = 1, sort = "latest") |>
    httr2::req_headers(Authorization = paste("Bearer", access_jwt)) |>
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_status(simple_request) == 401) {
    message("Invalid token. Creating a new session.")
    access_jwt <- bluesky_create_session()$access_jwt
  }
  return(access_jwt)
}
