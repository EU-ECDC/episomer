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

#' Get token
#'
#' @return Token
#' @export
#' @rdname session
bluesky_get_token <- function() {
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
