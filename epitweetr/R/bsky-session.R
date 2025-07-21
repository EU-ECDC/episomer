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
#' bsky_create_session()
#' }
#'
bsky_create_session <- function(
  handle = Sys.getenv("bluesky_id"),
  password = Sys.getenv("bluesky_pwd"),
  login_url = "https://bsky.social/xrpc/com.atproto.server.createSession"
) {
  if (!is_online()) {
    stop("No internet connection")
  }

  if (is.null(handle) || is.null(password)) {
    stop("Handle and password are required")
  }

  if (handle == "") {
    stop("Handle is required")
  }

  if (password == "") {
    stop("Password is required")
  }

  if (login_url == "") {
    stop("Login URL is required")
  }
  resp <- request(login_url) |>
    req_body_json(list(identifier = handle, password = password)) |>
    req_perform()

  resp_check_status(resp)

  session <- resp_body_json(resp)
  session <- list(
    handle = session$handle,
    access_jwt = session$accessJwt,
    did = session$did,
    refreshJwt = session$refreshJwt,
    created = Sys.time()
  )
  saveRDS(session, "bsky_session.rds")
  return(session)
}

#' Get token
#'
#' @return Token
#' @export
#' @rdname session
bsky_get_token <- function() {
  if (file.exists("bsky_session.rds")) {
    session <- readRDS("bsky_session.rds")
    access_jwt <- bsky_check_token_validity(session$access_jwt)
    if (session$created < Sys.time() - 3600) {
      session <- bsky_create_session()
      access_jwt <- session$access_jwt
    }
    return(access_jwt)
  }
  return(bsky_create_session()$access_jwt)
}

#' @noRd
#' @rdname session
#' @importFrom httr2 request req_url_query req_headers req_error req_perform resp_status
bsky_check_token_validity <- function(
  access_jwt,
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
) {
  simple_request <- request(search_url) |>
    req_url_query(q = "covid19", limit = 1, sort = "latest") |>
    req_headers(Authorization = paste("Bearer", access_jwt)) |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()
  if (resp_status(simple_request) == 401) {
    message("Invalid token. Creating a new session.")
    access_jwt <- bsky_create_session()$access_jwt
  }
  return(access_jwt)
}
