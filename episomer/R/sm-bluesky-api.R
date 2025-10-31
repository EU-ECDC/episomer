# @function_def_start (do not delete)

#' Get token
#'
#' @return Token
#' @export
#' @rdname session
sm_api_get_token_bluesky <- function() {
# @function_def_end (do not delete)
  if (is_secret_set("bluesky_session")) {
    session <- jsonlite::fromJSON(get_secret("bluesky_session"))
    access_jwt <- bluesky_check_token_validity(session$access_jwt)
    if (session$created < strptime(Sys.time() - 3600, "%Y-%m-%d %H:%M:%S")) {
      session <- bluesky_create_session()
      access_jwt <- session$access_jwt
    }
    return(access_jwt)
  }
  return(bluesky_create_session()$access_jwt)
}


# @function_def_start (do not delete)
sm_api_translate_query_bluesky <- function(parsed) {
# @function_def_end (do not delete)
  ret = list()
  for(i in 1:length(parsed$ors)) {
     comb = do.call(expand.grid, parsed$ors[[i]])
     if (nrow(comb) > 0) {
       r <- paste(lapply(1:nrow(comb), function(i) do.call(paste, as.list(comb[i, ]))))
       if(length(parsed$neg)>0) 
          r <- paste(r, do.call(paste, as.list(paste0("-", parsed$neg))))
       ret = c(ret, r)
     }
  }
  ret
}


# @function_def_start (do not delete)

#' Search posts for a given query
#'
#' @param query query to search for
#' @param token Access token
#' @param plan Plan object
#' @param max_retries Maximum number of retries
#' @param errors_for_retries Errors for retries
#' @param verbose Whether to print progress messages
#' @details You can find more information about the bluesky API here: \url{https://docs.bsky.app/docs/api/app-bsky-feed-search-posts}
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
# @function_def_end (do not delete)
  search_url = "https://bsky.social/xrpc/app.bsky.feed.searchPosts"
  sort <- "latest"
  number_of_posts_per_request <- 100

  # Make a single request and return results
  req <- httr2::request(search_url) |>
    httr2::req_url_query(
      q = query,
      limit = number_of_posts_per_request,
      sort = sort
    )

  if (!is.null(plan$plan_min_date)) {
    req <- req |>
      httr2::req_url_query(since = bluesky_format_date(plan$plan_min_date))
  }
  if (!is.null(plan$current_min_date)) {
    req <- req |>
      httr2::req_url_query(
        until = bluesky_format_date(plan$current_min_date - 0.000001)
      )
  } else {
    req <- req |>
      httr2::req_url_query(until = bluesky_format_date(plan$plan_max_date))
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
  json <- httr2::resp_body_json(resp)
  bluesky_parse_response(json)
}

# @function_def_start (do not delete)

#' @title Save bluesky credentials (login and password) and store them securely
#' @description Update configuration object and underlying storage with given bluesky username and password. The password is encrypted.
#' @param shiny_input_list List of shiny input values containing the bluesky specific credentials
#' @details Update authentication tokens in configuration object
#' @examples
#' if(FALSE){
#'  #Setting the configuration values
#'    sm_api_set_auth_bluesky(
#'      shiny_input_list = list(bluesky_user = "your user here", bluesky_password = "your password"),
#'    )
#' }
#' @seealso
#' \code{\link{save_config}}
#' @rdname set_auth
#' @export
sm_api_set_auth_bluesky <- function(shiny_input_list) {
# @function_def_end (do not delete)
  if(!"bluesky_user" %in% names(shiny_input_list)) {
    stop("bluesky_user is not in the shiny_input_list")
  }
  if(!"bluesky_password" %in% names(shiny_input_list)) {
    stop("bluesky_password is not in the shiny_input_list")
  }
  set_secret("bluesky_user", shiny_input_list$bluesky_user)
  set_secret("bluesky_password", shiny_input_list$bluesky_password)
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
bluesky_parse_date <- function(date_input) {
  lubridate::as_datetime(unlist(date_input))
}
#' @noRd
bluesky_parse_response <- function(response) {
  first <- function(x) unlist(x[min(1, length(x))])
  res = list(
    network = "bluesky",
    count = length(response$posts),
    pagination = list(min_created_at = NULL)
  )
  res[["posts"]] = lapply(response$posts, function(post) {
    record <- post$record
    quote <- bluesky_parse_quoted(post)
    features <- bluesky_parse_features(post)
    urls <- unique(c(features$urls, quote$urls))
    ret <- list(
      id = unlist(post$cid),
      uri = sprintf(
        "https://bsky.app/profile/%s/post/%s",
        post$author$did,
        sub(".*/", "", post$uri)
      ),
      created_at = bluesky_format_date(bluesky_parse_date(record$createdAt)),
      user_name = gsub(".bsky.social", "", post$author$handle),
      text = unlist(record$text),
      lang = first(record$langs),
      is_quote = quote$is_quote &&
        !is.null(quote$text) &&
        nchar(quote$text) > 0,
      quoted_text = quote$text,
      quoted_lang = quote$lang,
      tags = features$tags,
      urls = urls,
      categories = features$categories
    )
    if(bluesky_parse_date(record$createdAt) > Sys.time()) {
        warning(sprintf("Ignoring post with creation date in future :%s (%s)", record$createdAt, ret$uri))
        ret <- NULL
    }


    #msg(jsonlite::toJSON(list(text=substr(ret$text, 1, 30), lang = ret$lang, quote=substr(ret$quoted_text, 1, 30), qlang=ret$quoted_lang), auto_unbox = T, null = "null"))
    #if(!is.null(quote$text) && nchar(quote$text) > 0)
    #msg(jsonlite::toJSON(list(text=substr(ret$text, 1, 30), isq = ret$is_quote, quote=substr(ret$quoted_text, 1, 30), qlang=ret$quoted_lang), auto_unbox = T, null = "null"))
    ret
  })
  # removing null posts due to a creation date in future
  
  if (length(res$posts) > 0) {
      res$posts <- res$posts[sapply(res$posts, function(p) !is.null(p))]
  }
  if (length(res$posts) > 0) {
    res$pagination$min_created_at <- Reduce(
      function(x, y) if (x < y) x else y,
      lapply(res$posts, `[[`, "created_at")
    )
    res$posts <- res$posts[sapply(res$posts, function(p) !is.null(p$lang))]
  }
  res
}

bluesky_parse_quoted <- function(post) {
  # utilitary functions
  first <- function(x) {
    x <- x[!sapply(x, is.null)]
    unlist(x[min(1, length(x))])
  }
  mode <- function(x) first(names(sort(table(unlist(x)), decreasing = TRUE)))

  # extracting embedding data from post
  if ("embed" %in% names(post)) {
    quoted_texts = list()
    embeds = list()
    records = list()
    images = list()
    urls = list()
    is_quote <- FALSE

    if (
      "record" %in% names(post$embed) && "embeds" %in% names(post$embed$record)
    ) {
      embeds = post$embed$record$embeds
    }
    if ("external" %in% names(post$embed)) {
      embeds = c(embeds, list(post$embed))
    }
    found <- FALSE
    # case 1: URLS title and descriptions
    #         post$embed$record$embeds[[1]]$external$(title and description)
    for (embed in embeds) {
      if (
        "external" %in%
          names(embed) &&
          length(intersect(
            names(embed$external),
            c("uri", "title", "description")
          )) >
            0
      ) {
        found <- TRUE
        texts = sapply(
          list("title", "description"),
          function(attr) unlist(embed$external[[attr]])
        )
        quoted_texts = c(
          quoted_texts,
          list(list(
            text = paste(texts[!is.null(texts)], collapse = "\n"),
            lang = mode(post$record$langs)
          ))
        )
        if ("uri" %in% names(embed$external)) {
          urls = c(urls, embed$external$uri)
        }
      }
    }
    # case 2: quoted post
    #         post$embed$record$value$text
    #         post$embed$record$description
    #         post$embed$record$record$value$text
    #         post$embed$record$record$description
    if (
      "record" %in%
        names(post$embed) &&
        (("value" %in%
          names(post$embed$record) &&
          "text" %in% names(post$embed$record$value)) ||
          "description" %in% names(post$embed$record))
    ) {
      records = c(records, list(post$embed$record))
    }
    if (
      "record" %in%
        names(post$embed) &&
        "record" %in% names(post$embed$record) &&
        (("value" %in%
          names(post$embed$record$record) &&
          "text" %in% names(post$embed$record$record$value)) ||
          "description" %in% names(post$embed$record$record))
    ) {
      records = c(records, list(post$embed$record$record))
    }
    for (record in records) {
      found <- TRUE
      is_quote <- TRUE
      quoted_texts = c(
        quoted_texts,
        list(list(
          text = first(c(record$value$text, record$description)),
          lang = first(c(record$value$langs, post$record$langs))
        ))
      )
    }
    # case 3: Images alternative textes
    #         post$embed$images[[1]]$alt
    #         post$embed$media$images[[1]]$alt
    if ("images" %in% names(post$embed)) {
      found <- TRUE
      images = c(images, post$embed$images)
    }
    if (
      "media" %in% names(post$embed) && "images" %in% names(post$embed$media)
    ) {
      found <- TRUE
      images = c(images, post$embed$media$images)
    }
    for (image in images) {
      if ("alt" %in% names(image) && nchar(image$alt) > 0) {
        quoted_texts = c(
          quoted_texts,
          list(list(
            text = image$alt,
            lang = mode(post$record$langs)
          ))
        )
      }
    }
    # case 4: video (not considered)
    if ("playlist" %in% names(post$embed)) {
      found <- TRUE
    }
    # case 5: not Found, blocked or detached (not considered)
    if (
      "record" %in%
        names(post$embed) &&
        length(intersect(
          c("notFound", "blocked", "detached"),
          names(post$embed$record)
        )) >
          0
    ) {
      found <- TRUE
    }
    if (
      "record" %in%
        names(post$embed) &&
        length(intersect(
          c("notFound", "blocked", "detached"),
          names(post$embed$record$record)
        )) >
          0
    ) {
      found <- TRUE
    }

    if (!found) {
      jsonlite::write_json(post, sprintf("%s/badpost.json", conf$data_dir))
      warning(sprintf(
        ":-) 2 Embed information expected but not found in %s",
        jsonlite::toJSON(post, auto_inbox = TRUE)
      ))
    }
    text <- paste(sapply(quoted_texts, `[[`, "text"), collapse = "\n")
    lang = paste(
      substr(mode(sapply(quoted_texts, `[[`, "lang")), 1, 2),
      collapse = "\n"
    )
    list(
      text = if (is.null(text) || nchar(text) == 0) NULL else text,
      lang = if (is.null(lang) || nchar(lang) == 0) NULL else lang,
      urls = unique(urls),
      is_quote = is_quote
    )
  } else {
    list(
      is_quote = FALSE
    )
  }
}

bluesky_parse_features <- function(post) {
  # extracting features data from post
  ret <- list(tags = list(), urls = list(), categories = list())
  if ("record" %in% names(post) && "facets" %in% names(post$record)) {
    for (facet in post$record$facets) {
      if ("features" %in% names(facet)) {
        for (feature in facet$features) {
          ftype <- feature[["$type"]]
          if (ftype == "app.bsky.richtext.facet#tag") {
            ret$tags <- c(ret$tags, feature[["tag"]])
          } else if (ftype == "app.bsky.richtext.facet#link") {
            ret$urls <- c(ret$urls, feature[["uri"]])
          } else if (
            ftype %in%
              c(
                "app.bsky.richtext.facet#mention",
                "app.bsky.richtext.facet#format",
                "app.twitter.source#id",
                "blue.poll.post.facet#option"
              )
          ) {
            #pass
          } else {
            jsonlite::write_json(
              post,
              sprintf("%s/badpost.json", conf$data_dir)
            )
            stop(sprintf(
              ":-) Unexpected feature type %s in %s",
              ftype,
              jsonlite::toJSON(post, auto_unbox = TRUE)
            ))
          }
        }
      }
    }
  }
  ret
}

#' Get bearer token
#'
#' @param handle bluesky handle
#' @param password bluesky password
#' @param login_url bluesky login URL
#'
#' @return List with access_jwt and did
#' @export
#' @importFrom httr2 request req_body_json req_perform resp_check_status resp_body_json is_online req_headers
#' @details You can find more information about the bluesky API here: \url{https://docs.bsky.app/docs/api/com-atproto-server-create-session}
#' @rdname session
#' @examples
#' \dontrun{
#' bluesky_create_session()
#' }
#'
bluesky_create_session <- function(handle = NULL, password = NULL) {
  if (is.null(handle) && is_secret_set("bluesky_user")) {
    handle = get_secret("bluesky_user")
  } else if (is.null(handle) && Sys.getenv("bluesky_id") != "")
    handle = Sys.getenv("bluesky_id") else {
    stop("You have to provide a user_name to collect Bluesky posts")
  }

  if (is.null(password) && is_secret_set("bluesky_password")) {
    password = get_secret("bluesky_password")
  } else if (is.null(handle) && Sys.getenv("bluesky_pwd") != "")
    password = Sys.getenv("bluesky_pwd") else {
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
  set_secret("bluesky_session", json_session)
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


