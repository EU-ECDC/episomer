# This script includes all dedicated logic for interacting with the Spark/Scala API via HTTP REST requests

get_scala_api_endpoint <- function() {
  if(is.null(conf$fs_remote)) {
    paste(conf$fs_url,":", conf$fs_port, sep = "")
  } else {
    gsub("/$", "", conf$fs_remote)
  }
}

get_scala_geotraining_url <- function() {
  paste(get_scala_api_endpoint(), "geotraining-set", sep = "/")
}

get_scala_alert_training_url <- function() {
  paste(get_scala_api_endpoint(), "evaluate-alerts", sep = "/")
}

get_scala_posts_url <- function() {
  paste(get_scala_api_endpoint(), "posts", sep = "/")
}

get_scala_geolocated_posts_url <- function() {
  paste(get_scala_api_endpoint(), "geolocated-posts", sep = "/")
}

get_scala_commit_url <- function() {
  paste(get_scala_api_endpoint(), "commit", sep = "/")
}


get_scala_geolocate_text_url <- function() {
  paste(get_scala_api_endpoint(), "geolocate-text", sep = "/")
}

get_scala_aggregate_url <- function() {
  paste(get_scala_api_endpoint(), "aggregate", sep = "/")
}

get_scala_period_url <- function() {
  paste(get_scala_api_endpoint(), "period", sep = "/")
}

get_scala_status_url <- function() {
  paste(get_scala_api_endpoint(), "period?serie=country_counts", sep = "/")
}

get_scala_ping_url <- function() {
  paste(get_scala_api_endpoint(), "ping", sep = "/")
}

get_scala_recalc_hash_url <- function() {
  paste(get_scala_api_endpoint(), "recalculate-hash", sep = "/")
} 

get_scala_file_url <- function(...) {
    paste(get_scala_api_endpoint(), "file", ... , sep = "/")
}

get_scala_secrets_url <- function() {
    paste(get_scala_api_endpoint(), "secret", sep = "/")
}

get_scala_stopwords_url <- function(code) {
  paste(get_scala_api_endpoint(), sprintf("stopWords?code=%s", code), sep = "/")
}

epifile.exists <- function(path) {
    sapply(path, function(p) {
      if(!is_remote(p)) 
        file.exists(p)
      else
        jsonlite::read_json(sprintf("%s?action=exists", p))
    })
}

epidir.create <- function(path, recursive = FALSE, showWarnings = FALSE) {
    if(!is_remote(path)) 
        dir.create(path, recursive = recursive, showWarnings = showWarnings)
    else
         httr2::request(sprintf("%s?action=createDir&createDirs=%s&ignoreIfExists=true", path, tolower(recursive))) %>% 
            httr2::req_method("POST") %>%
            httr2::req_perform() 
}
epimd5sum <- function(path, recursive = FALSE) {
    sapply(path, function(p) {
      if(!is_remote(p)) 
        unname(md5sum(p))
      else
        jsonlite::read_json(sprintf("%s?action=md5sum", p))
    })
}

epiread_excel <- function(path, sheet = NULL, range = NULL, col_names = TRUE,
    col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
    guess_max = min(1000, n_max), .name_repair = "unique") {
    if(!is_remote(path)) 
        readxl::read_excel(path = path, sheet = sheet, range = range, col_names = col_names,
        col_types = col_types, na = na, trim_ws = trim_ws, skip = skip,
        n_max = n_max, guess_max = guess_max, .name_repair = .name_repair)
    else {
        tf <- tempfile(pattern="episomer")
        download.file(path, tf, mode="wb")
        ret <- readxl::read_excel(path = tf, sheet = sheet, range = range, col_names = col_names,
        col_types = col_types, na = na, trim_ws = trim_ws, skip = skip,
        n_max = n_max, guess_max = guess_max, .name_repair = .name_repair)
        file.remove(tf)
        ret
    }
}

epifile.upload <- function(localfile, dest, append = FALSE) {
    httr2::request(sprintf("%s?append=%s",dest, tolower(append))) %>% httr2::req_body_multipart(data = curl::form_file(localfile)) %>% httr2::req_perform()
}

epifile.touch <- function(path) {
  if(!is_remote(path)) {
    if(!file.exists(path)) {
      write("", file = path, append = FALSE)
    }
    Sys.setFileTime(path, Sys.time())
  } else {
    httr2::request(sprintf("%s?action=touch",path)) %>% httr2::req_method("POST") %>% httr2::req_perform()
  }
}
# Moves a file, within the local disk or within the fs machine.
#
# Used by the runner leases to release a lease: the lease file is moved out of runners/ instead of
# being deleted (the fs API has no delete action) so that "the file is absent" keeps its single
# meaning of "this runner is free" - see inst/schema/multi_services_plan.md section 3.4.
# A remote to remote move is one POST, so nothing travels through this process.
epifile.move <- function(from, to) {
    if(!is_remote(from) && !is_remote(to))
        file.rename(from, to)
    else if (is_remote(from) && is_remote(to))
        httr2::request(sprintf("%s?action=move", from)) %>%
            httr2::req_method("POST") %>%
            # the API resolves the destination against its own epiHome and refuses anything
            # absolute or containing "..", hence the relative form rather than the full URL
            httr2::req_body_json(data = to_remote_dest(to), auto_unbox = TRUE) %>%
            httr2::req_perform()
    else
        # a move with one foot on each machine would be a download or an upload followed by a
        # delete, and the fs API cannot delete: no caller needs it, so it is refused loudly
        stop("epifile.move only supports moving within the same file system (both local or both remote)")
}

epifile.copy <- function(from, to, overwrite = TRUE) {
    if(!is_remote(from) && !is_remote(to))
        file.copy(from, to, overwrite = overwrite)
    else if (is_remote(from) && is_remote(to))
        # same destination format as epifile.move above: the API expects a path relative to its
        # own epiHome, and used to be given the full URL of `to`, which it resolved to nonsense.
        # This branch has no call site today (the Shiny download handlers copy to a local
        # destination, alert.R and geo-tagging.R upload a local temp file), so the fix went
        # untested against a real server until a caller shows up.
        httr2::request(sprintf("%s?action=copy", from)) %>%
            httr2::req_method("POST") %>%
            httr2::req_body_json(data = to_remote_dest(to), auto_unbox = TRUE) %>%
            httr2::req_perform()
    else if (!is_remote(to)) {
        if(!file.exists(dirname(to))) 
            dir.create(dirname(to), recursive = TRUE)
        download.file(from, to, mode="wb")
    }
    else #!is_remote(from)
        epifile.upload(from, to)
}

epilist.files <- function(path, pattern = NULL, full.names = FALSE, recursive = FALSE) {
    if(!is_remote(path))
        list.files(path, pattern = pattern, full.names = full.names, recursive = recursive)
    else {
        files <- unlist(jsonlite::read_json(sprintf("%s?action=list&fullname=%s&recursive=%s", path, tolower(full.names), tolower(recursive))))
        if(!is.null(pattern))
             files <- files[grepl(pattern, files)]
        files
    }
}
# Last modification time of one or several paths, as a POSIXct vector (NA for a missing path).
# The runner leases rely on this function to decide whether a runner is still alive: the mtime is
# produced by the single machine hosting the fs, so no clock skew between runners can make a lease
# look fresh or expired when it is not (see inst/schema/multi_services_plan.md section 3.2).
epifile.mtime <- function(path) {
    do.call(c, lapply(path, function(p) {
      if(!is_remote(p))
        file.mtime(p)
      else {
        # `p`, not `path`: inside the lapply the existence test must be about the path being
        # visited, otherwise a vector of paths gets the answer of its first element only.
        if(!epifile.exists(p))
            # a typed NA, so that c()-ing the results keeps a POSIXct vector. A bare NA is logical
            # and would silently turn the whole vector into numbers.
            as.POSIXct(NA)
        else
            lubridate::as_datetime(jsonlite::read_json(sprintf("%s?action=mtime", p)))
      }
    }))
}

epilist.dirs <- function(path, full.names = TRUE) {
    if(!is_remote(path))
        list.dirs(path, recursive = FALSE, full.names = full.names)
    else
        unlist(jsonlite::read_json(sprintf("%s?action=listdir&fullname=%s", path, tolower(full.names))))
}

# Wrapper for jsonlite write_json ensuring atomic file write it replaces always the existing file. It ignores appends modifiers
write_json_atomic <- function(x, path, ...) {
  file_name <- tail(strsplit(path, "/|\\\\")[[1]], 1)
  if(!is_remote(path)) {
    dir_name <- substring(path, 1, nchar(path) - nchar(file_name) - 1)
    swap_file <- tempfile(
      pattern = paste("~", file_name, sep = ""),
      tmpdir = dir_name
    )
  } else {
    swap_file <- tempfile(
      pattern = paste("~", file_name, sep = ""),
    )
  }
  jsonlite::write_json(x = x, path = swap_file, ...)
  if(!is_remote(path)) 
    file.rename(swap_file, path)
  else {
    epifile.upload(swap_file, path)
    file.remove(swap_file) 
  }
}

fs_remote <- function() {
  !is.null(conf$fs_remote)
}

copy_secret_scripts <- function() {
  for(script_name in c("get_secret.R", "set_secret.R")) {
    script_base <- system.file(
      "extdata",
      script_name,
      package = get_package_name()
    )

    script_folder <- file.path(conf$data_dir, "jobs")
    script <- paste(script_folder, script_name, sep = "/")
    # Copying the script
    file.copy(from = script_base, to = script, overwrite = TRUE)
  }
}


#' @title Runs the episomer embedded database loop
#' @description Infinite loop ensuring that the episomer embedded database is running (Lucene + akka-http)
#' @param data_dir Path to the 'data directory' containing application settings, models and collected posts.
#' If not provided, the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, default: NA
#' @return nothing
#' @details Launches the episomer embedded database which is accessed via a REST API located on localhost at port 8080. You can test that the database is running by accesssing the local URL and adding "/ping". 
#' the REST API provide episomer a way to send and retrieve data related with posts and time series and to trigger geolocation or aggregation
#' The database is implemented using Apache Lucene indexes allowing episomer to access its data as a search engine but also as a tabular database.
#' \code{\link{health_check}} called each 60 seconds on a background process to send alerts to the administrator if some episomer components fail.
#' @examples
#' \dontrun{
#'    #Running the detect loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    fs_loop()
#' }
#' @rdname fs_loop
#' @seealso
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{search_loop}}
#'
#'  \code{\link{health_check}}
#' @importFrom future multisession plan future
#' @importFrom rlang current_env
#' @export
fs_loop <- function(data_dir = NA) {
  if (is.na(data_dir)) setup_config_if_not_already() else
    setup_config(data_dir = data_dir, fs_remote = NULL)
  data_dir <- conf$data_dir
  copy_secret_scripts()
  # calculating alerts per topic
  future::plan(future::multisession)
  monitor <- future::future(
    {
      message("Monitoring episomer")
      setup_config(data_dir = data_dir)
      x = 60
      step = 2
      while (TRUE) {
        register_fs_monitor()
        if (x == 0) {
          health_check()
          x = 60
        }
        x = x - step
        Sys.sleep(step)
      }
      0
    },
    globals = c(
      "data_dir",
      "setup_config",
      "health_check",
      "register_fs_monitor"
    ),
    envir = rlang::current_env()
  )

  message("Launching fs")
  # Registering the fs runner using current PID and ensuring no other instance of the search is actually running.
  register_fs_runner()
  # Giving the lease back when this loop ends, so that another machine can take the fs over
  # immediately instead of waiting for the whole TTL to expire (multi_services_plan.md 3.4).
  # release_runner_lease() checks it is still the holder before moving anything, so an instance
  # that silently lost its lease cannot take away the new holder's file.
  on.exit(release_runner_lease(), add = TRUE)

  # Infinite loop calling the fs runner
  while (TRUE) {
    tryCatch(
      {
        spark_job(
          paste(
            "fsService",
            "epiHome",
            conf$data_dir
          )
        )
      },
      error = function(e) {
        message(paste(
          "The episomer Scala API was stopped with the following error",
          e,
          "launching it again within 5 seconds",
          sep = "\n"
        ))
        Sys.sleep(5)
      }
    )
  }
}

#' @title perform full text search on posts collected with episomer
#' @description  perform full text search on posts collected with episomer (posts migrated from episomer v<1.0.x are also included)
#' @param query character. The query to be used if a text it will match the post text. To see how to match particular fields please see details, default:NULL
#' @param topic character, Vector of topics to include on the search default:NULL
#' @param from an object which can be converted to ‘"POSIXlt"’ only posts posted after or on this date will be included, default:NULL
#' @param to an object which can be converted to ‘"POSIXlt"’ only posts posted before or on this date will be included, default:NULL
#' @param countries character or numeric, the position or name of episomer regions to be included on the query, default:NULL
#' @param mentioning character, limit the search to the posts mentioning the given users, default:NULL
#' @param users character, limit the search to the posts created by the provided users, default:NULL
#' @param hide_users logical, whether to hide user names on output replacing them by the USER keyword, default:FALSE
#' @param action character, an action to be performed on the search results respecting the max parameter. Possible values are 'delete' or 'anonymise' , default:NULL
#' @param max integer, maximum number of posts to be included on the search, default:100
#' @param by_relevance logical, whether to sort the results by relevance of the matching query or by indexing order, default:FALSE
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, default: NA
#' @return a data frame containing the posts matching the selected filters, the data frame contains the following columns: user_name, created_date, is_geo_located, is_quote, text, 
#' text_loc, user_id, hash, quote_lang, totalCount, created_at, topic_post_id, topic, lang, user_name, quoted_text, id, quopted_text_loc, tags 
#' @details
#' episomer translate the query provided by all parameters into a single query that will be executed on post indexes which are weekly indexes.
#' The q parameter should respect the syntax of the Lucene classic parser \url{https://lucene.apache.org/core/8_5_0/queryparser/org/apache/lucene/queryparser/classic/QueryParser.html}
#' So other than the provided parameters, multi field queries are supported by using the syntax field_name:value1;value2
#' AND, OR and -(for excluding terms) are supported on q parameter.
#' Order by week is always applied before relevance so even if you provide by_relevance = TRUE all of the matching posts of the first week will be returned first
#' @examples
#' \dontrun{
#'    #Running the detect loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    df <- search_posts(
#'         q = "vaccination",
#'         topic="COVID-19",
#'         countries=c("Chile", "Australia", "France"),
#'         from = Sys.Date(),
#'         to = Sys.Date()
#'    )
#'    df$text
#' }
#' @rdname search_posts
#' @seealso
#'  \code{\link{search_loop}}
#'
#'  \code{\link{detect_loop}}
#' @importFrom utils URLencode
#' @importFrom jsonlite stream_in
#' @export
search_posts <- function(
  query = NULL,
  topic = NULL,
  from = NULL,
  to = NULL,
  countries = NULL,
  mentioning = NULL,
  users = NULL,
  hide_users = FALSE,
  action = NULL,
  max = 100,
  by_relevance = FALSE
) {
  u <- paste(
    get_scala_posts_url(),
    "?jsonnl=true&estimatecount=true&by_relevance=",
    tolower(by_relevance),
    sep = ""
  )
  if (hide_users) {
    u <- paste(u, "&hide_users=true", sep = "")
  }
  if (!is.null(action)) {
    u <- paste(u, "&action=", action, sep = "")
  }
  if (!is.null(query)) {
    u <- paste(u, "&q=", utils::URLencode(query, reserved = T), sep = "")
  }
  if (!is.null(topic)) {
    u <- paste(
      u,
      "&topic=",
      utils::URLencode(paste(topic, sep = "", collapse = ";"), reserved = T),
      sep = "",
      collapse = ""
    )
  }
  if (!is.null(from)) {
    u <- paste(u, "&from=", strftime(from, format = "%Y-%m-%d"), sep = "")
  }
  if (!is.null(to)) {
    u <- paste(u, "&to=", strftime(to, format = "%Y-%m-%d"), sep = "")
  }
  if (
    !is.null(countries) &&
      length(countries) > 0 &&
      (is.character(countries) || any(countries != 1))
  ) {
    # getting all regions
    regions <- get_country_items()
    # If countries are names they have to be changes to region indexes
    if (is.character(countries)) {
      countries = (1:length(regions))[sapply(
        1:length(regions),
        function(i) regions[[i]]$name %in% countries
      )]
    }
    country_codes <- Reduce(
      function(l1, l2) {
        unique(c(l1, l2))
      },
      lapply(as.integer(countries), function(i) unlist(regions[[i]]$codes))
    )
    u <- paste(
      u,
      "&country_code=",
      paste(country_codes, collapse = ";", sep = ""),
      sep = "",
      collapse = ""
    )
  }
  if (!is.null(mentioning)) {
    u <- paste(
      u,
      "&mentioning=",
      paste(mentioning, sep = "", collapse = ";"),
      sep = ""
    )
  }
  if (!is.null(users)) {
    u <- paste(u, "&user=", paste(users, sep = "", collapse = ";"), sep = "")
  }

  if (!is.null(max)) {
    u <- paste(u, "&max=", max, sep = "")
  }
  #message(u)
  u <- url(u)
  posts <- jsonlite::stream_in(u, verbose = FALSE)
  posts
}


# Registers a query to define custom aggregations on posts
# it is generic enough to choose variables, aggregation and filters
# deals with post deduplication at topic level
# regexp: regexp to limit the geolocation and search files to read
# vars: variable to read (evaluated after aggregation if required)
# sort_by: order to apply to the returned data frame
# filter_by: expressions to use for filtering posts
# sources_exp: variables to limit the source files to read (setting this will improve reading performance)
# handler: function that to perform a custom R based transformation on data returned by SPARK
# params: definition of custom param files to enable big queries
set_aggregated_posts <- function(
  name,
  dateCol,
  pks,
  aggr,
  vars = list("*"),
  group_by = list(),
  sort_by = list(),
  filter_by = list(),
  sources_exp = list(),
  params = list()
) {
  stop_if_no_config(paste("Cannot get posts without configuration setup"))
  post_result <- httr::POST(
    url = get_scala_aggregate_url(),
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(
        name = name,
        dateCol = dateCol,
        pks = pks,
        aggr = aggr,
        aggregation = list(
          columns = vars,
          groupBy = group_by,
          sortBy = sort_by,
          filterBy = filter_by,
          sourceExpressions = sources_exp,
          params = if (length(params) == 0) list(fake_param_epi = "") else
            params
        )
      ),
      simplify_vector = T,
      auto_unbox = T
    ),
    encode = "raw",
    encoding = "UTF-8"
  )
  if (httr::status_code(post_result) != 200) {
    print(substring(
      httr::content(post_result, "text", encoding = "UTF-8"),
      1,
      100
    ))
    stop()
  }
}


# Utility function to interact with a POST endpoint and process data on a streaming way provided by handler
stream_post <- function(uri, body, handler = NULL) {
  cum <- new.env()
  cum$tail <- c()
  cum$dfs <- list()

  if (!is.null(handler)) {
    # If a custom transformation is going to be done (a handler function has been set)
    # this function will be called on pages of 10k lines using the jsonlite stream_in function
    # the transformed results will be stored on a temporary file that will be read by stream_in
    tmp_file <- tempfile(pattern = "episomer", fileext = ".json")
    #message(tmp_file)
    con_tmp <- file(tmp_file, open = "w", encoding = "UTF-8")
  }

  pipeconnection <- function(x, handler = handler) {
    cuts <- which(x == 0x0a)
    if (length(cuts) == 0) cum$tail <- c(cum$tail, x) else {
      if (tail(cuts, n = 1) != length(x)) {
        bytes <- c(cum$tail, x[1:tail(cuts, n = 1)])
        cum$tail <- x[(tail(cuts, n = 1) + 1):length(x)]
      } else {
        bytes <- c(cum$tail, x)
        cum$tail <- c()
      }
      con <- rawConnection(bytes, "rb")
      on.exit(close(con))
      if (is.null(handler)) {
        cum$dfs[[length(cum$dfs) + 1]] <- jsonlite::stream_in(
          con,
          verbose = FALSE
        )
      } else
        jsonlite::stream_in(
          con,
          function(df) handler(df, con_tmp),
          verbose = FALSE
        )
    }
  }
  # Doing the post request
  parts = gregexpr("//[^/]+/", uri)
  split <- parts[[1]][[1]] + attr(parts[[1]], "match.length")
  #h <- httr::handle(substr(uri, 1, split-1))
  #path <- substr(uri, split, nchar(uri))
  #on.exit(httr::handle_reset(h))

  post_result <- tryCatch(
    {
      httr::POST(
        url = uri,
        httr::write_stream(function(x) {
          pipeconnection(x, handler)
        }),
        body = body,
        httr::content_type_json(),
        encode = "raw",
        encoding = "UTF-8"
      )
    },
    error = function(e) {
      closeAllConnections()
      stop(e)
    }
  )
  if (httr::status_code(post_result) != 200) {
    #message(uri)
    message(httr::status_code(post_result))
    message(httr::content(post_result, "text", encoding = "UTF-8"))
    stop()
  }

  #Applying last line if necessary
  if (length(cum$tail) > 0) {
    if (cum$tail[[length(cum$tail)]] != 0x0a)
      cum$tail[[length(cum$tail) + 1]] <- as.raw(0x0a)
    con <- rawConnection(cum$tail, "rb")
    if (is.null(handler)) {
      cum$dfs[[length(cum$dfs) + 1]] <- jsonlite::stream_in(
        con,
        verbose = FALSE
      )
    } else
      cum$dfs[[length(cum$dfs) + 1]] <- jsonlite::stream_in(
        con,
        function(df) handler(df, con_tmp),
        verbose = F
      )
    close(con)
  }
  #Transforming single lines responses in data frames
  #lapply(cum$dfs, function(l) if(typeof(l) == "list") data.frame(lapply(l, function(x) t(data.frame(x)))) else l)
  #joining response
  if (is.null(handler)) {
    jsonlite::rbind_pages(cum$dfs)
  } else {
    close(con_tmp)
    con_tmp <- file(tmp_file, open = "r", encoding = "UTF-8")
    ret <- jsonlite::stream_in(con_tmp, pagesize = 10000, verbose = FALSE)
    close(con_tmp)
    unlink(tmp_file)
    ret
  }
}

# Get time difference since last request
# This function is used from the Shiny app to report when was the last time that a request saved posts
# This is done by taking the last modified date of current year post files
last_fs_updates <- function(
  collections = c("posts", "topwords", "country_counts", "geolocated")
) {
  times <- lapply(collections, function(collection) {
    time <- epifile.mtime(epipath("fs", collection))
    if(!is.null(time)){
      time
    } else {
      NA
    }
  })
  setNames(times, collections)
}

commit_posts <- function() {
  post_result <- httr::POST(
    url = get_scala_commit_url(),
    httr::content_type_json(),
    encode = "raw",
    encoding = "UTF-8"
  )
  if (httr::status_code(post_result) != 200) {
    stop(print(substring(
      httr::content(post_result, "text", encoding = "UTF-8"),
      1,
      100
    )))
  }
}
