#' @title Runs the search loop
#' @description Infinite loop ensuring the permanent collection of tweets
#' @param data_dir optional path to the 'data directory' containing application settings, models and collected tweets. If not provided it will reuse the last set on the current session.
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, Default: NA
#' @return Nothing
#' @details The detect loop is a pure R function designed for downloading tweets from the Twitter search API. It can handle several topics ensuring that all of them will be downloaded fairly using a
#' round-robin philosophy and respecting Twitter API rate-limits.
#'
#' The progress of this task is reported on the 'topics.json' file which is read or created by this function. This function will try to collect tweets respecting a 'collect_span' window
#' in minutes, which is defined on the Shiny app and defaults to 60 minutes.
#'
#' To see more details about the collection algorithm please see epitweetr vignette.
#'
#' In order to work, this task needs Twitter credentials, which can be set on the Shiny app or using \code{\link{set_twitter_app_auth}}
#'
#' @examples
#' if(FALSE){
#'    #Running the search loop
#'    library(epitweetr)
#'    message('Please choose the epitweetr data directory')
#'    search_loop(file.choose())
#' }
#' @rdname search_loop
#' @seealso
#' \code{\link{set_twitter_app_auth}}
#' @export
search_loop <- function(
  network,
  data_dir = NA,
  dev_in_progress = TRUE,
  conf,
  kill_after = NULL
) {
  # Setting or reusing the data directory
  if (is.na(data_dir)) {
    setup_config_if_not_already()
  } else {
    setup_config(data_dir = data_dir)
  }

  if (!dev_in_progress) {
    # Registering the search runner using current PID and ensuring no other instance of the search is actually running.
    register_search_runner()
  }
  # Infinite loop for getting tweets if it is successfully registered as the search runner
  req2Commit <- 0
  last_check <- Sys.time()
  last_save <- Sys.time()
  first_call <- Sys.time()
  while (TRUE) {
    loop_start <- Sys.time()

    # Waiting until database system will be running
    if (!dev_in_progress) {
      while (!is_fs_running()) {
        msg("Epitweetr database is not yet running waiting for 5 seconds")
        Sys.sleep(5)
      }
      # Dismissing history if required from shiny app
      if (conf$dismiss_past_request > conf$dismiss_past_done) {
        if (
          length(conf$topics[sapply(conf$topics, function(t) {
            length(t$plan) == 0 || t$plan[[1]]$requests == 0
          })]) >
            0
        ) {
          msg(
            "Dismissing history has to wait until all plans have at least one plan with one request"
          )
        } else {
          msg("Dismissing history requested")

          for (i in 1:length(conf$topics)) {
            conf$topics[[i]]$plan <- finish_plans(plans = conf$topics[[i]]$plan)
          }
          conf$dismiss_past_done <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
        }
      }
    }
    # On each iteration this loop will perform one request for each active plans having the minimum number of requests
    # Creating plans for each topic if collect span is expired and calculating next execution time for each plan.
    for (i in 1:length(conf$topics)) {
      conf$topics[[i]]$plan <- update_plans(
        plans = conf$topics[[i]]$plan,
        schedule_span = conf$collect_span
      )
    }

    # Calculating how the time epitweetr should wait before executing each active plan. If bigger than zero then epitweetr will wait.
    # If waiting happens here, it means that epitweetr is able to collect all tweets under current twitter rate limits, so it could collect more topics or sooner.
    if (!dev_in_progress) {
      wait_for <- min(unlist(lapply(1:length(conf$topics), function(i) {
        can_wait_for(plans = conf$topics[[i]]$plan)
      })))
      if (wait_for > 0) {
        msg(paste(
          Sys.time(),
          ": All done! going to sleep for until",
          Sys.time() + wait_for,
          "during",
          wait_for,
          "seconds. Consider reducing the schedule_span for getting tweets sooner"
        ))
        commit_tweets()
        save_config(data_dir = conf$data_dir, topics = TRUE, properties = FALSE)
        Sys.sleep(wait_for)
      }
    }

    #getting only the next plan to execute for each topic (it could be a previous unfinished plan)
    next_plans <- lapply(1:length(conf$topics), function(i) {
      next_plan(plans = conf$topics[[i]]$plan)
    })

    #calculating the minimum number of requests those plans have already executed
    min_requests <- Reduce(
      min,
      lapply(1:length(conf$topics), function(i) {
        if (!is.null(next_plans[[i]])) {
          next_plans[[i]]$request
        } else {
          .Machine$integer.max
        }
      })
    )

    if (!dev_in_progress) {
      #updating series to aggregate
      register_series()
    }
    #msg(paste("iterating in topics", length(conf$topics), min_requests))
    #performing search only for plans with a minimum number of requests (round robin)
    requests_done <- 0
    for (i in 1:length(conf$topics)) {
      for (j in 1:length(conf$topics[[i]]$plan)) {
        if (!is.null(kill_after)) {
          if (difftime(Sys.time(), first_call, units = "secs") > kill_after) {
            # For unit tests
            stop("Killing search loop after ", kill_after, " seconds")
          }
        }
        plan <- conf$topics[[i]]$plan[[j]]
        # For debugging
        # jsonlite::write_json(
        #   plan,
        #   paste(format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), "plan.json")
        # )
        if (
          plan$requests <= min_requests &&
            (is.null(plan$end_on) || plan$requests == 0)
        ) {
          requests_done <- requests_done + 1
          #if search is performed on the first plan and we get an too old error, we will retry without time limit
          tryCatch(
            {
              conf$topics[[i]]$plan[[j]] = search_topic(
                plan = plan,
                query = conf$topics[[i]]$query,
                topic = conf$topics[[i]]$topic,
                conf = conf
              )
            },
            error = function(e) {
              if (j == 1 && e$message == "too-old") {
                msg("Recovering from too-old request")
                plan$since_id <- NULL
                plan$since_target <- NULL
                conf$topics[[i]]$plan[[j]] = search_topic(
                  plan = plan,
                  query = conf$topics[[i]]$query,
                  topic = conf$topics[[i]]$topic
                )
              } else if (e$message == "too-old") {
                msg("Canceling too-old request")
                conf$topics[[i]]$plan[[j]]$end_on <- Sys.time()
              } else {
                stop(paste(e$message, e))
              }
            }
          )
          req2Commit <- req2Commit + 1
          if (!dev_in_progress) {
            if (req2Commit > 100) {
              commit_tweets()
              req2Commit <- 0
            }
          }
        }
      }
    }
    if (!dev_in_progress) {
      #msg("iteration end")
      #checking at most once per 10 minutes
      if (difftime(Sys.time(), last_check, units = "mins") > 10) {
        # epitweetr sanity check and sendig email in case of issued
        #msg("health checked")
        last_check <- Sys.time()
        health_check()
      }

      #Updating config to take in consideration possible changes on topics or other settings (plans are saved before reloading config) at most once every 10 seconds
      if (difftime(Sys.time(), last_save, units = "secs") > 10) {
        last_save <- Sys.time()
        setup_config(data_dir = conf$data_dir, save_first = list("topics"))
        #msg("config saved and refreshed")
      }

      if (requests_done == 0) {
        message(
          "No requests performed on loop.... something may be wrong, sleeping 1 second"
        )
        Sys.sleep(1)
      }
    }
  }
}

#' @noRd
search_topic <- function(
  plan,
  query,
  topic,
  output_in_scala = FALSE,
  conf
) {
  network <- plan$network

  token <- get(sprintf("%s_get_token", network))()
  # Set date boundaries for the search
  boundaries <- get(sprintf("%s_set_date_boundaries", network))(plan)
  plan <- boundaries$plan
  max_text <- boundaries$max_text
  min_text <- boundaries$min_text

  msg(paste(
    "searching for topic",
    topic,
    "from",
    min_text,
    "until",
    max_text
  ))

  # Tweets are stored on the following folder structure data_folder/tweets/search/topic/year
  # Ensuring that folders for storing tweets are created
  year <- format(Sys.time(), "%Y")
  create_dirs(network, topic, year, conf) #, conf

  # Tweets are stored as gzipped files with the following naming: "YYYY.MM.DD.counter.json.gz"
  # Counter starts with 00001 and it is increased after the last file grows over 100M
  # getting prefix and regular expression for tweet archive name
  file_prefix <- paste(format(Sys.time(), "%Y.%m.%d"))
  file_pattern <- paste(format(Sys.time(), "%Y\\.%m\\.%d"))
  # TODO: update folder name
  dir <- file.path(conf$data_dir, network, "search", topic, year)

  # files will contain all files matching the naming pattern the last alphabetically is going to be measured to evaluate if a new file has to be started
  files <- sort(list.files(path = dir, pattern = file_prefix))

  # file_name will contain the name of the gz file to add
  file_name <- (if (length(files) == 0) {
    # default case for first file
    paste(
      file_prefix,
      formatC(1, width = 5, format = "d", flag = "0"),
      "json.gz",
      sep = "."
    )
  } else {
    #If last file matching pattern is smaller than 100MB we keep adding to the same file else a new incremented file is created
    last <- files[[length(files)]]
    if (file.info(paste(dir, last, sep = "/"))$size / (1024 * 1024) < 100) {
      last
    } else {
      #Try to get current index after date as integer and increasing it by one, if not possible a 00001 index will be added
      parts <- strsplit(gsub(".json.gz", "", last), split = "\\.")[[
        1
      ]]
      if (
        length(parts) <= 3 ||
          is.na(as.integer(parts[[length(parts)]]))
      ) {
        paste(
          c(
            parts,
            formatC(1, width = 5, format = "d", flag = "0"),
            "json.gz"
          ),
          collapse = "."
        )
      } else {
        paste(
          c(
            parts[1:length(parts) - 1],
            formatC(
              as.integer(parts[[length(parts)]]) + 1,
              width = 5,
              format = "d",
              flag = "0"
            ),
            "json.gz"
          ),
          collapse = "."
        )
      }
    }
  })

  # putting all parts together to get current file name
  # TODO: update folder name
  dest <- file.path(
    conf$data_dir,
    network,
    "search",
    topic,
    year,
    file_name
  )

  # Ensuring that query is smaller than 400 character (Twitter API limit)
  #if (nchar(query) < 400) {
  # doing the tweet search and storing the response object to obtain details on resp
  network_search <- get(sprintf("%s_search", network))
  content <- network_search(
    query = query,
    token = token,
    plan = plan
  )
  json <- content$results

  if (output_in_scala) {
    # Interpreting the content as JSON and storing the results on json (nested list with dataframes)
    # interpreting is necessary to know the number of obtained tweets and the id of the oldest tweet found and to keep tweet collecting stats
    # Saving uninterpreted content as a gzip archive
    tries <- 3
    done <- FALSE
    while (!done) {
      tries <- tries - 1
      tryCatch(
        {
          post_result <- httr::POST(
            url = paste0(
              get_scala_tweets_url(),
              "?topic=",
              curl::curl_escape(topic),
              "&geolocate=true"
            ),
            httr::content_type_json(),
            body = content$results,
            encode = "raw",
            encoding = "UTF-8",
            httr::timeout((4 - tries) * 5)
          )
          if (httr::status_code(post_result) != 200) {
            print(substring(
              httr::content(
                post_result,
                "text",
                encoding = "UTF-8"
              ),
              1,
              100
            ))
            stop()
          }
          done = TRUE
        },
        error = function(e) {
          msg(paste("Error found while sending tweets", e))
          if (tries < 0) {
            stop("too many retries")
          }
        }
      )
    }
  }

  # evaluating if rows are obtained
  got_rows <- get(sprintf("%s_got_rows", network))(content)
  if (got_rows) {
    year <- format(Sys.time(), "%Y")
    date_min_max <- get(sprintf("%s_get_json_date_min_max", network))(content)
    # If rows were obtained we update the stat file that will stored the posted date period of each gz archive.
    # This is used to improve aggregating performance, by targeting only the files containing tweets for a particular date
    update_file_stats(
      filename = gsub(".gz", "", file_name),
      topic = topic,
      year = year,
      first_date = date_min_max$first_date,
      last_date = date_min_max$last_date,
      conf
    )
    plan <- get(sprintf("%s_update_plan", network))(plan, got_rows, content)
  } else {
    plan <- get(sprintf("%s_no_rows_logic", network))(plan)
  }
  # else {
  # Managing the case when the query is too long
  #     warning(
  #         paste("Query too long for API for topic", topic),
  #         immediate. = TRUE
  #     )
  #     plan$requests = plan$requests
  #     return(plan)
  # }
  #}
  # Update plan boundaries based on search results
  plan
}


# Updating statistic files
# This function is called after each successful tweet request
# stat json files are stored on data_dir/year/xyz.gz where xyz is the name of a search gzip archive
# stat files contains an array per topic indicating the posted period on that the collected files with the same name
# these files are used to improve aggregating performance, by targeting only the files containing tweets for a particular date
# filename: gzip file to update stats
# topic: topic to update stats
# year: current year to separate the stat files per year
# first_date: oldest date on the tweets collected this will replace the stat if it is older than the current oldest date for the topic on the given file
# last_date: newest date on the tweets collected this will replace the stat if it is newer than the current newest date for the topic on the given file
update_file_stats <- function(
  filename,
  topic,
  year,
  first_date,
  last_date,
  conf
) {
  # getting the stat destination file
  stat_dir <- file.path(conf$data_dir, "stats")
  if (!file.exists(stat_dir)) {
    dir.create(stat_dir)
  }
  stat_dir <- file.path(stat_dir, year)
  if (!file.exists(stat_dir)) {
    dir.create(stat_dir)
  }
  dest <- file.path(stat_dir, filename)
  now <- Sys.time()
  #Setting UTC so it can be compares with twitter created dates
  attr(now, "tzone") <- "UTC"

  # reading current statistics if they exist
  stats <-
    if (!file.exists(dest)) {
      list()
    } else {
      jsonlite::read_json(dest, simplifyVector = FALSE, auto_unbox = TRUE)
    }

  # matching record or creating new one
  found <- FALSE
  # updating stat file if it is found
  if (length(stats) > 0) {
    for (i in 1:length(stats)) {
      if (stats[[i]]$topic == topic) {
        found <- TRUE
        stats[[i]]$collected_to <- now
        if (stats[[i]]$created_from > first_date) {
          stats[[i]]$created_from <- first_date
        }
        if (stats[[i]]$created_to < last_date) {
          stats[[i]]$created_to <- last_date
        }
        break
      }
    }
  }
  # creating new statistics if not found
  if (!found) {
    stats[[length(stats) + 1]] <- list(
      topic = topic,
      created_from = first_date,
      created_to = last_date,
      collected_from = now,
      collected_to = now
    )
  }

  # saving modified JSON file
  write_json_atomic(stats, dest, pretty = TRUE, force = TRUE, auto_unbox = TRUE)
}

# Helper function to parse Twitter date as provided by the Twitter API
parse_date <- function(str_date) {
  curLocale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", curLocale))
  Sys.setlocale("LC_TIME", "C")
  strptime(str_date, format = "%a %b %d %H:%M:%S +0000 %Y", tz = "UTC")
}

# Perform a single page search on twitter API
# This function will build the twitter search URL for performing a search request on the standard search
# https://developer.twitter.com/en/docs/twitter-api/v1/tweets/search/api-reference/get-search-tweets
# function called from the search_topic function
# q: text query
# since_id: id of the oldest targeted tweet
# max_id: id of the newest targeted tweet
# result_type: sort criteria for tweets (recent, popular and mix)
# twitter_search <- function(
#   q,
#   since_id = NULL,
#   max_id = NULL,
#   result_type = "recent",
#   count = 100
# ) {
#   search_url = list()
#   #usont v1 endpoint when delegated authentication if user set to use it
#   if (!is_secret_set("app") || "1.1" %in% conf$api_version) {
#     search_url[["1.1"]] <- paste(
#       search_endpoint[["1.1"]],
#       "?q=",
#       URLencode(q, reserved = T),
#       if (!is.null(since_id)) "&since_id=" else "",
#       if (!is.null(since_id)) since_id else "",
#       if (!is.null(max_id)) "&max_id=" else "",
#       if (!is.null(max_id)) max_id else "",
#       "&result_type=",
#       result_type,
#       "&count=",
#       count,
#       "&include_entities=true",
#       sep = ""
#     )
#   }
#   if (is_secret_set("app") && "2" %in% conf$api_version) {
#     search_url[["2"]] <- paste(
#       search_endpoint[["2"]],
#       "?query=",
#       URLencode(gsub(" AND ", " ", q), reserved = T),
#       if (!is.null(since_id)) "&since_id=" else "",
#       if (!is.null(since_id)) since_id - 1 else "",
#       if (!is.null(max_id)) "&until_id=" else "",
#       if (!is.null(max_id)) max_id + 1 else "",
#       "&max_results=",
#       count,
#       "&expansions=author_id,geo.place_id,referenced_tweets.id,referenced_tweets.id.author_id",
#       "&place.fields=country,country_code,full_name,name,place_type",
#       "&tweet.fields=author_id,context_annotations,entities,created_at,geo,id,in_reply_to_user_id,lang,possibly_sensitive,referenced_tweets,source,text", #,geo.coordinates
#       "&user.fields=description,id,location,name,username",
#       sep = ""
#     )
#   }
#   res <- twitter_get(search_url)
#   return(res)
# }

# Calculating how long in seconds should epitweetr wait before executing one of the plans in the list which would be the case only if all plans are finished before the end of the search span
can_wait_for <- function(plans) {
  plans <- if ("get_plan" %in% class(plans)) list(plans) else plans
  non_ended <- plans[sapply(plans, function(x) is.null(x$end_on))]
  if (length(non_ended) == 0) {
    expected_end <- Reduce(min, lapply(plans, function(x) x$expected_end))
    return(ceiling(as.numeric(difftime(
      expected_end,
      Sys.time(),
      units = "secs"
    ))))
  } else {
    return(0)
  }
}


# create topic directories if they do not exist
create_dirs <- function(network, topic = NA, year = NA, conf) {
  if (!file.exists(paste(conf$data_dir, sep = "/"))) {
    dir.create(paste(conf$data_dir, sep = "/"), showWarnings = FALSE)
  }
  if (!file.exists(paste(conf$data_dir, network, sep = "/"))) {
    dir.create(paste(conf$data_dir, network, sep = "/"), showWarnings = FALSE)
  }
  if (!file.exists(paste(conf$data_dir, network, "search", sep = "/"))) {
    dir.create(
      paste(conf$data_dir, network, "search", sep = "/"),
      showWarnings = FALSE
    )
  }
  if (!is.na(topic) && !is.na(year)) {
    if (
      !file.exists(paste(conf$data_dir, network, "search", topic, sep = "/"))
    ) {
      dir.create(
        paste(conf$data_dir, network, "search", topic, sep = "/"),
        showWarnings = FALSE
      )
    }
    if (
      !file.exists(paste(
        conf$data_dir,
        network,
        "search",
        topic,
        year,
        sep = "/"
      ))
    ) {
      dir.create(
        paste(conf$data_dir, network, "search", topic, year, sep = "/"),
        showWarnings = FALSE
      )
    }
  }
}

# Last search time on stored in the embedded database
last_search_time <- function() {
  last_fs_updates(c("tweets"))$tweets
}

# prints message with date
msg <- function(m) {
  message(paste0(Sys.time(), " [INFO]: -------> ", m))
}
