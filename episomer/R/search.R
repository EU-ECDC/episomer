#' @title Runs the search loop
#' @description Infinite loop ensuring the permanent collection of posts
#' @param data_dir optional path to the 'data directory' containing application settings, models and collected posts. If not provided it will reuse the last set on the current session.
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, Default: NA
#' @param sandboxed logical indicating if the search loop should be run in sandboxed mode. If TRUE, the search loop will not commit posts to the database. Default: FALSE
#' @param log_to_file logical indicating if the search loop should log to a file. Default: TRUE
#' @param max_requests integer indicating the maximum number of requests to perform. If 0, the search loop will run indefinitely. Default: 0
#' @return Nothing
#' @details The detect loop is a pure R function designed for downloading posts from the social media search API. It can handle several topics ensuring that all of them will be downloaded fairly using a
#' round-robin philosophy and respecting social media API rate-limits.
#'
#' The progress of this task is reported on the 'topics.json' file which is read or created by this function. This function will try to collect posts respecting a 'collect_span' window
#' in minutes, which is defined on the Shiny app and defaults to 60 minutes.
#'
#' To see more details about the collection algorithm please see episomer vignette.
#'
#' In order to work, this task needs Bluessky credentials, which can be set on the Shiny app or using \code{\link{set_auth}}
#'
#' @examples
#' if(FALSE){
#'    #Running the search loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    search_loop(file.choose())
#' }
#' @rdname search_loop
#' @seealso
#' \code{\link{set_auth}}
#' @export
search_loop <- function(
  data_dir = NA,
  sandboxed = FALSE,
  log_to_file = TRUE,
  max_requests = 0
) {
  # Setting or reusing the data directory
  if (is.na(data_dir)) {
    setup_config_if_not_already()
  } else {
    setup_config(data_dir = data_dir)
  }
  if (!sandboxed) {
    # Registering the search runner using current PID and ensuring no other instance of the search is actually running.
    register_search_runner()
  }

  if(log_to_file) {
      jobs_dir <- file.path(conf$data_dir, "jobs")
      if (!file.exists(jobs_dir)) {
        dir.create(jobs_dir, showWarnings = FALSE)
      }
  }
  sms <- active_social_media()
  cores <- as.numeric(length(sms))
  cl <- parallel::makePSOCKcluster(cores, outfile="")
  on.exit(parallel::stopCluster(cl))
  data_dir <- conf$data_dir
  parallel::clusterExport(cl, list("log_to_file", "data_dir", "sandboxed"), envir=rlang::current_env())
  
  # running search loops
  parallel::parLapply(cl, sms, function(sm) {
      # redirecting output to the sm file
      if(log_to_file) {
          jobs_dir <- file.path(data_dir, "jobs")
          outcon <- file(file.path(jobs_dir, sprintf("search.%s.log", sm)), open = "a")
          sink(outcon)
          sink(outcon, type = "message")
      }
      episomer::setup_config(data_dir)
      m <- paste("Running search agent for ",sm, Sys.time())
      message(m)
      episomer::search_loop_worker(sm, data_dir, sandboxed, max_requests)
  })
  stop("All search loop proccesses ended, which is unexpected")
  
}

#' @export
#' @rdname search_loop
#' @param network The social media network to search on.
#' @param data_dir The data directory to use.
#' @param sandboxed Whether to run in sandboxed mode.
#' @param max_requests The maximum number of requests to perform.
#' @return Nothing
#' @details The search loop worker is a function that runs the search loop for a given social media network.
#' It is used to run the search loop in a separate process.
search_loop_worker <- function(
  network,
  data_dir = NA,
  sandboxed = FALSE,
  max_requests = 0
) {
  message(sprintf("Starting runner for %s", network))
  # Setting or reusing the data directory
  if (is.na(data_dir)) {
    setup_config_if_not_already()
  } else {
    setup_config(data_dir = data_dir)
  }

  if (!sandboxed) {
    # Registering the search runner using current PID and ensuring no other instance of the search is actually running.
    register_search_runner(network)
  }
  # Infinite loop for getting posts if it is successfully registered as the search runner
  req2Commit <- 0
  last_check <- Sys.time()
  last_save <- Sys.time()
  first_call <- Sys.time()
  requests_done <- 0

  while (max_requests == 0 || requests_done <= max_requests) {
    loop_start <- Sys.time()

    # Waiting until database system will be running
    if (!sandboxed) {
      while (!is_fs_running()) {
        msg("episomer database is not yet running waiting for 5 seconds")
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
    # Creating plans for each topic if collect span is expired and calculating next execution time for each olan.
    for (i in 1:length(conf$topics)) {
      conf$topics[[i]]$plan <- update_plans_schedule(
        network = network,
        plans = conf$topics[[i]]$plan,
        schedule_span = conf$collect_span
      )
    }

    # Calculating how the time episomer should wait before executing each active plan. If bigger than zero then episomer will wait.
    # If waiting happens here, it means that episomer is able to collect all posts under current social media rate limits, so it could collect more topics or sooner.
    if (!sandboxed) {
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
          "seconds. Consider reducing the schedule_span for getting posts sooner"
        ))
        commit_posts()
        save_config(
          data_dir = conf$data_dir,
          sm_topics = list(network),
          properties = FALSE
        )
        Sys.sleep(wait_for)
      }
    }

    min_requests <- .Machine$integer.max
    #getting the min request on current non ended plans
    for (i in 1:length(conf$topics)) {
      for (j in 1:length(conf$topics[[i]]$plan)) {
        p = conf$topics[[i]]$plan[[j]]
        if (is.null(p$end_on) && p$requests < min_requests)
          min_requests <- p$requests
      }
    }

    if (!sandboxed) {
      #updating series to aggregate
      register_series()
    }

    #msg(paste("iterating in topics", length(conf$topics), min_requests))
    #performing search only for plans with a minimum number of requests (round robin)

    for (i in 1:length(conf$topics)) {
      for (j in 1:length(conf$topics[[i]]$plan)) {
        if (max_requests == 0 || requests_done <= max_requests) {
          plan <- conf$topics[[i]]$plan[[j]]
          if (
            plan$requests <= min_requests &&
              is.null(plan$end_on) &&
              plan$network == network
          ) {
            requests_done <- requests_done + 1
            #if search is performed on the first plan and we get an too old error, we will retry without time limit
            tryCatch(
              {
                conf$topics[[i]]$plan[[j]] = search_topic(
                  plan = plan,
                  query = conf$topics[[i]]$query,
                  topic = conf$topics[[i]]$topic,
                  sandboxed = sandboxed
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
            if (!sandboxed) {
              if (req2Commit > 100) {
                commit_posts()
                req2Commit <- 0
              }
            }
          }
        }
      }
    }
    if (!sandboxed) {
      #msg("iteration end")
      #checking at most once per 10 minutes
      if (difftime(Sys.time(), last_check, units = "mins") > 10) {
        # episomer sanity check and sendig email in case of issued
        #msg("health checked")
        last_check <- Sys.time()
        health_check()
      }
    }
    #Updating config to take in consideration possible changes on topics or other settings (plans are saved before reloading config) at most once every 10 seconds
    if (difftime(Sys.time(), last_save, units = "secs") > 10) {
      last_save <- Sys.time()
      setup_config(data_dir = conf$data_dir, save_topics_first = list(network))
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

#' @noRd
search_topic <- function(
  plan,
  query,
  topic,
  sandboxed = FALSE
) {
  token <- sm_api_get_token(plan$network)
  info <- sm_plan_next_search_info(plan)

  msg(paste(
    "searching for topic",
    topic,
    info
  ))

  # doing the post search and storing the response object to obtain details on resp
  results <- sm_api_search(
    query = query,
    token = token,
    plan = plan
  )
  if (!sandboxed) {
    # Interpreting the content as JSON and storing the results on json (nested list with dataframes)
    # interpreting is necessary to know the number of obtained posts and the id of the oldest post found and to keep post collecting stats
    # Saving uninterpreted content as a gzip archive
    tries <- 3
    done <- FALSE
    while (!done) {
      tries <- tries - 1
      tryCatch(
        {
          post_result <- httr::POST(
            url = paste0(
              get_scala_posts_url(),
              "?topic=",
              curl::curl_escape(topic),
              "&network=",
              plan$network,
              "&geolocate=true"
            ),
            httr::content_type_json(),
            body = jsonlite::toJSON(
              results$posts,
              auto_unbox = T,
              null = "null"
            ),
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
              500
            ))
            stop()
          }
          done = TRUE
        },
        error = function(e) {
          msg(paste("Error found while sending posts", e))
          if (tries < 0) {
            stop("too many retries")
          }
        }
      )
    }
  }
  update_plan_after_request(plan, results)
}


# Helper function to parse social media date as provided by the social media API
parse_date <- function(str_date) {
  curLocale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", curLocale))
  Sys.setlocale("LC_TIME", "C")
  strptime(str_date, format = "%a %b %d %H:%M:%S +0000 %Y", tz = "UTC")
}


# Calculating how long in seconds should episomer wait before executing one of the plans in the list which would be the case only if all plans are finished before the end of the search span
can_wait_for <- function(plans) {
  plans <- if (is.list(plans)) plans else list(plans)
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

# Last search time on stored in the embedded database
last_search_time <- function() {
  last_fs_updates(c("posts"))$posts
}

# prints message with date
msg <- function(m) {
  message(paste0(Sys.time(), " [INFO]: -------> ", m))
}
