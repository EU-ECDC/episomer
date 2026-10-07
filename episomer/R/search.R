#' @title Runs the search loop
#' @description Infinite loop ensuring the permanent collection of social media messages
#' @param data_dir optional path to the 'data directory' containing application settings, models and collected posts. If not provided it will reuse the last set on the current session.
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, Default: NA
#' @param sandboxed logical indicating if the search loop should be run in sandboxed mode. If TRUE, the search loop will not send or commit posts to the database. Default: FALSE
#' @param log_to_file logical indicating if the search loop should log to a file, one per collected
#' network (jobs/search.<network>.log). When FALSE every worker writes to the standard output of
#' this process instead, which is what a container deployment needs - nothing collects a file
#' inside a pod. Default: FALSE when the environment variable 'EPISOMER_LOGS_TO_STDOUT' is set to
#' TRUE, TRUE otherwise
#' @param max_requests integer indicating the maximum number of requests to perform. If 0, the search loop will run indefinitely. Default: 0
#' @return Nothing
#' @details The detect loop is a pure R function designed for downloading posts from the social media search API. It can handle several topics ensuring that all of them will be downloaded fairly using a
#' round-robin philosophy and respecting social media API rate-limits.
#'
#' The progress of this task is reported on the 'topics.*.json' files which are created by this function. This function will try to collect posts respecting a 'collect_span' window
#' in minutes, which is defined on the Shiny app and defaults to 60 minutes.
#'
#' To see more details about the collection algorithm please see episomer vignette.
#'
#' In order to work, this task needs Bluesky and/or Mastodon credentials,
#'  which can be set on the Shiny app or using \code{\link{sm_api_set_auth_bluesky}} or
#' \code{\link{sm_api_set_auth_mastodon}}
#'
#' @examples
#' \dontrun{
#'    #Running the search loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    search_loop(file.choose())
#' }
#' @rdname search_loop
#' @seealso
#' \code{\link{sm_api_set_auth_bluesky}}
#' \code{\link{sm_api_set_auth_mastodon}}
#' @export
search_loop <- function(
  data_dir = NA,
  sandboxed = FALSE,
  log_to_file = !logs_to_stdout(),
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
    #
    # wait = TRUE: a service does not ask once and give up (plan issue 14). The lease this finds
    # held is most often the one the previous container left behind on a `docker compose down`,
    # which released nothing because no R code runs on the way down (issue 13); waiting means the
    # service starts by itself as soon as that lease expires, instead of dying with an error that
    # nothing is left to retry.
    register_search_runner(wait = TRUE)
    # Releasing the parent "search" lease and the PID file on exit. Not strictly required by the
    # plan (which only lists the three loops that do the work), but the reason is the same: without
    # it, restarting the search service means waiting for the TTL before it may collect again.
    on.exit(unregister_runner(), add = TRUE)
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
  on.exit(parallel::stopCluster(cl), add = TRUE)
  data_dir <- conf$data_dir
  parallel::clusterExport(cl, list("log_to_file", "data_dir", "sandboxed"), envir=rlang::current_env())
  
  # running search loops
  # parLapply() only returns when every worker has returned, which normally never happens: the
  # workers run infinite loops. It does happen when a worker gives its network up because another
  # instance took its lease - search_loop_worker() then returns "lease-lost" instead of raising -
  # so the outcomes are collected and explained below rather than thrown away.
  outcomes <- parallel::parLapply(cl, sms, function(sm) {
      # redirecting output to the sm file
      if(log_to_file) {
          jobs_dir <- file.path(data_dir, "jobs")
          outcon <- file(file.path(jobs_dir, sprintf("search.%s.log", sm)), open = "a")
          sink(outcon)
          sink(outcon, type = "message")
          # Undoing the redirection is not cosmetic: a worker leaving without it hands its PSOCK
          # node back to the cluster with both sinks still pointing at a network log file, so
          # anything the node is asked to do next writes there instead of where it should.
          on.exit(
            {
              sink(NULL, type = "message")
              sink(NULL)
              close(outcon)
            },
            add = TRUE
          )
      }
      episomer::setup_config(data_dir)
      m <- paste("Running search agent for ",sm, Sys.time())
      message(m)
      # A worker that raises has its error carried back to this parent process by `parallel`, which
      # only hands it over once *every* node has returned - and the other networks are still
      # looping, so that is never. The error text was simply lost, and the log of the dead network
      # stopped mid-collection with nothing to say why. Catching it here writes it down while the
      # sinks above still point at that network's log, and turns it into an outcome
      # report_search_loop_outcomes() can name.
      tryCatch(
        episomer::search_loop_worker(sm, data_dir, sandboxed, max_requests),
        error = function(e) {
          message(sprintf(
            "%s [ERROR]: the search worker for %s stopped: %s",
            Sys.time(),
            sm,
            conditionMessage(e)
          ))
          paste("error:", conditionMessage(e))
        }
      )
  })
  report_search_loop_outcomes(sms, outcomes)
}

# Explains why the search loop ended, once all its workers have returned.
#
# Every worker logs into its own file (jobs/search.<network>.log), so without this the parent
# process - the one an operator or a supervisor actually watches - reported the end of the service
# with a single "which is unexpected", whatever the reason. A worker that stepped aside because
# another machine took its lease is not an incident: it is the standby mechanism working.
#
# Hence two outcomes:
#
#   * every worker lost its lease  -> this machine has simply been replaced, log it and return
#     normally so that a supervisor does not restart in a loop against a healthy other instance;
#   * anything else                -> the historical error, now naming the networks and what each
#     of them returned.
report_search_loop_outcomes <- function(networks, outcomes) {
  lost <- vapply(
    outcomes,
    function(outcome) identical(outcome, "lease-lost"),
    logical(1)
  )
  if (length(lost) > 0 && all(lost)) {
    msg(paste(
      "All search workers stopped because their lease was taken by another instance:",
      paste(networks, collapse = ", "),
      "- this machine is not the search runner anymore, stopping the search loop."
    ))
    return(invisible("lease-lost"))
  }
  if (any(lost)) {
    msg(paste(
      "Search workers that stopped on a lost lease:",
      paste(networks[lost], collapse = ", ")
    ))
  }
  stop(paste(
    "All search loop proccesses ended, which is unexpected -",
    paste(
      networks,
      vapply(outcomes, function(o) paste(format(o), collapse = " "), character(1)),
      sep = ": ",
      collapse = ", "
    )
  ))
}

#' @export
#' @rdname search_loop
#' @param network The social media network to search on.
#' @param data_dir The data directory to use.
#' @param sandboxed Whether to run in sandboxed mode.
#' @param max_requests The maximum number of requests to perform.
#' @return Invisibly, why the worker stopped: \code{"lease-lost"} when another instance took this
#' network over, \code{"ended"} when \code{max_requests} was reached. Read by \code{search_loop()}
#' to tell an orderly handover from an unexpected end. A failed request does not stop the worker,
#' it is logged and tried again; only \code{search_max_request_failures()} consecutive failures do,
#' and that one raises rather than returning.
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
    #
    # wait = TRUE for the same reason as in search_loop(), and it matters more here: this is the
    # lease a restarted `search` container really has to wait out, because its own PID file died
    # with the container (only jobs/ is mounted) and the fast reclaim of section 3.6 therefore
    # fails closed. The worker sits in wait_for_runner_lease() until the TTL runs out, then
    # collects - no error, no dead container (plan issue 14).
    register_search_runner(network, wait = TRUE)
    # This is the lease that really matters: exclusivity is per network ("search.bluesky",
    # "search.mastodon.piaille.fr"), because that is where two machines would push the same posts
    # twice. Releasing it lets the standby machine take this network over on its next restart, and
    # dropping the PID file with it keeps the local layer from claiming a runner that has stopped.
    on.exit(unregister_runner(), add = TRUE)
  }
  # Infinite loop for getting posts if it is successfully registered as the search runner
  req2Commit <- 0
  # The fs buffers the posts it indexes until commit_posts(), and the ones not committed when it stops
  # are lost. A request count alone is too slow for a topic that gets few posts, so
  # whichever comes first decides: more than 100 requests, or search_commit_interval() seconds.
  last_commit <- Sys.time()
  # Consecutive failed requests, whatever the topic they were made for: a network that answers is
  # a network that answers for every topic, so one counter for the worker rather than one per plan.
  # Reset by any request that goes through, read by search_max_request_failures().
  request_failures <- 0
  last_check <- Sys.time()
  last_save <- Sys.time()
  first_call <- Sys.time()
  requests_done <- 0

  # The whole loop runs under the handler for episomer_lease_lost: every stop_if_lease_lost()
  # below - and the ones raised from inside search_topic(), including from the rate-limit waits
  # buried in req_perform() - lands here and becomes a clean exit with a log line, instead of an
  # error crashing the worker process (R/runners-lease.R).
  #
  # The on.exit(unregister_runner()) above still runs, and that is fine: it refuses to release a
  # lease that is not ours anymore, which is exactly the situation here, and only drops the local
  # PID file, which does describe this process.
  outcome <- run_until_lease_lost({
    while (max_requests == 0 || requests_done <= max_requests) {
      loop_start <- Sys.time()

      # The entries of conf$topics this worker collects, recomputed on every turn because the
      # periodic setup_config() below reloads them - a topic added to the Excel file appears here.
      #
      # conf$topics holds every network's topics, so this is what keeps a worker to its own
      # (R/config.R): without it each worker collected all of them, since the plan it creates for
      # an entry with no plan yet carries its *own* network and then passes the plan$network test of
      # the round robin below. Every loop over topics in this function goes through these indexes.
      my_topics <- topics_of_network(network)
      if (length(my_topics) == 0) {
        # Nothing to collect: the network is activated but no topic has been translated for it yet.
        # Sleeping rather than spinning, with a heartbeat so the lease survives it, and reloading
        # the config before looping - this branch skips the periodic reload at the bottom, so
        # without it a worker that started too early would never see the topics appear.
        msg(paste("No topic to collect on", network, "- waiting 10 seconds"))
        sleep_with_heartbeat(10)
        setup_config(data_dir = conf$data_dir)
        next
      }

      # Proving this runner is still alive, once per turn of the loop. This is the heartbeat of the
      # normal case, when everything goes fast and the loop never sleeps: the renewals of the waits
      # below only happen when there is a wait. if_due, so an iteration that takes a second costs
      # nothing (R/runners-lease.R).
      renew_runner_lease_if_due()

      # Waiting until database system will be running
      if (!sandboxed) {
        while (!is_fs_running()) {
          # Unbounded wait: the fs may be down for much longer than the search TTL, and this runner
          # must not lose its lease while it is simply waiting for the database to come back.
          msg("episomer database is not yet running waiting for 5 seconds")
          sleep_with_heartbeat(5)
        }
        # Dismissing history if required from shiny app
        if (conf$dismiss_past_request > conf$dismiss_past_done) {
          if (
            length(conf$topics[my_topics][sapply(
              conf$topics[my_topics],
              function(t) {
                length(t$plan) == 0 || t$plan[[1]]$requests == 0
              }
            )]) >
              0
          ) {
            msg(
              "Dismissing history has to wait until all plans have at least one plan with one request"
            )
          } else {
            msg("Dismissing history requested")

            for (i in my_topics) {
              conf$topics[[i]]$plan <- finish_plans(plans = conf$topics[[i]]$plan)
            }
            conf$dismiss_past_done <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
          }
        }
      }

      # On each iteration this loop will perform one request for each active plans having the minimum number of requests
      # Creating plans for each topic if collect span is expired and calculating next execution time for each olan.
      for (i in my_topics) {
        conf$topics[[i]]$plan <- update_plans_schedule(
          network = network,
          plans = conf$topics[[i]]$plan,
          schedule_span = conf$collect_span
        )
      }

      # Calculating how the time episomer should wait before executing each active plan. If bigger than zero then episomer will wait.
      # If waiting happens here, it means that episomer is able to collect all posts under current social media rate limits, so it could collect more topics or sooner.
      if (!sandboxed) {
        wait_for <- min(unlist(lapply(my_topics, function(i) {
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
          # Guard point: this is a write, so it must not happen if another instance took this
          # network over while we were collecting. commit_posts() flushes the posts buffered by the
          # fs and save_config() rewrites topics.<network>.json, i.e. the collection bookmarks - two
          # runners writing those alternately would make each of them re-collect what the other
          # already had. Raises episomer_lease_lost, caught at the top of the loop
          # (R/runners-lease.R).
          stop_if_lease_lost()
          commit_posts()
          req2Commit <- 0
          last_commit <- Sys.time()
          save_config(
            data_dir = conf$data_dir,
            sm_topics = list(network),
            properties = FALSE
          )
          # The longest silence of the whole loop, and the reason sleep_with_heartbeat() exists:
          # wait_for is bounded only by collect_span (60 minutes by default) against a 300-second
          # lease TTL, so a plain Sys.sleep() here would hand this network over to a standby machine
          # while the collection is perfectly healthy (plan section 3.3).
          sleep_with_heartbeat(wait_for)
        }
      }

      min_requests <- .Machine$integer.max
      #getting the min request on current non ended plans
      for (i in my_topics) {
        for (j in seq_along(conf$topics[[i]]$plan)) {
          p = conf$topics[[i]]$plan[[j]]
          if (is.null(p$end_on) && p$requests < min_requests)
            min_requests <- p$requests
        }
      }

      if (!sandboxed) {
        #updating series to aggregate
        # Guard point: register_series() POSTs the aggregation definitions to the fs
        # (set_aggregated_posts(), R/fs.R), which triggers the recomputation of every series. It is
        # the "aggregation trigger" of the plan (section 6.3); the geolocation one is not a separate
        # call but the `geolocate=true` of the post upload in search_topic(), guarded there.
        stop_if_lease_lost()
        register_series()
      }

      #msg(paste("iterating in topics", length(conf$topics), min_requests))
      #performing search only for plans with a minimum number of requests (round robin)
      message(sprintf("--------------- SEARCH LOOP (%s topics on %s)----------------------------------------------------", length(my_topics), network))
      for (i in my_topics) {
        for (j in seq_along(conf$topics[[i]]$plan)) {
          if (max_requests == 0 || requests_done <= max_requests) {
            plan <- conf$topics[[i]]$plan[[j]]
            if (
              plan$requests <= min_requests &&
                is.null(plan$end_on) &&
                plan$network == network
            ) {
              # One turn of the outer loop is not one request: with many topics, a full round robin
              # pass can take longer than the search TTL without ever sleeping. if_due makes this
              # free (one lookup in an environment) except once every heartbeat span.
              renew_runner_lease_if_due()
              requests_done <- requests_done + 1
              #if search is performed on the first plan and we get an too old error, we will retry without time limit
              #message(sprintf("This is plan %s, doing request %s out of %s", j, plan$requests + 1,length(conf$topics[[i]]$plan) ))
              attempt <- try_search_topic(
                plan = plan,
                query = conf$topics[[i]]$query,
                topic = conf$topics[[i]]$topic,
                sandboxed = sandboxed,
                is_first_plan = (j == 1)
              )
              if (identical(attempt$status, "done")) {
                conf$topics[[i]]$plan[[j]] <- attempt$plan
                request_failures <- 0
              } else if (identical(attempt$status, "too-old")) {
                # Not a failure: the API simply has nothing left that old, so this plan is over.
                msg("Canceling too-old request")
                conf$topics[[i]]$plan[[j]]$end_on <- Sys.time()
                request_failures <- 0
              } else {
                # A failed request used to end the worker on the spot, and with it the collection of
                # this whole network - a single 500 from the social media API, or a DNS hiccup, was
                # enough. It is now absorbed: the plan is left untouched (its `requests` counter has
                # not moved either), so the round robin comes back to it after the backoff below.
                #
                # This is also the only place the error text is ever written down. The worker runs
                # inside a PSOCK node, whose errors are handed back to the parent process and only
                # read once *every* node has returned - which never happens, the other networks are
                # still looping. Logging here is what turns "the log of this network stops mid-line"
                # into something an operator can act on.
                request_failures <- request_failures + 1
                msg(sprintf(
                  "Request failed for topic %s on %s (%s consecutive failure(s) out of %s allowed): %s",
                  conf$topics[[i]]$topic,
                  network,
                  request_failures,
                  search_max_request_failures(),
                  conditionMessage(attempt$error)
                ))
                if (request_failures >= search_max_request_failures()) {
                  stop(sprintf(
                    "%s consecutive requests failed on %s, giving this network up. Last error: %s",
                    request_failures,
                    network,
                    conditionMessage(attempt$error)
                  ))
                }
                search_retry_wait(request_failures)
              }
              req2Commit <- req2Commit + 1
              if (!sandboxed) {
                if (
                  req2Commit > 100 ||
                    difftime(Sys.time(), last_commit, units = "secs") >=
                      search_commit_interval()
                ) {
                  # Guard point: same reasoning as the periodic save above, at the other place where
                  # this loop flushes what it collected. Checked here rather than on every request -
                  # the expensive guard is the one in search_topic(), which protects the posts
                  # themselves.
                  stop_if_lease_lost()
                  # setting a timestamp for indicating proper run of search loop
                  search_touch()
                  # commiting posts
                  commit_posts()
                  req2Commit <- 0
                  last_commit <- Sys.time()
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
    # Only reached with max_requests set: the loop is infinite otherwise. Returned so that
    # search_loop() can tell this from the "lease-lost" of a worker that stepped aside.
    "ended"
  })
  invisible(outcome)
}

# Runs one request of one plan and says what happened, without letting a failure end the collection
# of the whole network.
#
# Three outcomes, all returned rather than raised:
#
#   list(status = "done",    plan  = <the plan updated by the request>)
#   list(status = "too-old", error = <condition>)   the API has nothing that old left; plan is over
#   list(status = "failed",  error = <condition>)   anything else; the caller retries it later
#
# episomer_lease_lost is deliberately *not* turned into a "failed": it does not say that a request
# went wrong, it says that this instance is not the runner of this network anymore. It has to reach
# run_until_lease_lost() at the top of the loop with its class intact - the historical handler here
# rebuilt a plain error from its text, which made an orderly handover look like a crash
# (R/runners-lease.R). A retried lost lease would be worse still: this process must stop writing,
# not try again.
try_search_topic <- function(plan, query, topic, sandboxed, is_first_plan) {
  request <- function(plan) {
    list(
      status = "done",
      plan = search_topic(
        plan = plan,
        query = query,
        topic = topic,
        sandboxed = sandboxed
      )
    )
  }
  classify <- function(e) {
    if (inherits(e, "episomer_lease_lost")) {
      stop(e)
    }
    if (identical(conditionMessage(e), "too-old")) {
      list(status = "too-old", error = e)
    } else {
      list(status = "failed", error = e)
    }
  }
  attempt <- tryCatch(request(plan), error = classify)
  # The first plan of a topic is the one walking back in time: when the API tells it that its
  # window is too old, dropping the lower bound and asking again is what lets it resume from what
  # is still served, instead of ending the plan for good. The retry runs with the same `sandboxed`
  # as the first attempt - it used to be left to its default, so a sandboxed run recovering from a
  # too-old request wrote to the database after all.
  if (!identical(attempt$status, "too-old") || !is_first_plan) {
    return(attempt)
  }
  msg("Recovering from too-old request")
  plan$since_id <- NULL
  plan$since_target <- NULL
  tryCatch(request(plan), error = classify)
}

# How many consecutive failed requests a search worker tolerates before giving its network up.
#
# "Retry for ever" is not the right answer: a wrong credential or a query the API rejects fails the
# same way every time, and a worker that keeps retrying keeps renewing its lease - the service
# would look perfectly healthy while collecting nothing. Giving up releases the lease, which is
# precisely what makes the network appear in missing_search_jobs() and in the services status table
# of the Shiny app.
#
# Ten failures with the backoff below means roughly twenty minutes of trying before that happens,
# which is far more than any transient failure lasts and far less than a working day of silence.
# A function rather than a constant so that a test can shorten it.
search_max_request_failures <- function() {
  10
}

# Waits before trying a failed request again: exponential, capped at five minutes.
#
# Two reasons for the cap and for the growth. An API that just answered 500 does not want to be
# asked again immediately, and the wait must stay short enough for a collection to resume by itself
# once the other end is back. The waiting is done by sleep_with_heartbeat(), so even the longest
# backoff keeps proving the runner is alive and stops at once if another instance took over
# (R/runners-lease.R).
# Seconds after which the posts buffered by the fs are committed even if 100 requests were not made
# since the last commit. A function rather than a constant so that a test can change it.
search_commit_interval <- function() {
  300
}

search_retry_wait <- function(failures) {
  wait <- min(5 * 2^(failures - 1), 300)
  msg(paste("Waiting", wait, "seconds before trying this request again"))
  sleep_with_heartbeat(wait)
  invisible(wait)
}

#printtime <- function(t) {}
#' updates the modificatin date of a flag file indicating that messages have been sent to the embedded database.
search_touch <- function() {
  epifile.touch(get_search_touch_path())
}

#' performs a search following the given plan, query and topic.
#' @param plan list, the plan that defines the timespan to search for
#' @param query char, the query to use when searching for the topic
#' @param topic char, the topic associated to this search
#' @param sandboxed logical, whether to un in sandboxed mode which prevent sending data to the embedded database.
search_topic <- function(
  plan,
  query,
  topic,
  sandboxed = FALSE
) {

  info <- sm_plan_next_search_info(plan)
  msg(paste(
    "searching for topic",
    topic,
    info
  ))
  
  token <- sm_api_get_token(plan$network)
  # doing the post search and storing the response object to obtain details on resp
  
  results <- sm_api_search(
    query = query,
    token = token,
    plan = plan
  )
  if (!sandboxed) {
    # The guard that protects the data rather than the bookkeeping (plan section 6.3). Two search
    # loops running the same topic do not merely fight over topics.<network>.json: they push the
    # same posts twice, and each upload also triggers the geolocation (`geolocate=true` below).
    #
    # Placed after the social media request and before the upload on purpose: the request itself is
    # harmless - it costs one call of somebody else's rate limit - while everything after this line
    # writes. It is also the latest possible moment, and the lease may well have been taken *during*
    # the request, which can wait minutes on a rate limit.
    stop_if_lease_lost()
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
              sm_family(plan$network), # We use the family to deal with multi-instances network (e.g Mastodon, we want to merge all the instances into a single SM)
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
  unlist(last_fs_updates(c("posts"))$posts)
}

# prints message with date
msg <- function(m) {
  message(paste0(Sys.time(), " [INFO]: -------> ", m))
}
