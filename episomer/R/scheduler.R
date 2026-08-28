

#' @title Get the \code{\link{detect_loop}} task status
#' @description Reads the status of the \code{\link{detect_loop}} tasks and update it with changes requested by the Shiny app
#' @param statuses Character vector for limiting the status of the returned tasks, default: list()
#' @return A named list containing all necessary information to run and monitor the detect loop tasks.
#' @details After reading the tasks.json file and parsing it with jsonlite, this function will update the necessary fields in the
#' tasks for executing and monitoring them.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    tasks <- get_tasks()
#' }
#' @seealso
#'  \code{\link{download_dependencies}}
#'
#'  \code{\link{update_geonames}}
#'
#'  \code{\link{update_languages}}
#'
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{generate_alerts}}
#'
#' @rdname get_tasks
#' @export
#' @importFrom jsonlite fromJSON
get_tasks <- function(statuses = list()) {
  stop_if_no_config()
  # get the task path which should be data_folder/tasks.json
  # this file is only written by the detect loop process and read by all (shiny, search)
  tasks_path <- get_tasks_path()

  #Loading tasks from file if exists
  tasks <- if (epifile.exists(tasks_path)) {
    #if tasks files exists getting tasks from disk
    t <- tryCatch(
      jsonlite::fromJSON(tasks_path, simplifyVector = FALSE, auto_unbox = TRUE),
      error = function(e) {
        message("recovering from error on reading tasks")
        Sys.sleep(5)
        jsonlite::fromJSON(
          tasks_path,
          simplifyVector = FALSE,
          auto_unbox = TRUE
        )
      }
    )
    # casting date time values (they are interpreted as character by jsonlite)
    for (i in 1:length(t)) {
      t[[i]]$scheduled_for <- if (inherits(t[[i]]$scheduled_for, "numeric"))
        as.POSIXlt(t[[i]]$scheduled_for / 1000, origin = "1970-01-01") else NA
      t[[i]]$started_on <- if (inherits(t[[i]]$started_on, "numeric"))
        as.POSIXlt(t[[i]]$started_on / 1000, origin = "1970-01-01") else NA
      t[[i]]$end_on <- if (inherits(t[[i]]$end_on, "numeric"))
        as.POSIXlt(t[[i]]$end_on / 1000, origin = "1970-01-01") else NA
      t[[i]]$status <- if (inherits(t[[i]]$status, "character"))
        t[[i]]$status else NA
    }
    t
  } else {
    # If no tasks file is setup a default is built based on configuration
    list()
  }
  # Setting default alerts task status if not set
  if (!exists("alerts", where = tasks)) {
    #alerts
    tasks$alerts <- list(
      task = "alerts",
      order = 1,
      started_on = NA,
      end_on = NA,
      status = NA,
      scheduled_for = NA
    )
  } else if (exists("alerts", where = tasks) && tasks$alerts$order != 1) {
    #dealing with bad order case after migration
    tasks$alerts$order = 1
  }
  tasks
}

# Getting the scheduler task lists and plan execution by setting the scheduled_for times
# pan If tasks file exits it will read it, or get default tasks otherwise
plan_tasks <- function(statuses = list()) {
  tasks <- get_tasks(statuses)
  # Updating next execution time for each task
  now <- Sys.time()

  #sorting tasks by normal execution order
  sorted_tasks <- order(sapply(tasks, function(l) l$order))
  change <- FALSE
  for (i in sorted_tasks) {
    # Scheduling pending tasks one shot tasks (not recurrent) they will simply be executed in order when pending
    if (tasks[[i]]$task %in% c("alerts")) {
      # dealing with recurrent tasks first if take in consideration the case when a manual request for execution has been performed
      if (in_requested_status(tasks[[i]])) {
        tasks[[i]]$status <- "scheduled"
        tasks[[i]]$scheduled_for <- now
        tasks[[i]]$failures <- 0
        change <- TRUE
        break # Just the first requested task is executed
      } else {
        # In this case this is the next task to be executed on normal scheduling
        tasks[[i]]$status <- "scheduled"
        if (is.na(tasks[[i]]$end_on)) {
          # If taks has never ended then schedule it straight away
          tasks[[i]]$scheduled_for <- now + (i - 1) / 1000
        } else if (tasks[[i]]$end_on >= tasks[[i]]$scheduled_for) {
          #if the task has ended after the last schedule (which should always be the case)
          # the last schedule was already executed a new schedule has to be set
          day_slots <- get_task_day_slots(tasks[[i]]) #Getting all day slots (times when the detect loop can be executed
          next_slots <- day_slots[day_slots > tasks[[i]]$scheduled_for] #getting the future execution slots
          #if next slot is in future set it. If it is in past, set now
          tasks[[i]]$scheduled_for <- if (
            length(next_slots) > 0 && next_slots[[1]] > now
          )
            next_slots[[1]] else now + (i - 1) / 1000
        }
        change <- TRUE
        break # only first task is set to scheduled
      }
    }
  }
  # saving changes done on tasks
  if (change) save_tasks(tasks)

  # filtering the returned tasks by requested status
  if (length(statuses) == 0) tasks else
    tasks[sapply(tasks, function(t) t$status %in% statuses)]
}

# get detect loop day slots based on start hour
# detect loop recurrent tasks will only be executed on this times
get_task_day_slots <- function(task) {
  span <- conf$schedule_span
  start_hour <- conf$schedule_start_hour
  per_day <- as.integer(24 * 60 / span)
  # calculating possible next slot for
  hour_slots <- sort((c(0:(per_day - 1)) * (span / 60) + start_hour) %% 24)
  hour_slots <- c(hour_slots, hour_slots[[1]] + 24) #This adding first slot of next day
  day_slots <- hour_slots * 60 * 60 + (as.POSIXlt(as.Date(task$scheduled_for)))
  day_slots
}

# saves the provided task lists to the data directory.
save_tasks <- function(tasks) {
  stop_if_no_config()
  tasks_path <- get_tasks_path()
  write_json_atomic(
    tasks,
    tasks_path,
    pretty = TRUE,
    force = TRUE,
    auto_unbox = TRUE,
    POSIXt = "epoch"
  )
}

#' @title Runs the detect loop
#' @description Infinite loop ensuring the daily signal detection and email alerts
#' @param data_dir Path to the 'data directory' containing application settings, models and collected posts.
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, default: NA
#' @return nothing
#' @details The detect loop run the recurrent tasks \code{\link{generate_alerts}}
#'
#' The loop report progress on the 'tasks.json' file which is read or created by this function.
#'
#' The recurrent tasks are scheduled to be executed each 'detect span' minutes, which is a parameter set on the Shiny app.
#'
#' If any of these tasks fails it will be retried three times before going to abort status. Aborted tasks can be relaunched from the Shiny app.
#' @examples
#' \dontrun{
#'    #Running the detect loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    detect_loop()
#' }
#' @rdname detect_loop
#' @seealso
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{generate_alerts}}
#'
#'  \code{\link{get_tasks}}
#' @export
detect_loop <- function(data_dir = NA) {
  # ensuring that config is set or using the one provided as a parameter
  if (is.na(data_dir)) setup_config_if_not_already() else
    setup_config(data_dir = data_dir)

  # registering the detect runner (PID file + lease) or stopping if it is already running here or
  # on another machine
  register_detect_runner()
  # Same clean release as in fs_loop(): a detect lease has a long TTL (30 min, because a single
  # task can run for very long), so not releasing it would make a simple restart wait half an hour.
  on.exit(unregister_runner(), add = TRUE)

  # lass_sleeping_message set to ensure it will be shown if all is done
  last_sleeping_message <- Sys.time() - (conf$schedule_span * 60)
  # Same handler as in search_loop_worker(): the guard points below raise episomer_lease_lost,
  # and this turns it into a clean end of the detect service with an explicit log line rather
  # than an error. The on.exit(unregister_runner()) above refuses to release a lease that is not
  # ours anymore, so it stays as it is (R/runners-lease.R).
  run_until_lease_lost({
    while (TRUE) {
      # Proving the detect runner is still alive, once per turn of the loop (R/runners-lease.R).
      # It only covers the loop itself: a single task (alerts, geolocation, aggregation) can run for
      # a very long time without coming back here, which is exactly why the detect TTL is 30 minutes
      # and not 5 (plan section 3.5).
      renew_runner_lease_if_due()
      # getting tasks to execute with one scheduled
      # Guard point: plan_tasks() is a reader that also writes - it saves tasks.json as soon as it
      # changes a schedule (R/scheduler.R, `if (change) save_tasks(tasks)`). Guarding here rather
      # than inside save_tasks() itself, because the Shiny app calls save_tasks() too and holds no
      # lease at all (plan section 6.4): a guard down there would abort the app.
      stop_if_lease_lost()
      tasks <- plan_tasks()
      # seeing if there is something to do
      # either there is a scheduled task
      # or a task is in an inconsistency state (running) or failed (failed or aborted)
      # aborted tasks are being taken into consideration since they should stop further taks from being executed
      if (
        length(tasks[sapply(
          tasks,
          function(t)
            !is.na(t$status) &&
              (t$status %in%
                c("failed", "running", "aborted") ||
                (t$status %in% c("scheduled") && t$scheduled_for <= Sys.time()))
        )]) >
          0
      ) {
        # getting the position of the task to execute (first in one of the active status)
        i_next <- order(sapply(
          tasks,
          function(t)
            if (
              !is.na(t$status) &&
                t$status %in% c("aborted", "scheduled", "failed", "running")
            )
              t$order else 999
        ))[[1]]
        # proceeding if task is not already aborted (otherwise keep looping)
        if (tasks[[i_next]]$status != "aborted") {
          # increasing number of failures if failed of running
          if (tasks[[i_next]]$status %in% c("failed", "running")) {
            tasks[[i_next]]$failures = (if (
              !exists("failures", where = tasks[[i_next]])
            )
              1 else tasks[[i_next]]$failures + 1)
          }
          # setting to aborted if the max number of retries has been reached
          if (
            tasks[[i_next]]$status %in%
              c("failed", "running") &&
              tasks[[i_next]]$failures > 1
          ) {
            tasks[[i_next]]$status = "aborted"
            tasks[[i_next]]$message = paste(
              "Max number of retries reached",
              tasks[[i_next]]$message,
              sep = "\n"
            )
            # Guard point: giving up on a task is recorded in tasks.json, and the Shiny app reads it
            # to offer a manual relaunch. The new holder is running its own copy of this loop, so a
            # write from here would fight with it over the same file.
            stop_if_lease_lost()
            save_tasks(tasks)
            health_check(one_per_day = FALSE)
          } else {
            # if task is not aborted proceeding with execution
            message(paste(Sys.time(), ": Executing task", tasks[[i_next]]$task))
            # Guard point: the heaviest write of the detect loop. generate_alerts() detects the
            # alerts, POSTs them to the fs and mails the subscribers - having two instances do that
            # for the same period means duplicated alerts *and* duplicated emails. It is checked
            # before starting rather than inside, because the task is one long block that reports its
            # own progress through update_alerts_task(); the residual risk is a task started just
            # before the takeover, which the long detect TTL (30 min) is meant to make rare.
            stop_if_lease_lost()
            if (tasks[[i_next]]$task == "alerts") {
              tasks <- generate_alerts(tasks)
            }
            # resetting failures to zero if successfully executed
            if (tasks[[i_next]]$status == "success") tasks[[i_next]]$failures = 0

            # saving tasks
            # Guard point again: the task above can run for a very long time, so holding the lease
            # when it started says nothing about holding it now.
            stop_if_lease_lost()
            save_tasks(tasks)
          }
        }
      } else if (
        last_sleeping_message + (conf$schedule_span * 60) < Sys.time() &&
          length(tasks[sapply(
            tasks,
            function(t)
              !is.na(t$status) &&
                (t$status %in% c("scheduled") && t$scheduled_for >= Sys.time())
          )]) >
            0
      ) {
        #nothing to do (and not aborted tasks and 'nothing to do' message has not yet been shown for current detect slot
        i_next <- order(sapply(
          tasks,
          function(t)
            if (!is.na(t$status) && t$status %in% c("scheduled")) t$order else 999
        ))[[1]]
        message(paste(
          Sys.time(),
          ": Nothing else todo. Going to sleep each 5 seconds until ",
          format(tasks[[i_next]]$scheduled_for, tz = Sys.timezone(), usetz = TRUE)
        ))
        last_sleeping_message <- Sys.time()
      }
      # episomer sanity check and sending email in case of issues
      health_check()
      # updating config to capture changes coming from search loop or shiny app
      setup_config(data_dir = conf$data_dir)
      # sleep for 5 seconds, renewing the lease if the heartbeat span has passed. Five seconds is
      # shorter than any heartbeat span, so this is really the renewal at the top of the loop above
      # doing the work - what sleep_with_heartbeat() adds here is stop_if_lease_lost(), which makes
      # the loop notice a stolen lease while it is idle instead of on its next write.
      sleep_with_heartbeat(5)
    }
  })
  invisible(NULL)
}

# Check if provided task is in pending status which means that a user has requested a manual run and the task is not already running or scheduled
in_pending_status <- function(task) {
  ((is.na(task$status) ||
    task$status %in% c("pending", "success", "failure", "aborted")) &&
    ((task$task == "dependencies" &&
      !is.na(conf$dep_updated_on) &&
      (is.na(task$started_on) ||
        task$started_on <
          strptime(conf$dep_updated_on, "%Y-%m-%d %H:%M:%S"))) ||
      (task$task == "geonames" &&
        !is.na(conf$geonames_updated_on) &&
        (is.na(task$started_on) ||
          task$started_on <
            strptime(conf$geonames_updated_on, "%Y-%m-%d %H:%M:%S"))) ||
      (task$task == "languages" &&
        !is.na(conf$lang_updated_on) &&
        (is.na(task$started_on) ||
          task$started_on <
            strptime(conf$lang_updated_on, "%Y-%m-%d %H:%M:%S"))) ||
      (task$task == "alerts" &&
        !is.na(conf$alerts_requested_on) &&
        (is.na(task$started_on) ||
          task$started_on <
            strptime(conf$alerts_requested_on, "%Y-%m-%d %H:%M:%S")))))
}

# Check if provided task is in requested status which means that a user has requested manual run
in_requested_status <- function(task) {
  (((task$task == "dependencies" &&
    !is.na(conf$dep_updated_on) &&
    (is.na(task$started_on) ||
      task$started_on < strptime(conf$dep_updated_on, "%Y-%m-%d %H:%M:%S"))) ||
    (task$task == "geonames" &&
      !is.na(conf$geonames_updated_on) &&
      (is.na(task$started_on) ||
        task$started_on <
          strptime(conf$geonames_updated_on, "%Y-%m-%d %H:%M:%S"))) ||
    (task$task == "languages" &&
      !is.na(conf$lang_updated_on) &&
      (is.na(task$started_on) ||
        task$started_on <
          strptime(conf$lang_updated_on, "%Y-%m-%d %H:%M:%S"))) ||
    (task$task == "alerts" &&
      !is.na(conf$alerts_requested_on) &&
      (is.na(task$started_on) ||
        task$started_on <
          strptime(conf$alerts_requested_on, "%Y-%m-%d %H:%M:%S")))))
}


# Updating alert task for reporting progress on alert detection
update_alerts_task <- function(
  tasks,
  status,
  message,
  start = FALSE,
  end = FALSE
) {
  if (start) tasks$alerts$started_on = Sys.time()
  if (end) tasks$alerts$end_on = Sys.time()
  tasks$alerts$status = status
  tasks$alerts$message = message
  save_tasks(tasks)
  return(tasks)
}


