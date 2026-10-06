# Records, in `e`, the moment a runnning flag last changed value, so the status panel can say how
# long a service has been stopped (or running). `name` is the field prefix ("fs", "search",
# "detect"): this writes `e[[name]_running]` and, only on a change (or the first call), resets
# `e[[name]_since]` to now.
update_status_since <- function(e, name, running) {
  running_field <- paste0(name, "_running")
  since_field <- paste0(name, "_since")
  changed <- !exists(since_field, where = e) ||
    !identical(e[[running_field]], running)
  if (changed) {
    e[[since_field]] <- Sys.time()
  }
  e[[running_field]] <- running
}

# Formats a duration for the status panel, e.g. "3 minutes" or "less than a minute".
format_status_duration <- function(since) {
  minutes <- floor(as.numeric(difftime(Sys.time(), since, units = "mins")))
  if (minutes < 1) {
    return("less than a minute")
  }
  paste(minutes, if (minutes == 1) "minute" else "minutes")
}

# Get or updates data used for dashboard filters based on configuration values and aggregated period
refresh_dashboard_data <- function(e = new.env(), fixed_period = NULL) {
  e$topics <- {
    codes <- unique(sapply(conf$topics, function(t) t$topic))
    names <- get_topics_labels()[codes]
    sort(setNames(c("", codes), c("", unname(names))))
  }
  e$countries <- {
    regions <- get_country_items()
    setNames(1:length(regions), sapply(regions, function(r) r$name))
  }
  e$sms <- {
    sms <- active_social_media() %>% sm_family() %>% unique()
    setNames(sms, paste0(toupper(substr(sms, 1, 1)), substr(sms, 2, nchar(sms))))
  }
  agg_dates <- get_aggregated_period()
  if (is.na(agg_dates$first) || is.na(agg_dates$last)) {
    agg_dates$first = Sys.Date()
    agg_dates$last = Sys.Date()
  }
  e$date_min <- strftime(agg_dates$first, format = "%Y-%m-%d")
  e$date_max <- strftime(agg_dates$last, format = "%Y-%m-%d")
  collected_days <- agg_dates$last - agg_dates$first
  e$fixed_period <- (if (!is.null(fixed_period)) fixed_period else if (
    is.na(collected_days)
  )
    "custom" else if (collected_days < 7) "custom" else "last 7 days")
  e$date_start <- (if (e$fixed_period == "custom" && exists("date_start", e))
    e$date_start else if (e$fixed_period == "last 7 days" && collected_days > 7)
    strftime(agg_dates$last - 7, format = "%Y-%m-%d") else if (
    e$fixed_period == "last 30 days" && collected_days > 30
  )
    strftime(agg_dates$last - 30, format = "%Y-%m-%d") else if (
    e$fixed_period == "last 60 days" && collected_days > 60
  )
    strftime(agg_dates$last - 60, format = "%Y-%m-%d") else if (
    e$fixed_period == "last 180 days" && collected_days > 180
  )
    strftime(agg_dates$last - 180, format = "%Y-%m-%d") else e$date_min)
  e$date_end <- e$date_max
  
  if (!exists("topics_refresh_flag", envir = e)) {
    e$topics_refresh_flag <- shiny::reactiveVal(0)
  }
  return(e)
}


# Get or update data for config page from configuration, Data collection & processing and Requirements & alerts pipeline files
#
# Isolated on purpose. The body below both reads and writes the e$*_refresh_flag() reactive values:
# it reads them to compare a file mtime against the one it saw last time, and writes the new mtime
# when the file has changed. Read and written in the same reactive context, that is the textbook
# self-invalidating observer - and every caller of this function is an observe() or an
# observeEvent(). The write invalidates the caller, Shiny re-runs it inside the same flush, the
# file it watches (tasks.json, rewritten on every runner iteration; properties.json, rewritten by
# the autosave) has moved again, so it writes again, and the flush never drains. While a flush is
# draining, no input event is processed: a click on Run reaches its observeEvent() minutes late,
# once the watched files happen to stop changing. The flags still invalidate the outputs that read
# them, which is what they are for - only the caller stops depending on what it publishes.
refresh_config_data <- function(
  e = new.env(),
  limit = list("langs", "topics", "tasks", "geo")
) {
  shiny::isolate(refresh_config_data_impl(e = e, limit = limit))
}

refresh_config_data_impl <- function(e, limit) {
  # Refreshing configuration
  setup_config(data_dir = conf$data_dir, ignore_properties = TRUE)

  #Creating the flag for properties refresh
  if (!exists("properties_refresh_flag", where = e)) {
    e$properties_refresh_flag <- shiny::reactiveVal()
  }
  #Creating the flag for status refredh
  if (!exists("process_refresh_flag", where = e)) {
    e$process_refresh_flag <- shiny::reactiveVal()
  }
  #Creating the flag for subscribers refresh
  if (!exists("subscribers_refresh_flag", where = e)) {
    e$subscribers_refresh_flag <- shiny::reactiveVal()
  }
  #Creating the flag for geotraining refresh
  if (!exists("geotraining_refresh_flag", where = e)) {
    e$geotraining_refresh_flag <- shiny::reactiveVal()
  }
  #Creating the flag for alert training refresh
  if (!exists("alert_training_refresh_flag", where = e)) {
    e$alert_training_refresh_flag <- shiny::reactiveVal()
  }
  #Creating the flag for countries refresh
  if (!exists("countries_refresh_flag", where = e)) {
    e$countries_refresh_flag <- shiny::reactiveVal()
  }

  # Updating language related fields
  if ("langs" %in% limit) {
    langs <- get_available_languages()
    if (!exists("langs_refresh_flag", where = e)) {
      e$langs_refresh_flag <- shiny::reactiveVal(0)
    } else if (
      epifile.exists(get_tasks_path()) &&
        epifile.mtime(get_tasks_path()) > e$langs_refresh_flag()
    ) {
      e$langs_refresh_flag(epifile.mtime(get_tasks_path()))
    } else if (
      epifile.exists(get_properties_path()) &&
        epifile.mtime(get_properties_path()) > e$langs_refresh_flag()
    ) {
      e$langs_refresh_flag(epifile.mtime(get_properties_path()))
    }
    e$lang_items <- setNames(langs$Code, langs$`Full Label`)
    e$lang_names <- setNames(langs$Label, langs$Code)
    e$langs <- data.frame(
      Language = sapply(conf$languages, `[[`, 'name'),
      Code = sapply(conf$languages, `[[`, 'code')
    )
  }
  # Updating topics related fields
  if ("topics" %in% limit) {
    # Updating the reactive value tasks_refresh to force dependencies invalidation
    update <- FALSE
    if (
      !exists("topics_refresh_flag", envir = e) ||
        !exists("plans_refresh_flag", envir = e)
    ) {
      e$topics_refresh_flag <- shiny::reactiveVal(0)
      e$plans_refresh_flag <- shiny::reactiveVal(0)
      update <- TRUE
    }
    if (
      !update &&
        epifile.exists(get_user_topics_path()) &&
        epifile.mtime(get_user_topics_path()) != e$topics_refresh_flag()
    ) {
      update <- TRUE
      e$topics_refresh_flag(epifile.mtime(get_user_topics_path()))
    }
    if (
      !update &&
        epifile.exists(get_plans_path()) &&
        epifile.mtime(get_plans_path()) != e$plans_refresh_flag()
    ) {
      update <- TRUE
      e$plans_refresh_flag(epifile.mtime(get_plans_path()))
    }
    if (update) {
      e$topics_df <- get_topics_df()
    }
  }
  # Updating tasks related fields
  if ("tasks" %in% limit) {
    # Updating the reactive value tasks_refresh to force dependencies invalidation
    update <- FALSE
    # These four are cluster-wide answers, read from the runner leases for detect and search and
    # from /ping for fs (plan section 6.2). That is what the app wants: it is a pure reader of the
    # runner state and holds no lease itself (section 6.4), and a service running on another machine
    # of the deployment must show as running here. What machine each of them runs on is what the
    # status table of issue 8 will add.
    #
    # update_status_since() also stamps the moment each of the three last flipped state, which is
    # what lets the status panel say "Stopped (for x minutes)" instead of a bare "Stopped": neither
    # the fs ping nor the detect/search leases carry that history themselves (a lease that has gone
    # free or stale has nothing left to say about when it stopped), so it is tracked here, across
    # refreshes, in this same long-lived environment.
    update_status_since(e, "detect", is_detect_running())
    update_status_since(e, "fs", is_fs_running())
    update_status_since(e, "search", is_search_running())
    e$missing_search_jobs <- missing_search_jobs()
    e$search_diff <- round(Sys.time() - last_search_time())

    # The status table of plan issue 8: one row per runner holding a lease, wherever it runs. It
    # answers what the three booleans above cannot - *which machine* is collecting, and whether a
    # runner that reads as stopped is free or held by a lease nobody renews anymore. Refreshed with
    # the rest of the "tasks" block, i.e. every ten seconds while the configuration page is open.
    e$runner_leases_df <- runner_leases_df()

    # Detecting if some change has happened since last evaluation
    if (!exists("tasks_refresh_flag", where = e)) {
      e$tasks_refresh_flag <- shiny::reactiveVal()
      update <- TRUE
    } else if (
      epifile.exists(get_tasks_path()) &&
        epifile.mtime(get_tasks_path()) != e$tasks_refresh_flag()
    ) {
      update <- TRUE
    }
    if (update) {
      if (epifile.exists(get_tasks_path()))
        e$tasks_refresh_flag(epifile.mtime(get_tasks_path()))
      tasks <- get_tasks()
      sorted_tasks <- order(sapply(tasks, function(l) l$order))
      e$tasks <- tasks[sorted_tasks]
      e$tasks_df <- data.frame(
        Task = sapply(e$tasks, function(t) t$task),
        Status = sapply(
          e$tasks,
          function(t) if (in_pending_status(t)) "pending" else t$status
        ),
        Scheduled = sapply(
          e$tasks,
          function(t)
            strftime(
              t$scheduled_for,
              format = "%Y-%m-%d %H:%M:%OS",
              origin = '1970-01-01'
            )
        ),
        `Last Start` = sapply(
          e$tasks,
          function(t)
            strftime(
              t$started_on,
              format = "%Y-%m-%d %H:%M:%OS",
              origin = '1970-01-01'
            )
        ),
        `Last End` = sapply(
          e$tasks,
          function(t)
            strftime(
              t$end_on,
              format = "%Y-%m-%d %H:%M:%OS",
              origin = '1970-01-01'
            )
        ),
        Message = sapply(
          e$tasks,
          function(t)
            if (exists("message", where = t) && !is.null(t$message))
              t$message else ""
        )
      )
      row.names(e$tasks_df) <- sapply(e$tasks, function(t) t$order)
    }
  }
  # Updating geolocation test related fields
  if ("geo" %in% limit) {
  }
  return(e)
}

# validate that dashboard can be rendered
can_render <- function(input, d) {
  shiny::validate(
    shiny::need(
      is_fs_running(),
      'Embedded database service is not running, please make sure you have activated it (running update dependencies is necessary after upgrade from episomer version 0.1+ to episomer 1.0+)'
    ),
    shiny::need(
      check_series_present(),
      paste(
        'No aggregated data found on ',
        # epipath() and not conf$data_dir: check_series_present() looks under epipath("fs", <series>)
        # (R/check.R), so on a remote fs the message has to name the API location it actually
        # looked at, not a local directory that was never read.
        epipath("fs"),
        " please make sure the Requirements & alerts pipeline has successfully ran"
      )
    ),
    shiny::need(
      is.null(input) ||
        (!is.na(input$period[[1]]) &&
          !is.na(input$period[[2]]) &&
          (input$period[[1]] <= input$period[[2]])),
      'Please select a start and end period for the report. The start period must be a date earlier than the end period'
    ),
    shiny::need(is.null(input) || (input$topics != ''), 'Please select a topic')
  )
}

# validate that chart is not empty
chart_not_empty <- function(chart) {
  shiny::validate(
    shiny::need(!("waiver" %in% class(chart$data)), chart$labels$title)
  )
}


get_alertsdb_html <- function() {
  alerts <- get_alert_training_df()
  shiny::validate(
    shiny::need(!is.null(alerts), 'No alerts generated for the selected period')
  )
  alerts$topposts <- sapply(alerts$topposts, function(postsbylang) {
    if (length(postsbylang) == 0) "" else {
      paste(
        "<UL>",
        lapply(1:length(postsbylang), function(i) {
          paste(
            "<LI>",
            names(postsbylang)[[i]],
            "<OL>",
            paste(
              lapply(postsbylang[[i]], function(t) {
                paste0(
                  "<LI>",
                  htmltools::htmlEscape(t),
                  "</LI>"
                )
              }),
              collapse = ""
            ),
            "</OL>",
            "</LI>"
          )
        }),
        "</UL>",
        collapse = ""
      )
    }
  })
  alerts
}

get_alertsdb_runs_html <- function() {
  runs <- get_alert_training_runs_df()
  runs$f1score <- format(runs$f1score, digits = 3)
  runs$accuracy <- format(runs$accuracy, digits = 3)
  runs$precision_by_class <- gsub("\n", "<BR>", runs$precision_by_class)
  runs$sensitivity_by_class <- gsub("\n", "<BR>", runs$sensitivity_by_class)
  runs$fscore_by_class <- gsub("\n", "<BR>", runs$fscore_by_class)
  runs$custom_parameters <- sapply(runs$custom_parameters, function(params) {
    if (length(params) == 0) "" else {
      paste(
        "<UL>",
        lapply(1:length(params), function(i) {
          paste(
            "<LI>",
            names(params)[[i]],
            ":",
            params[[i]],
            "</LI>"
          )
        }),
        "</UL>",
        collapse = ""
      )
    }
  })
  runs
}

# Default destination offered for a snapshot, created if it does not exist yet.
#
# Always a *local* directory: create_snapshot() (R/check.R) writes the archive with dir.create()
# and normalizePath() on the machine running the app, whatever the files it collects come from.
# The data directory is therefore the right base whenever there is one - which is the usual case,
# the fs machine included.
#
# The fallback is for an app running away from the fs, where conf$data_dir is a placeholder under
# tempdir() (see episomer_app()) and an archive dropped there would be thrown away with the
# session. It is the working directory: where the session was started from is where the person
# running it is already looking, which makes the proposed path self-explanatory. Note that a
# session started from a checkout will propose a path inside the source tree - it stays a default,
# the field it fills is editable, and nothing is written until the snapshot is asked for.
ensure_snapshot_folder <- function() {
  path <- if (has_local_data_dir()) {
    file.path(conf$data_dir, "snapshot")
  } else {
    file.path(getwd(), "episomer-snapshots")
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

update_config_from_input <- function(input) {
  conf$collect_span <- input$conf_collect_span
  conf$schedule_span <- input$conf_schedule_span
  conf$spark_cores <- input$conf_spark_cores
  conf$spark_memory <- input$conf_spark_memory
  conf$geolocation_threshold <- input$geolocation_threshold
  conf$regions_disclaimer <- input$conf_regions_disclaimer
  conf$alert_alpha <- input$conf_alpha
  conf$alert_alpha_outlier <- input$conf_alpha_outlier
  conf$alert_k_decay <- input$conf_k_decay
  conf$alert_history <- input$conf_history
  conf$alert_same_weekday_baseline <- input$conf_same_weekday_baseline
  conf$alert_with_bonferroni_correction <- input$conf_with_bonferroni_correction
  conf$alert_with_quotes <- input$conf_with_quotes
  conf$smtp_host <- input$conf_smtp_host
  conf$smtp_port <- input$conf_smtp_port
  conf$smtp_from <- input$conf_smtp_from
  conf$smtp_login <- input$conf_smtp_login
  conf$smtp_insecure <- input$conf_smtp_insecure
  conf$smtp_password <- input$conf_smtp_password
  conf$admin_email <- input$conf_admin_email
  conf$force_date_format <- input$conf_force_date_format
}
