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
    sms <- active_social_media()
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
refresh_config_data <- function(
  e = new.env(),
  limit = list("langs", "topics", "tasks", "geo")
) {
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
    lang_tasks <- get_tasks()$language
    if (!exists("langs_refresh_flag", where = e)) {
      e$langs_refresh_flag <- shiny::reactiveVal(0)
    } else if (
      file.exists(get_tasks_path()) &&
        file.info(get_tasks_path())$mtime > e$langs_refresh_flag()
    ) {
      e$langs_refresh_flag(file.info(get_tasks_path())$mtime)
    } else if (
      file.exists(get_properties_path()) &&
        file.info(get_properties_path())$mtime > e$langs_refresh_flag()
    ) {
      e$langs_refresh_flag(file.info(get_properties_path())$mtime)
    }
    e$lang_items <- setNames(langs$Code, langs$`Full Label`)
    e$lang_names <- setNames(langs$Label, langs$Code)
    e$langs <- data.frame(
      Language = unlist(lang_tasks$names),
      Code = unlist(lang_tasks$codes),
      Status = unlist(lang_tasks$statuses),
      URL = unlist(lang_tasks$urls)
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
        file.exists(get_user_topics_path()) &&
        file.info(get_user_topics_path())$mtime != e$topics_refresh_flag()
    ) {
      update <- TRUE
      e$topics_refresh_flag(file.info(get_user_topics_path())$mtime)
    }
    if (
      !update &&
        file.exists(get_plans_path()) &&
        file.info(get_plans_path())$mtime != e$plans_refresh_flag()
    ) {
      update <- TRUE
      e$plans_refresh_flag(file.info(get_plans_path())$mtime)
    }
    if (update) {
      e$topics_df <- get_topics_df()
    }
  }
  # Updating tasks related fields
  if ("tasks" %in% limit) {
    # Updating the reactive value tasks_refresh to force dependencies invalidation
    update <- FALSE
    e$detect_running <- is_detect_running()
    e$fs_running <- is_fs_running()
    e$search_running <- is_search_running()
    e$missing_search_jobs <- missing_search_jobs()
    e$search_diff <- round(Sys.time() - last_search_time())

    # Detecting if some change has happened since last evaluation
    if (!exists("tasks_refresh_flag", where = e)) {
      e$tasks_refresh_flag <- shiny::reactiveVal()
      update <- TRUE
    } else if (
      file.exists(get_tasks_path()) &&
        file.info(get_tasks_path())$mtime != e$tasks_refresh_flag()
    ) {
      update <- TRUE
    }
    if (update) {
      if (file.exists(get_tasks_path()))
        e$tasks_refresh_flag(file.info(get_tasks_path())$mtime)
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
      file.exists(conf$data_dir),
      'Please go to configuration tab and setup post collection (no data directory found)'
    ),
    shiny::need(
      check_series_present(),
      paste(
        'No aggregated data found on ',
        paste(conf$data_dir, "series", sep = "/"),
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

# getting the default snapshot folder and create it if does not exists
ensure_snapshot_folder <- function() {
  path <- file.path(conf$data_dir, "snapshot")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

update_config_from_input <- function(input) {
  conf$collect_span <- input$conf_collect_span
  conf$schedule_span <- input$conf_schedule_span
  conf$keyring <- input$conf_keyring
  conf$spark_cores <- input$conf_spark_cores
  conf$spark_memory <- input$conf_spark_memory
  conf$onthefly_api <- input$conf_onthefly_api
  conf$geolocation_threshold <- input$geolocation_threshold
  conf$geonames_url <- input$conf_geonames_url
  conf$maven_repo <- input$conf_maven_repo
  conf$winutils_url <- input$conf_winutils_url
  conf$geonames_simplify <- input$conf_geonames_simplify
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
  conf$sm_alerts_bluesky <- input$conf_sm_alerts_bluesky
  conf$sm_activated_bluesky <- input$conf_sm_activated_bluesky
}
