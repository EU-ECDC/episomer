# @function_def_start (do not delete)

#' @noRd
sm_plan_parse_attributes_bluesky <- function(...) {
# @function_def_end (do not delete)
  p <- list(...)
  ret <- list(
    "plan_max_date" = if (!is.null(unlist(p$plan_max_date)))
      strptime(unlist(p$plan_max_date), "%Y-%m-%d %H:%M:%OSZ", tz = "UTC") else
      NULL,
    "plan_min_date" = if (!is.null(unlist(p$plan_min_date)))
      strptime(unlist(p$plan_min_date), "%Y-%m-%d %H:%M:%OSZ", tz = "UTC") else
      NULL,
    "current_min_date" = if (!is.null(unlist(p$current_min_date)))
      strptime(
        unlist(p$current_min_date),
        "%Y-%m-%d %H:%M:%OSZ",
        tz = "UTC"
      ) else NULL
  )
  ret
}

# @function_def_start (do not delete)

#' @noRd
sm_plan_format_bluesky <- function(plan) {
# @function_def_end (do not delete)
  if (!is.null(plan$plan_max_date))
    plan$plan_max_date = strftime(
      plan$plan_max_date,
      "%Y-%m-%d %H:%M:%OS6Z",
      tz = "UTC"
    )
  if (!is.null(plan$plan_min_date))
    plan$plan_min_date = strftime(
      plan$plan_min_date,
      "%Y-%m-%d %H:%M:%OS6Z",
      tz = "UTC"
    )
  if (!is.null(plan$current_min_date))
    plan$current_min_date = strftime(
      plan$current_min_date,
      "%Y-%m-%d %H:%M:%OS6Z",
      tz = "UTC"
    )
  plan
}

# @function_def_start (do not delete)

#' @noRd
sm_plan_first_attributes_bluesky <- function() {
# @function_def_end (do not delete)
  list(
    plan_max_date = strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC"),
    plan_min_date = strftime(Sys.Date() - conf$collect_history_days, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC"),
    current_min_date = NULL
  )
}

# @function_def_start (do not delete)

#' @noRd
sm_plan_next_attributes_bluesky <- function(plans) {
# @function_def_end (do not delete)
  list(
    plan_max_date = strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC"),
    plan_min_date = strftime(
      plans[[1]]$plan_max_date,
      "%Y-%m-%d %H:%M:%OS6Z",
      tz = "UTC"
    ),
    current_min_date = NULL
  )
}

# @function_def_start (do not delete)

#' @noRd
sm_plan_get_progress_bluesky <- function(plan) {
# @function_def_end (do not delete)
  if (
    !is.null(plan$plan_max_date) &&
      !is.null(plan$plan_min_date) &&
      !is.null(plan$current_min_date)
  ) {
    as.numeric(plan$plan_max_date - plan$current_min_date, unit = "secs") /
      as.numeric(plan$plan_max_date - plan$plan_min_date, unit = "secs")
  } else {
    NULL
  }
}

# @function_def_start (do not delete)

#' @noRd
sm_api_update_plan_after_request_bluesky <- function(plan, results) {
# @function_def_end (do not delete)
  if (!is.null(results$pagination$min_created_at)) {
    plan$current_min_date <- lubridate::as_datetime(
      results$pagination$min_created_at
    )
  }
  return(plan)
}

# @function_def_start (do not delete)

#' @noRd
sm_api_got_rows_bluesky <- function(results) {
# @function_def_end (do not delete)
  (exists("posts", results) & length(results$posts) > 0)
}

# @function_def_start (do not delete)

#' @noRd
sm_plan_search_info_bluesky <- function(plan) {
# @function_def_end (do not delete)
  info = ""
  if (is.null(plan$current_min_date)) {
    info = paste(
      info,
      "before (first)",
      strftime(plan$plan_max_date, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
    )
  } else {
    info = paste(
      info,
      "before",
      strftime(plan$current_min_date, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
    )
  }
  if (!is.null(plan$plan_min_date)) {
    info = paste(
      info,
      "until",
      strftime(plan$plan_min_date, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
    )
  }
  return(info)
}
