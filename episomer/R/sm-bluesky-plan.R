# @function_def_start (do not delete)

#' parse custom attributes for a plan in this specific social media and returns them as a named list with attributes in the proper type
#' @param ... list, triple dot arguments to be passed as a parameter tu underlying functions 
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


#' prepare a plan for serialisation by transforming attributes with non standard serialisation types e.g. dates to string on the format expected by the parse functions
#' @param plan list, the plan with the attributes go format
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


#' returns a list with the custom social media plan attributes with the default values for a first plan in a topic.
sm_plan_first_attributes_bluesky <- function() {
# @function_def_end (do not delete)
  list(
    plan_max_date = strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC"),
    plan_min_date = strftime(Sys.Date() - conf$collect_history_days, "%Y-%m-%d %H:%M:%OS6Z", tz = "UTC"),
    current_min_date = NULL
  )
}

# @function_def_start (do not delete)

#' returns a list with the custom social media plan attributes with the values for the plan to be run after the plans provided as parameter
#' @param plans list, the list with current plans of the given topic to calculate next plan attributes
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

#' calculates and returns the estimated progress of a plan using custom attributes
#' @param plan list, the plan to calculate the progress from
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

#' Return a message to log what the next query is going to be for the current plan
#' @param plan list, the plan to print the information about
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
