#' @noRd
bluesky_parse_plan_elements <- function(...) {
    p <- list(...)
    ret <- list(
        "plan_max_date" =    if (!is.null(unlist(p$plan_max_date)))    strptime(unlist(p$plan_max_date)   ,"%Y-%m-%d %H:%M:%OSZ", tz = "UTC") else NULL,
        "plan_min_date" =    if (!is.null(unlist(p$plan_min_date)))    strptime(unlist(p$plan_min_date)   ,"%Y-%m-%d %H:%M:%OSZ", tz = "UTC") else NULL,
        "current_min_date" = if (!is.null(unlist(p$current_min_date))) strptime(unlist(p$current_min_date),"%Y-%m-%d %H:%M:%OSZ", tz = "UTC") else NULL
    )
    ret
}

#' @noRd
bluesky_format_plan <- function(plan) {
    if (!is.null(plan$plan_max_date))    plan$plan_max_date    = strftime(plan$plan_max_date   ,"%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
    if (!is.null(plan$plan_min_date))    plan$plan_min_date    = strftime(plan$plan_min_date   ,"%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
    if (!is.null(plan$current_min_date)) plan$current_min_date = strftime(plan$current_min_date,"%Y-%m-%d %H:%M:%OS6Z", tz = "UTC")
    plan
}

#' @noRd
bluesky_first_plan_elements <- function() {
    list(
	plan_max_date = strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS6Z", tz="UTC"),
	plan_min_date = NULL,
	current_min_date = NULL
    )
}

#' @noRd
bluesky_next_plan_elements <- function(plans) {
    list(
	plan_max_date = strftime(Sys.time(), "%Y-%m-%d %H:%M:%OS6Z", tz="UTC"),
	plan_min_date = strftime(plans[[1]]$plan_max_date, "%Y-%m-%d %H:%M:%OS6Z", tz="UTC"),
	current_min_date = NULL
    )
}

#' @noRd
bluesky_update_plan_after_request <- function(plan, result) {
    if(!is.null(result$query_min_date)) { 
        plan$current_min_date <- result$query_min_date
    }
    return(plan)
}

bluesky_got_rows <- function(results) {
  (exists("posts", results) & length(results$posts) > 0)
}


#' @noRd
bluesky_get_plan_progress <- function(plan) {
    if(!is.null(plan$plan_max_date) && !is.null(plan$plan_min_date) && !is.null(plan$current_min_date)) {
        as.numeric(plan$plan_max_date - plan$current_min_date, unit="secs")/as.numeric(plan$plan_max_date - plan$plan_min_date, unit="secs")
    } else {
      NULL
    }
}
#' @noRd
bluesky_next_search_info <- function(plan) {
   info = ""
   if(is.null(plan$current_min_date)) {
       info = paste(info, "before (first)", strftime(plan$plan_max_date, "%Y-%m-%d %H:%M:%OS6Z", tz="UTC"))
   } else {
       info = paste(info, "before", strftime(plan$current_min_date, "%Y-%m-%d %H:%M:%OS6Z", tz="UTC"))
   }
   if(!is.null(plan$plan_min_date)) {
       info = paste(info, "until", strftime(plan$plan_min_date, "%Y-%m-%d %H:%M:%OS6Z", tz="UTC"))
   }
   return(info)
}
