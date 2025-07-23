#' @noRd
bluesky_get_plan_elements <- function(...) {
    elements_to_add <- list(...)
    expected_names <- c(
        "research_max_date",
        "research_min_date",
        "boundaries_date_min",
        "boundaries_date_max",
        "newest_messages_from_previous_queries",
        "oldest_messages_from_previous_queries",
        "has_more"
    )
    elements_to_add <- elements_to_add[
        names(elements_to_add) %in% expected_names
    ]

    list(
        "research_max_date" = if (
            !is.null(unlist(elements_to_add$research_max_date))
        ) {
            strptime(
                unlist(elements_to_add$research_max_date),
                "%Y-%m-%d %H:%M:%S"
            )
        } else {
            NULL
        },
        "research_min_date" = if (
            !is.null(unlist(elements_to_add$research_min_date))
        ) {
            strptime(
                unlist(elements_to_add$research_min_date),
                "%Y-%m-%d %H:%M:%S"
            )
        } else {
            NULL
        },
        "boundaries_date_min" = if (
            !is.null(unlist(elements_to_add$boundaries_date_min))
        ) {
            strptime(
                unlist(elements_to_add$boundaries_date_min),
                "%Y-%m-%d %H:%M:%S"
            )
        } else {
            NULL
        },
        "boundaries_date_max" = if (
            !is.null(unlist(elements_to_add$boundaries_date_max))
        ) {
            strptime(
                unlist(elements_to_add$boundaries_date_max),
                "%Y-%m-%d %H:%M:%S"
            )
        } else {
            NULL
        },
        "newest_messages_from_previous_queries" = if (
            !is.null(unlist(
                elements_to_add$newest_messages_from_previous_queries
            ))
        ) {
            elements_to_add$newest_messages_from_previous_queries
        } else {
            NULL
        },
        "oldest_messages_from_previous_queries" = if (
            !is.null(unlist(
                elements_to_add$oldest_messages_from_previous_queries
            ))
        ) {
            elements_to_add$oldest_messages_from_previous_queries
        } else {
            NULL
        },
        "has_more" = if (!is.null(unlist(elements_to_add$has_more))) {
            elements_to_add$has_more
        } else {
            NULL
        }
    )
}

#' @noRd
bluesky_new_plan_creation_based_on_previous_one_values <- function(plans) {
    return(
        list(
            "research_max_date" = plans[[1]]$research_max_date,
            "research_min_date" = plans[[1]]$research_min_date,
            "boundaries_date_max" = plans[[1]]$boundaries_date_max,
            "boundaries_date_min" = plans[[1]]$boundaries_date_min,
            "has_more" = plans[[1]]$has_more,
            "newest_messages_from_previous_queries" = plans[[
                1
            ]]$newest_messages_from_previous_queries,
            "oldest_messages_from_previous_queries" = plans[[
                1
            ]]$oldest_messages_from_previous_queries
        )
    )
}

#' @noRd
bluesky_finish_plan <- function(p) {
    list(
        "research_max_date" = NULL,
        "research_min_date" = NULL,
        "boundaries_date_min" = strftime(
            p$boundaries_date_min,
            "%Y-%m-%d %H:%M:%S"
        ),
        "boundaries_date_max" = strftime(
            p$boundaries_date_max,
            "%Y-%m-%d %H:%M:%S"
        ),
        "newest_messages_from_previous_queries" = NULL,
        "oldest_messages_from_previous_queries" = NULL,
        "has_more" = FALSE
    )
}

#' @noRd
bluesky_update_plan <- function(plan, got_rows, content, tz = "UTC") {
    plan$requests <- plan$requests + 1

    if (is.null(plan$start_on)) {
        plan$start_on = lubridate::as_datetime(Sys.time(), tz = tz)
    }

    if (!content$has_more) {
        plan$end_on <- lubridate::as_datetime(Sys.time(), tz = tz)
    }

    plan$newest_messages_from_previous_queries <- c(
        plan$newest_messages_from_previous_queries,
        content$newest_message_in_a_query
    )

    plan$oldest_messages_from_previous_queries <- c(
        plan$oldest_messages_from_previous_queries,
        content$oldest_message_in_a_query
    )

    # If there are more messages to retrieve, we update the research max date
    if (content$has_more) {
        plan$research_max_date <- content$oldest_message_in_a_query
        plan$boundaries_date_min <- min(c(
            plan$boundaries_date_min,
            content$oldest_message_in_a_query
        ))
    }

    # If there are no more messages to retrieve, we set the boundaries_date_max to the newest message retrieved in the entire research
    if (!content$has_more) {
        plan$boundaries_date_max <- max(c(
            plan$boundaries_date_max,
            plan$newest_messages_from_previous_queries
        ))
        plan$boundaries_date_min <- min(c(
            plan$boundaries_date_min,
            content$oldest_message_in_a_query
        ))
        plan$research_min_date <- NULL
        plan$research_max_date <- NULL
        plan$newest_messages_from_previous_queries <- NULL
        plan$oldest_messages_from_previous_queries <- NULL
    }

    plan$has_more <- content$has_more
    if (!is.null(plan$boundaries_date_max)) {
        plan$boundaries_date_max <- plan$boundaries_date_max %>%
            lubridate::as_datetime(tz = tz)
    }
    if (!is.null(plan$boundaries_date_min)) {
        plan$boundaries_date_min <- plan$boundaries_date_min %>%
            lubridate::as_datetime(tz = tz)
    }
    if (!is.null(plan$research_max_date)) {
        plan$research_max_date <- plan$research_max_date %>%
            lubridate::as_datetime(tz = tz)
    }
    if (!is.null(plan$research_min_date)) {
        plan$research_min_date <- plan$research_min_date %>%
            lubridate::as_datetime(tz = tz)
    }

    if (!is.null(plan$newest_messages_from_previous_queries)) {
        plan$newest_messages_from_previous_queries <-
            plan$newest_messages_from_previous_queries %>%
            lubridate::as_datetime(tz = tz)
    }
    if (!is.null(plan$oldest_messages_from_previous_queries)) {
        plan$oldest_messages_from_previous_queries <- plan$oldest_messages_from_previous_queries %>%
            lubridate::as_datetime(tz = tz)
    }
    return(plan)
}
