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
bluesky_new_plan_logic_if_previous_has_passed <- function(plans) {
    return(
        list(
            "research_max_date" = plans[[1]]$boundaries_date_min
        )
    )
}

#' @noRd
bluesky_finish_plan_logic <- function(p) {
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
