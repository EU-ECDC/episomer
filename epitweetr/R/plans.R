# @title Update get plans
# @description Updating plans for a particular topic
# @param plans The existing plans for the topic, default: list()
# @param schedule_target target minutes for finishing a plan
# @return updated list of 'get_plan'
# @details
# This function will update the plan list of a topic taking in consideration the search span
# This function is called at the beginning of each search loop iteration applying the following rules
# If no plans are set, a new plan for getting all possible tweets will be set
# If current plan has started and the expected end has passed, a new plan will be added for collecting new tweets (previous plan will be stored for future execution if possible)
# Any finished plans after the first will be discharged. Note that after 7 days, all plans should be discharged because of empty results and as a measure of precaution, a maximum of 100 plans are kept)
# @returns the updated list of plans
# @examples
# if(FALSE){
#  #Getting default plan
#  update_plans(plans = list(), schedule_span = 120)
#  #Updating topics for first topic
#  update_plans(plans = conf$topics[[1]]$plan, schedule_span = conf$collect_span)
# }
# @rdname update_plans
update_plans <- function(plans = list(), schedule_span) {
    # Testing if there are plans present
    if (length(plans) == 0) {
        # Getting default plan for when no existing plans are present setting the expected end
        return(list(get_plan(expected_end = Sys.time() + 60 * schedule_span)))
    } else if (
        plans[[1]]$requests > 0 && plans[[1]]$expected_end < Sys.time()
    ) {
        # creating a new plan if expected end has passed
        first <-
            get_plan(
                expected_end = if (
                    Sys.time() > plans[[1]]$expected_end + 60 * schedule_span
                ) {
                    Sys.time() + 60 * schedule_span
                } else {
                    plans[[1]]$expected_end + 60 * schedule_span
                },
                since_target = plans[[1]]$max_id + 1
            )
        # removing ended plans
        non_ended <- plans[sapply(plans, function(x) is.null(x$end_on))]
        # removing plans if more of 100 plans are activeff
        return(append(
            list(first),
            if (length(non_ended) < 100) non_ended else non_ended[1:100]
        ))
    } else {
        first <- plans[[1]]
        rest <- plans[-1]
        # removing ended plans
        non_ended <- rest[unlist(sapply(rest, function(x) is.null(x$end_on)))]
        # removing ended plans
        return(append(
            list(first),
            if (length(non_ended) < 100) non_ended else non_ended[1:100]
        ))
    }
}


# finish the provided plans
finish_plans <- function(plans = list()) {
    # Testing if there are plans present
    if (length(plans) == 0) {
        list()
    } else {
        # creating a new plan if expected end has passed
        lapply(plans, function(p) {
            get_plan(
                expected_end = strftime(
                    if (is.null(p$end_on)) {
                        Sys.time() - conf$schedule_span * 60
                    } else {
                        p$end_on
                    },
                    "%Y-%m-%d %H:%M:%S"
                ),
                scheduled_for = strftime(p$scheduled_for, "%Y-%m-%d %H:%M:%S"),
                start_on = strftime(
                    if (is.null(p$start_on)) {
                        Sys.time() - conf$schedule_span * 60
                    } else {
                        p$start_on
                    },
                    "%Y-%m-%d %H:%M:%S"
                ),
                end_on = strftime(
                    if (is.null(p$end_on)) {
                        Sys.time() - conf$schedule_span * 60
                    } else {
                        p$end_on
                    },
                    "%Y-%m-%d %H:%M:%S"
                ),
                max_id = p$max_id,
                since_id = p$since_target,
                since_target = p$since_target,
                requests = p$requests,
                progress = 1.0
            )
        })
    }
}

# Get next plan to plan to download
next_plan <- function(plans) {
    plans <- if ("get_plan" %in% class(plans)) list(plans) else plans
    non_ended <- plans[sapply(plans, function(x) is.null(x$end_on))]
    if (length(non_ended) == 0) {
        return(NULL)
    } else {
        return(non_ended[[1]])
    }
}


# next plan generic function
request_finished <- function(current, got_rows, max_id, since_id = NULL) {
    UseMethod("request_finished", current)
}

# Update a plan after search request is done
# If first request, started and max will be set
# If results are non-empty result span, since_id and max id are set
# If results are less than requested the search is supposed to be finished
# If results are equals to the requested limit, more tweets are expected. In that case if the expected end has not yet arrived and we can estimate the remaining number of requests the next schedule will be set to an estimation of the necessary requests to finish. If we do not know, the current schedule will be left untouched.
request_finished.get_plan <- function(
    current,
    got_rows,
    max_id,
    since_id = NULL
) {
    # increasing the number of requests
    current$requests <- current$requests + 1

    # setting the start date after first request and max id that will be obtained by this plan
    if (is.null(current$start_on)) {
        current$start_on = Sys.time()
        current$max_id = bit64::as.integer64(max_id)
    }
    # setting the oldest id obtained by this plan (which should not go before since_target)
    if (!is.null(since_id)) {
        current$since_id <- bit64::as.integer64(since_id)
    }
    # calculating progress
    if (
        !is.null(current$since_target) &&
            !is.null(current$since_id) &&
            !is.null(current$max_id)
    ) {
        current$progress <- as.double(current$max_id - current$since_id) /
            as.double(current$max_id - current$since_target)
    }
    # Setting the end of the plan if no lines have been obtained
    if (
        !got_rows ||
            (!is.null(current$since_target) &&
                current$max_id == current$since_target)
    ) {
        current$end_on <- Sys.time()
        #current$progress <- 1.0
    } else {
        if (Sys.time() < current$expected_end && current$progress > 0.0) {
            # this property was designed to delay plans that cab quickly finish, but it has finally not been used.
            progressByRequest <- current$progress / current$requests
            requestsToFinish <- (1.0 - current$progress) / progressByRequest
            current$scheduled_for = Sys.time() +
                as.integer(difftime(
                    current$expected_end,
                    Sys.time(),
                    units = "secs"
                )) /
                    requestsToFinish
        }
    }
    return(current)
}
