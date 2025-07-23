# @title get_plan S3 class constructor
# @description Create a new 'get plan' for importing tweets using the Search API
# @param expected_end Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the target end datetime of this plan
# @param scheduled_for Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the expected datetime for next execution, default: Sys.time()
# @param start_on Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the datetime when this plan was first executed, default: NULL
# @param end_on Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the datetime when this plan has finished, default: NULL
# @param max_id Integer(64), the newest tweet collected by this plan represented by its tweet id. This value is defined after the first successful request is done and does not change by request, default: NULL
# @param since_id Integer(64) the oldest tweet that has currently been collected by this plan, this value is updated after each request, default: NULL
# @param since_target Interger(64), the oldest tweet id that is expected to be obtained by this plan, this value is set as the max_id from the previous plan + 1, default: NULL
# @param results_span Number of minutes after which this plan expires counting from start date, default: 0
# @param requests Integer, number of requests successfully executed, default: 0
# @param progress Numeric, percentage of progress of current plan defined when since_target_id is known or when a request returns no more results, default: 0
# @return The get_plan object defined by input parameters
# @details A plan is an S3 class representing a commitment to download tweets from the search API
# It targets a specific time frame defined from the last tweet collected by the previous plan, if any, and the last tweet collected on its first request
# This commitment will be valid during a period of time defined from the time of its first execution until the end_on parameter
# a plan will perform several requests to the search API and each time a request is performed the number of requests will be increased.
# The field scheduled_for indicates the time when the next request is expected to be executed.
# @examples
# if(FALSE){
#  #creating the default plan
#  get_plan()
# }
# @seealso
#  \code{\link[bit64]{as.integer64.character}}
# @rdname get_plan
# @importFrom bit64 as.integer64
get_plan <- function(
    network,
    expected_end,
    scheduled_for = Sys.time(),
    start_on = NULL,
    end_on = NULL,
    results_span = 0,
    requests = 0,
    progress = 0.0,
    ...
) {
    common_elements <- list(
        "network" = network,
        "expected_end" = if (!is.null(unlist(expected_end))) {
            strptime(unlist(expected_end), "%Y-%m-%d %H:%M:%S")
        } else {
            NULL
        },
        "scheduled_for" = if (!is.null(unlist(scheduled_for))) {
            strptime(unlist(scheduled_for), "%Y-%m-%d %H:%M:%S")
        } else {
            NULL
        },
        "start_on" = if (!is.null(unlist(start_on))) {
            strptime(unlist(start_on), "%Y-%m-%d %H:%M:%S")
        } else {
            NULL
        },
        "end_on" = if (!is.null(unlist(end_on))) {
            strptime(unlist(end_on), "%Y-%m-%d %H:%M:%S")
        } else {
            NULL
        },
        "requests" = unlist(requests),
        "progress" = unlist(progress)
    )
    specific_elements <- get(
        sprintf("%s_get_plan_elements", network)
    )(...)
    me <- c(common_elements, specific_elements)
    class(me) <- append(class(me), "get_plan")
    return(me)
}


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
        new_plan_network_specific_logic <- get(
            sprintf(
                "%s_new_plan_creation_based_on_previous_one_values",
                plans[[1]]$network
            )
        )
        params <- list(
            network = plans[[1]]$network,
            expected_end = if (
                Sys.time() > plans[[1]]$expected_end + 60 * schedule_span
            ) {
                Sys.time() + 60 * schedule_span
            } else {
                plans[[1]]$expected_end + 60 * schedule_span
            }
        )
        params <- c(
            params,
            new_plan_network_specific_logic(plans)
        )
        first <- do.call(get_plan, params)

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
            plan_common_elements <- list(
                network = p$network,
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
                requests = p$requests,
                progress = 1.0
            )
            network_specific_logic <- get(
                sprintf("%s_finish_plan", p$network)
            )
            params <- c(plan_common_elements, network_specific_logic(p))
            do.call(get_plan, params)
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
# request_finished <- function(current, got_rows, max_id, since_id = NULL) {
#     UseMethod("request_finished", current)
# }

# Update a plan after search request is done
# If first request, started and max will be set
# If results are non-empty result span, since_id and max id are set
# If results are less than requested the search is supposed to be finished
# If results are equals to the requested limit, more tweets are expected. In that case if the expected end has not yet arrived and we can estimate the remaining number of requests the next schedule will be set to an estimation of the necessary requests to finish. If we do not know, the current schedule will be left untouched.
# request_finished.get_plan <- function(
#     current,
#     got_rows,
#     max_id,
#     since_id = NULL
# ) {
#     # increasing the number of requests
#     current$requests <- current$requests + 1

#     # setting the start date after first request and max id that will be obtained by this plan
#     if (is.null(current$start_on)) {
#         current$start_on = Sys.time()
#         current$max_id = bit64::as.integer64(max_id)
#     }
#     # setting the oldest id obtained by this plan (which should not go before since_target)
#     if (!is.null(since_id)) {
#         current$since_id <- bit64::as.integer64(since_id)
#     }
#     # calculating progress
#     if (
#         !is.null(current$since_target) &&
#             !is.null(current$since_id) &&
#             !is.null(current$max_id)
#     ) {
#         current$progress <- as.double(current$max_id - current$since_id) /
#             as.double(current$max_id - current$since_target)
#     }
#     # Setting the end of the plan if no lines have been obtained
#     if (
#         !got_rows ||
#             (!is.null(current$since_target) &&
#                 current$max_id == current$since_target)
#     ) {
#         current$end_on <- Sys.time()
#         #current$progress <- 1.0
#     } else {
#         if (Sys.time() < current$expected_end && current$progress > 0.0) {
#             # this property was designed to delay plans that cab quickly finish, but it has finally not been used.
#             progressByRequest <- current$progress / current$requests
#             requestsToFinish <- (1.0 - current$progress) / progressByRequest
#             current$scheduled_for = Sys.time() +
#                 as.integer(difftime(
#                     current$expected_end,
#                     Sys.time(),
#                     units = "secs"
#                 )) /
#                     requestsToFinish
#         }
#     }
#     return(current)
# }
