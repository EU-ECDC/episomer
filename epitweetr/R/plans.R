# @title get_plan S3 class constructor
# @description Create a new 'get plan' for importing tweets using the Search API
# @param expected_end Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the target end datetime of this plan
# @param scheduled_for Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the expected datetime for next execution, default: Sys.time()
# @param start_on Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the datetime when this plan was first executed, default: NULL
# @param end_on Character(\%Y-\%m-\%d \%H:\%M:\%S) establishing the datetime when this plan has finished, default: NULL
# @param requests Integer, number of requests successfully executed, default: 0
# @param got_rowa Boolean, if the plan has collected rows, default: FALSE
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
parse_plan_elements <- function(
    network,
    expected_end,
    scheduled_for = Sys.time(),
    start_on = NULL,
    end_on = NULL,
    requests = 0,
    got_rows = FALSE,
    progress = 0.0,
    ...
) {
    common_elements <- list(
        "network" = network,
	"expected_end" = if (!is.null(unlist(expected_end))) strptime(unlist(expected_end), "%Y-%m-%d %H:%M:%S") else NULL,
        "scheduled_for" = if (!is.null(unlist(scheduled_for))) strptime(unlist(scheduled_for), "%Y-%m-%d %H:%M:%S") else NULL,
        "start_on" = if (!is.null(unlist(start_on))) strptime(unlist(start_on), "%Y-%m-%d %H:%M:%S") else NULL,
        "end_on" = if (!is.null(unlist(end_on))) strptime(unlist(end_on), "%Y-%m-%d %H:%M:%S") else NULL,
        "requests" = unlist(requests),
        "got_rows" = unlist(got_rows),
        "progress" = unlist(progress)
    )
    specific_elements <- get(
        sprintf("%s_parse_plan_elements", network)
    )(...)
    me <- c(common_elements, specific_elements)
    return(me)
}

request_got_rows <- function(plan, results) { get(sprintf("%s_got_rows", plan$network))(results) }
get_plan_progress <- function(plan) { get(sprintf("%s_get_plan_progress", plan$network))(plan) }
update_plan_after_request_by_network <- function(plan, results) { get(sprintf("%s_update_plan_after_request", plan$network))(plan, results) }

# Update a plan after search request is done
# If first request, started and max will be set
# If results are non-empty the current social media cursor is updated
# If no results are obtained the search is supposed to be finished
update_plan_after_request <- function(plan, results) {
    # increasing the number of requests
    plan$requests <- plan$requests + 1

    # setting the start date if not set
    if (is.null(plan$start_on)) {
        plan$start_on = Sys.time()
    }

    # check is the current request got rows
    req_got_rows <- request_got_rows(plan, results)
    if(req_got_rows) {
        # set got rows for this plan
	plan$got_rows <- TRUE

    	# updating plan by network parameters
        plan <- update_plan_after_request_by_network(plan, results)
	# setting plan progress
	plan$progress <- get_plan_progress(plan) 
    } else {
        # if no rows have been obtained then we consider the plan has ended
        plan$end_on <- Sys.time()
	plan$progress <- 1.0 
    }
    return(plan)
} 


first_plan_element_by_network <- function(network) { get(sprintf("%s_first_plan_elements", network))() }
next_plan_element_by_network <- function(network, plans) { get(sprintf("%s_first_plan_elements", network))(plans) }

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
update_plans <- function(plans = list(), network, schedule_span) {
    # Testing if there are plans present
    if (length(plans) == 0) {
        # Getting default plan for when no existing plans are present setting the expected end
	elems <- list(
            network = network,
            expected_end = strftime(Sys.time() + 60 * schedule_span, "%Y-%m-%d %H:%M:%S")
	)
        elems <- c(
            elems,
	    first_plan_element_by_network(network)
	)
	new_plan <- do.call(parse_plan_elements, elems)
        return(list(new_plan))
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
        elems <- list(
            network = plans[[1]]$network,
            # expected end should be 'schedule_span' minutes after previous plan expected en unless this is in the past in which case the span would cound from now
	    expected_end = if (
                Sys.time() > plans[[1]]$expected_end + 60 * schedule_span
            ) {
                strftime(Sys.time() + 60 * schedule_span, "%Y-%m-%d %H:%M:%S") 
            } else {
                strftime(plans[[1]]$expected_end + 60 * schedule_span, "%Y-%m-%d %H:%M:%S")
            }
        )
        elems <- c(
            elems,
            next_plan_element_by_network(network, plans)
        )
        first <- do.call(parse_plan_elements, elems)

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
            network_specific_logic <- get(sprintf("%s_finish_plan", p$network))
            params <- c(plan_common_elements, network_specific_logic(p))
            do.call(get_plan, params)
        })
    }
}


