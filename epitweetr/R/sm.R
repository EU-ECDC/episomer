sm_plan_parse_attributes <- function(network, ...) {
  get(sprintf("sm_plan_parse_attributes_%s", network))(...)
}

sm_plan_format <- function(plan) {
  get(sprintf("sm_plan_format_%s", plan$network))(plan)
}

sm_plan_first_attributes <- function(network) {
  get(sprintf("sm_plan_first_attributes_%s", network))()
}

sm_plan_next_attributes <- function(network, plans) {
  get(sprintf("sm_plan_next_attributes_%s", network))(plans)
}

sm_plan_get_progress <- function(plan) {
  get(sprintf("sm_plan_get_progress_%s", plan$network))(plan)
}

sm_plan_next_search_info <- function(plan) {
  get(sprintf("sm_plan_search_info_%s", plan$network))(plan)
}

sm_api_get_token <- function(network) {
  get(sprintf("sm_api_get_token_%s", network))()
}

sm_api_translate_query <- function(network, parts, excluded) {
  get(sprintf("sm_api_translate_query_%s", network))(parts, excluded)
}

sm_api_search <- function(query, token, plan) {
  get(sprintf("sm_api_search_%s", plan$network))(query, token, plan)
}

sm_api_update_plan_after_request <- function(plan, results) {
  get(sprintf("sm_api_update_plan_after_request_%s", plan$network))(
    plan,
    results
  )
}
