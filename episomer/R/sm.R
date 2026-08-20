# functions in this script are the generic functions that needs to be implemented for 
# code in episomer call these functions instead of the social media specific passing the plan which contains the name of the social media.
# social media specific functions are identified by naming convention using adding the name of the social media at the end of the function name.
#
# Network identifiers
# -------------------
# A network identifier is either a plain family name (e.g. "bluesky") or a family name followed
# by the domain of the instance it collects from (e.g. "mastodon.mstdn.party"). Federated social
# media are made of independent instances, each with its own API endpoint and its own access
# token, so each instance is handled as a network of its own: it gets its own plan file
# (topics.<network>.json), its own search process, its own PID and its own secrets, all for free
# because those are already keyed by network.
#
# The instances to collect from are listed in the <family>_instances configuration property, and
# the whole family is switched on or off at once by sm_activated_<family> / sm_alerts_<family>.
# Once the identifier is built, the instance is read back from it by sm_instance(), so nothing
# downstream has to know about the property: plans need no instance attribute either since
# plan$network already carries it.
#
# The identifier is a persistent key (plan file name, secret name, network column on the stored
# posts): it must be chosen once and never renamed, renaming one orphans its history.

#' returns the code family of a network, i.e. the suffix of the sm_* functions implementing it
#' @details for a network bound to an instance the family is the part before the first dot,
#' "mastodon.mstdn.party" is served by the sm_*_mastodon functions. Networks without an instance
#' are their own family.
#' @param network char, the name of the social media
sm_family <- function(network) sub("\\..*$", "", network)

#' returns the instance domain a network collects from, or NULL when the network is not federated
#' @param network char, the name of the social media
sm_instance <- function(network) {
  if (grepl(".", network, fixed = TRUE)) sub("^[^.]*\\.", "", network) else NULL
}

#' whether a social media family is federated, i.e. made of independent instances each collected
#' as a network of its own
#' @details a federated family serves one network per domain listed in its <family>_instances
#' configuration property, and none when that list is empty. Families that do not implement
#' sm_is_federated_<family> are not federated.
#' @param family char, the social media family
sm_is_federated <- function(family) {
  implementation <- sprintf("sm_is_federated_%s", family)
  if (exists(implementation)) isTRUE(get(implementation)()) else FALSE
}

#' returns the name of a secret belonging to a network
#' @details network identifiers contain dots, which are not safe in every secret backend (secret
#' names are also used as environment variable names, see get_env_secrets), so they are flattened
#' to underscores: the token of "mastodon.mstdn.party" is stored as "mastodon_mstdn_party_token".
#' @param network char, the name of the social media
#' @param name char, the name of the secret within the network e.g. "token"
sm_secret_name <- function(network, name) {
  sprintf("%s_%s", gsub("[^A-Za-z0-9]", "_", network), name)
}

#' parse custom attributes for a plan in this specific social media and returns them as a named list with attributes in the proper type
#' @param network char, the name of the social media 
#' @param ... list, triple dot arguments to be passed as a parameter tu underlying functions 
sm_plan_parse_attributes <- function(network, ...) {
  get(sprintf("sm_plan_parse_attributes_%s", network))(...)
}

#' prepare a plan for serialisation by transforming attributes with non standard serialisation types e.g. dates to string on the format expected by the parse functions
#' @param plan, the plan to format the attributes
sm_plan_format <- function(plan) {
  get(sprintf("sm_plan_format_%s", plan$network))(plan)
}

#' returns a list with the custom social media plan attributes with the default values for a first plan in a topic.
#' @param network char, the name of the social media 
sm_plan_first_attributes <- function(network) {
  get(sprintf("sm_plan_first_attributes_%s", network))()
}

#' returns a list with the custom social media plan attributes with the values for the plan to be run after the plans provided as parameter
#' @param network char, network the name of the social media 
#' @param plans list, list of plans to use for calculate new plan attributes
sm_plan_next_attributes <- function(network, plans) {
  get(sprintf("sm_plan_next_attributes_%s", network))(plans)
}

#' calculates and returns the estimated progress of a plan using custom attributes
#' @param plan list, the plan to obtain the progress from
sm_plan_get_progress <- function(plan) {
  get(sprintf("sm_plan_get_progress_%s", plan$network))(plan)
}

#' Return a message to log what the next query is going to be for the current plan
#' @param plan list, the plan to obtain the message 
sm_plan_next_search_info <- function(plan) {
  get(sprintf("sm_plan_search_info_%s", plan$network))(plan)
}

#' Get social media token
#' @details this function implements the authethication on the respective social media and stores it securely. 
#' It is called by the search loop before starting message collection
#' @param network char, network the name of the social media 
sm_api_get_token <- function(network) {
  get(sprintf("sm_api_get_token_%s", network))()
}

#' Translate a parsed query to the social media query format
#' @details This function receive as a parameter a parsed query from the a topic in the list of topics. 
#' It can return one or many queries that matches the expected results using the underlying social media API
#' @param network char, the name of the social media 
#' @param parsed char, parsed query to translate in one or many native equivalent queries 
sm_api_translate_query <- function(network, parsed) {
  get(sprintf("sm_api_translate_query_%s", network))(parsed)
}

#' Search posts for a given query and produce standardised results
#' @details This function impleents the search of messages for a given query and plan.
#' @param query char, the query to use in the search in nattive format
#' @param token char, the token to authenticate against the underlying social media api 
#' @param plan list, the plan containing the targeted time frame
sm_api_search <- function(query, token, plan) {
  get(sprintf("sm_api_search_%s", plan$network))(query, token, plan)
}

#' Takes a plan after a request has been performed and the standardised results provided as returned by the function sm_api_search_xxx
#' it should return a plan with necessary information updated so next request will continue paginating as expected to obtain
#' targeted messages
#' @param plan list, the plan producing the resusts
#' @param results list, the result of the query in episomer format
sm_api_update_plan_after_request <- function(plan, results) {
  get(sprintf("sm_api_update_plan_after_request_%s", plan$network))(
    plan,
    results
  )
}

#' @title Save credentials for a social media provider and store them securely
#' @description Update configuration object and underlying storage with given bluesky username and password. The password is encrypted.
#' @param network char, the name of the social media
#' @param shiny_input_list list, the shiny input object to extract the information from if not provided the password will be requested interactively
sm_api_set_auth <- function(network, shiny_input_list=NULL) {
  get(sprintf("sm_api_set_auth_%s", network))(
    shiny_input_list
  )
}
