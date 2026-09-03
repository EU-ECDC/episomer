test_that("sm_plan_parse_attributes calls the bluesky implementation", {
  local_mocked_bindings(
    sm_plan_parse_attributes_bluesky = function(...) {
      list(test_attr = "parsed_value")
    }
  )

  result <- sm_plan_parse_attributes("bluesky", arg1 = "value1")
  expect_equal(result$test_attr, "parsed_value")
})

test_that("sm_plan_format calls the bluesky implementation", {
  local_mocked_bindings(
    sm_plan_format_bluesky = function(plan) {
      plan$formatted <- TRUE
      plan
    }
  )

  plan <- list(network = "bluesky", data = "test")
  result <- sm_plan_format(plan)
  expect_true(result$formatted)
})

test_that("sm_plan_first_attributes calls the bluesky implementation", {
  local_mocked_bindings(
    sm_plan_first_attributes_bluesky = function() {
      list(first_attr = "first_value")
    }
  )

  result <- sm_plan_first_attributes("bluesky")
  expect_equal(result$first_attr, "first_value")
})

test_that("sm_plan_next_attributes calls the bluesky implementation", {
  local_mocked_bindings(
    sm_plan_next_attributes_bluesky = function(plans) {
      list(next_attr = paste0("next_", length(plans)))
    }
  )

  existing_plans <- list(list(page = 1), list(page = 2))
  result <- sm_plan_next_attributes("bluesky", existing_plans)
  expect_equal(result$next_attr, "next_2")
})

test_that("sm_plan_get_progress calls the bluesky implementation", {
  local_mocked_bindings(
    sm_plan_get_progress_bluesky = function(plan) {
      0.75
    }
  )

  plan <- list(network = "bluesky", status = "in_progress")
  result <- sm_plan_get_progress(plan)
  expect_equal(result, 0.75)
})

test_that("sm_plan_next_search_info calls the bluesky implementation", {
  local_mocked_bindings(
    sm_plan_search_info_bluesky = function(plan) {
      "Mock search info"
    }
  )

  plan <- list(network = "bluesky", page = 1)
  result <- sm_plan_next_search_info(plan)
  expect_equal(result, "Mock search info")
})

test_that("sm_api_get_token calls the bluesky implementation", {
  local_mocked_bindings(
    sm_api_get_token_bluesky = function(network) {
      "mock_token_bluesky"
    }
  )

  result <- sm_api_get_token("bluesky")
  expect_equal(result, "mock_token_bluesky")
})

test_that("sm_api_has_credentials calls the bluesky implementation", {
  local_mocked_bindings(
    sm_api_has_credentials_bluesky = function(network) {
      TRUE
    }
  )

  result <- sm_api_has_credentials("bluesky")
  expect_true(result)
})

test_that("sm_api_has_credentials calls the mastodon implementation for a federated network", {
  local_mocked_bindings(
    sm_api_has_credentials_mastodon = function(network) {
      identical(network, "mastodon.mstdn.party")
    }
  )

  expect_true(sm_api_has_credentials("mastodon.mstdn.party"))
})

test_that("sm_api_translate_query calls the bluesky implementation", {
  local_mocked_bindings(
    sm_api_translate_query_bluesky = function(parsed) {
      c("query1", "query2")
    }
  )

  result <- sm_api_translate_query("bluesky", list(terms = "test"))
  expect_equal(length(result), 2)
})

test_that("sm_api_search calls the bluesky implementation", {
  local_mocked_bindings(
    sm_api_search_bluesky = function(query, token, plan) {
      list(results = c("post1", "post2"), count = 2)
    }
  )

  plan <- list(network = "bluesky", limit = 10)
  result <- sm_api_search("test_query", "token", plan)
  expect_equal(result$count, 2)
})

test_that("sm_api_update_plan_after_request calls the bluesky implementation", {
  local_mocked_bindings(
    sm_api_update_plan_after_request_bluesky = function(plan, results) {
      plan$updated <- TRUE
      plan
    }
  )

  plan <- list(network = "bluesky", page = 1)
  results <- list(data = "test")
  result <- sm_api_update_plan_after_request(plan, results)
  expect_true(result$updated)
})

test_that("sm_api_set_auth calls the bluesky implementation", {
  local_mocked_bindings(
    sm_api_set_auth_bluesky = function(shiny_input_list = NULL, network) {
      list(authenticated = TRUE, network = network)
    }
  )

  result <- sm_api_set_auth("bluesky", shiny_input_list = NULL)
  expect_true(result$authenticated)
  expect_equal(result$network, "bluesky")
})

test_that("sm_plan_parse_attributes routes to mastodon family", {
  local_mocked_bindings(
    sm_plan_parse_attributes_mastodon = function(...) {
      list(mastodon_attr = "mastodon_value")
    }
  )

  result <- sm_plan_parse_attributes("mastodon.fosstodon.org")
  expect_equal(result$mastodon_attr, "mastodon_value")
})

test_that("sm_api_translate_query routes to mastodon family", {
  local_mocked_bindings(
    sm_api_translate_query_mastodon = function(parsed) {
      c("mastodon_query")
    }
  )

  result <- sm_api_translate_query("mastodon.fosstodon.org", list())
  expect_equal(result[1], "mastodon_query")
})

test_that("sm_plan_format routes to mastodon family", {
  local_mocked_bindings(
    sm_plan_format_mastodon = function(plan) {
      plan$network_type <- "mastodon"
      plan
    }
  )

  plan <- list(network = "mastodon.fosstodon.org")
  result <- sm_plan_format(plan)
  expect_equal(result$network_type, "mastodon")
})
