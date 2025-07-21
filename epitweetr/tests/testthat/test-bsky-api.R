test_that("bsky_search works with valid parameters", {
  # Mock successful search response
  mock_posts <- list(
    list(
      uri = "at://did:plc:test123/app.bsky.feed.post/abc123",
      cid = "cid123",
      author = list(
        handle = "test.bsky.app",
        did = "did:plc:test123",
        createdAt = "2023-01-01T00:00:00.000Z"
      ),
      record = list(
        text = "Test post about covid19",
        createdAt = "2023-01-01T00:00:00.000Z"
      ),
      indexedAt = "2023-01-01T00:00:00.000Z"
    )
  )

  mock_response <- list(
    posts = mock_posts,
    cursor = NULL
  )

  with_mocked_bindings(
    code = {
      result <- bsky_search(
        "covid19",
        "mock_token",
        number_of_posts_per_request = 100
      )

      expect_type(result, "list")
      expect_length(result, 5)
      post <- result$results[[1]]
      expect_equal(
        post[[1]]$uri,
        "at://did:plc:test123/app.bsky.feed.post/abc123"
      )
      expect_equal(post[[1]]$record$text, "Test post about covid19")
    },
    is_online = online,
    request = mock_request,
    req_url_query = mock_req_url_query,
    req_headers = mock_req_headers,
    req_perform = function(req) mock_req_perform(req, mock_response),
    resp_body_json = function(resp) mock_resp_body_json(resp, mock_response),
    last_response = mock_last_response,
    resp_status = mock_resp_status
  )
})

test_that("bsky_search works when retrieving many posts", {
  # Mock successful search response
  mock_posts <- create_mock_posts(n = 200)

  mock_response <- list(
    posts = mock_posts,
    cursor = NULL
  )

  with_mocked_bindings(
    code = {
      result <- bsky_search("covid19", "mock_token")

      expect_type(result, "list")
      posts <- result$results$posts
      expect_length(posts, 200)
    },
    is_online = online,
    request = mock_request,
    req_url_query = mock_req_url_query,
    req_headers = mock_req_headers,
    req_perform = function(req) mock_req_perform(req, mock_response),
    resp_body_json = function(resp) mock_resp_body_json(resp, mock_response),
    last_response = mock_last_response,
    resp_status = mock_resp_status
  )
})

test_that("bsky_search fails with no internet connection", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_search("covid19", "mock_token"),
        "No internet connection"
      )
    },
    is_online = offline
  )
})

test_that("bsky_search handles empty results", {
  mock_response <- list(
    posts = list(),
    cursor = NULL
  )

  with_mocked_bindings(
    code = {
      result <- bsky_search("nonexistentkeyword", "mock_token")

      expect_type(result, "list")
      expect_length(result$results$posts, 0)
    },
    is_online = online,
    request = mock_request,
    req_url_query = mock_req_url_query,
    req_headers = mock_req_headers,
    req_perform = function(req) mock_req_perform(req, mock_response),
    resp_body_json = function(resp) mock_resp_body_json(resp, mock_response),
    last_response = mock_last_response,
    resp_status = mock_resp_status
  )
})

test_that("bsky_search handles API errors", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_search("covid19", "invalid_token"),
        "HTTP 401 Unauthorized"
      )
    },
    is_online = online,
    request = mock_request,
    req_url_query = mock_req_url_query,
    req_headers = mock_req_headers,
    req_perform = mock_error_req_perform,
    resp_body_json = mock_error_resp_body_json,
    last_response = mock_last_response,
    resp_status = mock_resp_status
  )
})
