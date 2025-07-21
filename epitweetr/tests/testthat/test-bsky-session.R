test_that("bsky_create_session works with valid credentials", {
  mock_response <- list(
    handle = "test.bsky.app",
    accessJwt = "mock_access_token_123",
    did = "did:plc:mock123",
    refreshJwt = "mock_refresh_token_123"
  )

  with_mocked_bindings(
    code = {
      result <- bsky_create_session("test.bsky.app", "password123")

      expect_type(result, "list")
      expect_named(
        result,
        c("handle", "access_jwt", "did", "refreshJwt", "created")
      )
      expect_equal(result$handle, "test.bsky.app")
      expect_equal(result$access_jwt, "mock_access_token_123")
      expect_equal(result$did, "did:plc:mock123")
      expect_equal(result$refreshJwt, "mock_refresh_token_123")
    },
    is_online = online,
    request = mock_request,
    req_body_json = mock_req_body_json,
    req_perform = function(req) mock_req_perform(req, mock_response),
    resp_check_status = mock_resp_check_status,
    resp_body_json = function(resp) mock_resp_body_json(resp, mock_response)
  )
})

test_that("bsky_create_session fails with no internet connection", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_create_session("test.bsky.app", "password123"),
        "No internet connection"
      )
    },
    is_online = offline
  )
})

test_that("bsky_create_session fails with missing handle", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_create_session(handle = NULL, password = "password123"),
        "Handle and password are required"
      )

      expect_error(
        bsky_create_session(handle = "", password = "password123"),
        "Handle is required"
      )
    },
    is_online = online
  )
})

test_that("bsky_create_session fails with missing password", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_create_session(handle = "test.bsky.app", password = NULL),
        "Handle and password are required"
      )

      expect_error(
        bsky_create_session(handle = "test.bsky.app", password = ""),
        "Password is required"
      )
    },
    is_online = online
  )
})

test_that("bsky_create_session fails with empty login URL", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_create_session(
          "test.bsky.app",
          "password123",
          login_url = ""
        ),
        "Login URL is required"
      )
    },
    is_online = online
  )
})

test_that("bsky_create_session handles API errors", {
  with_mocked_bindings(
    code = {
      expect_error(
        bsky_create_session("test.bsky.app", "wrongpassword"),
        "HTTP 401 Unauthorized"
      )
    },
    is_online = online,
    request = mock_request,
    req_body_json = mock_req_body_json,
    req_perform = mock_error_req_perform,
    resp_check_status = function(resp) {
      stop("HTTP 401 Unauthorized")
    }
  )
})

unlink("bsky_session.rds")
