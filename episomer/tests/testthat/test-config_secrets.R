# The remote branch of is_secret_set(), get_secret() and set_secret(), i.e. the one taken when the
# secrets live behind the fs API rather than in a local keyring (R/config.R).
#
# What these tests are about is the *error* path. The API runs an Rscript sub process to read the
# keyring and, when that process fails, answers 406 with the process stderr in the body
# (API.scala, "Could not get the secret exit(<code>):<err>"). That body is the only place the real
# cause is written - a wrong EPISOMER_KR_PASSWORD, an Rscript missing from the PATH of the API
# process, a keyring file that cannot be read - and it used to be dropped: jsonlite::read_json()
# reads through url(), which reports "cannot open the connection ... HTTP status was '406 Not
# Acceptable'" and nothing else, while printing the passphrase in clear as part of the URL.

local_secrets_conf <- function(.local_envir = parent.frame()) {
  local_conf(fs_remote = "http://127.0.0.1:8080", .local_envir = .local_envir)
  withr::local_envvar(
    EPISOMER_KR_PASSWORD = "a-passphrase",
    EPISOMER_KR_PASSWORD_FILE = "",
    .local_envir = .local_envir
  )
}

# The API completes with a JsObject turned into a String, so the answer is JSON in a text/plain
# body - hence the parsing on the string rather than resp_body_json().
api_response <- function(body, status_code = 200) {
  httr2::response(status_code = status_code, body = charToRaw(body))
}

local_mocked_api <- function(body, status_code = 200, .local_envir = parent.frame()) {
  httr2::local_mocked_responses(
    list(api_response(body, status_code = status_code)),
    env = .local_envir
  )
}

test_that("is_secret_set asks the fs API and reads its status", {
  local_secrets_conf()
  local_mocked_api('{"status":"ok","value":"s3cr3t","code":0}')

  expect_true(is_secret_set("smtp_password"))
})

test_that("is_secret_set is false when the fs API knows no such secret", {
  local_secrets_conf()
  local_mocked_api('{"status":"ko","message":"Not found\\n","code":0}')

  expect_false(is_secret_set("smtp_password"))
})

test_that("get_secret returns the value the fs API answers", {
  local_secrets_conf()
  local_mocked_api('{"status":"ok","value":"s3cr3t","code":0}')

  expect_identical(get_secret("smtp_password"), "s3cr3t")
})

test_that("get_secret sends the secret name and the passphrase as query parameters", {
  local_secrets_conf()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    api_response('{"status":"ok","value":"s3cr3t","code":0}')
  })

  get_secret("smtp_password")

  expect_match(seen$url, "^http://127\\.0\\.0\\.1:8080/secret\\?")
  expect_match(seen$url, "secret=smtp_password")
  expect_match(seen$url, "passphrase=a-passphrase")
})

test_that("a passphrase needing escaping reaches the fs API encoded", {
  # req_url_query() encodes it: the previous sprintf() left that to an explicit url_encode() call,
  # and the get_secret()/set_secret() sides have to agree or the keyring never unlocks
  local_conf(fs_remote = "http://127.0.0.1:8080")
  withr::local_envvar(EPISOMER_KR_PASSWORD = "a b&c=d", EPISOMER_KR_PASSWORD_FILE = "")
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    api_response('{"status":"ok","value":"s3cr3t","code":0}')
  })

  get_secret("smtp_password")

  expect_match(seen$url, "passphrase=a(%20|\\+)b%26c%3Dd")
})

# The point of the change ------------------------------------------------------------------------

test_that("a failure of the fs API reports what its sub process wrote on stderr", {
  local_secrets_conf()
  local_mocked_api(
    status_code = 406,
    body = paste(
      "Could not get the secret exit(1):Error in kb$keyring_unlock(keyring = kr_name) :",
      "cannot unlock keyring, wrong password"
    )
  )

  expect_error(get_secret("smtp_password"), "wrong password")
})

test_that("is_secret_set reports it too, since it is the call setup() makes first", {
  # setup() reads the smtp password through is_secret_set() before get_secret() (R/config.R), so a
  # locked keyring surfaces here and nowhere else
  local_secrets_conf()
  local_mocked_api(
    status_code = 406,
    body = "Could not get the secret exit(1):Error: cannot unlock keyring, wrong password"
  )

  expect_error(is_secret_set("smtp_password"), "wrong password")
})

test_that("the passphrase stays out of the error of a failed call", {
  # it used to be printed in clear, as part of the URL url() puts in its message, on any console
  # and in any screenshot of one
  local_secrets_conf()
  local_mocked_api(
    status_code = 406,
    body = "Could not get the secret exit(1):Error: cannot unlock keyring"
  )

  err <- expect_error(get_secret("smtp_password"))

  expect_false(grepl("a-passphrase", conditionMessage(err), fixed = TRUE))
})

test_that("get_secret names the reason when the fs API answers ko", {
  # the sub process ran and exited 0, but read nothing - get_secret.R writes "Not found" on stderr
  # and the API passes it on as a message
  local_secrets_conf()
  local_mocked_api('{"status":"ko","message":"Not found\\n","code":0}')

  expect_error(get_secret("smtp_password"), "Not found")
})

# Writing ----------------------------------------------------------------------------------------

test_that("set_secret POSTs the secret, the passphrase and the value", {
  local_secrets_conf()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    api_response('{"status":"ok","code":0}')
  })

  set_secret("smtp_password", "s3cr3t")

  expect_identical(seen$method, "POST")
  expect_match(seen$url, "^http://127\\.0\\.0\\.1:8080/secret\\?")
  expect_match(seen$url, "secret=smtp_password")
  expect_match(seen$url, "passphrase=a-passphrase")
  expect_match(seen$url, "value=s3cr3t")
})

test_that("a value needing escaping reaches the fs API encoded", {
  # bluesky_session is stored as JSON (R/sm-bluesky-api.R), so the value carries braces, quotes and
  # the odd "&" of a token - all of which would end the parameter early unencoded
  local_secrets_conf()
  seen <- NULL
  httr2::local_mocked_responses(function(req) {
    seen <<- req
    api_response('{"status":"ok","code":0}')
  })

  set_secret("bluesky_session", '{"a":"b&c"}')

  expect_match(seen$url, "value=%7B%22a%22%3A%22b%26c%22%7D")
})

test_that("a failure of the write reports what the sub process wrote on stderr", {
  local_secrets_conf()
  local_mocked_api(
    status_code = 406,
    body = "Could not set the secret exit(1):Error: cannot unlock keyring, wrong password"
  )

  expect_error(set_secret("smtp_password", "s3cr3t"), "wrong password")
})

test_that("the passphrase stays out of the error of a failed write", {
  local_secrets_conf()
  local_mocked_api(
    status_code = 406,
    body = "Could not set the secret exit(1):Error: cannot unlock keyring"
  )

  err <- expect_error(set_secret("smtp_password", "s3cr3t"))

  expect_false(grepl("a-passphrase", conditionMessage(err), fixed = TRUE))
})

test_that("set_secret names the reason when the fs API answers ko", {
  local_secrets_conf()
  local_mocked_api('{"status":"ko","message":"Not found\\n","code":0}')

  expect_error(set_secret("smtp_password", "s3cr3t"), "Not found")
})

test_that("set_secret says so when the fs API answers ko without a reason", {
  local_secrets_conf()
  local_mocked_api('{"status":"ko","code":0}')

  expect_error(set_secret("smtp_password", "s3cr3t"), "no reason given by the fs API")
})
