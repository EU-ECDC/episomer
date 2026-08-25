test_that("get_scala_api_endpoint works when remote is null", {
  local_conf(
    "fs_remote" = NULL,
    fs_url = "toto",
    fs_port = 8080
  )
  api <- get_scala_api_endpoint()
  expect_equal(api, "toto:8080")
})


test_that("get_scala_api_endpoint works when remote is not null", {
  local_conf(
    "fs_remote" = "toto:0808"
  )
  api <- get_scala_api_endpoint()
  expect_equal(api, "toto:0808")
})


test_that("get_scala_xxx_url functions work", {
  remote <- "toto:0808"
  local_conf(
    "fs_remote" = remote
  )
  expect_equal(
    get_scala_geotraining_url(),
    paste(remote, "geotraining-set", sep = "/")
  )

  expect_equal(
    get_scala_alert_training_url(),
    paste(remote, "evaluate-alerts", sep = "/")
  )

  expect_equal(
    get_scala_posts_url(),
    paste(remote, "posts", sep = "/")
  )

  expect_equal(
    get_scala_geolocated_posts_url(),
    paste(remote, "geolocated-posts", sep = "/")
  )

  expect_equal(
    get_scala_commit_url(),
    paste(remote, "commit", sep = "/")
  )

  expect_equal(
    get_scala_geolocate_text_url(),
    paste(remote, "geolocate-text", sep = "/")
  )

  expect_equal(
    get_scala_aggregate_url(),
    paste(remote, "aggregate", sep = "/")
  )

  expect_equal(
    get_scala_period_url(),
    paste(remote, "period", sep = "/")
  )

  expect_equal(
    get_scala_status_url(),
    paste(remote, "period?serie=country_counts", sep = "/")
  )

  expect_equal(
    get_scala_ping_url(),
    paste(remote, "ping", sep = "/")
  )

  expect_equal(
    get_scala_recalc_hash_url(),
    paste(remote, "recalculate-hash", sep = "/")
  )

  expect_equal(
    get_scala_file_url("a", "b"),
    paste(remote, "file/a/b", sep = "/")
  )

  expect_equal(
    get_scala_secrets_url(),
    paste(remote, "secret", sep = "/")
  )

  expect_equal(
    get_scala_stopwords_url("123456"),
    paste(remote, "stopWords?code=123456", sep = "/")
  )
})
