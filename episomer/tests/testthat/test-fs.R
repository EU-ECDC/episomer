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


# epifile.move() and the mtime fix (multi-instance services, plan issue 2) ------------------------
#
# Only the local branch is covered: the remote one needs a running fs API. The destination format
# it sends is tested through to_remote_dest() in test-paths.R.

test_that("epifile.move renames a file locally", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  from <- file.path(data_dir, "detect.json")
  to <- file.path(data_dir, "detect.moved.json")
  writeLines("payload", from)

  expect_true(epifile.move(from, to))

  expect_false(file.exists(from))
  expect_identical(readLines(to), "payload")
})

test_that("epifile.move refuses to move across file systems", {
  # it would be a download or an upload followed by a delete, and the fs API cannot delete
  local_conf(data_dir = "/data", fs_remote = "http://example.com:8080")

  expect_error(
    epifile.move("/data/detect.json", "http://example.com:8080/file/detect.json"),
    "same file system"
  )
})

test_that("epifile.mtime answers about each path of a vector", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  existing <- file.path(data_dir, "here.json")
  missing <- file.path(data_dir, "gone.json")
  writeLines("", existing)

  times <- epifile.mtime(c(existing, missing))

  # a POSIXct vector, not numbers: the runner leases compare these values with difftime()
  expect_s3_class(times, "POSIXct")
  expect_length(times, 2)
  expect_false(is.na(times[[1]]))
  expect_true(is.na(times[[2]]))
})
