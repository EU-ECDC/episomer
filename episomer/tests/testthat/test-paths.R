test_that("fs_remote works", {
  local_conf(
    "fs_remote" = NULL
  )
  expect_false(fs_remote())

  remote <- "toto:0808"
  local_conf(
    "fs_remote" = remote
  )
  expect_true(fs_remote())
})

test_that("epipath works", {
  local_conf(
    "fs_remote" = NULL,
    data_dir = "data_dir"
  )
  expect_equal(
    epipath("some_file.txt"),
    "data_dir/some_file.txt"
  )

  remote <- "toto:0808"
  local_conf(
    "fs_remote" = remote
  )

  expect_equal(
    epipath("some_file.txt"),
    paste(remote, "file/some_file.txt", sep = "/")
  )
})

test_that("is_remote returns FALSE for non-URL paths", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_false(is_remote("/local/path"))
  expect_false(is_remote("relative/path"))
  expect_false(is_remote("file.txt"))
})

test_that("is_remote returns FALSE for non-http(s) URLs", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_false(is_remote("ftp://example.com/file/something"))
  expect_false(is_remote("file:///local/path"))
})

test_that("is_remote throws error when conf$fs_remote is NULL", {
  local_conf("fs_remote" = NULL)

  expect_error(
    is_remote("http://example.com/file/something"),
    "Cannot validate url agains fs_remote"
  )
})

test_that("is_remote returns TRUE for matching remote URLs with file prefix", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_true(is_remote("http://example.com:8080/file/data.json"))
  expect_true(is_remote("http://example.com:8080/file/nested/path/file.txt"))
})

test_that("is_remote returns FALSE when path doesn't start with 'file/'", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_false(is_remote("http://example.com:8080/other/data.json"))
  expect_false(is_remote("http://example.com:8080/data.json"))
})

test_that("is_remote correctly matches without explicit port", {
  local_conf("fs_remote" = "http://example.com")

  expect_true(is_remote("http://example.com/file/data.json"))
})

test_that("is_remote returns FALSE for mismatched domain", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_false(is_remote("http://different.com:8080/file/data.json"))
})

test_that("is_remote returns FALSE for mismatched port", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_false(is_remote("http://example.com:9090/file/data.json"))
})

test_that("is_remote returns FALSE for mismatched scheme", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_false(is_remote("https://example.com:8080/file/data.json"))
})

test_that("is_remote works with https", {
  local_conf("fs_remote" = "https://example.com:443")

  expect_true(is_remote("https://example.com:443/file/secure_data.json"))
  expect_false(is_remote("http://example.com:443/file/secure_data.json"))
})

test_that("to_remote_relative removes 'file' prefix from path", {
  local_conf("fs_remote" = "http://example.com:8080")

  result <- to_remote_relative("http://example.com:8080/file/data.json")
  expect_equal(result, "/data.json")

  result <- to_remote_relative(
    "http://example.com:8080/file/nested/path/file.txt"
  )
  expect_equal(result, "/nested/path/file.txt")
})

test_that("to_remote_dest gives the path the fs API expects in a move or a copy body", {
  local_conf("fs_remote" = "http://example.com:8080")

  # the API resolves the destination against its own epiHome and refuses anything absolute, so the
  # leading slash left by to_remote_relative() has to go
  expect_equal(
    to_remote_dest("http://example.com:8080/file/runners/old/detect.abc.json"),
    "runners/old/detect.abc.json"
  )
  expect_equal(
    to_remote_dest("http://example.com:8080/file/data.json"),
    "data.json"
  )
})

test_that("to_remote_relative throws error for non-remote paths", {
  local_conf("fs_remote" = "http://example.com:8080")

  expect_error(
    to_remote_relative("/local/path"),
    "This function only work with remote episomer paths"
  )

  expect_error(
    to_remote_relative("http://different.com/file/data.json"),
    "This function only work with remote episomer paths"
  )
})


test_that("to_remote_if_needed returns path unchanged when already remote", {
  local_conf(
    "fs_remote" = "http://example.com:8080",
    data_dir = "/data"
  )

  remote_path <- "http://example.com:8080/file/data.json"
  result <- to_remote_if_needed(remote_path)
  expect_equal(result, remote_path)
})

test_that("to_remote_if_needed returns path unchanged when fs_remote disabled", {
  local_conf(
    "fs_remote" = NULL,
    data_dir = "/data"
  )

  expect_false(fs_remote())

  local_path <- "/data/file.txt"
  result <- to_remote_if_needed(local_path)
  expect_equal(result, local_path)
})

test_that("to_remote_if_needed handles paths without data_dir prefix", {
  local_conf(
    "fs_remote" = "http://example.com:8080",
    data_dir = "/data"
  )

  other_path <- "/other/file.txt"
  result <- to_remote_if_needed(other_path)
  # Path without data_dir prefix should be returned unchanged
  expect_equal(result, other_path)
})


test_that("get_alert_training_path returns user path when it exists", {
  local_conf(data_dir = "/data")

  user_path <- "/data/alert-training.xlsx"
  local_mocked_bindings(
    epifile.exists = function(path) identical(path, user_path),
    .package = "episomer"
  )

  expect_equal(get_alert_training_path(), user_path)
})

test_that("get_alert_training_path falls back to default when user path doesn't exist", {
  local_conf(data_dir = "/data")

  local_mocked_bindings(
    epifile.exists = function(path) FALSE,
    .package = "episomer"
  )

  result <- get_alert_training_path()
  expect_match(result, "alert-training.xlsx$")
  expect_match(result, "extdata")
})

test_that("get_geotraining_path returns user path when it exists", {
  local_conf(data_dir = "/data")

  user_path <- "/data/geo-training.xlsx"
  local_mocked_bindings(
    epifile.exists = function(path) identical(path, user_path),
    .package = "episomer"
  )

  expect_equal(get_geotraining_path(), user_path)
})

test_that("get_geotraining_path falls back to default when user path doesn't exist", {
  local_conf(data_dir = "/data")

  local_mocked_bindings(
    epifile.exists = function(path) FALSE,
    .package = "episomer"
  )

  result <- get_geotraining_path()
  expect_match(result, "geo-training.xlsx$")
  expect_match(result, "extdata")
})

test_that("get_topic_keywords_path creates geo folder if needed", {
  local_conf(data_dir = withr::local_tempdir())

  geo_folder <- epipath("geo")

  # Initially folder doesn't exist
  expect_false(dir.exists(geo_folder))

  result <- get_topic_keywords_path()

  # Folder should be created
  expect_true(dir.exists(geo_folder))
  # Result should be correct path
  expect_equal(result, file.path(geo_folder, "topic-keywords.json"))
})

test_that("get_topic_keywords_path doesn't recreate existing geo folder", {
  local_conf(data_dir = withr::local_tempdir())

  geo_folder <- epipath("geo")
  dir.create(geo_folder)

  # Should not error when folder already exists
  result <- get_topic_keywords_path()
  expect_equal(result, file.path(geo_folder, "topic-keywords.json"))
})

test_that("get_alert_file creates alert folders with correct structure", {
  local_conf(data_dir = withr::local_tempdir())

  date <- as.POSIXct("2024-03-15 10:30:00", tz = "UTC")
  result <- get_alert_file(date)

  # Check path structure: alerts/2024/2024.03.15-alerts.json
  expect_match(result, "alerts/2024/2024.03.15-alerts.json$")

  # Verify folders are created
  alert_folder <- epipath("alerts")
  year_folder <- file.path(alert_folder, "2024")
  expect_true(dir.exists(alert_folder))
  expect_true(dir.exists(year_folder))
})

test_that("get_alert_file handles multiple dates correctly", {
  local_conf(data_dir = withr::local_tempdir())

  date1 <- as.POSIXct("2024-03-15", tz = "UTC")
  date2 <- as.POSIXct("2025-12-31", tz = "UTC")

  result1 <- get_alert_file(date1)
  result2 <- get_alert_file(date2)

  expect_match(result1, "2024.03.15-alerts.json$")
  expect_match(result2, "2025.12.31-alerts.json$")

  # Both years should be created
  expect_true(dir.exists(file.path(epipath("alerts"), "2024")))
  expect_true(dir.exists(file.path(epipath("alerts"), "2025")))
})

test_that("get_plans_paths returns named vector for active social media", {
  local_mocked_bindings(
    active_social_media = function() c("mastodon.social", "bluesky.net"),
    .package = "episomer"
  )
  local_conf(data_dir = "/data")

  result <- get_plans_paths()

  expect_length(result, 2)
  expect_named(result, c("mastodon.social", "bluesky.net"))
  expect_match(result[[1]], "mastodon\\.social\\.json$")
  expect_match(result[[2]], "bluesky\\.net\\.json$")
})

test_that("get_plans_paths returns character(0) when no active social media", {
  local_mocked_bindings(
    active_social_media = function() character(0),
    .package = "episomer"
  )
  local_conf(data_dir = "/data")

  result <- get_plans_paths()

  expect_length(result, 0)
  expect_equal(result, character(0))
})

test_that("get_plans_paths substitutes .json extension correctly", {
  local_mocked_bindings(
    active_social_media = function() c("mastodon", "bluesky"),
    .package = "episomer"
  )
  local_conf(data_dir = "/data")

  result <- get_plans_paths()

  # Should replace .json with .sm.json
  expect_match(result[["mastodon"]], "topics\\.mastodon\\.json$")
  expect_match(result[["bluesky"]], "topics\\.bluesky\\.json$")
})
