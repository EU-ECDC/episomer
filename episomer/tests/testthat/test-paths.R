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
