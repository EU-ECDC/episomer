# Tests for get_app_jar_path() (R/paths.R) and check_jar_version() (R/check.R).
#
# The embedded jar filename now carries its own version (ecdc-episomer-bundle_2.13-<version>.jar,
# set from ../episomer/DESCRIPTION at `sbt package` time), so get_app_jar_path() locates it by
# pattern instead of a hardcoded name, and check_jar_version() compares that embedded version
# against the R package version to catch a stale/mismatched jar left by a partial rebuild.
#
# check_jar_version() feeds into setup_can_run() (R/check.R:586) the same way check_java_version()
# does: any warning it raises is turned into FALSE by that function's tryCatch.

# the warning message of a call that is expected to warn
warning_of <- function(expr) {
  tryCatch(expr, warning = function(w) conditionMessage(w))
}

test_that("get_app_jar_path finds a jar matching the versioned pattern", {
  java_dir <- withr::local_tempdir()
  jar_path <- file.path(java_dir, "ecdc-episomer-bundle_2.13-4.1.1.jar")
  file.create(jar_path)

  local_mocked_bindings(
    get_java_dir_path = function() java_dir,
    .package = "episomer"
  )

  expect_equal(get_app_jar_path(), jar_path)
})

test_that("get_app_jar_path ignores files that don't match the bundle pattern", {
  java_dir <- withr::local_tempdir()
  file.create(file.path(java_dir, "README"))
  file.create(file.path(java_dir, "some-other-lib.jar"))

  local_mocked_bindings(
    get_java_dir_path = function() java_dir,
    .package = "episomer"
  )

  expect_equal(get_app_jar_path(), "")
})

test_that("get_app_jar_path returns empty string when no jar is present", {
  java_dir <- withr::local_tempdir()

  local_mocked_bindings(
    get_java_dir_path = function() java_dir,
    .package = "episomer"
  )

  expect_equal(get_app_jar_path(), "")
})

test_that("check_jar_version returns TRUE when jar version matches package version", {
  pkg_version <- as.character(utils::packageVersion("episomer"))
  jar_path <- file.path(
    withr::local_tempdir(),
    sprintf("ecdc-episomer-bundle_2.13-%s.jar", pkg_version)
  )
  file.create(jar_path)

  local_mocked_bindings(
    get_app_jar_path = function() jar_path,
    .package = "episomer"
  )

  expect_true(check_jar_version())
})

test_that("check_jar_version warns and returns FALSE when jar version doesn't match", {
  jar_path <- file.path(withr::local_tempdir(), "ecdc-episomer-bundle_2.13-0.0.1.jar")
  file.create(jar_path)

  local_mocked_bindings(
    get_app_jar_path = function() jar_path,
    .package = "episomer"
  )

  msg <- warning_of(check_jar_version())
  expect_match(msg, "does not match the package version")
  expect_false(suppressWarnings(check_jar_version()))
})

test_that("check_jar_version warns and returns FALSE when the jar file is missing", {
  local_mocked_bindings(
    get_app_jar_path = function() file.path(withr::local_tempdir(), "ecdc-episomer-bundle_2.13-1.0.jar"),
    .package = "episomer"
  )

  msg <- warning_of(check_jar_version())
  expect_match(msg, "could not find the embedded java bundle jar")
  expect_false(suppressWarnings(check_jar_version()))
})

test_that("check_jar_version warns and returns FALSE when get_app_jar_path finds nothing", {
  local_mocked_bindings(
    get_app_jar_path = function() "",
    .package = "episomer"
  )

  msg <- warning_of(check_jar_version())
  expect_match(msg, "could not find the embedded java bundle jar")
  expect_false(suppressWarnings(check_jar_version()))
})
