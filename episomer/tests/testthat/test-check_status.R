test_that("is_64bit reads the architecture", {
  skip_on_os("windows")
  expect_type(is_64bit(), "logical")
  testthat::local_mocked_bindings(system = function(...) "Linux host 5.15 x86_64 GNU/Linux", .package = "base")
  expect_true(is_64bit())
  testthat::local_mocked_bindings(system = function(...) "Linux host 5.15 i686 GNU/Linux", .package = "base")
  expect_false(is_64bit())
})

test_that("check_pandoc and check_tex follow the binaries on the path", {
  testthat::local_mocked_bindings(Sys.which = function(x) c(x = ""), .package = "base")
  expect_warning(expect_false(check_pandoc()), "Pandoc must be installed")
  expect_warning(expect_false(check_tex()), "tex distribution")
  testthat::local_mocked_bindings(Sys.which = function(x) c(x = "/usr/bin/x"), .package = "base")
  expect_true(check_pandoc())
  expect_true(check_tex())
})

test_that("check_scheduler is only a concern on Windows", {
  skip_on_os("windows")
  expect_true(check_scheduler())
})

test_that("service checks follow the status of their service", {
  testthat::local_mocked_bindings(
    is_search_running = function() FALSE,
    is_detect_running = function() FALSE,
    is_fs_running = function() FALSE,
    .package = "episomer"
  )
  expect_warning(expect_false(check_search_running()), "Data collection")
  expect_warning(expect_false(check_detect_running()), "Requirements & alerts")
  expect_warning(expect_false(check_fs_running()), "Embedded database")
  testthat::local_mocked_bindings(
    is_search_running = function() TRUE,
    is_detect_running = function() TRUE,
    is_fs_running = function() TRUE,
    .package = "episomer"
  )
  expect_true(check_search_running())
  expect_true(check_detect_running())
  expect_true(check_fs_running())
})

test_that("check_posts_present depends on the last post update", {
  testthat::local_mocked_bindings(last_fs_updates = function(...) NA, .package = "episomer")
  expect_warning(expect_false(check_posts_present()), "No posts found")
  testthat::local_mocked_bindings(last_fs_updates = function(...) Sys.time(), .package = "episomer")
  expect_true(check_posts_present())
})

test_that("check_java_deps compares the downloaded jars with the expected list", {
  dir <- withr::local_tempdir()
  jars <- file.path(dir, "jars")
  deps <- file.path(dir, "deps.txt")
  testthat::local_mocked_bindings(
    get_jars_dest_path = function() jars,
    get_sbt_file_dep_path = function() deps,
    .package = "episomer"
  )
  expect_warning(expect_false(check_java_deps()), "dependencies task")
  dir.create(jars)
  writeLines(c("a.jar", "b.jar"), deps)
  file.create(file.path(jars, "a.jar"))
  expect_warning(expect_false(check_java_deps()), "dependencies task")
  file.create(file.path(jars, "b.jar"))
  expect_true(check_java_deps())
})

test_that("check_geonames needs the unzipped file and a built index", {
  dir <- withr::local_tempdir()
  txt <- file.path(dir, "allCountries.txt")
  index <- file.path(dir, "geonames")
  testthat::local_mocked_bindings(
    get_geonames_txt_path = function() txt,
    get_geonames_index_path = function() index,
    .package = "episomer"
  )
  expect_warning(expect_false(check_geonames()), "downloaded and unzipped")
  file.create(txt)
  expect_warning(expect_false(check_geonames()), "not been indexed")
  dir.create(file.path(index, "lucIndex1"), recursive = TRUE)
  expect_warning(expect_false(check_geonames()), "not been indexed")
  file.create(file.path(index, "lucIndex1", "_0.cfs"))
  expect_true(check_geonames())
})

test_that("check_languages needs the vectors of every language and a built index", {
  dir <- withr::local_tempdir()
  vectors <- file.path(dir, "en.txt.gz")
  index <- file.path(dir, "langindex")
  local_conf(fs_remote = NULL, languages = list(list(code = "en", vectors = vectors)))
  testthat::local_mocked_bindings(
    to_remote_if_needed = function(p) p,
    get_lang_index_path = function() index,
    .package = "episomer"
  )
  expect_warning(expect_false(check_languages()), "languages models")
  conf$languages <- list()
  expect_warning(expect_false(check_languages()), "languages models")
  conf$languages <- list(list(code = "en", vectors = vectors))
  file.create(vectors)
  expect_warning(expect_false(check_languages()), "index has not been built")
  dir.create(file.path(index, "lucIndex1"), recursive = TRUE)
  file.create(file.path(index, "lucIndex1", "_0.cfs"))
  expect_true(check_languages())
})

test_that("check_alerts_present looks for a json file in the last year folder", {
  dir <- local_lease_conf()
  expect_warning(expect_false(check_alerts_present()), "No alert files")
  dir.create(file.path(dir, "alerts", "2026"), recursive = TRUE)
  expect_warning(expect_false(check_alerts_present()), "No alert files")
  writeLines("{}", file.path(dir, "alerts", "2026", "alerts_2026-01-01.json"))
  expect_true(check_alerts_present())
})
