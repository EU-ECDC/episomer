local_cached_clean <- function(.local_envir = parent.frame()) {
  saved <- as.list(cached, all.names = TRUE)
  withr::defer(
    {
      rm(list = ls(cached, all.names = TRUE), envir = cached)
      list2env(saved, envir = cached)
    },
    envir = .local_envir
  )
  rm(list = ls(cached, all.names = TRUE), envir = cached)
}

fake_items <- function() {
  list(
    list(name = "Europe", code = "EU", codes = c("FR", "DE"), level = 1, minLat = 35, maxLat = 70, minLong = -10, maxLong = 40),
    list(name = "France", code = "FR", codes = "FR", level = 3, minLat = 41, maxLat = 51, minLong = -5, maxLong = 9),
    list(name = "Germany", code = "DE", codes = "DE", level = 3, minLat = 47, maxLat = 55, minLong = 6, maxLong = 15)
  )
}

test_that("get_location_var coalesces the text and quoted text columns", {
  expect_equal(
    get_location_var("geo_code"),
    "coalesce(text_loc.geo_code, quoted_text_loc.geo_code)"
  )
})

test_that("get_location_columns lists the two location structs", {
  expect_equal(get_location_columns("any"), list("text_loc", "quoted_text_loc"))
})

test_that("drop_country_items_cache only removes the country items entries", {
  local_cached_clean()
  cached$country_items_level <- 1
  cached$country_items_name <- 2
  cached$other <- 3
  expect_null(drop_country_items_cache())
  expect_equal(ls(cached), "other")
  expect_null(drop_country_items_cache())
})

test_that("country maps are built from the country items", {
  testthat::local_mocked_bindings(get_country_items = function(...) fake_items(), .package = "episomer")
  expect_equal(get_country_code_map(), c(FR = "France", DE = "Germany"))
  expect_equal(get_country_codes_by_name(), c(Europe = "EU", France = "FR", Germany = "DE"))
  expect_equal(get_country_index_map(), c(FR = 2L, DE = 3L))
})

test_that("get_regions_df describes every item", {
  testthat::local_mocked_bindings(get_country_items = function(...) fake_items(), .package = "episomer")
  df <- get_regions_df()
  expect_equal(df$Name, c("Europe", "France", "Germany"))
  expect_equal(df$Codes, c("FR, DE", "FR", "DE"))
  expect_equal(df$Level, c(1, 3, 3))
  expect_equal(df$MinLatitude, c(35, 41, 47))
  expect_equal(df$MaxLingityde, c(40, 9, 15))
})

test_that("get_stop_words adds the fixed words, drops quotes and backslashes, and caches", {
  local_cached_clean()
  calls <- 0
  testthat::local_mocked_bindings(
    get_scala_stopwords_url = function(code) paste0("http://fs/stop/", code),
    .package = "episomer"
  )
  testthat::local_mocked_bindings(
    read_json = function(...) {
      calls <<- calls + 1
      list("the", "don't", "a\\b", "of")
    },
    .package = "jsonlite"
  )
  words <- get_stop_words("en")
  expect_equal(words, c("rt", "RT", "http", "https", "t.co", "amp", "via", "the", "of"))
  expect_equal(get_stop_words("en"), words)
  expect_equal(calls, 1)
})

test_that("get_stop_words returns an empty vector, uncached, for an unknown language", {
  local_cached_clean()
  testthat::local_mocked_bindings(
    get_scala_stopwords_url = function(code) "http://fs/stop",
    .package = "episomer"
  )
  testthat::local_mocked_bindings(read_json = function(...) list(), .package = "jsonlite")
  expect_identical(get_stop_words("xx"), character(0))
  expect_false(exists("stop_words_xx", where = cached))
})

# A data directory where the language "en" has its vectors, the training set and its evaluation, the
# evaluation being the newest: nothing to retrain.
local_trained_dir <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  local_conf(data_dir = dir, fs_remote = NULL, .local_envir = .local_envir)
  dir.create(file.path(dir, "languages"))
  vectors <- get_lang_vectors_path("en")
  conf$languages <- list(list(code = "en", vectors = vectors))
  for (p in c(vectors, get_user_geotraining_path(), get_geotraining_evaluation_path())) {
    file.create(p)
  }
  now <- Sys.time()
  Sys.setFileTime(vectors, now - 300)
  Sys.setFileTime(get_user_geotraining_path(), now - 200)
  Sys.setFileTime(get_geotraining_evaluation_path(), now - 100)
  dir
}

test_that("get_lang_traing_need is FALSE when the evaluation is the newest file", {
  local_trained_dir()
  expect_false(get_lang_traing_need())
})

test_that("get_lang_traing_need is TRUE when vectors are missing", {
  local_trained_dir()
  file.remove(get_lang_vectors_path("en"))
  expect_true(get_lang_traing_need())
})

test_that("get_lang_traing_need is TRUE when the evaluation is missing", {
  local_trained_dir()
  file.remove(get_geotraining_evaluation_path())
  expect_true(get_lang_traing_need())
})

test_that("get_lang_traing_need is TRUE when the training set is newer than the evaluation", {
  local_trained_dir()
  Sys.setFileTime(get_user_geotraining_path(), Sys.time())
  expect_true(get_lang_traing_need())
})

test_that("get_lang_traing_need is TRUE when the vectors are newer than the evaluation", {
  local_trained_dir()
  Sys.setFileTime(get_lang_vectors_path("en"), Sys.time())
  expect_true(get_lang_traing_need())
})

test_that("mark_geotraining_evaluated touches an existing evaluation and never creates one", {
  local_trained_dir()
  eval_path <- get_geotraining_evaluation_path()
  before <- file.mtime(eval_path)
  Sys.setFileTime(get_user_geotraining_path(), Sys.time())
  expect_true(get_lang_traing_need())
  mark_geotraining_evaluated()
  expect_gt(file.mtime(eval_path), before)
  expect_false(get_lang_traing_need())

  file.remove(eval_path)
  mark_geotraining_evaluated()
  expect_false(file.exists(eval_path))
})
