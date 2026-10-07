# EPISOMER_LANGUAGES is the one way a containerised deployment can change the language list: the
# wizard runs with silent = TRUE there and would otherwise keep whatever properties.json holds.
# See apply_env_languages() in R/config.R.

# A conf holding `codes` as its language list, in a throwaway data directory. save_config() is
# called by apply_env_languages(), so the directory has to be a real writable one.
local_lang_conf <- function(codes, .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  dir.create(file.path(dir, "languages"))
  local_conf(
    data_dir = dir,
    fs_remote = NULL,
    languages = lapply(codes, function(code) {
      list(
        code = code,
        # The names languages.xlsx carries for these codes, i.e. what the wizard would have
        # written: apply_env_languages() leaves the languages it keeps untouched.
        name = c(en = "English", fr = "French")[[code]],
        vectors = file.path(dir, "languages", paste0(code, ".txt.gz")),
        modified_on = "2026-01-01 00:00:00"
      )
    }),
    .local_envir = .local_envir
  )
  dir
}

conf_codes <- function() unlist(lapply(conf$languages, `[[`, "code"))

test_that("an unset or empty variable leaves the configured languages alone", {
  local_lang_conf(c("en", "fr"))
  expect_false(apply_env_languages(""))
  expect_equal(conf_codes(), c("en", "fr"))
})

test_that("a variable set to separators only is an error, not a silent reset", {
  local_lang_conf(c("en", "fr"))
  expect_error(apply_env_languages("  ;  "), "lists no language")
  expect_equal(conf_codes(), c("en", "fr"))
})

test_that("a list matching the configuration is a no-op", {
  local_lang_conf(c("en", "fr"))
  conf$lang_updated_on <- "2026-01-01 00:00:00"
  expect_message(
    expect_false(apply_env_languages("FR;en")),
    "nothing to do"
  )
  expect_equal(conf_codes(), c("en", "fr"))
  # Bumping this would make setup() offer a model retraining at every container start.
  expect_equal(conf$lang_updated_on, "2026-01-01 00:00:00")
})

test_that("languages are added, removed and ordered as the variable spells them", {
  local_lang_conf(c("en", "fr"))
  expect_true(suppressMessages(apply_env_languages("fr;es;pt")))
  expect_equal(conf_codes(), c("fr", "es", "pt"))
  # The names come from languages.xlsx, not from the variable.
  expect_equal(
    unlist(lapply(conf$languages, `[[`, "name")),
    c("French", "Spanish", "Portuguese")
  )
  # And the added ones point at a vectors path under the data directory, so the wizard downloads
  # them on the same run.
  expect_true(all(grepl(
    file.path(conf$data_dir, "languages"),
    unlist(lapply(conf$languages, `[[`, "vectors")),
    fixed = TRUE
  )))
})

test_that("removing a language deletes its local files", {
  dir <- local_lang_conf(c("en", "fr"))
  vectors <- file.path(dir, "languages", "fr.txt.gz")
  writeLines("vectors", vectors)
  writeLines("model", sprintf("%s.model.I.txt", vectors))
  expect_true(suppressMessages(apply_env_languages("en")))
  expect_equal(conf_codes(), "en")
  expect_false(file.exists(vectors))
  expect_false(file.exists(sprintf("%s.model.I.txt", vectors)))
})

test_that("the new list is persisted to properties.json", {
  dir <- local_lang_conf(c("en", "fr"))
  suppressMessages(apply_env_languages("es"))
  saved <- jsonlite::read_json(file.path(dir, "properties.json"))
  expect_equal(unlist(lapply(saved$languages, `[[`, "code")), "es")
})

test_that("an unknown code is an error naming it, and changes nothing", {
  local_lang_conf(c("en", "fr"))
  expect_error(apply_env_languages("fr;xx"), "unknown language code\\(s\\) xx")
  expect_equal(conf_codes(), c("en", "fr"))
})

test_that("the variable is read from the environment by default", {
  local_lang_conf(c("en", "fr"))
  withr::local_envvar(EPISOMER_LANGUAGES = "es")
  expect_true(suppressMessages(apply_env_languages()))
  expect_equal(conf_codes(), "es")
})
