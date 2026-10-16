# Configuration properties tuning the runner leases (multi-instance services, plan issue 3).
#
# runner_heartbeat_span, runner_ttl_search, runner_ttl_detect, runner_ttl_fs and
# runner_claim_guard must make the whole round trip: default -> conf -> properties.json -> conf,
# and arrive as numbers whichever source they came from. See R/config.R, R/runners-lease.R and
# inst/schema/multi_services_plan.md section 6.5.

# The five properties and the defaults documented in the plan.
runner_props <- list(
  runner_heartbeat_span = 30,
  runner_ttl_search = 300,
  runner_ttl_detect = 1800,
  runner_ttl_fs = 120,
  runner_claim_guard = 5
)

# Defaults --------------------------------------------------------------------------------------

test_that("get_empty_config() carries the runner lease properties as numbers", {
  empty <- get_empty_config(data_dir = "/somewhere")

  for (prop in names(runner_props)) {
    expect_equal(empty[[prop]], runner_props[[prop]], info = prop)
    # arithmetic is done on all of them (difftime comparisons, Sys.sleep), so the type matters
    # as much as the value
    expect_type(empty[[prop]], "double")
  }
})

test_that("the default heartbeat beats several times inside every TTL", {
  # A single missed renewal - a slow fs, a network hiccup - must never expire a healthy lease,
  # which is only true as long as the heartbeat is much shorter than the shortest TTL.
  empty <- get_empty_config(data_dir = "/somewhere")
  ttls <- c(empty$runner_ttl_search, empty$runner_ttl_detect, empty$runner_ttl_fs)

  expect_true(all(ttls >= 3 * empty$runner_heartbeat_span))
})

# Round trip through the configuration ----------------------------------------------------------

test_that("setup_config() puts the runner lease defaults in conf", {
  local_conf()
  data_dir <- withr::local_tempdir()

  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  for (prop in names(runner_props)) {
    expect_equal(conf[[prop]], runner_props[[prop]], info = prop)
  }
})

test_that("save_config() writes the runner lease properties and setup_config() reads them back", {
  # Unlike instance_id / instance_host, these five are shared configuration: every instance must
  # agree on the same TTL, otherwise one machine declares a lease expired while another still
  # believes it is alive.
  data_dir <- withr::local_tempdir()
  local_conf(
    data_dir = data_dir,
    fs_remote = NULL,
    runner_heartbeat_span = 10,
    runner_ttl_search = 60,
    runner_ttl_detect = 900,
    runner_ttl_fs = 45,
    runner_claim_guard = 2
  )

  save_config(properties = TRUE, sm_topics = list())
  written <- jsonlite::read_json(get_properties_path())

  expect_equal(written$runner_ttl_search, 60)
  expect_equal(written$runner_claim_guard, 2)

  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  expect_equal(conf$runner_heartbeat_span, 10)
  expect_equal(conf$runner_ttl_search, 60)
  expect_equal(conf$runner_ttl_detect, 900)
  expect_equal(conf$runner_ttl_fs, 45)
  expect_equal(conf$runner_claim_guard, 2)
})

test_that("the runner lease properties survive a properties.json written by an older version", {
  # runner_lease_ttl() falls back on its own defaults when conf has nothing, so upgrading must not
  # need a hand-edited properties.json.
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  jsonlite::write_json(
    list(collect_span = 42),
    get_properties_path(),
    auto_unbox = TRUE
  )

  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  expect_equal(conf$collect_span, 42)
  expect_equal(conf$runner_ttl_search, runner_props$runner_ttl_search)
  expect_equal(runner_lease_ttl("search.bluesky"), runner_props$runner_ttl_search)
})

# Typing of the EPISOMER_OPTION_* environment ----------------------------------------------------

test_that("coerce_env_props() turns a numeric property into a number", {
  defaults <- list(runner_ttl_search = 300, fs_port = 8080)

  coerced <- coerce_env_props(
    list(runner_ttl_search = "600", fs_port = "9090"),
    defaults
  )

  expect_identical(coerced$runner_ttl_search, 600)
  expect_identical(coerced$fs_port, 9090)
})

test_that("coerce_env_props() turns a boolean property into a logical", {
  defaults <- list(sm_activated_bluesky = TRUE, use_mkl = FALSE)

  coerced <- coerce_env_props(
    list(sm_activated_bluesky = "TRUE", use_mkl = "false"),
    defaults
  )

  expect_identical(coerced$sm_activated_bluesky, TRUE)
  expect_identical(coerced$use_mkl, FALSE)
})

test_that("coerce_env_props() accepts 1 and 0 for a boolean property", {
  # writing a boolean as 1/0 in a compose file is common enough, and as.logical("1") is NA
  coerced <- coerce_env_props(
    list(use_mkl = "1", geonames_simplify = "0"),
    list(use_mkl = FALSE, geonames_simplify = TRUE)
  )

  expect_identical(coerced$use_mkl, TRUE)
  expect_identical(coerced$geonames_simplify, FALSE)
})

test_that("coerce_env_props() leaves alone what it cannot type", {
  defaults <- list(
    fs_url = "http://localhost", # character: nothing to do
    lang_updated_on = NA, # NA is a logical in R, but this one holds a date
    languages = list(list(code = "en")), # a list, unreachable from an env variable
    topics_md5 = ""
  )
  props <- list(
    fs_url = "http://fs",
    lang_updated_on = "2024-01-01 00:00:00",
    languages = "whatever",
    topics_md5 = "d41d8cd98f00b204e9800998ecf8427e",
    completely_unknown = "42" # no default: no type to infer
  )

  expect_identical(coerce_env_props(props, defaults), props)
})

test_that("coerce_env_props() rejects a value it cannot convert", {
  # an unusable EPISOMER_OPTION_* has to be visible at startup, not three hours later in the
  # middle of a loop
  expect_error(
    coerce_env_props(list(runner_ttl_search = "five"), list(runner_ttl_search = 300)),
    "EPISOMER_OPTION_RUNNER_TTL_SEARCH"
  )
  expect_error(
    coerce_env_props(list(use_mkl = "yes"), list(use_mkl = FALSE)),
    "boolean"
  )
})

test_that("an EPISOMER_OPTION_ runner property reaches the lease code as a number", {
  # End to end: environment -> get_env_props() -> coerce_env_props() -> conf -> runner_lease_ttl().
  # The properties file has to exist for setup_config() to read the environment at all (see the
  # merge guard in setup_config), and it must not already carry the property (next test).
  local_conf(data_dir = withr::local_tempdir(), fs_remote = NULL)
  data_dir <- conf$data_dir
  jsonlite::write_json(list(collect_span = 60), get_properties_path(), auto_unbox = TRUE)
  withr::local_envvar(EPISOMER_OPTION_RUNNER_TTL_SEARCH = "600")

  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  # a number, not the string "600": the whole point of coerce_env_props()
  expect_identical(conf$runner_ttl_search, 600)
  expect_identical(runner_lease_ttl("search.bluesky"), 600)
  # the other four keep their defaults
  expect_equal(conf$runner_ttl_detect, runner_props$runner_ttl_detect)
})

test_that("the environment takes precedence over properties.json", {
  # Documented behaviour of the merge in setup_config(): defaults < properties.json < environment.
  # A declarative deployment must be able to change a setting by editing its manifest, even on a
  # data directory that already holds a properties.json written by an earlier start.
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  jsonlite::write_json(
    list(runner_ttl_search = 120, runner_ttl_detect = 130),
    get_properties_path(),
    auto_unbox = TRUE
  )
  withr::local_envvar(EPISOMER_OPTION_RUNNER_TTL_SEARCH = "600")

  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  expect_identical(conf$runner_ttl_search, 600)
  # a property with no variable keeps its stored value
  expect_equal(conf$runner_ttl_detect, 130)
})
