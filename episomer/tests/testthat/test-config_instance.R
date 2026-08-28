# Identity of the running instance (multi-instance services, plan issue 1).
# conf$instance_id and conf$instance_host tell "this runner is me" from "this runner is somebody
# else" once several machines share the same data directory. See R/config.R and
# inst/schema/multi_services_plan.md.

test_that("new_instance_id() gives a different identifier on each call", {
  ids <- replicate(20, new_instance_id())

  expect_length(unique(ids), 20)
  expect_true(all(nzchar(ids)))
})

test_that("new_instance_id() does not depend on, nor consume, the random seed", {
  # Two instances started from the same script - hence possibly from the same set.seed() - must
  # not end up sharing an identity, and generating one must not shift the results of a script
  # that seeded the generator on purpose.
  set.seed(1)
  first <- new_instance_id()
  seed_after_first <- .Random.seed

  set.seed(1)
  second <- new_instance_id()

  expect_false(identical(first, second))
  expect_identical(.Random.seed, seed_after_first)
})

test_that("get_instance_host() prefers EPISOMER_INSTANCE over the system host name", {
  withr::local_envvar(EPISOMER_INSTANCE = "machine-B")

  expect_identical(get_instance_host(), "machine-B")
})

test_that("get_instance_host() falls back to the system host name", {
  # NA unsets the variable for the duration of the test
  withr::local_envvar(EPISOMER_INSTANCE = NA)

  expect_identical(get_instance_host(), Sys.info()[["nodename"]])

  # an empty value is treated as unset too: docker passes "" for a variable declared but not filled
  withr::local_envvar(EPISOMER_INSTANCE = "")

  expect_identical(get_instance_host(), Sys.info()[["nodename"]])
})

test_that("setup_config() keeps the same instance_id across calls in one session", {
  local_conf()
  withr::local_envvar(EPISOMER_INSTANCE = "machine-B")
  data_dir <- withr::local_tempdir()

  # ignore_* keeps the call to the identity part only: no properties file, no topics, no keyring
  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_properties = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )
  first <- conf$instance_id

  expect_true(nzchar(first))
  expect_identical(conf$instance_host, "machine-B")

  # setup_config() is called again on every configuration refresh: a new identity here would make
  # a running loop look like a different instance and lose the runner lease it holds
  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_properties = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  expect_identical(conf$instance_id, first)
})

test_that("setup_config() refreshes instance_host on each call", {
  local_conf()
  data_dir <- withr::local_tempdir()
  setup <- function() {
    setup_config(
      data_dir = data_dir,
      ignore_keyring = TRUE,
      ignore_properties = TRUE,
      ignore_topics = TRUE,
      fs_remote = NULL
    )
  }

  withr::with_envvar(c(EPISOMER_INSTANCE = "machine-B"), setup())
  expect_identical(conf$instance_host, "machine-B")

  # unlike the identifier, the host name is only informational, so a deployment may fix a wrong
  # EPISOMER_INSTANCE without restarting the process
  withr::with_envvar(c(EPISOMER_INSTANCE = "machine-C"), setup())
  expect_identical(conf$instance_host, "machine-C")
})

test_that("save_config() does not write the instance identity to properties.json", {
  # properties.json is shared by every machine using the same data directory: writing an identity
  # there would hand this process' identity to all the others.
  data_dir <- withr::local_tempdir()
  local_conf(
    data_dir = data_dir,
    fs_remote = NULL,
    instance_id = "an-identifier",
    instance_host = "machine-B",
    collect_span = 60
  )

  save_config(properties = TRUE, sm_topics = list())
  written <- jsonlite::read_json(get_properties_path())

  expect_false("instance_id" %in% names(written))
  expect_false("instance_host" %in% names(written))
  expect_equal(written$collect_span, 60)
})

test_that("setup_config() ignores an instance identity found in properties.json", {
  # symmetric protection: a file written by an older version, or edited by hand, must not
  # overwrite the identity of the running process
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  jsonlite::write_json(
    list(
      instance_id = "somebody-else",
      instance_host = "machine-C",
      collect_span = 42,
      # merge_configs() simplifies its result when every merged value is a length-1 scalar, which
      # turns the numbers into strings. A real properties.json always holds list properties
      # (languages, known_users), so keep one here to stay on the realistic path.
      known_users = list("someone")
    ),
    get_properties_path(),
    auto_unbox = TRUE
  )

  setup_config(
    data_dir = data_dir,
    ignore_keyring = TRUE,
    ignore_topics = TRUE,
    fs_remote = NULL
  )

  expect_false(identical(conf$instance_id, "somebody-else"))
  expect_false(identical(conf$instance_host, "machine-C"))
  # the legitimate properties of the file were read though
  expect_equal(conf$collect_span, 42)
})
