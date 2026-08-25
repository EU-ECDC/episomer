test_that("supported_social_media works", {
  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = FALSE
  )

  expect_setequal(supported_social_media(), c("bluesky"))

  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE
  )

  expect_setequal(supported_social_media(), c("bluesky"))

  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    mastodon_instances = c("toto", "titi")
  )
  expect_setequal(
    supported_social_media(),
    c("bluesky", "mastodon.titi", "mastodon.toto")
  )
})


test_that("social_media_families lists the families having an activation switch", {
  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = FALSE
  )

  expect_setequal(social_media_families(), c("bluesky", "mastodon"))
})

test_that("social_media_families ignores the other configuration properties", {
  local_conf(
    data_dir = "/tmp/episomer",
    sm_activated_mastodon = TRUE,
    sm_alerts_mastodon = TRUE,
    mastodon_instances = list("mstdn.party")
  )

  expect_equal(social_media_families(), "mastodon")
})

test_that("social_media_families returns character(0) on a configuration without social media", {
  local_conf(data_dir = "/tmp/episomer")

  expect_identical(social_media_families(), character(0))
})


test_that("social_media_families lists the instances of families having an activation switch", {
  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    "mastodon_instances" = list(
      "mastodon.social",
      "mstdn.party"
    )
  )
  sm_instances_of(family = "bluesky")
  sm_instances_of(family = "mastodon")
  expect_identical(sm_instances_of(family = "bluesky"), character(0))
  expect_identical(
    sm_instances_of(family = "mastodon"),
    c("mastodon.social", "mstdn.party")
  )

  local_conf(
    sm_activated_mastodon = FALSE
  )

  expect_identical(sm_instances_of(family = "mastodon"), character(0))
})

test_that("active_social_media works", {
  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    "mastodon_instances" = list(
      "mastodon.social",
      "mstdn.party"
    )
  )
  expect_equal(
    active_social_media(),
    c("bluesky", "mastodon.mastodon.social", "mastodon.mstdn.party")
  )

  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE
  )
  expect_equal(active_social_media(), c("bluesky"))

  local_conf(
    sm_activated_bluesky = FALSE,
    sm_activated_mastodon = FALSE
  )

  expect_identical(active_social_media(), character(0))
})


test_that("alerts_social_media works", {
  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    sm_alerts_bluesky = TRUE,
    sm_alerts_mastodon = TRUE,
    "mastodon_instances" = list(
      "mastodon.social",
      "mstdn.party"
    )
  )
  expect_equal(
    alerts_social_media(),
    c("bluesky", "mastodon.mastodon.social", "mastodon.mstdn.party")
  )

  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE
  )
  expect_identical(alerts_social_media(), character(0))

  local_conf(
    sm_alerts_bluesky = TRUE,
    sm_alerts_mastodon = TRUE,
    "mastodon_instances" = list(
      "mastodon.social",
      "mstdn.party"
    )
  )

  expect_identical(alerts_social_media(), character(0))
})


test_that("sm_add_instance fails with an unknown social media", {
  local_conf(
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    "mastodon_instances" = list(
      "mastodon.social",
      "mstdn.party"
    )
  )
  expect_error(
    sm_add_instance(family = "pif", instance = "paf"),
    "There is no social media family 'pif', known families are: bluesky, mastodon"
  )
})


test_that("sm_add_instance fails with a not federated social media", {
  local_conf(
    sm_activated_bluesky = TRUE
  )
  expect_error(
    sm_add_instance(family = "bluesky", instance = "paf"),
    "The social media 'bluesky' is not federated, it has no instances to choose from"
  )
})


test_that("sm_add_instance fails with a not valid instance name", {
  local_conf(
    sm_activated_mastodon = TRUE
  )
  expect_error(
    sm_add_instance(family = "mastodon", instance = "paf"),
    "'paf' is not a valid instance domain"
  )
})

test_that("sm_add_instance works", {
  local_conf(
    sm_activated_mastodon = TRUE
  )

  res <- sm_add_instance(family = "mastodon", instance = "fosstodon.org")

  expect_equal(res, "mastodon.fosstodon.org")

  expect_equal(
    unlist(conf$mastodon_instances),
    "fosstodon.org"
  )
})


test_that("sm_remove_instance fails when the instance to be deleted is not present", {
  local_conf(
    sm_activated_mastodon = TRUE,
    mastodon_instances = "mastodon.social"
  )

  expect_error(
    sm_remove_instance(family = "mastodon", "pif"),
    "The instance 'pif' is not among the mastodon instances collected"
  )
})


test_that("sm_remove_instance fails works", {
  local_conf(
    sm_activated_mastodon = TRUE,
    mastodon_instances = c("mastodon.social", "mstdn.party")
  )
  sm_remove_instance(family = "mastodon", "mstdn.party")

  expect_false(
    "mstdn.party" %in% unlist(conf$mastodon_instances)
  )
})

test_that("sm_config_properties works", {
  local_conf(
    sm_activated_mastodon = TRUE,
    mastodon_instances = c("mastodon.social", "mstdn.party"),
    bobo = FALSE
  )

  expect_equal(
    sm_config_properties(conf),
    c("mastodon_instances", "sm_activated_mastodon")
  )
})
