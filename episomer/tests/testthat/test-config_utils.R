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
