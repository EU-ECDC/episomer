test_that("sm_family works", {
  expect_equal(sm_family("bluesky"), "bluesky")
  expect_equal(sm_family("mastodon.toto.titi"), "mastodon")
})

test_that("sm_instance works", {
  expect_null(sm_instance("bluesky"))
  expect_equal(sm_instance("mastodon.toto.titi"), "toto.titi")
})


test_that("sm_is_federated works", {
  expect_false(sm_is_federated("bluesky"))
  expect_true(sm_is_federated("mastodon"))
})

test_that("sm_secret_name works", {
  expect_equal(sm_secret_name(network = "mastodon.titi.toto", name = "token"), "mastodon_titi_toto_token")
})
