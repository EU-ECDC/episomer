test_that("sm_family extracts the social media family from network", {
  expect_equal(sm_family("twitter"), "twitter")
  expect_equal(sm_family("mastodon.mstdn.party"), "mastodon")
  expect_equal(sm_family("bluesky.bsky.social"), "bluesky")
})

test_that("sm_family handles various domain formats", {
  expect_equal(sm_family("mastodon.fosstodon.org"), "mastodon")
  expect_equal(sm_family("mastodon.techhub.social"), "mastodon")
  expect_equal(sm_family("service.subdomain.example.com"), "service")
})

test_that("sm_instance returns NULL for non-federated networks", {
  expect_null(sm_instance("twitter"))
  expect_null(sm_instance("bluesky"))
})

test_that("sm_instance extracts the instance domain from federated networks", {
  expect_equal(sm_instance("mastodon.mstdn.party"), "mstdn.party")
  expect_equal(sm_instance("mastodon.fosstodon.org"), "fosstodon.org")
  expect_equal(sm_instance("bluesky.bsky.social"), "bsky.social")
})

test_that("sm_instance handles multiple dots in domain", {
  expect_equal(
    sm_instance("mastodon.subdomain.example.co.uk"),
    "subdomain.example.co.uk"
  )
})

test_that("sm_secret_name converts network names to safe secret names", {
  expect_equal(sm_secret_name("twitter", "token"), "twitter_token")
  expect_equal(sm_secret_name("bluesky", "password"), "bluesky_password")
})

test_that("sm_secret_name replaces dots and special characters with underscores", {
  expect_equal(
    sm_secret_name("mastodon.mstdn.party", "token"),
    "mastodon_mstdn_party_token"
  )
  expect_equal(
    sm_secret_name("mastodon.fosstodon.org", "secret"),
    "mastodon_fosstodon_org_secret"
  )
})

test_that("sm_secret_name handles various special characters", {
  expect_equal(
    sm_secret_name("network-with-dashes", "token"),
    "network_with_dashes_token"
  )
  expect_equal(
    sm_secret_name("network_with_underscores", "token"),
    "network_with_underscores_token"
  )
})

test_that("sm_is_federated returns FALSE for bluesky", {
  expect_false(sm_is_federated("bluesky"))
})

test_that("sm_is_federated returns TRUE for mastodon", {
  expect_true(sm_is_federated("mastodon"))
})


test_that("sm_is_federated returns FALSE for non-existent families", {
  expect_false(sm_is_federated("nonexistent_family"))
})
