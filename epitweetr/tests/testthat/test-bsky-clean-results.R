test_that("extract_author_infos works", {
  author_infos <- bsky_extract_post_author_infos(example_post)
  expect_equal(author_infos$author_handle, example_post$author$handle)
  expect_equal(author_infos$author_did, example_post$author$did)
  expect_equal(author_infos$created_at, example_post$author$createdAt)
})

test_that("extract_text works", {
  text <- bsky_extract_post_text(example_post)
  expect_equal(text, example_post$record$text)
})

test_that("extract_created_at works", {
  created_at <- extract_post_created_at(example_post)
  expect_equal(created_at, example_post$record$createdAt)
})

test_that("extract_langs works", {
  langs <- bsky_extract_post_langs(example_post)
  expect_equal(langs, "de")
})

test_that("extract_hashtags works", {
  hashtags <- bsky_extract_post_hashtags(example_post)
  expect_equal(hashtags, c("Übertragung", "Influenza"))
})

test_that("bsky_create_post_url works", {
  post_url <- bsky_create_post_url(example_post)
  expect_equal(
    post_url,
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
})

test_that("bsky_extract_post_elements works", {
  elements <- bsky_extract_post_elements(example_post)
  expect_equal(elements$author_infos$author_handle, example_post$author$handle)
  expect_equal(elements$author_infos$author_did, example_post$author$did)
  expect_equal(elements$author_infos$created_at, example_post$author$createdAt)
  expect_equal(elements$text, example_post$record$text)
  expect_equal(elements$langs, "de")
  expect_equal(elements$hashtags, c("Übertragung", "Influenza"))
  expect_equal(
    elements$url,
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
  expect_equal(elements$id, digest::digest(elements$url, algo = "md5"))
})


# Check when posts have missing elements
test_that("extract_author_infos works", {
  post_missing_author <- example_post
  post_missing_author$author$handle <- NULL
  author_infos <- bsky_extract_post_author_infos(post_missing_author)
  expect_equal(author_infos$author_handle, NULL)
  expect_equal(author_infos$author_did, post_missing_author$author$did)
  expect_equal(author_infos$created_at, post_missing_author$author$createdAt)
})


test_that("extract_text works", {
  post_missing_text <- example_post
  post_missing_text$record$text <- NULL
  text <- bsky_extract_post_text(post_missing_text)
  expect_equal(text, NULL)
})

test_that("extract_created_at works", {
  post_missing_created_at <- example_post
  post_missing_created_at$record$createdAt <- NULL
  created_at <- extract_post_created_at(post_missing_created_at)
  expect_equal(created_at, NULL)
})

test_that("extract_langs works", {
  post_missing_langs <- example_post
  post_missing_langs$record$langs <- NULL
  langs <- bsky_extract_post_langs(post_missing_langs)
  expect_equal(langs, "")
})

test_that("extract_hashtags works", {
  post_missing_hashtags <- example_post
  post_missing_hashtags$record$facets <- NULL
  hashtags <- bsky_extract_post_hashtags(post_missing_hashtags)
  expect_equal(hashtags, NULL)
})


test_that("bsky_create_post_url works", {
  post_missing_url <- example_post
  post_missing_url$uri <- NULL
  post_url <- bsky_create_post_url(post_missing_url)
  expect_equal(
    post_url,
    NULL
  )
})

# Check many posts
example_posts <- list(
  example_post,
  post_with_2_langs,
  post_without_hashtags,
  post_with_many_hashtags
)
test_that("bsky_extract_many_posts_author_infos works", {
  author_infos <- bsky_extract_many_posts_author_infos(example_posts)
  expect_equal(
    author_infos[[1]]$author_handle,
    example_posts[[1]]$author$handle
  )
  expect_equal(author_infos[[1]]$author_did, example_posts[[1]]$author$did)
  expect_equal(
    author_infos[[1]]$created_at,
    example_posts[[1]]$author$createdAt
  )
  expect_equal(
    author_infos[[2]]$author_handle,
    example_posts[[2]]$author$handle
  )
  expect_equal(author_infos[[2]]$author_did, example_posts[[2]]$author$did)
  expect_equal(
    author_infos[[2]]$created_at,
    example_posts[[2]]$author$createdAt
  )
  expect_equal(
    author_infos[[3]]$author_handle,
    example_posts[[3]]$author$handle
  )
  expect_equal(author_infos[[3]]$author_did, example_posts[[3]]$author$did)
  expect_equal(
    author_infos[[3]]$created_at,
    example_posts[[3]]$author$createdAt
  )
  expect_equal(
    author_infos[[4]]$author_handle,
    example_posts[[4]]$author$handle
  )
  expect_equal(author_infos[[4]]$author_did, example_posts[[4]]$author$did)
  expect_equal(
    author_infos[[4]]$created_at,
    example_posts[[4]]$author$createdAt
  )
})


test_that("bsky_extract_many_posts_text works", {
  text <- bsky_extract_many_posts_text(example_posts)
  expect_equal(text[[1]], example_posts[[1]]$record$text)
  expect_equal(text[[2]], example_posts[[2]]$record$text)
  expect_equal(text[[3]], example_posts[[3]]$record$text)
  expect_equal(text[[4]], example_posts[[4]]$record$text)
})

test_that("bsky_extract_many_posts_created_at works", {
  created_at <- bsky_extract_many_posts_created_at(example_posts)
  expect_equal(created_at[[1]], example_posts[[1]]$record$createdAt)
  expect_equal(created_at[[2]], example_posts[[2]]$record$createdAt)
  expect_equal(created_at[[3]], example_posts[[3]]$record$createdAt)
  expect_equal(created_at[[4]], example_posts[[4]]$record$createdAt)
})

test_that("bsky_extract_many_posts_langs works", {
  langs <- bsky_extract_many_posts_langs(example_posts)
  expect_equal(langs[[1]], "de")
  expect_equal(langs[[2]], "aa|de")
  expect_equal(langs[[3]], "")
  expect_equal(langs[[4]], "en")
})

test_that("bsky_extract_many_posts_hashtags works", {
  hashtags <- bsky_extract_many_posts_hashtags(example_posts)
  expect_equal(hashtags[[1]], c("Übertragung", "Influenza"))
  expect_equal(
    hashtags[[2]],
    c("Covidisnotover", "maskup", "cleanair", "Maskenpflicht", "Covid19")
  )
  expect_equal(hashtags[[3]], NULL)
  expect_equal(
    hashtags[[4]],
    c(
      "WearAMask",
      "Covid19",
      "CoronaVirus",
      "PandemicPanda",
      "WednesdayMorning"
    )
  )
})

test_that("bsky_extract_many_posts_elements works", {
  elements <- bsky_extract_many_posts_elements(example_posts)
  expect_equal(
    elements[[1]]$author_infos$author_handle,
    example_posts[[1]]$author$handle
  )
  expect_equal(
    elements[[1]]$author_infos$author_did,
    example_posts[[1]]$author$did
  )
  expect_equal(
    elements[[1]]$author_infos$created_at,
    example_posts[[1]]$author$createdAt
  )
  expect_equal(elements[[1]]$text, example_posts[[1]]$record$text)
  expect_equal(elements[[1]]$langs, "de")
  expect_equal(elements[[1]]$hashtags, c("Übertragung", "Influenza"))
  expect_equal(
    elements[[1]]$url,
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
  expect_equal(
    elements[[1]]$id,
    digest::digest(elements[[1]]$url, algo = "md5")
  )

  expect_equal(
    names(elements),
    purrr::map(elements, "id") %>% purrr::reduce(c)
  )
})

test_that("bsky_create_post_url works", {
  post_url <- bsky_create_post_url(example_post)
  expect_equal(
    post_url,
    "https://bsky.app/profile/did:plc:k6vq7z2d5lyhhqnxs72tlchg/post/3ltlzhcvjqc2g"
  )
})
