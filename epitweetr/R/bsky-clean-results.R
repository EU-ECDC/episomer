#' Extract elements from a post
#'
#' @param post A post object
#' @return A list of elements
#' @export
#' @rdname extract_elements
bsky_extract_post_elements <- function(post) {
  list(
    author_infos = bsky_extract_post_author_infos(post),
    text = bsky_extract_post_text(post),
    created_at = extract_post_created_at(post),
    langs = bsky_extract_post_langs(post),
    hashtags = bsky_extract_post_hashtags(post),
    url = bsky_create_post_url(post),
    id = bsky_create_post_id(post)
  )
}

#' Extract elements from many posts
#'
#' @param posts A list of post objects
#' @return A list of lists of elements
#' @export
#' @importFrom stats setNames
#' @importFrom purrr map
#' @rdname extract_elements
bsky_extract_many_posts_elements <- function(posts) {
  posts %>%
    map(bsky_extract_post_elements) %>%
    setNames(map(posts, bsky_create_post_id))
}

#' Extract author information from a post
#'
#' @param post A post object
#' @return A list with author handle, did, and created at
#' @export
#' @rdname extract_elements
bsky_extract_post_author_infos <- function(post) {
  list(
    author_handle = post$author$handle,
    author_did = post$author$did,
    created_at = post$author$createdAt
  )
}

#' Extract author information from many posts
#'
#' @param posts A list of post objects
#' @return A list of lists with author handle, did, and created at
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_author_infos <- function(posts) {
  posts %>%
    map(bsky_extract_post_author_infos)
}

#' Extract text from a post
#'
#' @param post A post object
#' @return The text of the post
#' @export
#' @rdname extract_elements
bsky_extract_post_text <- function(post) {
  post$record$text
}

#' Extract text from many posts
#'
#' @param posts A list of post objects
#' @return A list of texts
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_text <- function(posts) {
  posts %>%
    map(bsky_extract_post_text)
}

#' Extract created at from a post
#'
#' @param post A post object
#' @return The created at of the post
#' @export
#' @rdname extract_elements
extract_post_created_at <- function(post) {
  post$record$createdAt
}

#' Extract created at from many posts
#'
#' @param posts A list of post objects
#' @return A list of created ats
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_created_at <- function(posts) {
  posts %>%
    map(extract_post_created_at)
}

#' Extract langs from a post
#'
#' @param post A post object
#' @return The langs of the post
#' @export
#' @rdname extract_elements
bsky_extract_post_langs <- function(post) {
  post$record$langs %>%
    unlist() %>%
    paste(collapse = "|")
}

#' Extract langs from many posts
#'
#' @param posts A list of post objects
#' @return A list of langs
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_langs <- function(posts) {
  posts %>%
    map(bsky_extract_post_langs)
}

#' Extract hashtags from a post
#'
#' @param post A post object
#' @return A list of hashtags
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_post_hashtags <- function(post) {
  post$record$facets %>%
    map("features") %>%
    map(function(.x) map(.x, "tag")) %>%
    unlist()
}

#' Extract hashtags from many posts
#'
#' @param posts A list of post objects
#' @return A list of lists of hashtags
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_hashtags <- function(posts) {
  posts %>%
    map(bsky_extract_post_hashtags)
}

#' Create post URL
#'
#' @param post A post object
#' @return The URL of the post
#' @export
#' @rdname extract_elements
bsky_create_post_url <- function(post) {
  # Check if post has uri
  if (is.null(post$uri)) {
    return(NULL)
  }

  # Extract post rkey
  post_rkey = sub(".*/", "", post$uri)

  # Construct web URL
  web_url = paste0(
    "https://bsky.app/profile/",
    post$author$did,
    "/post/",
    sub(".*/", "", post$uri)
  )

  return(web_url)
}

#' Create post URL from many posts
#'
#' @param posts A list of post objects
#' @return A list of URLs
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_url <- function(posts) {
  posts %>%
    map(bsky_create_post_url)
}

#' Create post ID
#'
#' @param post A post object
#' @return The ID of the post
#' @export
#' @importFrom digest digest
#' @rdname extract_elements
bsky_create_post_id <- function(post) {
  # Check if post has uri
  if (is.null(post$uri)) {
    return(NULL)
  }

  # Create post URL
  post_url <- bsky_create_post_url(post)
  digest(post_url, algo = "md5")
}

#' Create post ID from many posts
#'
#' @param posts A list of post objects
#' @return A list of IDs
#' @importFrom purrr map
#' @export
#' @rdname extract_elements
bsky_extract_many_posts_id <- function(posts) {
  posts %>%
    map(bsky_create_post_id)
}
