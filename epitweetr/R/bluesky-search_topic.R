bluesky_set_date_boundaries <- function(plan) {
  # We set the upper bound of the research: if missing we set it to the current time
  max_text <- plan$research_max_date
  if (is.null(plan$research_max_date)) {
    plan$research_max_date <- Sys.time()
    max_text <- "last message"
  }

  # We set the lower bound of the research
  # If messages were retrived before, we set it to the newest message retrieved by the previous query
  if (is.null(plan$research_min_date)) {
    min_text <- "first message"
  }
  if (!is.null(plan$research_min_date)) {
    min_text <- plan$research_min_date
  }
  if (!is.null(plan$boundaries_date_max)) {
    plan$research_min_date <- plan$boundaries_date_max
    min_text <- plan$research_min_date

    if (plan$research_max_date <= plan$research_min_date) {
      stop("We already have all the data we need")
    }
  }

  list(plan = plan, max_text = max_text, min_text = min_text)
}


bluesky_got_rows <- function(content) {
  (exists("posts", content$results) & length(content$results$posts) > 0)
}

bluesky_no_rows_logic <- function(plan) {
  plan$has_more <- FALSE
  plan$end_on <- Sys.time()
  return(plan)
}

bluesky_get_json_date_min_max <- function(content) {
  first_date <- bluesky_extract_many_posts_created_at(content$results$posts) %>%
    unlist() %>%
    min()
  last_date <- bluesky_extract_many_posts_created_at(content$results$posts) %>%
    unlist() %>%
    max()
  list(first_date = first_date, last_date = last_date)
}
