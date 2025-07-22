topic1 <- list(
  query = "covid19",
  topic = "covid19",
  plan = list(list(
    research_max_date = "2025-07-15T00:01:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    research_min_date = "2025-07-13T00:01:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    network = "bluesky",
    requests = 0,
    expected_end = Sys.time() + 60
  ))
)

topic2 <- list(
  query = "influenza",
  topic = "influenza",
  plan = list(list(
    network = "bluesky",
    research_max_date = "2025-07-08T01:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    research_min_date = "2025-07-06T02:00:00" %>%
      lubridate::as_datetime(tz = "UTC"),
    requests = 0,
    expected_end = Sys.time() + 60
  ))
)

topics <- list(topic1, topic2)
conf <- list(topics = topics, collect_span = 100, data_dir = tempfile())
dir.create(conf$data_dir, recursive = TRUE)

test_that("search_loop works", {
  kill_after = 10

  expect_error(
    search_loop(
      network = "bluesky",
      data_dir = conf$data_dir,
      dev_in_progress = TRUE,
      conf = conf,
      kill_after = kill_after
    ),
    paste0("Killing search loop after ", kill_after, " seconds")
  )
})

unlink("bluesky_session.rds", recursive = TRUE)
