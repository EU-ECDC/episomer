test_that("get_alerts skips files of the alerts folder whose name holds no date", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  local_mocked_bindings(get_country_items = function(...) list())
  year_dir <- file.path(data_dir, "alerts", "2026")
  dir.create(year_dir, recursive = TRUE)
  writeLines(
    '{"topic":"t1","country":"France","hour":1,"number_of_posts":5,"topwords":"a"}',
    file.path(year_dir, "2026.10.15-alerts.json")
  )
  # a stray file: its date reads as NA, which used to index the file list with NA and hand a NULL
  # path to file() ("invalid 'description' argument")
  writeLines("junk", file.path(year_dir, "notes.txt"))

  df <- get_alerts(from = "2026-10-01", until = "2026-10-31")

  expect_identical(nrow(df), 1L)
  expect_identical(df$topic, "t1")
})

test_that("epilist.files turns the \ of an fs API running on Windows into /", {
  local_conf(fs_remote = "http://localhost:8080")
  local_mocked_bindings(
    read_json = function(...) list("http://localhost:8080/file/alerts\\2026\\2026.09.23-alerts.json"),
    .package = "jsonlite"
  )

  expect_identical(
    epilist.files("http://localhost:8080/file/alerts", full.names = TRUE, recursive = TRUE),
    "http://localhost:8080/file/alerts/2026/2026.09.23-alerts.json"
  )
})
