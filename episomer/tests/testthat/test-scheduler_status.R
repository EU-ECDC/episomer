# conf always defines these requests, as NA when nothing was requested
local_requests <- function(..., .local_envir = parent.frame()) {
  local_conf(
    dep_updated_on = NA, geonames_updated_on = NA, lang_updated_on = NA,
    alerts_requested_on = NA, ...,
    .local_envir = .local_envir
  )
}

task_at <- function(name, started_on = NA, status = NA) {
  list(task = name, started_on = started_on, status = status)
}

test_that("in_requested_status is FALSE when nothing was requested", {
  local_requests()
  for (name in c("dependencies", "geonames", "languages", "alerts")) {
    expect_false(in_requested_status(task_at(name)))
  }
})

test_that("in_requested_status is TRUE for a request on a task that never ran", {
  local_requests(alerts_requested_on = "2026-01-02 10:00:00")
  expect_true(in_requested_status(task_at("alerts")))
})

test_that("in_requested_status compares the request with the last start", {
  local_requests(alerts_requested_on = "2026-01-02 10:00:00")
  before <- as.POSIXlt("2026-01-02 09:00:00")
  after <- as.POSIXlt("2026-01-02 11:00:00")
  expect_true(in_requested_status(task_at("alerts", started_on = before)))
  expect_false(in_requested_status(task_at("alerts", started_on = after)))
})

test_that("in_requested_status only looks at the property of the task itself", {
  local_requests(geonames_updated_on = "2026-01-02 10:00:00")
  expect_true(in_requested_status(task_at("geonames")))
  expect_false(in_requested_status(task_at("languages")))
  expect_false(in_requested_status(task_at("alerts")))
})

test_that("in_pending_status requires a settled status", {
  local_requests(lang_updated_on = "2026-01-02 10:00:00")
  for (s in c(NA, "pending", "success", "failure", "aborted")) {
    expect_true(in_pending_status(task_at("languages", status = s)))
  }
  for (s in c("running", "scheduled")) {
    expect_false(in_pending_status(task_at("languages", status = s)))
  }
})

test_that("get_task_day_slots lists the slots of a day plus the first of the next", {
  local_conf(schedule_span = 360, schedule_start_hour = 2)
  slots <- get_task_day_slots(list(scheduled_for = as.POSIXlt("2026-03-10 15:00:00", tz = "UTC")))
  hours <- as.numeric(difftime(slots, as.POSIXct(as.Date("2026-03-10")), units = "hours"))
  expect_length(slots, 5)
  expect_equal(unname(hours - hours[[1]]), c(0, 6, 12, 18, 24))
  expect_equal(hours[[1]] %% 6, 2 %% 6)
})

test_that("update_status_since stamps only on change", {
  e <- new.env()
  update_status_since(e, "fs", TRUE)
  first <- e$fs_since
  expect_true(e$fs_running)
  expect_s3_class(first, "POSIXct")
  update_status_since(e, "fs", TRUE)
  expect_identical(e$fs_since, first)
  e$fs_since <- first - 100
  update_status_since(e, "fs", FALSE)
  expect_gt(e$fs_since, first - 100)
  expect_false(e$fs_running)
})

test_that("format_status_duration formats minutes", {
  now <- Sys.time()
  expect_equal(format_status_duration(now), "less than a minute")
  expect_equal(format_status_duration(now - 90), "1 minute")
  expect_equal(format_status_duration(now - 600), "10 minutes")
})
