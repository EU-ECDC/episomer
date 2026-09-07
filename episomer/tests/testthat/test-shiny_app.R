withr::with_tempdir({
  test_that("episomer_app launches in dashboard mode without error", {
    dir.create("R")
    file.create(file.path(getwd(), "R/", "dummy.R"))

    local_mocked_bindings(
      get_episomer_kr_password = function() "test_password_123",
      is_secret_set = function(secret) FALSE,
      get_secret = function(secret) "test_secret_value",
      set_secret = function(secret, value) TRUE,
      get_key_ring = function(backend = NULL) {
        list(
          keyring_list = function() data.frame(keyring = character()),
          keyring_create = function(...) TRUE,
          keyring_set_default = function(...) TRUE,
          keyring_is_locked = function(...) FALSE,
          keyring_unlock = function(...) TRUE,
          set_with_value = function(...) TRUE,
          list = function(...) data.frame(username = character())
        )
      },
      refresh_dashboard_data = function(...) {
        list(
          sms = c("bluesky", "mastodon"),
          topics = c("covid", "trump"),
          countries = c("France", "Spain"),
          fixed_period = "last 7 days",
          date_start = Sys.Date() - 7,
          date_end = Sys.Date(),
          date_min = Sys.Date() - 30,
          date_max = Sys.Date()
        )
      },
      setup_config = function(...) TRUE,
      .package = "episomer"
    )

    local_conf(
      alert_alpha = .1,
      alert_alpha_outlier = .1,
      alert_k_decay = 1
    )

    expect_no_error({
      app <- episomer_app(data_dir = getwd(), profile = "dashboard")
    })
  })
})


withr::with_tempdir({
  test_that("episomer_app launches in admin mode without error", {
    dir.create("R")
    file.create(file.path(getwd(), "R/", "dummy.R"))

    local_mocked_bindings(
      get_episomer_kr_password = function() "test_password_123",
      is_secret_set = function(secret) FALSE,
      get_secret = function(secret) "test_secret_value",
      set_secret = function(secret, value) TRUE,
      get_key_ring = function(backend = NULL) {
        list(
          keyring_list = function() data.frame(keyring = character()),
          keyring_create = function(...) TRUE,
          keyring_set_default = function(...) TRUE,
          keyring_is_locked = function(...) FALSE,
          keyring_unlock = function(...) TRUE,
          set_with_value = function(...) TRUE,
          list = function(...) data.frame(username = character())
        )
      },
      refresh_dashboard_data = function(...) {
        list(
          sms = c("bluesky", "mastodon"),
          topics = c("covid", "trump"),
          countries = c("France", "Spain"),
          fixed_period = "last 7 days",
          date_start = Sys.Date() - 7,
          date_end = Sys.Date(),
          date_min = Sys.Date() - 30,
          date_max = Sys.Date()
        )
      },
      refresh_config_data = function(...) list(),
      setup_config = function(...) TRUE,
      .package = "episomer"
    )

    local_conf(
      alert_alpha = .1,
      alert_alpha_outlier = .1,
      alert_k_decay = 1,
      data_dir = getwd()
    )

    expect_no_error({
      app <- episomer_app(data_dir = getwd(), profile = "admin")
    })
  })
})


withr::with_tempdir({
  test_that("episomer_app launches in both profiles without error", {
    dir.create("R")
    file.create(file.path(getwd(), "R/", "dummy.R"))

    local_mocked_bindings(
      get_episomer_kr_password = function() "test_password_123",
      is_secret_set = function(secret) FALSE,
      get_secret = function(secret) "test_secret_value",
      set_secret = function(secret, value) TRUE,
      get_key_ring = function(backend = NULL) {
        list(
          keyring_list = function() data.frame(keyring = character()),
          keyring_create = function(...) TRUE,
          keyring_set_default = function(...) TRUE,
          keyring_is_locked = function(...) FALSE,
          keyring_unlock = function(...) TRUE,
          set_with_value = function(...) TRUE,
          list = function(...) data.frame(username = character())
        )
      },
      refresh_dashboard_data = function(...) {
        list(
          sms = c("bluesky", "mastodon"),
          topics = c("covid", "trump"),
          countries = c("France", "Spain"),
          fixed_period = "last 7 days",
          date_start = Sys.Date() - 7,
          date_end = Sys.Date(),
          date_min = Sys.Date() - 30,
          date_max = Sys.Date()
        )
      },
      refresh_config_data = function(...) list(),
      setup_config = function(...) TRUE,
      .package = "episomer"
    )

    local_conf(
      alert_alpha = .1,
      alert_alpha_outlier = .1,
      alert_k_decay = 1,
      data_dir = getwd()
    )

    expect_no_error({
      app <- episomer_app(data_dir = getwd(), profile = c("dashboard", "admin"))
    })
  })
})


test_that("episomer_app raises error with invalid profile", {
  local_mocked_bindings(
    get_episomer_kr_password = function() "test_password_123",
    .package = "episomer"
  )

  expect_error(
    episomer_app(data_dir = "/tmp", profile = "invalid_profile"),
    'should be one of "dashboard", "admin"'
  )
})


withr::with_tempdir({
  test_that("episomer_app raises error when setup_config fails", {
    dir.create("R")
    file.create(file.path(getwd(), "R/", "dummy.R"))

    local_mocked_bindings(
      get_episomer_kr_password = function() "test_password_123",
      setup_config = function(...) stop("Configuration failed"),
      .package = "episomer"
    )

    expect_error(
      episomer_app(data_dir = getwd(), profile = "dashboard"),
      "Configuration failed"
    )
  })
})


withr::with_tempdir({
  test_that("episomer_app raises error when refresh_dashboard_data fails", {
    dir.create("R")
    file.create(file.path(getwd(), "R/", "dummy.R"))

    local_mocked_bindings(
      get_episomer_kr_password = function() "test_password_123",
      is_secret_set = function(secret) FALSE,
      get_secret = function(secret) "test_secret_value",
      set_secret = function(secret, value) TRUE,
      get_key_ring = function(backend = NULL) {
        list(
          keyring_list = function() data.frame(keyring = character()),
          keyring_create = function(...) TRUE,
          keyring_set_default = function(...) TRUE,
          keyring_is_locked = function(...) FALSE,
          keyring_unlock = function(...) TRUE,
          set_with_value = function(...) TRUE,
          list = function(...) data.frame(username = character())
        )
      },
      refresh_dashboard_data = function(...) {
        stop("Dashboard data refresh failed")
      },
      setup_config = function(...) TRUE,
      .package = "episomer"
    )

    expect_error(
      episomer_app(data_dir = getwd(), profile = "dashboard"),
      "Dashboard data refresh failed"
    )
  })
})


withr::with_tempdir({
  test_that("episomer_app raises error when refresh_config_data fails in admin mode", {
    dir.create("R")
    file.create(file.path(getwd(), "R/", "dummy.R"))

    local_mocked_bindings(
      get_episomer_kr_password = function() "test_password_123",
      is_secret_set = function(secret) FALSE,
      get_secret = function(secret) "test_secret_value",
      set_secret = function(secret, value) TRUE,
      get_key_ring = function(backend = NULL) {
        list(
          keyring_list = function() data.frame(keyring = character()),
          keyring_create = function(...) TRUE,
          keyring_set_default = function(...) TRUE,
          keyring_is_locked = function(...) FALSE,
          keyring_unlock = function(...) TRUE,
          set_with_value = function(...) TRUE,
          list = function(...) data.frame(username = character())
        )
      },
      refresh_dashboard_data = function(...) {
        list(
          sms = c("bluesky"),
          topics = c("covid"),
          countries = c("France"),
          fixed_period = "last 7 days",
          date_start = Sys.Date() - 7,
          date_end = Sys.Date(),
          date_min = Sys.Date() - 30,
          date_max = Sys.Date()
        )
      },
      refresh_config_data = function(...) stop("Config data refresh failed"),
      setup_config = function(...) TRUE,
      .package = "episomer"
    )

    expect_error(
      episomer_app(data_dir = getwd(), profile = "admin"),
      "Config data refresh failed"
    )
  })
})
