# `conf` is a package level environment created when the package is loaded (R/config.R), and the
# configuration readers look it up in the package namespace, not in their caller: evaluating the
# test code in another environment (e.g. withr::with_environment) would leave them reading the real
# configuration. The environment is mutable though, so the tests replace its contents and put back
# what was there when the calling test exits.
#
# local_conf(sm_activated_bluesky = TRUE) gives the test a configuration holding just that property.
local_conf <- function(..., .local_envir = parent.frame()) {
  saved <- as.list(conf, all.names = TRUE)
  withr::defer(
    {
      rm(list = ls(conf, all.names = TRUE), envir = conf)
      list2env(saved, envir = conf)
    },
    envir = .local_envir
  )
  rm(list = ls(conf, all.names = TRUE), envir = conf)
  list2env(list(...), envir = conf)
  invisible(conf)
}
