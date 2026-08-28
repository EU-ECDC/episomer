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

# Runs `code` as if it were another episomer instance, i.e. another R process on another machine
# sharing the same data directory. That is all a "second instance" is, as far as the runner leases
# are concerned: a different conf$instance_id (see R/runners-lease.R).
#
#   with_instance("instance-B", code = claim_runner_lease("search.bluesky", guard = 0))
#
# `code` is evaluated lazily, so it runs with the borrowed identity in place, and the identity of
# the calling test is restored whatever happens.
with_instance <- function(id, host = "other-host", code) {
  saved <- list(instance_id = conf$instance_id, instance_host = conf$instance_host)
  on.exit(list2env(saved, envir = conf), add = TRUE)
  conf$instance_id <- id
  conf$instance_host <- host
  force(code)
}
