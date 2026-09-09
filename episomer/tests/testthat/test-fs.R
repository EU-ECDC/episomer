# get_scala_api_endpoint() answers "where is the fs API, seen from this process". conf$fs_url is
# NOT that answer: the Scala API uses it as its bind interface and it travels in the shared
# properties.json, so a containerised fs writes there a Docker-internal name ("http://fs"). The
# tests below pin the four steps of the resolution order documented in R/fs.R.
#
# fs_url_resolves_here() is mocked rather than exercised: the real one does a DNS lookup, which
# would make these tests depend on the resolver of the machine running them and cost half a second
# per miss. Its own behaviour is tested separately at the end of this block.

test_that("get_scala_api_endpoint prefers fs_remote over everything else", {
  withr::local_envvar(EPI_HOST = "http://from-env:8080")
  local_conf(
    "fs_remote" = "http://explicit:1234/",
    fs_url = "http://fs",
    fs_port = 8080
  )
  # trailing slash dropped, and neither EPI_HOST nor fs_url gets a say
  expect_equal(get_scala_api_endpoint(), "http://explicit:1234")
})

test_that("get_scala_api_endpoint falls back on EPI_HOST when fs_remote is NULL", {
  # setup() passes fs_remote = NULL on purpose (it works on the local disk), so EPI_HOST has to be
  # honoured here or the wizard would ignore it.
  withr::local_envvar(EPI_HOST = "http://fs:8080/")
  local_conf(
    "fs_remote" = NULL,
    fs_url = "http://fs",
    fs_port = 8080
  )
  expect_equal(get_scala_api_endpoint(), "http://fs:8080")
})

test_that("get_scala_api_endpoint uses fs_url:fs_port when that host resolves here", {
  # The fs machine itself: it is the only process that both binds and calls that address.
  withr::local_envvar(EPI_HOST = "")
  local_mocked_bindings(fs_url_resolves_here = function() TRUE, .package = "episomer")
  local_conf(
    "fs_remote" = NULL,
    fs_url = "http://fs",
    fs_port = 8080
  )
  expect_equal(get_scala_api_endpoint(), "http://fs:8080")
})

test_that("get_scala_api_endpoint falls back on localhost when fs_url does not resolve here", {
  # The bug this fixes: docker/compose.yml sets EPISOMER_OPTION_FS_URL=http://fs on the fs
  # service, which saves it in the shared properties.json; an R session on the *host* then read
  # "http://fs" and concluded the database was down, while the port is published on localhost.
  withr::local_envvar(EPI_HOST = "")
  local_mocked_bindings(fs_url_resolves_here = function() FALSE, .package = "episomer")
  local_conf(
    "fs_remote" = NULL,
    fs_url = "http://fs",
    fs_port = 8080
  )
  # "localhost" comes back as 127.0.0.1: see prefer_loopback_ipv4() in R/fs.R.
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:8080")
})

test_that("get_scala_api_endpoint follows the temporary port shift of the retraining", {
  # setup() shifts conf$fs_port by 1000 while it retrains the language models, so the endpoint
  # must be read from conf on every call and never frozen with the host resolution.
  withr::local_envvar(EPI_HOST = "")
  local_conf(
    "fs_remote" = NULL,
    fs_url = "http://localhost",
    fs_port = 9080
  )
  local_mocked_bindings(fs_url_resolves_here = function() TRUE, .package = "episomer")
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:9080")

  local_mocked_bindings(fs_url_resolves_here = function() FALSE, .package = "episomer")
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:9080")
})

# A wildcard fs_url is the value a deployment behind a Kubernetes Service wants: "listen on every
# interface". It is a bind address and never an address to call - connecting to 0.0.0.0 reaches the
# loopback on Linux by kernel convention and is simply invalid on Windows - so the endpoint
# resolution has to step over it even though the resolver answers for it.

test_that("a wildcard fs_url falls back to localhost instead of being called", {
  withr::local_envvar(EPI_HOST = "")
  local_conf("fs_remote" = NULL, fs_url = "http://0.0.0.0", fs_port = 8080)
  expect_false(fs_url_resolves_here())
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:8080")

  # scheme-less, and the IPv6 wildcard, are the same answer
  local_conf("fs_remote" = NULL, fs_url = "0.0.0.0", fs_port = 8080)
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:8080")
  local_conf("fs_remote" = NULL, fs_url = "http://[::]", fs_port = 8080)
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:8080")
})

test_that("is_wildcard_bind_host accepts only the wildcards", {
  expect_true(is_wildcard_bind_host("0.0.0.0"))
  expect_true(is_wildcard_bind_host("http://0.0.0.0"))
  expect_true(is_wildcard_bind_host("http://0.0.0.0/"))
  expect_true(is_wildcard_bind_host("http://0.0.0.0:8080"))
  expect_true(is_wildcard_bind_host("::"))
  expect_true(is_wildcard_bind_host("http://[::]"))

  # a real interface, a name, and the loopback are all callable addresses
  expect_false(is_wildcard_bind_host("http://localhost"))
  expect_false(is_wildcard_bind_host("http://127.0.0.1"))
  expect_false(is_wildcard_bind_host("http://10.0.0.0"))
  expect_false(is_wildcard_bind_host("http://fs"))
  expect_false(is_wildcard_bind_host(""))
  expect_false(is_wildcard_bind_host(NULL))
  expect_false(is_wildcard_bind_host(NA))
})

test_that("a wildcard fs_url is not reported as unbindable", {
  # The mirror question of the two above: 0.0.0.0 is the one address that binds everywhere, and
  # Settings.hostResolvesHere() on the Scala side accepts it. Warning here would call a working
  # configuration broken.
  local_conf("fs_remote" = NULL, fs_url = "http://0.0.0.0", fs_port = 8080)
  expect_false(warn_if_fs_url_not_bindable())

  # while a name that exists nowhere on this machine still gets its diagnosis
  local_conf("fs_remote" = NULL, fs_url = "http://fs", fs_port = 8080)
  expect_message(expect_true(warn_if_fs_url_not_bindable()), "cannot resolve")
})

test_that("prefer_loopback_ipv4 rewrites only the exact host localhost", {
  # Windows resolves "localhost" to ::1 only, where the Scala API does not listen; every request
  # then pays a failed connection before falling back to IPv4.
  expect_equal(prefer_loopback_ipv4("http://localhost:8080"), "http://127.0.0.1:8080")
  expect_equal(prefer_loopback_ipv4("https://localhost/ping"), "https://127.0.0.1/ping")
  expect_equal(prefer_loopback_ipv4("http://localhost"), "http://127.0.0.1")

  # Anything that is not that host is left to the resolver.
  expect_equal(prefer_loopback_ipv4("http://fs:8080"), "http://fs:8080")
  expect_equal(prefer_loopback_ipv4("http://localhost.example:8080"), "http://localhost.example:8080")
  expect_equal(prefer_loopback_ipv4("http://my-localhost:8080"), "http://my-localhost:8080")
})

test_that("fs_url_resolves_here answers for a scheme-less name and an empty fs_url", {
  local_conf("fs_remote" = NULL, fs_url = "localhost", fs_port = 8080)
  expect_true(fs_url_resolves_here())

  # Nothing to resolve: not an error, just "no, do not bother calling it".
  local_conf("fs_remote" = NULL, fs_url = "", fs_port = 8080)
  expect_false(fs_url_resolves_here())
  local_conf("fs_remote" = NULL, fs_url = NA, fs_port = 8080)
  expect_false(fs_url_resolves_here())
  local_conf("fs_remote" = NULL, fs_url = NULL, fs_port = 8080)
  expect_false(fs_url_resolves_here())
})

test_that("fs_url_resolves_here memoises its answer and invalidates it on a new fs_url", {
  # A missed lookup costs about half a second and the resolver does not cache the miss, while
  # get_scala_api_endpoint() runs on every API request.
  local_conf("fs_remote" = NULL, fs_url = "http://localhost", fs_port = 8080)
  expect_true(fs_url_resolves_here())
  expect_equal(conf$fs_url_resolved_for, "http://localhost")

  # Poisoning the memo is the only way to observe that it is read rather than recomputed.
  conf$fs_url_resolves <- FALSE
  expect_false(fs_url_resolves_here())

  # A different fs_url is a different question, so the memo must not answer it.
  conf$fs_url <- "http://127.0.0.1"
  expect_true(fs_url_resolves_here())
  expect_equal(conf$fs_url_resolved_for, "http://127.0.0.1")
})


test_that("get_scala_api_endpoint works when remote is not null", {
  local_conf(
    "fs_remote" = "toto:0808"
  )
  api <- get_scala_api_endpoint()
  expect_equal(api, "toto:0808")
})


test_that("get_scala_xxx_url functions work", {
  remote <- "toto:0808"
  local_conf(
    "fs_remote" = remote
  )
  expect_equal(
    get_scala_geotraining_url(),
    paste(remote, "geotraining-set", sep = "/")
  )

  expect_equal(
    get_scala_alert_training_url(),
    paste(remote, "evaluate-alerts", sep = "/")
  )

  expect_equal(
    get_scala_posts_url(),
    paste(remote, "posts", sep = "/")
  )

  expect_equal(
    get_scala_geolocated_posts_url(),
    paste(remote, "geolocated-posts", sep = "/")
  )

  expect_equal(
    get_scala_commit_url(),
    paste(remote, "commit", sep = "/")
  )

  expect_equal(
    get_scala_geolocate_text_url(),
    paste(remote, "geolocate-text", sep = "/")
  )

  expect_equal(
    get_scala_aggregate_url(),
    paste(remote, "aggregate", sep = "/")
  )

  expect_equal(
    get_scala_period_url(),
    paste(remote, "period", sep = "/")
  )

  expect_equal(
    get_scala_status_url(),
    paste(remote, "period?serie=country_counts", sep = "/")
  )

  expect_equal(
    get_scala_ping_url(),
    paste(remote, "ping", sep = "/")
  )

  expect_equal(
    get_scala_recalc_hash_url(),
    paste(remote, "recalculate-hash", sep = "/")
  )

  expect_equal(
    get_scala_file_url("a", "b"),
    paste(remote, "file/a/b", sep = "/")
  )

  expect_equal(
    get_scala_secrets_url(),
    paste(remote, "secret", sep = "/")
  )

  expect_equal(
    get_scala_stopwords_url("123456"),
    paste(remote, "stopWords?code=123456", sep = "/")
  )
})


# epifile.move() and the mtime fix (multi-instance services, plan issue 2) ------------------------
#
# Only the local branch is covered: the remote one needs a running fs API. The destination format
# it sends is tested through to_remote_dest() in test-paths.R.

test_that("epifile.move renames a file locally", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  from <- file.path(data_dir, "detect.json")
  to <- file.path(data_dir, "detect.moved.json")
  writeLines("payload", from)

  expect_true(epifile.move(from, to))

  expect_false(file.exists(from))
  expect_identical(readLines(to), "payload")
})

test_that("epifile.move refuses to move across file systems", {
  # it would be a download or an upload followed by a delete, and the fs API cannot delete
  local_conf(data_dir = "/data", fs_remote = "http://example.com:8080")

  expect_error(
    epifile.move("/data/detect.json", "http://example.com:8080/file/detect.json"),
    "same file system"
  )
})

test_that("epifile.mtime answers about each path of a vector", {
  data_dir <- withr::local_tempdir()
  local_conf(data_dir = data_dir, fs_remote = NULL)
  existing <- file.path(data_dir, "here.json")
  missing <- file.path(data_dir, "gone.json")
  writeLines("", existing)

  times <- epifile.mtime(c(existing, missing))

  # a POSIXct vector, not numbers: the runner leases compare these values with difftime()
  expect_s3_class(times, "POSIXct")
  expect_length(times, 2)
  expect_false(is.na(times[[1]]))
  expect_true(is.na(times[[2]]))
})

# The bind half of the same problem ---------------------------------------------------------------

# get_scala_api_endpoint() above is the *client* half: where do I call the fs. The bind half is the
# Scala API's, which reads fs_url out of properties.json itself and binds it verbatim - so the host
# running setup() on a data directory a container has written to used to bind "fs", a name that
# exists only inside the Docker network, and die on java.net.SocketException: Unresolved address
# before serving a request. The fallback lives in Settings.fsBindHost; what R owes the operator is
# saying so in jobs/fs.log, where the only trace of it used to be a Pekko stack trace.

test_that("warn_if_fs_url_not_bindable() says so when fs_url does not resolve here", {
  local_mocked_bindings(fs_url_resolves_here = function() FALSE, .package = "episomer")
  local_conf(fs_url = "http://fs", fs_port = 8080)

  expect_message(
    expect_true(warn_if_fs_url_not_bindable()),
    "a host this machine cannot resolve"
  )
  # the operator needs both halves: what was configured, and what will be bound instead
  expect_message(warn_if_fs_url_not_bindable(), "http://fs")
  expect_message(warn_if_fs_url_not_bindable(), "localhost:8080")
})

test_that("warn_if_fs_url_not_bindable() is silent on the machine fs_url belongs to", {
  local_mocked_bindings(fs_url_resolves_here = function() TRUE, .package = "episomer")
  local_conf(fs_url = "http://fs", fs_port = 8080)

  expect_silent(expect_false(warn_if_fs_url_not_bindable()))
})
