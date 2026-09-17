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

test_that("get_scala_api_endpoint reads the port from conf on every call", {
  # The endpoint must never be frozen together with the host resolution: the memo of
  # fs_url_resolves_here() is keyed on fs_url alone, because a port plays no part in a name lookup.
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

# The session override is rule 0 of the resolution order: this process started a database of its
# own - the language retraining of setup() - and says where. It has to beat every other rule,
# including the two explicit ones, because it describes a service the caller has just launched.
test_that("the fs endpoint override wins over fs_remote, EPI_HOST and fs_url", {
  withr::local_envvar(EPI_HOST = "http://from-env:8080")
  local_conf(
    "fs_remote" = "http://explicit:1234",
    fs_url = "http://fs",
    fs_port = 8080
  )
  set_fs_endpoint_override("localhost", 9080)
  expect_equal(get_scala_api_endpoint(), "http://127.0.0.1:9080")

  # And it goes away without leaving anything behind, which is the whole reason it exists rather
  # than a temporary shift of conf$fs_port written to properties.json.
  clear_fs_endpoint_override()
  expect_null(fs_endpoint_override())
  expect_equal(get_scala_api_endpoint(), "http://explicit:1234")
})

test_that("clear_fs_endpoint_override is a no-op when no override was set", {
  local_conf("fs_remote" = NULL, fs_url = "http://localhost", fs_port = 8080)
  expect_null(fs_endpoint_override())
  expect_silent(clear_fs_endpoint_override())
  expect_null(fs_endpoint_override())
})

test_that("a bind host is accepted with or without a scheme", {
  # conf$fs_url carries a scheme ("http://localhost") while Http().newServerAt() takes an
  # interface, so both forms have to be understood on the way in and only one on the way out.
  expect_equal(fs_bind_host_url("localhost"), "http://localhost")
  expect_equal(fs_bind_host_url("http://localhost"), "http://localhost")
  expect_equal(fs_bind_host_url("http://localhost/"), "http://localhost")
  expect_equal(fs_bind_host_url(" localhost "), "http://localhost")

  expect_equal(fs_bind_host_only("localhost"), "localhost")
  expect_equal(fs_bind_host_only("http://0.0.0.0"), "0.0.0.0")
  expect_equal(fs_bind_host_only("https://fs/"), "fs")
})

test_that("set_fs_endpoint_override normalises localhost to the IPv4 loopback", {
  # Same reason as prefer_loopback_ipv4() above: on Windows "localhost" resolves to ::1 only,
  # where the Scala API does not listen, and every request pays a failed connection first.
  local_conf("fs_remote" = NULL, fs_url = "http://localhost", fs_port = 8080)
  set_fs_endpoint_override("localhost", 9080)
  expect_equal(fs_endpoint_override(), "http://127.0.0.1:9080")
  clear_fs_endpoint_override()
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
  # The mirror question of the two above: 0.0.0.0 is the one address that binds everywhere, and the
  # API hands fs_url to newServerAt() without inspecting it. Warning here would call a working
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
# Scala API's, which reads fs_url out of properties.json and binds it verbatim - so a value that
# does not name an interface of this machine fails the bind and kills the JVM on
# java.net.SocketException: Unresolved address, from a service that has already taken the lease.
# What R owes the operator is saying so in jobs/fs.log in words, on the last line before the
# process becomes the JVM's and the only remaining trace would be a Pekko stack trace.

test_that("warn_if_fs_url_not_bindable() says so when fs_url does not resolve here", {
  local_mocked_bindings(fs_url_resolves_here = function() FALSE, .package = "episomer")
  local_conf(fs_url = "http://fs", fs_port = 8080)

  expect_message(
    expect_true(warn_if_fs_url_not_bindable()),
    "a host this machine cannot resolve"
  )
  # the operator needs the configured value, what is about to happen to it, and a way out
  expect_message(warn_if_fs_url_not_bindable(), "http://fs")
  expect_message(warn_if_fs_url_not_bindable(), "will fail to start")
  expect_message(warn_if_fs_url_not_bindable(), "0\\.0\\.0\\.0")
})

test_that("warn_if_fs_url_not_bindable() is silent on the machine fs_url belongs to", {
  local_mocked_bindings(fs_url_resolves_here = function() TRUE, .package = "episomer")
  local_conf(fs_url = "http://fs", fs_port = 8080)

  expect_silent(expect_false(warn_if_fs_url_not_bindable()))
})

# The retraining POST and its two libcurl bounds ---------------------------------------------------

# retrain_languages() holds one HTTP request open for as long as the JVM takes to index and retrain
# every language, receiving nothing while it waits. It used to say options(timeout = 1800), which is
# a base R setting httr never reads, so the real bound was libcurl's default low-speed abort: less
# than 1 byte a second for 600 seconds. A retraining longer than ten minutes therefore died at
# exactly 600 s with the training already running, and in a container took the whole service down
# with it.

test_that("retrain_languages() bounds its request by fs_batch_timeout and allows a silent one", {
  local_conf(fs_remote = NULL, fs_batch_timeout = 3600)
  seen <- NULL
  local_mocked_bindings(
    get_geotraining_df = function() data.frame(a = 1),
    get_geotraining_evaluation_path = function() tempfile(),
    .package = "episomer"
  )
  local_mocked_bindings(
    POST = function(url, ...) {
      seen <<- Filter(function(x) inherits(x, "request"), list(...))
      structure(list(status_code = 200L), class = "response")
    },
    status_code = function(x) 200L,
    content = function(...) "ok",
    .package = "httr"
  )

  retrain_languages()

  opts <- do.call(c, lapply(seen, `[[`, "options"))
  # the total bound is what the Scala side gives itself for a batch of this kind
  expect_equal(opts$timeout, 3600)
  # and the low-speed abort is off, so a JVM that says nothing for an hour is not an error
  expect_equal(opts$low_speed_limit, 0)
})

test_that("retrain_languages() writes the evaluation where the fs is, local or remote", {
  # The evaluation used to be written with file() + writeLines(), and file() on an http:// URL can
  # only be opened for reading: on the default configuration (conf$fs_remote =
  # "http://localhost:8080") the write died on "cannot open the connection", the file was never
  # produced, and get_lang_traing_need() kept asking for a retraining that had just succeeded.
  local_mocked_bindings(
    POST = function(url, ...) structure(list(status_code = 200L), class = "response"),
    status_code = function(x) 200L,
    content = function(...) "[{\"test\":\"fr\"}]",
    .package = "httr"
  )

  # local fs: the text lands on disk
  local({
    dest <- withr::local_tempfile(fileext = ".json")
    local_conf(fs_remote = NULL, fs_batch_timeout = 3600)
    local_mocked_bindings(
      get_geotraining_df = function() data.frame(a = 1),
      get_geotraining_evaluation_path = function() dest,
      .package = "episomer"
    )

    retrain_languages()

    expect_true(file.exists(dest))
    expect_equal(readLines(dest, encoding = "UTF-8"), "[{\"test\":\"fr\"}]")
  })

  # remote fs: the text is uploaded to the same path instead, and nothing is left behind
  uploaded <- NULL
  local_conf(fs_remote = "http://localhost:8080", fs_batch_timeout = 3600)
  local_mocked_bindings(
    get_geotraining_df = function() data.frame(a = 1),
    get_geotraining_evaluation_path = function()
      "http://localhost:8080/file/geo-training-evaluation.json",
    epifile.upload = function(localfile, dest, append = FALSE) {
      uploaded <<- list(dest = dest, text = readLines(localfile, encoding = "UTF-8"))
    },
    .package = "episomer"
  )

  retrain_languages()

  expect_equal(uploaded$dest, "http://localhost:8080/file/geo-training-evaluation.json")
  expect_equal(uploaded$text, "[{\"test\":\"fr\"}]")
})

test_that("epifile.exists() answers logical(0) about no path at all", {
  # get_lang_traing_need() filters its language codes by this answer, and sapply() over an empty
  # vector returns an empty list, which is not a valid subscript
  local_conf(fs_remote = NULL)

  expect_identical(epifile.exists(character(0)), logical(0))
  expect_identical(c("fr")[epifile.exists(character(0))], character(0))
})

test_that("options(timeout) does not bound an httr request, which is why it is not used", {
  # The reason the line above is not options(timeout = ): base R's setting is read by download.file()
  # and url(), and libcurl never looks at it. Asserted on the option list httr actually builds
  # rather than by making a request, so nothing here waits on a socket.
  cfg <- httr::config(timeout = 3600, low_speed_limit = 0)
  expect_equal(cfg$options$timeout, 3600)
  expect_equal(cfg$options$low_speed_limit, 0)
  expect_null(httr::config()$options$timeout)
})

# A failed request is not an answer, and not a reason to exit -------------------------------------

# epifile.mtime() is called by service loops: health_check() asks it about fs/posts every five
# seconds from the sleep of the detect scheduler, and read_runner_lease() asks it about the lease
# file. It used to let an API error through, and the detect loop then ended on "Execution halted" -
# observed on a 500 the API returned while Lucene was merging segments under fs/posts, a file
# having been deleted between the directory listing and the stat (API.recursiveLastModifiedTime).

test_that("epifile.mtime reports an unreachable path as NA instead of erroring", {
  local_conf(fs_remote = "http://127.0.0.1:1/")
  local_mocked_bindings(
    epifile.exists = function(...) TRUE,
    .package = "episomer"
  )
  local_mocked_bindings(
    read_json = function(...) stop("HTTP status was '500 Internal Server Error'"),
    .package = "jsonlite"
  )

  expect_message(
    got <- epifile.mtime(epipath("fs", "posts")),
    "Reporting it as unknown"
  )
  expect_s3_class(got, "POSIXct")
  expect_true(is.na(got))
})

test_that("epifile.mtime keeps answering about the other paths of a vector", {
  # one bad answer must not cost the good ones, and the result has to stay a POSIXct vector
  local_conf(fs_remote = "http://127.0.0.1:1/")
  local_mocked_bindings(epifile.exists = function(...) TRUE, .package = "episomer")
  seen <- 0L
  local_mocked_bindings(
    read_json = function(...) {
      seen <<- seen + 1L
      if (seen == 1L) stop("500") else "2026-09-10T10:37:58Z"
    },
    .package = "jsonlite"
  )

  got <- suppressMessages(epifile.mtime(epipath("fs", c("posts", "topwords"))))

  expect_s3_class(got, "POSIXct")
  expect_length(got, 2L)
  expect_true(is.na(got[[1]]))
  expect_false(is.na(got[[2]]))
})
