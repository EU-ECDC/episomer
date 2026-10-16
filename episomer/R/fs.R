# This script includes all dedicated logic for interacting with the Spark/Scala API via HTTP REST requests

# The base URL of the fs API *as seen from this process*.
#
# conf$fs_url is deliberately not the last word here. The Scala API uses it as its **bind
# interface** (scala/src/main/scala/API.scala: `Http().newServerAt(conf.fsUrl.replace("http://",
# ""), conf.fsPort)`) and it is stored in properties.json, which every instance sharing the data
# directory reads. A containerised fs therefore writes there the name it binds to *inside* the
# Docker network ("http://fs", EPISOMER_OPTION_FS_URL in docker/compose.yml), and any other
# machine reading that file - the host running setup() or the Shiny app - would call a name that
# does not exist outside that network, silently concluding that the database is down.
#
# Resolution order, from the most explicit to the last resort:
#
#   0. the session override      this very process started a database on an address of its own and
#                                said so, see set_fs_endpoint_override() below. It comes first
#                                because it is the only rule describing a service the caller has
#                                just launched itself.
#   1. conf$fs_remote           the caller said where the fs is: episomer_app(fs_remote = ...), or
#                               EPI_HOST through the setup_config() default.
#   2. EPI_HOST                 machine-local, and already how the search / detect containers are
#                               told where the fs is. Needed on its own because setup() passes
#                               fs_remote = NULL on purpose (it works on the local disk) and would
#                               otherwise ignore it.
#   3. conf$fs_url:conf$fs_port correct on the fs machine itself, the only process that both binds
#                               and calls that address.
#   4. http://localhost:fs_port when conf$fs_url is a wildcard ("http://0.0.0.0", what a
#                               deployment behind a Service or a load balancer sets it to), or when
#                               its host does not resolve here - the "Docker service name read from
#                               outside the network" case. A wildcard listener answers on localhost,
#                               and in the container case the port is published on the host, so
#                               localhost reaches it either way.
get_scala_api_endpoint <- function() {
  override <- fs_endpoint_override()
  if (!is.null(override)) {
    return(override)
  }
  if (!is.null(conf$fs_remote)) {
    return(prefer_loopback_ipv4(gsub("/$", "", conf$fs_remote)))
  }
  epi_host <- Sys.getenv("EPI_HOST")
  if (epi_host != "") {
    return(prefer_loopback_ipv4(gsub("/$", "", epi_host)))
  }
  prefer_loopback_ipv4(
    if (fs_url_resolves_here()) {
      paste(conf$fs_url, ":", conf$fs_port, sep = "")
    } else {
      paste("http://localhost", ":", conf$fs_port, sep = "")
    }
  )
}

# Where the fs API answers *for this process only*, once this process has started a database of its
# own on an address that is deliberately not the configured one.
#
# The one caller today is the language model retraining of setup(): that step needs a running
# database, and that database must not be found by anybody else while it runs. The services that
# wait for the fs to be healthy before starting - search and detect, `depends_on: service_healthy`
# in docker/compose.yml, a readiness probe in Kubernetes - all watch conf$fs_url:conf$fs_port, so
# a retraining database bound somewhere else is invisible to them, which is the whole point.
#
# It is a *session* value, held in conf and never written to properties.json, and that is the point
# of it. What it replaces was a temporary shift of conf$fs_port followed by save_config(), undone
# in the `finally` of a tryCatch() in setup(). A process killed outright never runs that finally -
# SIGKILL, an OOM kill, a Kubernetes pod evicted or its liveness probe failing - so the shifted
# port stayed in properties.json and the next start shifted it again: 8080, 9080, 10080, 11080,
# with a configuration file nobody had asked to change.
#
# Neither conf$fs_remote nor conf$fs_port would do:
#   * conf$fs_remote does more than name an endpoint - fs_remote() gates to_remote_if_needed()
#     (R/paths.R), so setting it turns every path under the data directory into an API URL, and the
#     retraining works on the local disk;
#   * conf$fs_port is persisted by save_config(), i.e. it outlives the process that changed it.
set_fs_endpoint_override <- function(bind_host, bind_port) {
  conf$fs_endpoint_override <- prefer_loopback_ipv4(sprintf(
    "%s:%s",
    fs_bind_host_url(bind_host),
    bind_port
  ))
  invisible(conf$fs_endpoint_override)
}

# Forgets the override, i.e. goes back to the configured address. Called when the temporary
# database has been stopped; a process that dies before getting here loses it for free, which is
# exactly what the previous conf$fs_port shift could not do.
clear_fs_endpoint_override <- function() {
  if (exists("fs_endpoint_override", envir = conf, inherits = FALSE)) {
    rm("fs_endpoint_override", envir = conf)
  }
  invisible(NULL)
}

fs_endpoint_override <- function() {
  value <- conf$fs_endpoint_override
  if (is.null(value) || length(value) != 1 || is.na(value) || value == "") {
    return(NULL)
  }
  value
}

# A bind host in the "scheme://host" form conf$fs_url is written in, so that "localhost",
# "http://localhost" and "http://localhost/" are all accepted from a caller.
fs_bind_host_url <- function(bind_host) {
  host <- sub("/+$", "", trimws(bind_host))
  if (!grepl("^https?://", host)) {
    host <- paste("http://", host, sep = "")
  }
  host
}

# The host part alone, which is what the Scala API binds: Http().newServerAt() takes an interface,
# not a URL, and Settings.fsBindHost strips the scheme off fs_url for the same reason.
fs_bind_host_only <- function(bind_host) {
  sub("^https?://", "", fs_bind_host_url(bind_host))
}

# Rewrites a "localhost" host part into 127.0.0.1, leaving every other address alone.
#
# The Scala API binds conf$fs_url, "http://localhost" by default, and the JVM turns that into the
# IPv4 loopback: the listening socket is 127.0.0.1:8080 and nothing answers on [::1]:8080. On Linux
# both families are in /etc/hosts, so a client asking for "localhost" is handed 127.0.0.1 and the
# first connection works. On Windows, "localhost" resolves to ::1 *only* - the DNS client answers
# it, there is no hosts entry - so libcurl tries the address nothing listens on, waits for it to
# fail, and only then falls back to IPv4. It pays that on every request, and the API is called once
# per file existence test or modification time: measured on this code path, epifile.mtime() went
# from 10ms to 442ms a call, which is what made the Shiny app take minutes to open.
#
# Done here rather than by changing the defaults because the address reaches this function from
# four different places (conf$fs_remote, EPI_HOST, conf$fs_url, the fallback above) and any of them
# can carry "localhost". Only the exact host "localhost" is touched: a real name is left to the
# resolver, which is the only thing that knows what it should answer.
# The scheme is matched by a look-behind and not by a capturing group on purpose: the replacement
# starts with a digit, and "\\1127.0.0.1" would be read as the eleventh back-reference followed by
# "7.0.0.1".
prefer_loopback_ipv4 <- function(endpoint) {
  sub(
    "(?<=^http://|^https://)localhost(?=$|[:/])",
    "127.0.0.1",
    endpoint,
    perl = TRUE
  )
}

# TRUE when the host part of conf$fs_url can be resolved from this process, i.e. when calling it is
# even worth trying. A bare name with no scheme is treated as the host, which is what
# get_scala_api_endpoint() does with it anyway.
#
# Memoised in conf: a failed lookup costs about half a second, the resolver does not cache the
# miss, and get_scala_api_endpoint() is called on every single API request. The memo is keyed on
# the value it was computed for, so any change of fs_url invalidates it on its own; the port plays
# no part in a name resolution, which is why the port shift of the retraining does not.
fs_url_resolves_here <- function() {
  if (is.null(conf$fs_url) || is.na(conf$fs_url) || conf$fs_url == "") {
    return(FALSE)
  }
  # A wildcard is answered before the resolver is asked, because the resolver says yes: it is a
  # perfectly good *bind* address - "every interface of this machine", which is what a deployment
  # behind a load balancer or a Kubernetes Service wants fs_url to be - and not an address anything
  # can call. Connecting to 0.0.0.0 happens to reach the loopback on Linux and is simply invalid on
  # Windows, so letting it through as an endpoint would put a broken URL in front of every API call
  # of a Windows session reading a properties.json a container wrote. Falling through to rule 4
  # gives localhost:fs_port, which is where a wildcard listener answers anyway.
  if (is_wildcard_bind_host(conf$fs_url)) {
    return(FALSE)
  }
  if (identical(conf$fs_url_resolved_for, conf$fs_url)) {
    return(conf$fs_url_resolves)
  }
  host <- urltools::url_parse(conf$fs_url)$domain
  if (is.na(host) || host == "") {
    host <- conf$fs_url
  }
  resolves <- !is.null(curl::nslookup(host, error = FALSE))
  conf$fs_url_resolved_for <- conf$fs_url
  conf$fs_url_resolves <- resolves
  resolves
}

# TRUE when `host` is a wildcard bind address, i.e. "listen on every interface" rather than the name
# of somewhere to send a request. Both families are covered: 0.0.0.0 for IPv4, :: (also written
# [::]) for IPv6.
is_wildcard_bind_host <- function(host) {
  if (is.null(host) || length(host) != 1 || is.na(host)) {
    return(FALSE)
  }
  bare <- sub("^https?://", "", sub("/+$", "", trimws(host)))
  bare <- sub(":[0-9]+$", "", bare)
  bare %in% c("0.0.0.0", "::", "[::]", "0:0:0:0:0:0:0:0", "[0:0:0:0:0:0:0:0]")
}

# Says so when conf$fs_url names a host this machine cannot resolve, i.e. when the address the Scala
# API is about to bind to does not exist here.
#
# The API binds conf$fs_url verbatim, so an unresolvable value is fatal: Pekko fails the bind and
# the JVM dies on "java.net.SocketException: Unresolved address" before serving a request, having
# already taken the fs lease and written its PID file. This is what jobs/fs.log has to say about it
# in R's own words, on the last line before the process becomes the JVM's and every diagnosis
# arrives as a stack trace.
#
# It is a diagnosis and not a guard: the bind decision belongs to the side that binds, and a
# process that silently listened somewhere other than where it was told to would be the worse
# outcome - under an orchestrator it would pass a loopback health check while the Service routed to
# nothing. What to do about it is on the configuration page: fs_url has to be an interface this
# machine holds, and "http://0.0.0.0" is the value that is one everywhere.
warn_if_fs_url_not_bindable <- function() {
  # A wildcard is bindable - it is the one value that is bindable everywhere - even though
  # fs_url_resolves_here() answers FALSE for it. The two questions differ on purpose: that function
  # asks "is this an address to call", this one asks "is this an address to listen on". Warning here
  # would report a working configuration as broken.
  if (is_wildcard_bind_host(conf$fs_url) || fs_url_resolves_here()) {
    return(invisible(FALSE))
  }
  message(sprintf(
    "conf$fs_url is '%s', a host this machine cannot resolve: the database is about to bind it and will fail to start. That value comes from properties.json, which every instance sharing this data directory reads - a container writes there the Docker service name it binds, and that name means nothing outside that network. Set fs_url on the configuration page to an interface this machine holds ('http://0.0.0.0' is one everywhere).",
    conf$fs_url
  ))
  invisible(TRUE)
}

get_scala_geotraining_url <- function() {
  paste(get_scala_api_endpoint(), "geotraining-set", sep = "/")
}

get_scala_alert_training_url <- function() {
  paste(get_scala_api_endpoint(), "evaluate-alerts", sep = "/")
}

get_scala_posts_url <- function() {
  paste(get_scala_api_endpoint(), "posts", sep = "/")
}

get_scala_geolocated_posts_url <- function() {
  paste(get_scala_api_endpoint(), "geolocated-posts", sep = "/")
}

get_scala_commit_url <- function() {
  paste(get_scala_api_endpoint(), "commit", sep = "/")
}


get_scala_geolocate_text_url <- function() {
  paste(get_scala_api_endpoint(), "geolocate-text", sep = "/")
}

get_scala_aggregate_url <- function() {
  paste(get_scala_api_endpoint(), "aggregate", sep = "/")
}

get_scala_period_url <- function() {
  paste(get_scala_api_endpoint(), "period", sep = "/")
}

get_scala_status_url <- function() {
  paste(get_scala_api_endpoint(), "period?serie=country_counts", sep = "/")
}

get_scala_ping_url <- function() {
  paste(get_scala_api_endpoint(), "ping", sep = "/")
}

get_scala_version_url <- function() {
  paste(get_scala_api_endpoint(), "version", sep = "/")
}

get_scala_recalc_hash_url <- function() {
  paste(get_scala_api_endpoint(), "recalculate-hash", sep = "/")
}

get_scala_file_url <- function(...) {
  paste(get_scala_api_endpoint(), "file", ..., sep = "/")
}

get_scala_secrets_url <- function() {
  paste(get_scala_api_endpoint(), "secret", sep = "/")
}

get_scala_stopwords_url <- function(code) {
  paste(get_scala_api_endpoint(), sprintf("stopWords?code=%s", code), sep = "/")
}

epifile.exists <- function(path) {
  # sapply() over an empty vector gives an empty list, and a list is not a valid subscript, so
  # callers filtering a vector by this answer (get_lang_traing_need()) need logical(0) here
  if (length(path) == 0) {
    return(logical(0))
  }
  sapply(path, function(p) {
    if (!is_remote(p)) {
      file.exists(p)
    } else {
      jsonlite::read_json(sprintf("%s?action=exists", p))
    }
  })
}

epidir.create <- function(path, recursive = FALSE, showWarnings = FALSE) {
  if (!is_remote(path)) {
    dir.create(path, recursive = recursive, showWarnings = showWarnings)
  } else {
    httr2::request(sprintf(
      "%s?action=createDir&createDirs=%s&ignoreIfExists=true",
      path,
      tolower(recursive)
    )) %>%
      httr2::req_method("POST") %>%
      httr2::req_perform()
  }
}
epimd5sum <- function(path, recursive = FALSE) {
  sapply(path, function(p) {
    if (!is_remote(p)) {
      unname(md5sum(p))
    } else {
      jsonlite::read_json(sprintf("%s?action=md5sum", p))
    }
  })
}

epiread_excel <- function(
  path,
  sheet = NULL,
  range = NULL,
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  skip = 0,
  n_max = Inf,
  guess_max = min(1000, n_max),
  .name_repair = "unique"
) {
  if (!is_remote(path)) {
    readxl::read_excel(
      path = path,
      sheet = sheet,
      range = range,
      col_names = col_names,
      col_types = col_types,
      na = na,
      trim_ws = trim_ws,
      skip = skip,
      n_max = n_max,
      guess_max = guess_max,
      .name_repair = .name_repair
    )
  } else {
    tf <- tempfile(pattern = "episomer")
    download.file(path, tf, mode = "wb")
    ret <- readxl::read_excel(
      path = tf,
      sheet = sheet,
      range = range,
      col_names = col_names,
      col_types = col_types,
      na = na,
      trim_ws = trim_ws,
      skip = skip,
      n_max = n_max,
      guess_max = guess_max,
      .name_repair = .name_repair
    )
    file.remove(tf)
    ret
  }
}

epifile.upload <- function(localfile, dest, append = FALSE) {
  httr2::request(sprintf("%s?append=%s", dest, tolower(append))) %>%
    httr2::req_body_multipart(data = curl::form_file(localfile)) %>%
    httr2::req_perform()
}

epifile.touch <- function(path) {
  if (!is_remote(path)) {
    if (!file.exists(path)) {
      write("", file = path, append = FALSE)
    }
    Sys.setFileTime(path, Sys.time())
  } else {
    httr2::request(sprintf("%s?action=touch", path)) %>%
      httr2::req_method("POST") %>%
      httr2::req_perform()
  }
}
# Moves a file, within the local disk or within the fs machine.
#
# Used by the runner leases to release a lease: the lease file is moved out of runners/ instead of
# being deleted (the fs API has no delete action) so that "the file is absent" keeps its single
# meaning of "this runner is free" - see inst/schema/multi_services_plan.md section 3.4.
# A remote to remote move is one POST, so nothing travels through this process.
epifile.move <- function(from, to) {
  if (!is_remote(from) && !is_remote(to)) {
    file.rename(from, to)
  } else if (is_remote(from) && is_remote(to)) {
    httr2::request(sprintf("%s?action=move", from)) %>%
      httr2::req_method("POST") %>%
      # the API resolves the destination against its own epiHome and refuses anything
      # absolute or containing "..", hence the relative form rather than the full URL
      httr2::req_body_json(data = to_remote_dest(to), auto_unbox = TRUE) %>%
      httr2::req_perform()
  } else {
    # a move with one foot on each machine would be a download or an upload followed by a
    # delete, and the fs API cannot delete: no caller needs it, so it is refused loudly
    stop(
      "epifile.move only supports moving within the same file system (both local or both remote)"
    )
  }
}

# Writes the last `n` lines of the local file `from` to `to`, reading backwards by blocks so
# the cost does not depend on the size of the file.
tail_file <- function(from, to, n) {
  size <- file.size(from)
  con <- file(from, "rb")
  on.exit(close(con))
  block <- 64 * 1024
  pos <- size
  found <- 0
  start <- 0
  while (pos > 0 && found < n) {
    len <- min(block, pos)
    pos <- pos - len
    seek(con, pos)
    bytes <- readBin(con, "raw", len)
    nl <- which(bytes == as.raw(10))
    # a trailing newline ends the last line, it does not start another one
    nl <- nl[pos + nl < size]
    if (found + length(nl) >= n) {
      start <- pos + nl[length(nl) - (n - found) + 1]
      found <- n
    } else {
      found <- found + length(nl)
    }
  }
  seek(con, start)
  writeBin(readBin(con, "raw", size - start), to)
  invisible(to)
}

# `tail_lines` > 0 copies only the last lines of a text file (logs); the remote case asks the
# fs API for them (action=tail), a server too old to know it sends the whole file.
epifile.copy <- function(
  from,
  to,
  overwrite = TRUE,
  allow_partial = FALSE,
  tail_lines = 0
) {
  if (!is_remote(from) && !is_remote(to)) {
    if (tail_lines > 0 && file.exists(from) && !file.info(from)$isdir) {
      tryCatch(
        {
          tail_file(from, to, tail_lines)
          TRUE
        },
        error = function(e) {
          # on Windows a log opened without sharing by the process writing it (cmd redirection of
          # the scheduled task) cannot be read: skip it rather than failing the whole caller
          if (!allow_partial) {
            stop(e)
          }
          warning(
            sprintf("Skipping %s: %s", from, conditionMessage(e)),
            call. = FALSE
          )
          FALSE
        }
      )
    } else {
      file.copy(from, to, overwrite = overwrite)
    }
  } else if (is_remote(from) && is_remote(to)) {
    # same destination format as epifile.move above: the API expects a path relative to its
    # own epiHome, and used to be given the full URL of `to`, which it resolved to nonsense.
    # This branch has no call site today (the Shiny download handlers copy to a local
    # destination, alert.R and geo-tagging.R upload a local temp file), so the fix went
    # untested against a real server until a caller shows up.
    httr2::request(sprintf("%s?action=copy", from)) %>%
      httr2::req_method("POST") %>%
      httr2::req_body_json(data = to_remote_dest(to), auto_unbox = TRUE) %>%
      httr2::req_perform()
  } else if (!is_remote(to)) {
    if (!file.exists(dirname(to))) {
      dir.create(dirname(to), recursive = TRUE)
    }
    # a remote file can be mid-write (e.g. fs_loop appending to fs.log) when this reads it, so
    # the reported Content-Length and the actual bytes streamed can disagree; retry a few times
    # rather than failing the whole caller (e.g. create_snapshot) on one flaky file
    max_attempts <- 3
    for (attempt in seq_len(max_attempts)) {
      result <- tryCatch(
        download.file(
          if (tail_lines > 0) {
            sprintf("%s?action=tail&lines=%d", from, as.integer(tail_lines))
          } else {
            from
          },
          to,
          mode = "wb",
          quiet = TRUE
        ),
        error = function(e) e
      )
      if (!inherits(result, "error")) {
        break
      }
      if (attempt == max_attempts) {
        # an append-only file (logs) that keeps growing never gets a clean read: a truncated
        # copy is still useful there, whereas for data files it would be silent corruption
        if (allow_partial && file.exists(to) && file.size(to) > 0) {
          warning(
            sprintf(
              "Partial copy of %s (%s bytes kept): %s",
              from,
              file.size(to),
              conditionMessage(result)
            ),
            call. = FALSE
          )
          break
        }
        if (allow_partial) {
          # nothing could be read at all: typically a log locked by the process writing it
          # (Windows system error 32); a snapshot without that log beats no snapshot
          warning(
            sprintf("Skipping %s: %s", from, conditionMessage(result)),
            call. = FALSE
          )
          break
        }
        stop(result)
      }
      Sys.sleep(1)
    }
  } else {
    #!is_remote(from)
    epifile.upload(from, to)
  }
}

epilist.files <- function(
  path,
  pattern = NULL,
  full.names = FALSE,
  recursive = FALSE
) {
  if (!is_remote(path)) {
    list.files(
      path,
      pattern = pattern,
      full.names = full.names,
      recursive = recursive
    )
  } else {
    files <- unlist(jsonlite::read_json(sprintf(
      "%s?action=list&fullname=%s&recursive=%s",
      path,
      tolower(full.names),
      tolower(recursive)
    )))
    # an fs API older than the fix in API.scala, running on Windows, joins the part below the
    # listed directory with "\" ("http://host/file/alerts\2026\2026.10.07-alerts.json")
    files <- gsub("\\", "/", files, fixed = TRUE)
    if (full.names) {
      files <- to_fs_remote_url(files)
    }
    if (!is.null(pattern)) {
      files <- files[grepl(pattern, files)]
    }
    files
  }
}
# Last modification time of one or several paths, as a POSIXct vector (NA for a missing path).
# The runner leases rely on this function to decide whether a runner is still alive: the mtime is
# produced by the single machine hosting the fs, so no clock skew between runners can make a lease
# look fresh or expired when it is not (see inst/schema/multi_services_plan.md section 3.2).
# The modification time of each element of `path`, NA where there is no answer about one.
#
# The remote branch is wrapped because it is two HTTP calls, and the callers are service loops that
# must not exit on one that fails. health_check() asks this about fs/posts every five seconds from
# the sleep of the detect scheduler (last_fs_updates(), R/scheduler.R), and the answer used to be
# an uncaught error: a 500 from the API ended the whole detect loop on "Execution halted", hours
# after it had done its work. read_runner_lease() (R/runners-lease.R) is the other caller, and it
# already documents NA as "trust the lease until it expires".
#
# NA rather than a retry because that is what this function already means by "no answer about this
# path" - the branch above returns it for a path that does not exist - and every caller handles it.
# Said out loud in the log, so that a genuinely unreachable database is still diagnosable: it is
# health_check() and is_fs_running() that are supposed to notice that, not a service dying.
epifile.mtime <- function(path) {
  do.call(
    c,
    lapply(path, function(p) {
      if (!is_remote(p)) {
        file.mtime(p)
      } else {
        tryCatch(
          {
            # `p`, not `path`: inside the lapply the existence test must be about the path being
            # visited, otherwise a vector of paths gets the answer of its first element only.
            if (!epifile.exists(p)) {
              # a typed NA, so that c()-ing the results keeps a POSIXct vector. A bare NA is
              # logical and would silently turn the whole vector into numbers.
              as.POSIXct(NA)
            } else {
              lubridate::as_datetime(jsonlite::read_json(sprintf(
                "%s?action=mtime",
                p
              )))
            }
          },
          error = function(e) {
            message(sprintf(
              "Could not get the modification time of %s: %s. Reporting it as unknown.",
              p,
              conditionMessage(e)
            ))
            as.POSIXct(NA)
          }
        )
      }
    })
  )
}

epilist.dirs <- function(path, full.names = TRUE) {
  if (!is_remote(path)) {
    list.dirs(path, recursive = FALSE, full.names = full.names)
  } else {
    dirs <- unlist(jsonlite::read_json(sprintf(
      "%s?action=listdir&fullname=%s",
      path,
      tolower(full.names)
    )))
    if (full.names) to_fs_remote_url(dirs) else dirs
  }
}

# Wrapper for writeLines() accepting a destination on a remote fs.
#
# writeLines() needs a connection, and file() on an http:// URL can only ever be opened for
# reading: writing the retraining evaluation straight to get_geotraining_evaluation_path() failed
# with "cannot open the connection" as soon as the fs was reached over HTTP - which is the default,
# conf$fs_remote being "http://localhost:8080" unless a caller says otherwise. The file was then
# simply absent, and get_lang_traing_need() kept answering that the languages still need training.
#
# The text is always written locally first and then placed, atomically on a local disk (rename) or
# by a single upload when the fs is remote, exactly as write_json_atomic() below.
epiwriteLines <- function(text, path, encoding = "UTF-8") {
  file_name <- tail(strsplit(path, "/|\\\\")[[1]], 1)
  if (!is_remote(path)) {
    dir_name <- substring(path, 1, nchar(path) - nchar(file_name) - 1)
    swap_file <- tempfile(
      pattern = paste("~", file_name, sep = ""),
      tmpdir = dir_name
    )
  } else {
    swap_file <- tempfile(
      pattern = paste("~", file_name, sep = "")
    )
  }
  con <- file(swap_file, encoding = encoding)
  writeLines(text, con)
  close(con)
  if (!is_remote(path)) {
    file.rename(swap_file, path)
  } else {
    epifile.upload(swap_file, path)
    file.remove(swap_file)
  }
  invisible(path)
}

# Wrapper for jsonlite write_json ensuring atomic file write it replaces always the existing file. It ignores appends modifiers
write_json_atomic <- function(x, path, ...) {
  file_name <- tail(strsplit(path, "/|\\\\")[[1]], 1)
  if (!is_remote(path)) {
    dir_name <- substring(path, 1, nchar(path) - nchar(file_name) - 1)
    swap_file <- tempfile(
      pattern = paste("~", file_name, sep = ""),
      tmpdir = dir_name
    )
  } else {
    swap_file <- tempfile(
      pattern = paste("~", file_name, sep = ""),
    )
  }
  jsonlite::write_json(x = x, path = swap_file, ...)
  if (!is_remote(path)) {
    file.rename(swap_file, path)
  } else {
    epifile.upload(swap_file, path)
    file.remove(swap_file)
  }
}

fs_remote <- function() {
  !is.null(conf$fs_remote)
}

copy_secret_scripts <- function() {
  for (script_name in c("get_secret.R", "set_secret.R")) {
    script_base <- system.file(
      "extdata",
      script_name,
      package = get_package_name()
    )

    script_folder <- file.path(conf$data_dir, "jobs")
    script <- paste(script_folder, script_name, sep = "/")
    # Copying the script
    file.copy(from = script_base, to = script, overwrite = TRUE)
  }
}


#' @title Runs the episomer embedded database loop
#' @description Infinite loop ensuring that the episomer embedded database is running (Lucene + akka-http)
#' @param data_dir Path to the 'data directory' containing application settings, models and collected posts.
#' If not provided, the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, default: NA
#' @param bind_host Character, the interface the database has to bind instead of \code{conf$fs_url},
#'   with or without a scheme ("localhost" and "http://localhost" are both accepted). Meant for a
#'   database that must not be found on its configured address, see
#'   \code{\link{register_fs_runner_task}}, default: NULL
#' @param bind_port Numeric, the port the database has to bind instead of \code{conf$fs_port},
#'   default: NULL
#' @return nothing
#' @details Launches the episomer embedded database which is accessed via a REST API located on localhost at port 8080. You can test that the database is running by accesssing the local URL and adding "/ping".
#' the REST API provide episomer a way to send and retrieve data related with posts and time series and to trigger geolocation or aggregation
#' The database is implemented using Apache Lucene indexes allowing episomer to access its data as a search engine but also as a tabular database.
#' \code{\link{health_check}} called each 60 seconds on a background process to send alerts to the administrator if some episomer components fail.
#' @examples
#' \dontrun{
#'    #Running the detect loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    fs_loop()
#' }
#' @rdname fs_loop
#' @seealso
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{search_loop}}
#'
#'  \code{\link{health_check}}
#' @importFrom future multisession plan future
#' @importFrom rlang current_env
#' @export
fs_loop <- function(data_dir = NA, bind_host = NULL, bind_port = NULL) {
  if (is.na(data_dir)) {
    setup_config_if_not_already()
  } else {
    setup_config(data_dir = data_dir, fs_remote = NULL)
  }
  data_dir <- conf$data_dir
  # Told by the caller where to bind, so the service is not on the configured address and nothing
  # in this process may look for it there either - the fs monitor below and the health check it
  # runs included. register_fs_runner_task(bind_host =, bind_port =) is what puts these on the
  # command line of jobs/fs.R; see set_fs_endpoint_override() at the top of this file for why they
  # travel as arguments rather than through properties.json.
  if (!is.null(bind_host) || !is.null(bind_port)) {
    bind_host <- if (is.null(bind_host)) conf$fs_url else bind_host
    bind_port <- if (is.null(bind_port)) conf$fs_port else bind_port
    set_fs_endpoint_override(bind_host, bind_port)
    message(sprintf(
      "Binding the episomer database on %s:%s, as asked by the caller instead of the configured %s:%s",
      fs_bind_host_only(bind_host),
      bind_port,
      fs_bind_host_only(conf$fs_url),
      conf$fs_port
    ))
  }
  copy_secret_scripts()
  # calculating alerts per topic
  future::plan(future::multisession)
  monitor <- start_fs_monitor(data_dir, bind_host, bind_port)

  message("Launching fs")
  # Registering the fs runner using current PID and ensuring no other instance of the search is actually running.
  # wait = TRUE, as in the two other service loops: a lease left behind by a killed predecessor is
  # waited out rather than turned into a service that exits and never comes back.
  register_fs_runner(wait = TRUE)
  # Giving the lease and the PID file back when this loop ends, so that another machine can take
  # the fs over immediately instead of waiting for the whole TTL to expire (multi_services_plan.md
  # 3.4). release_runner_lease() checks it is still the holder before moving anything, so an
  # instance that silently lost its lease cannot take away the new holder's file.
  on.exit(unregister_runner(), add = TRUE)

  # Said once, here: from the next line on this process is the JVM's, and whatever it has to say
  # about the address it binds arrives as a stack trace. Skipped when the caller named the
  # interface itself: conf$fs_url is then not the address being bound, so its resolvability is not
  # the question.
  if (is.null(bind_host)) {
    warn_if_fs_url_not_bindable()
  }

  # Infinite loop calling the fs runner
  while (TRUE) {
    tryCatch(
      {
        spark_job(
          paste(
            c(
              "fsService",
              "epiHome",
              # Quoted like every other path handed to the Scala side (see conf_geonames_as_arg()
              # in R/spark-bridge.R): the arguments end up in a command line, and Episomer.main()
              # reads what follows the command as 'key' 'value' pairs. A data_dir holding a space -
              # which is what the default of a Windows installation looks like - would be split
              # into two arguments and the service would refuse to start on an odd argument count.
              paste("\"", conf$data_dir, "\"", sep = ""),
              # Two more optional 'key' 'value' pairs, read by Episomer.main() and handed to
              # API.run(), which binds them instead of conf.fsBindHost / conf.fsPort
              # (scala/src/main/scala/Episomer.scala and API.scala). The bind address of a
              # temporary database has no business being written in the shared properties.json.
              if (!is.null(bind_host)) {
                c("bindHost", fs_bind_host_only(bind_host))
              },
              if (!is.null(bind_port)) c("bindPort", as.character(bind_port))
            ),
            collapse = " "
          )
        )
      },
      error = function(e) {
        message(paste(
          "The episomer Scala API was stopped with the following error",
          e,
          "launching it again within 5 seconds",
          sep = "\n"
        ))
        Sys.sleep(5)
      }
    )
    # spark_job() only gives the control back when the Scala service has stopped, which makes this
    # the one moment this process can look at anything else - hence the monitor check here rather
    # than on a timer that would never fire.
    monitor <- restart_fs_monitor_if_dead(
      monitor,
      data_dir,
      bind_host,
      bind_port
    )
  }
}

# Starts the fs monitor in a background worker and returns its future.
#
# The monitor is the only R code of the fs service still running once fs_loop() has handed its own
# process over to spark_job(), and it carries two things the service cannot do without: the
# heartbeat of the fs lease (renew_fs_runner_lease(), R/runners-lease.R) and health_check().
#
# It is also, from fs_loop()'s point of view, completely mute: future captures the worker's output
# and conditions and replays them only when the value is collected, and fs_loop() never collects
# anything. Twice already the future died on its first lines and took the heartbeat and the alerts
# with it in total silence, the only visible symptom being a lease going stale on a service that was
# running perfectly:
#
#   * setup_config() defaulting fs_remote to "http://localhost:8080" and reading properties.json
#     over an address the Scala API does not bind to (it binds conf$fs_url, "http://fs" in
#     docker/compose.yml) - hence the fs_remote = NULL below;
#   * a leftover fs_mon.PID from the previous run naming a PID that a PSOCK worker of the new run
#     had been given back, so register_fs_monitor() refused to register a runner nobody was running
#     (fixed at the source in release_dead_local_runner_lease(), R/runners-lease.R).
#
# What follows is what makes a third one visible: the loop writes to <data_dir>/fs_mon.log, survives
# what can be survived, and fs_loop() collects the value of a monitor that died anyway.
start_fs_monitor <- function(data_dir, bind_host = NULL, bind_port = NULL) {
  future::future(
    fs_monitor_loop(data_dir, bind_host, bind_port),
    globals = c("data_dir", "bind_host", "bind_port", "fs_monitor_loop"),
    envir = rlang::current_env(),
    # stdout = NA, i.e. "do not capture": future's default is to sink the worker's output into a
    # buffer replayed when the value is collected, and nobody ever collects this one (see above).
    # For a monitor writing to the standard output that buffer is exactly where the lines must not
    # go. Left uncaptured, they come out of the worker process itself, which inherits the standard
    # output of this one (a multisession worker is started without an outfile redirection), so
    # they reach `docker logs` like everything else this process writes.
    stdout = if (logs_to_stdout()) NA else TRUE
  )
}

# The body of the fs monitor, running in the worker started above.
fs_monitor_loop <- function(data_dir, bind_host = NULL, bind_port = NULL) {
  fs_monitor_log("Monitoring episomer", data_dir = data_dir)
  # fs_remote = NULL, exactly as the fs_loop() branch above: this worker runs on the fs machine
  # itself, /episomer-data is its own local disk and the Scala API is not up yet when it starts.
  # This one is fatal and stays fatal - a monitor with no configuration has nothing to monitor - so
  # it is only logged on the way out.
  tryCatch(
    setup_config(data_dir = data_dir, fs_remote = NULL),
    error = function(e) {
      fs_monitor_log(
        paste(
          "stopping, the configuration could not be set up:",
          conditionMessage(e)
        ),
        data_dir = data_dir
      )
      stop(e)
    }
  )
  # Carried over from fs_loop(): this is a separate R process, so the session override of the
  # endpoint (set_fs_endpoint_override(), top of this file) does not come with the globals - and
  # health_check() below calls the API. Without it the monitor would ask conf$fs_url:conf$fs_port
  # about a database bound somewhere else, and report a perfectly healthy service as down.
  if (!is.null(bind_host) || !is.null(bind_port)) {
    set_fs_endpoint_override(
      if (is.null(bind_host)) conf$fs_url else bind_host,
      if (is.null(bind_port)) conf$fs_port else bind_port
    )
  }
  x <- 60
  step <- 2
  while (TRUE) {
    # Nothing in here may end the monitor. Every one of these three calls has a failure mode that
    # is none of the monitor's business and that a later iteration may well see disappear - an
    # fs_mon lease held by another live instance, an fs that has stopped answering /ping, an alert
    # that cannot be sent - while stopping on any of them takes away the fs heartbeat, i.e. it
    # makes a healthy service look dead to the whole cluster. They are logged and retried.
    fs_monitor_try(register_fs_monitor(), "registering the fs monitor")
    # Beating for the fs runner as well: the fs process itself is inside spark_job() and will
    # not come back to renew anything (see renew_fs_runner_lease(), R/runners-lease.R).
    fs_monitor_try(renew_fs_runner_lease(), "renewing the fs lease")
    if (x == 0) {
      fs_monitor_try(health_check(), "running the health check")
      x <- 60
    }
    x <- x - step
    Sys.sleep(step)
  }
  0
}

# Runs `expr`, and turns a failure into one line of fs_mon.log instead of the end of the monitor.
#
# `expr` is a promise forced inside the tryCatch(), so the error is caught wherever in the call it
# happened. Rate limited: see fs_monitor_log_once().
fs_monitor_try <- function(expr, what) {
  tryCatch(
    expr,
    error = function(e) {
      fs_monitor_log_once(paste0(what, " failed: ", conditionMessage(e)))
      invisible(NULL)
    }
  )
}

# Last time each monitor message was written, for the rate limiting below. Lives in the worker
# process, which is where the monitor loop runs.
fs_monitor_last_log <- new.env(parent = emptyenv())

# Logs `m`, at most once per `span` seconds for a given message.
#
# The monitor iterates every two seconds and the failures it survives are the lasting kind, so
# logging each of them thirty times a minute would bury the line that matters and grow fs_mon.log
# without bound. A repeated message is written once a minute, its repetitions are dropped.
fs_monitor_log_once <- function(m, span = 60, data_dir = conf$data_dir) {
  last <- fs_monitor_last_log[[m]]
  if (
    !is.null(last) &&
      as.numeric(difftime(Sys.time(), last, units = "secs")) < span
  ) {
    return(invisible(FALSE))
  }
  fs_monitor_last_log[[m]] <- Sys.time()
  fs_monitor_log(m, data_dir = data_dir)
  invisible(TRUE)
}

# Appends one timestamped line to <data_dir>/fs_mon.log, or to the standard output when the
# deployment asked for it (logs_to_stdout(), R/config.R).
#
# A file, and deliberately not message(): the monitor runs in a future worker whose messages nobody
# ever reads (see start_fs_monitor()) and which future accumulates in a buffer meant to be replayed
# when the value is collected - for a loop that runs for weeks and never returns, writing to that
# buffer is a slow leak, not a log. The stdout branch is written with the same write(), to the
# worker's own standard output, which start_fs_monitor() keeps uncaptured and which the worker
# inherits from this process - a real file descriptor, not a captured condition, so neither of the
# two problems above applies to it.
#
# Never raises: a monitor that cannot write its log still has a lease to renew.
fs_monitor_log <- function(m, data_dir = conf$data_dir) {
  line <- paste0(strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [fs_mon]: ", m)
  try(
    if (logs_to_stdout()) {
      write(line, file = "")
    } else {
      write(line, file = file.path(data_dir, "fs_mon.log"), append = TRUE)
    },
    silent = TRUE
  )
  invisible(line)
}

# Restarts the monitor if it has died, and says in the logs of the fs process why it had to.
#
# The monitor loop never returns, so a resolved future is a monitor that ended on an error.
# Collecting its value is the only way to get that error out of the worker: it is raised here,
# caught, and turned into a message that reaches `docker logs` - fs_mon.log has it too, but an
# operator looking at a service that stopped collecting should not have to know that.
restart_fs_monitor_if_dead <- function(
  monitor,
  data_dir,
  bind_host = NULL,
  bind_port = NULL
) {
  if (!future::resolved(monitor)) {
    return(monitor)
  }
  tryCatch(
    future::value(monitor),
    error = function(e) {
      message(paste(
        "The episomer fs monitor stopped with the following error",
        conditionMessage(e),
        "starting it again",
        sep = "\n"
      ))
    }
  )
  start_fs_monitor(data_dir, bind_host, bind_port)
}

#' @title perform full text search on posts collected with episomer
#' @description  perform full text search on posts collected with episomer (posts migrated from episomer v<1.0.x are also included)
#' @param query character. The query to be used if a text it will match the post text. To see how to match particular fields please see details, default:NULL
#' @param topic character, Vector of topics to include on the search default:NULL
#' @param from an object which can be converted to ‘"POSIXlt"’ only posts posted after or on this date will be included, default:NULL
#' @param to an object which can be converted to ‘"POSIXlt"’ only posts posted before or on this date will be included, default:NULL
#' @param countries character or numeric, the position or name of episomer regions to be included on the query, default:NULL
#' @param mentioning character, limit the search to the posts mentioning the given users, default:NULL
#' @param users character, limit the search to the posts created by the provided users, default:NULL
#' @param hide_users logical, whether to hide user names on output replacing them by the USER keyword, default:FALSE
#' @param action character, an action to be performed on the search results respecting the max parameter. Possible values are 'delete' or 'anonymise' , default:NULL
#' @param max integer, maximum number of posts to be included on the search, default:100
#' @param by_relevance logical, whether to sort the results by relevance of the matching query or by indexing order, default:FALSE
#' If not provided the system will try to reuse the existing one from last session call of \code{\link{setup_config}} or use the EPI_HOME environment variable, default: NA
#' @return a data frame containing the posts matching the selected filters, the data frame contains the following columns: user_name, created_date, is_geo_located, is_quote, text,
#' text_loc, user_id, hash, quote_lang, totalCount, created_at, topic_post_id, topic, lang, user_name, quoted_text, id, quopted_text_loc, tags
#' @details
#' episomer translate the query provided by all parameters into a single query that will be executed on post indexes which are weekly indexes.
#' The q parameter should respect the syntax of the Lucene classic parser \url{https://lucene.apache.org/core/8_5_0/queryparser/org/apache/lucene/queryparser/classic/QueryParser.html}
#' So other than the provided parameters, multi field queries are supported by using the syntax field_name:value1;value2
#' AND, OR and -(for excluding terms) are supported on q parameter.
#' Order by week is always applied before relevance so even if you provide by_relevance = TRUE all of the matching posts of the first week will be returned first
#' @examples
#' \dontrun{
#'    #Running the detect loop
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    df <- search_posts(
#'         q = "vaccination",
#'         topic="COVID-19",
#'         countries=c("Chile", "Australia", "France"),
#'         from = Sys.Date(),
#'         to = Sys.Date()
#'    )
#'    df$text
#' }
#' @rdname search_posts
#' @seealso
#'  \code{\link{search_loop}}
#'
#'  \code{\link{detect_loop}}
#' @importFrom utils URLencode
#' @importFrom jsonlite stream_in
#' @export
search_posts <- function(
  query = NULL,
  topic = NULL,
  from = NULL,
  to = NULL,
  countries = NULL,
  mentioning = NULL,
  users = NULL,
  hide_users = FALSE,
  action = NULL,
  max = 100,
  by_relevance = FALSE
) {
  u <- paste(
    get_scala_posts_url(),
    "?jsonnl=true&estimatecount=true&by_relevance=",
    tolower(by_relevance),
    sep = ""
  )
  if (hide_users) {
    u <- paste(u, "&hide_users=true", sep = "")
  }
  if (!is.null(action)) {
    u <- paste(u, "&action=", action, sep = "")
  }
  if (!is.null(query)) {
    u <- paste(u, "&q=", utils::URLencode(query, reserved = T), sep = "")
  }
  if (!is.null(topic)) {
    u <- paste(
      u,
      "&topic=",
      utils::URLencode(paste(topic, sep = "", collapse = ";"), reserved = T),
      sep = "",
      collapse = ""
    )
  }
  if (!is.null(from)) {
    u <- paste(u, "&from=", strftime(from, format = "%Y-%m-%d"), sep = "")
  }
  if (!is.null(to)) {
    u <- paste(u, "&to=", strftime(to, format = "%Y-%m-%d"), sep = "")
  }
  if (
    !is.null(countries) &&
      length(countries) > 0 &&
      (is.character(countries) || any(countries != 1))
  ) {
    # getting all regions
    regions <- get_country_items()
    # If countries are names they have to be changes to region indexes
    if (is.character(countries)) {
      countries <- (1:length(regions))[sapply(
        1:length(regions),
        function(i) regions[[i]]$name %in% countries
      )]
    }
    country_codes <- Reduce(
      function(l1, l2) {
        unique(c(l1, l2))
      },
      lapply(as.integer(countries), function(i) unlist(regions[[i]]$codes))
    )
    u <- paste(
      u,
      "&country_code=",
      paste(country_codes, collapse = ";", sep = ""),
      sep = "",
      collapse = ""
    )
  }
  if (!is.null(mentioning)) {
    u <- paste(
      u,
      "&mentioning=",
      paste(mentioning, sep = "", collapse = ";"),
      sep = ""
    )
  }
  if (!is.null(users)) {
    u <- paste(u, "&user=", paste(users, sep = "", collapse = ";"), sep = "")
  }

  if (!is.null(max)) {
    u <- paste(u, "&max=", max, sep = "")
  }
  #message(u)
  u <- url(u)
  posts <- jsonlite::stream_in(u, verbose = FALSE)
  posts
}


# Registers a query to define custom aggregations on posts
# it is generic enough to choose variables, aggregation and filters
# deals with post deduplication at topic level
# regexp: regexp to limit the geolocation and search files to read
# vars: variable to read (evaluated after aggregation if required)
# sort_by: order to apply to the returned data frame
# filter_by: expressions to use for filtering posts
# sources_exp: variables to limit the source files to read (setting this will improve reading performance)
# handler: function that to perform a custom R based transformation on data returned by SPARK
# params: definition of custom param files to enable big queries
set_aggregated_posts <- function(
  name,
  dateCol,
  pks,
  aggr,
  vars = list("*"),
  group_by = list(),
  sort_by = list(),
  filter_by = list(),
  sources_exp = list(),
  params = list()
) {
  stop_if_no_config(paste("Cannot get posts without configuration setup"))
  post_result <- httr::POST(
    url = get_scala_aggregate_url(),
    httr::content_type_json(),
    body = jsonlite::toJSON(
      list(
        name = name,
        dateCol = dateCol,
        pks = pks,
        aggr = aggr,
        aggregation = list(
          columns = vars,
          groupBy = group_by,
          sortBy = sort_by,
          filterBy = filter_by,
          sourceExpressions = sources_exp,
          params = if (length(params) == 0) {
            list(fake_param_epi = "")
          } else {
            params
          }
        )
      ),
      simplify_vector = T,
      auto_unbox = T
    ),
    encode = "raw",
    encoding = "UTF-8"
  )
  if (httr::status_code(post_result) != 200) {
    print(substring(
      httr::content(post_result, "text", encoding = "UTF-8"),
      1,
      100
    ))
    stop()
  }
}


# Utility function to interact with a POST endpoint and process data on a streaming way provided by handler
stream_post <- function(uri, body, handler = NULL) {
  cum <- new.env()
  cum$tail <- c()
  cum$dfs <- list()

  if (!is.null(handler)) {
    # If a custom transformation is going to be done (a handler function has been set)
    # this function will be called on pages of 10k lines using the jsonlite stream_in function
    # the transformed results will be stored on a temporary file that will be read by stream_in
    tmp_file <- tempfile(pattern = "episomer", fileext = ".json")
    #message(tmp_file)
    con_tmp <- file(tmp_file, open = "w", encoding = "UTF-8")
  }

  pipeconnection <- function(x, handler = handler) {
    cuts <- which(x == 0x0a)
    if (length(cuts) == 0) {
      cum$tail <- c(cum$tail, x)
    } else {
      if (tail(cuts, n = 1) != length(x)) {
        bytes <- c(cum$tail, x[1:tail(cuts, n = 1)])
        cum$tail <- x[(tail(cuts, n = 1) + 1):length(x)]
      } else {
        bytes <- c(cum$tail, x)
        cum$tail <- c()
      }
      con <- rawConnection(bytes, "rb")
      on.exit(close(con))
      if (is.null(handler)) {
        cum$dfs[[length(cum$dfs) + 1]] <- jsonlite::stream_in(
          con,
          verbose = FALSE
        )
      } else {
        jsonlite::stream_in(
          con,
          function(df) handler(df, con_tmp),
          verbose = FALSE
        )
      }
    }
  }
  # Doing the post request
  parts <- gregexpr("//[^/]+/", uri)
  split <- parts[[1]][[1]] + attr(parts[[1]], "match.length")
  #h <- httr::handle(substr(uri, 1, split-1))
  #path <- substr(uri, split, nchar(uri))
  #on.exit(httr::handle_reset(h))

  post_result <- tryCatch(
    {
      httr::POST(
        url = uri,
        httr::write_stream(function(x) {
          pipeconnection(x, handler)
        }),
        body = body,
        httr::content_type_json(),
        encode = "raw",
        encoding = "UTF-8"
      )
    },
    error = function(e) {
      closeAllConnections()
      stop(e)
    }
  )
  if (httr::status_code(post_result) != 200) {
    #message(uri)
    message(httr::status_code(post_result))
    message(httr::content(post_result, "text", encoding = "UTF-8"))
    stop()
  }

  #Applying last line if necessary
  if (length(cum$tail) > 0) {
    if (cum$tail[[length(cum$tail)]] != 0x0a) {
      cum$tail[[length(cum$tail) + 1]] <- as.raw(0x0a)
    }
    con <- rawConnection(cum$tail, "rb")
    if (is.null(handler)) {
      cum$dfs[[length(cum$dfs) + 1]] <- jsonlite::stream_in(
        con,
        verbose = FALSE
      )
    } else {
      cum$dfs[[length(cum$dfs) + 1]] <- jsonlite::stream_in(
        con,
        function(df) handler(df, con_tmp),
        verbose = F
      )
    }
    close(con)
  }
  #Transforming single lines responses in data frames
  #lapply(cum$dfs, function(l) if(typeof(l) == "list") data.frame(lapply(l, function(x) t(data.frame(x)))) else l)
  #joining response
  if (is.null(handler)) {
    jsonlite::rbind_pages(cum$dfs)
  } else {
    close(con_tmp)
    con_tmp <- file(tmp_file, open = "r", encoding = "UTF-8")
    ret <- jsonlite::stream_in(con_tmp, pagesize = 10000, verbose = FALSE)
    close(con_tmp)
    unlink(tmp_file)
    ret
  }
}

# Get time difference since last request
# This function is used from the Shiny app to report when was the last time that a request saved posts
# This is done by taking the last modified date of current year post files
last_fs_updates <- function(
  collections = c("posts", "topwords", "country_counts", "geolocated")
) {
  times <- lapply(collections, function(collection) {
    time <- epifile.mtime(epipath("fs", collection))
    if (!is.null(time)) {
      time
    } else {
      NA
    }
  })
  setNames(times, collections)
}

commit_posts <- function() {
  post_result <- httr::POST(
    url = get_scala_commit_url(),
    httr::content_type_json(),
    encode = "raw",
    encoding = "UTF-8"
  )
  if (httr::status_code(post_result) != 200) {
    stop(print(substring(
      httr::content(post_result, "text", encoding = "UTF-8"),
      1,
      100
    )))
  }
}

# Version of the jar executed by the fs service, or NA when fs is unreachable or too old to
# expose /version. Compared in the Shiny config page with the version of the installed package.
get_fs_jar_version <- function() {
  tryCatch(
    {
      res <- httr::GET(url = get_scala_version_url(), httr::timeout(2.0))
      if (res$status_code != 200) {
        return(NA_character_)
      }
      jsonlite::fromJSON(httr::content(
        res,
        as = "text",
        encoding = "UTF-8"
      ))$version
    },
    error = function(e) NA_character_
  )
}
