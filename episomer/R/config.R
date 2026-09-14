# Environment for storing configuration
conf <- new.env()

# Get package name
get_package_name <- function() environmentName(environment(setup_config))

# Get the keyring for the provided backend or a platform dependent default if backend is null
# this function is used to save the social media and smtp credentials on a secure store when saving properties
# this function uses the keyring R package which provides a common interface for accessing system dependent keyring
# backend: backend name to use it will be guessed based on user OS if not defined.
get_key_ring <- function(backend = NULL) {
  if (!is.null(backend)) {
    Sys.setenv(kr_backend = backend)
  }
  Sys.setenv(kr_name = "episomer_kr_name")
  Sys.setenv(kr_service = "kr_service")
  keyring = "file"#Sys.getenv("kr_backend")
  if (keyring == "secret_service") {
    kb <- keyring::backend_secret_service$new()
  } else if (keyring == "wincred") {
    kb <- keyring::backend_wincred$new()
  } else if (keyring == "macos") {
    kb <- keyring::backend_macos$new()
  } else if (keyring == "file") {
    kb <- keyring::backend_file$new()
  } else {
    kb <- keyring::backend_env$new()
  }
  kr_name <- NULL
  # creating the file keyring if requested and not existent
  if (keyring == "file") {
    kr_name <- Sys.getenv("kr_name")
    krs <- kb$keyring_list()
    if (nrow(krs[krs$keyring == kr_name, ]) == 0) {
      kb$keyring_create(kr_name, get_episomer_kr_password())
    }
    kb$keyring_set_default(kr_name)
  }

  # unlocking keyring if unlocked using environment variable to get the password
  if (kb$keyring_is_locked(keyring = kr_name)) {
    kb$keyring_unlock(
        keyring = kr_name,
        password = get_episomer_kr_password()
    )
    # unlocking keyring if unlocked using password prompt
  }
  return(kb)
}

# Set a secret from the chosen secret management backend
set_secret <- function(secret, value) {
  if(!fs_remote()) {
    get_key_ring()$set_with_value(
      service = Sys.getenv("kr_service"),
      username = secret,
      password = value
    )
  } else {
    ret <- secrets_api_set(secret, value)
    if(ret$status != "ok")
       stop(secrets_api_reason("Error on setting secret", ret))
  }
}

# The request both directions of the /secret endpoint of the fs API are built from.
#
# The API reads and writes the keyring by running an Rscript sub process, and when that process
# fails it answers 406 with the process stderr in the body ("Could not get the secret
# exit(<code>):<err>", API.scala). That body is the only place the real cause is written - a wrong
# EPISOMER_KR_PASSWORD, an Rscript missing from the PATH of the API process, a keyring that cannot
# be read - and it used to be dropped on both sides:
#
#   * reading went through jsonlite::read_json(), i.e. url(), which reports "cannot open the
#     connection to '<the whole URL>' ... HTTP status was '406 Not Acceptable'" and nothing else,
#     printing the passphrase in clear on the console and into any screenshot of one;
#   * writing raised the httr2 error of the 406, whose message is the status line alone.
#
# req_error(body = ) puts the stderr of the sub process in the message instead, and the URL stays
# out of it.
secrets_api_request <- function(secret, ...) {
  httr2::request(get_scala_secrets_url()) %>%
    httr2::req_url_query(
      secret = secret,
      passphrase = get_episomer_kr_password(),
      ...
    ) %>%
    httr2::req_error(body = function(resp) {
      body <- try(httr2::resp_body_string(resp), silent = TRUE)
      if (inherits(body, "try-error") || !nzchar(body)) NULL else body
    })
}

# The API completes with a JsObject turned into a String, so its answer is JSON served as
# text/plain and resp_body_json() would refuse it on its content type. simplifyVector = FALSE
# keeps the shape read_json() returned: a list of scalars.
secrets_api_answer <- function(resp) {
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

# The endpoint also answers "ko" *with a 200* when the sub process ran but did nothing useful -
# get_secret.R writing "Not found" on stderr, say. The reason is then in the answer, not in an
# HTTP error, and saying it beats a bare "could not".
secrets_api_reason <- function(what, ret) {
  paste0(
    what,
    ": ",
    if (is.null(ret$message)) "no reason given by the fs API" else ret$message
  )
}

secrets_api_get <- function(secret) {
  secrets_api_answer(secrets_api_request(secret) %>% httr2::req_perform())
}

secrets_api_set <- function(secret, value) {
  secrets_api_answer(
    secrets_api_request(secret, value = value) %>%
      httr2::req_method("POST") %>%
      httr2::req_perform()
  )
}

# Checks whether a secret is set on the secret management backend
is_secret_set <- function(secret) {
  if(!fs_remote()) {
    secrets <- get_key_ring()$list(service = Sys.getenv("kr_service"))
    env_secrets <- get_env_secrets()
    return(nrow(secrets[secrets$username == secret, ]) > 0 || exists(secret, where = env_secrets))

  } else {
    secrets_api_get(secret)$status == "ok"
  }
}

get_episomer_kr_password <- function() {
  if (
    Sys.getenv("EPISOMER_KR_PASSWORD") == "" &
      Sys.getenv("EPISOMER_KR_PASSWORD_FILE") == ""
  ) {
    stop(paste(
      "No encryption password available for the episomer secrets.",
      "Set the environment variable 'EPISOMER_KR_PASSWORD' with the password, or",
      "'EPISOMER_KR_PASSWORD_FILE' with the path of a file whose first line is the password.",
      "Put them in the user",
      "'.Renviron' file, which every R process reads at start up.",
      "To create or open this file please use usethis::edit_r_environ(). Restart your R session once the .Renviron file has been filled.",
      sep = "\n"
    ))
  }

  if (Sys.getenv("EPISOMER_KR_PASSWORD") != "") {} else if (
    Sys.getenv("EPISOMER_KR_PASSWORD_FILE") != "" &&
      file.exists(Sys.getenv("EPISOMER_KR_PASSWORD_FILE"))
  ) {
    con <- file(
      Sys.getenv("EPISOMER_KR_PASSWORD_FILE"),
      "r",
      encoding = "UTF-8"
    )
    val <- readLines(con, n = 1, warn = FALSE)
    close(con)
    Sys.setenv(EPISOMER_KR_PASSWORD = val)
  }
  return(Sys.getenv("EPISOMER_KR_PASSWORD"))
}
# Get a secret from the chosen secret management backend
get_secret <- function(secret) {
  if(!fs_remote()) {
    secrets <- get_key_ring()$list(service = Sys.getenv("kr_service"))
    env_secrets <- get_env_secrets()
    if (nrow(secrets[secrets$username == secret, ]) > 0)
        get_key_ring()$get(service = Sys.getenv("kr_service"), username = secret)
    else if( exists(secret, where = env_secrets)) { 
        env_secrets[[secret]]
    } else {
        stop("Could not found the requested secret")
    }
  } else {
    res <- secrets_api_get(secret)
    if(res$status == "ok")
       res$value
    else
       stop(secrets_api_reason("Could not retrieve the secret", res))
  }
}

# get empty config for initialization, this represents default values for all configuration properties
get_empty_config <- function(data_dir) {
  ret <- list()
  ret$keyring <- "file"
    #if (.Platform$OS.type == "windows") "wincred" else if (
    #  Sys.info()[['sysname']] == "Darwin"
    #)
    #  "macos" else "file"
  ret$data_dir <- data_dir
  ret$collect_span <- 60
  ret$schedule_span <- 90
  ret$schedule_start_hour <- 8
  ret$languages <- list(
    list(
      code = "en",
      name = "English",
      vectors = paste(ret$data_dir, "languages/en.txt.gz", sep = "/"),
      modified_on = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    list(
      code = "fr",
      name = "French",
      vectors = paste(ret$data_dir, "languages/fr.txt.gz", sep = "/"),
      modified_on = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    list(
      code = "es",
      name = "Spanish",
      vectors = paste(ret$data_dir, "languages/es.txt.gz", sep = "/"),
      modified_on = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    list(
      code = "pt",
      name = "Portuguese",
      vectors = paste(ret$data_dir, "languages/pt.txt.gz", sep = "/"),
      modified_on = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  ret$lang_updated_on <- NA
  ret$geonames_updated_on <- NA
  ret$dep_updated_on <- NA
  ret$geotag_requested_on <- NA
  ret$aggregate_requested_on <- NA
  ret$alerts_requested_on <- NA
  ret$geonames_url <- "http://download.geonames.org/export/dump/allCountries.zip"
  ret$geolocation_threshold <- 5
  ret$known_users <- list()
  ret$spark_cores <- {
    all_cores <- floor(
      parallel::detectCores(all.tests = FALSE, logical = TRUE) * 0.3
    )
    if (is.na(all_cores)) 1 else if (all_cores < 1) 1 else all_cores
  }
  ret$spark_memory <- "4g"
  ret$onthefly_api <- .Platform$OS.type != "windows"
  ret$topics <- list()
  ret$topics_md5 <- ""
  ret$alert_alpha <- 0.025
  ret$alert_alpha_outlier <- 0.05
  ret$alert_k_decay <- 4
  ret$alert_history <- 7
  ret$alert_same_weekday_baseline <- FALSE
  ret$alert_with_quotes <- FALSE
  ret$alert_with_bonferroni_correction <- TRUE
  ret$use_mkl <- FALSE
  ret$geonames_simplify <- TRUE
  ret$regions_disclaimer <- ""
  ret$smtp_host <- ""
  ret$smtp_port <- 25
  ret$smtp_from <- ""
  ret$smtp_login <- ""
  ret$smtp_password <- ""
  ret$smtp_insecure <- FALSE
  ret$force_date_format <- ""
  ret$maven_repo <- "https://repo1.maven.org/maven2"
  ret$winutils_url <- "https://github.com/steveloughran/winutils/raw/master/hadoop-3.0.0/bin/winutils.exe"
  ret$fs_url <- "http://localhost"
  ret$fs_port <- 8080
  # Where the *temporary* database of the language model retraining binds (setup(), option 5).
  #
  # Deliberately a pair of its own and not fs_url / fs_port, which they are often mistaken for.
  # fs_url and fs_port describe a service other machines have to reach; this pair describes one
  # that nobody else may reach, and the two requirements are opposites. Reusing fs_url here would
  # bind the retraining database on whatever the real service binds - a Docker service name, a
  # wildcard - and either fail where that name is not an address this machine holds, or publish on
  # every interface a database whose whole purpose is to be invisible.
  #
  # localhost is the default because it is the one address that is bindable and callable on every
  # platform, needs no name resolution, no published port and no service routing, and stays
  # reachable while an orchestrator still considers the pod unready. The port is fixed and known
  # rather than derived from fs_port: what waits for the real service to be healthy watches
  # fs_port, and a deployment exposes ports explicitly and cannot expose a moving target.
  #
  # Change them here, in properties.json, or with EPISOMER_OPTION_FS_SETUP_HOST /
  # EPISOMER_OPTION_FS_SETUP_PORT - the latter if 9080 is already taken on the machine.
  ret$fs_setup_host <- "localhost"
  ret$fs_setup_port <- 9080
  ret$fs_batch_timeout <- 60 * 60
  ret$fs_query_timeout <- 60
  # Runner leases (see R/runners-lease.R and inst/schema/multi_services_plan.md section 6.5).
  # All in seconds. They tune the exclusion mechanism that stops two machines sharing the same
  # data directory from acting as the same runner, so they belong to the *cluster*, not to one
  # machine: they are stored in properties.json, which every instance reads, and they are
  # deliberately absent from the Shiny configuration page. A TTL mistyped in the UI would silently
  # break exclusivity for every machine at once, and these are deployment knobs, not analysis
  # settings. Change them in properties.json, or through EPISOMER_OPTION_RUNNER_* on a first
  # deployment (see coerce_env_props() and the precedence note in setup_config()).
  #
  # Seconds between two lease renewals. It is also the granularity of sleep_with_heartbeat():
  # a loop sleeping for 40 minutes still proves it is alive every runner_heartbeat_span seconds.
  # Must stay well below every TTL below - roughly one order of magnitude - so that a few lost
  # renewals in a row (a slow fs, a network hiccup) do not expire a perfectly healthy lease.
  ret$runner_heartbeat_span <- 30
  # Time after which a lease that stopped being renewed is considered free, per service. Too short
  # means false expiries, i.e. two runners collecting at once; too long only delays the takeover by
  # a standby machine. Erring on the long side is therefore the safe direction.
  #   search : the longest expected silence is a rate-limit wait, chunked by the heartbeat
  #   detect : one single task (alerts, geolocation, aggregation) can legitimately run for ages
  #   fs     : shortest, because a dead fs is also detected by /ping
  ret$runner_ttl_search <- 300
  ret$runner_ttl_detect <- 1800
  ret$runner_ttl_fs <- 120
  # Seconds between the optimistic write of a claim and the read that confirms it. The fs API has
  # no compare-and-swap, so two instances claiming at the same moment both write and the later
  # write wins; this pause has to be longer than the worst-case time for a write to land, so that
  # the loser really sees the winner's identity when it reads back.
  ret$runner_claim_guard <- 5
  # How a service reacts to a lease that is not free yet.
  # Inf, i.e. "wait as long as it takes", is the default because the situation it covers is a
  # service whose predecessor was killed without releasing anything - every `docker compose down`,
  # every crash - and the right thing to do then is to start as soon as the lease expires. It also
  # gives the standby machine of a two-machine deployment for free: it waits, and the day the
  # primary stops, it collects. A finite number of seconds turns the wait back into the historical
  # refusal once it runs out, for a deployment whose supervisor would rather see the service die.
  ret$runner_wait_timeout <- Inf
  ret$runner_wait_interval <- 15
  ret$admin_email <- ""
  # Each social media family is switched on or off as a whole by its sm_activated_<family> /
  # sm_alerts_<family> pair. A federated family such as mastodon is made of independent instances
  # listed in <family>_instances: each of them is collected as a network of its own named
  # <family>.<instance> (see R/sm.R), and the family switches gate all of them at once.
  ret$sm_alerts_bluesky <- TRUE
  ret$sm_activated_bluesky <- TRUE
  ret$sm_alerts_mastodon <- FALSE
  ret$sm_activated_mastodon <- FALSE
  # domains of the mastodon instances to collect from e.g. list("mastodon.social", "mstdn.party"),
  # managed from the social media menu of setup(). Stored as a list so it stays a JSON array.
  ret$mastodon_instances <- list()
  ret$dismiss_past_request <- "1971-01-01 00:00:00"
  ret$dismiss_past_done <- "2000-01-01 00:00:00"
  ret$collect_history_days <-30
  # Mastodon backfill stop criterion: number of CONSECUTIVE search requests returning no post
  # inside the target window (created_at >= plan_min_date) before a plan is considered finished.
  # A single below-window request is not trusted because Mastodon's created_at is noisy (a
  # "supposedly older" request can still surface more recent posts, see sm_api_search_mastodon).
  ret$mastodon_max_empty_requests <- 5
  return(ret)
}

# Identity of the running episomer instance ------------------------------------------------------
#
# An "instance" is one R process (a search loop, a detect loop, the fs service or the Shiny app).
# Once several machines share the same data directory through the fs API, they need a way to say
# "this runner is me" versus "this runner is somebody else": that is what conf$instance_id and
# conf$instance_host are for. See inst/schema/multi_services_plan.md (sections 2 and 4).
#
# Both are *session state*, not configuration: they describe the current process, so they must
# never end up in properties.json, which is shared by every machine (see save_config).
#
# A third piece of session state joins them once a loop has started: conf$runner_name, the name of
# the runner this process holds the lease for. It is set by register_runner() (R/runners.R) rather
# than here, because only a process that successfully claimed a lease may carry one, and it is what
# lets renew_runner_lease() / stop_if_lease_lost() / release_runner_lease() be called with no
# argument. It is absent in the Shiny app, which is a reader of the runner state and not a runner.

# Random identifier for this R process, e.g. "7nn0ei1963ddf23a7".
#
# What matters here is uniqueness inside the cluster, not cryptographic quality: the identifier is
# public (anybody reaching the fs API can read it) and is only compared for equality.
#
# tempfile() is used rather than sample.int() because it does not consume the R random number
# generator: a script calling set.seed() before setup_config() must not get a predictable, and
# therefore possibly colliding, identity. The temp directory name (Rtmp<random>) is drawn once per
# process and the file name adds a per-process random part, so the pair separates two processes
# started at the same second, on the same machine or on two different ones.
new_instance_id <- function() {
  tolower(paste0(sub("^Rtmp", "", basename(tempdir())), basename(tempfile(""))))
}

# Name of the machine this process runs on.
#
# Informational only (the Shiny app displays "search.bluesky is running on machine-B"), so a
# readable value is worth more than a unique one - the instance_id already carries uniqueness.
# Container host names are random hex strings, hence the EPISOMER_INSTANCE override that
# deployments are expected to set (see docker/compose.yml).
get_instance_host <- function() {
  host <- Sys.getenv("EPISOMER_INSTANCE")
  if (host != "") host else Sys.info()[["nodename"]]
}

#' @title Load episomer application settings
#' @description Load episomer application settings from the designated data directory
#' @param data_dir Path to the directory containing the application settings (it must exist).
#' If not provided it takes the value of the latest call to setup_config in the current session, or the value of the EPI_HOME environment variable or episomer subdirectory in the working directory,
#' default: if (exists("data_dir", where = conf)) conf$data_dir else if (Sys.getenv("EPI_HOME") !=
#'    "") Sys.getenv("EPI_HOME") else file.path(tempdir(), "episomer")
#' @param ignore_keyring Whether to skip loading settings from the keyring (social media and SMTP credentials), default: FALSE
#' @param ignore_properties Whether to skip loading settings managed by the Shiny app in properties.json file, Default: FALSE
#' @param ignore_topics Whether to skip loading settings defined in the topics.xlsx file and download plans from topics.json file, default: FALSE
#' @param save_properties_first Whether to save current settings before loading new ones from disk, default: FALSE
#' @param save_topics_first List of social media to save its topics to disk before loading new ones from disk, default: list()
#' @param fs_remote address of the central instance of episomer for accessing
#' @return Nothing
#' @details episomer relies on settings and data stored in a system folder, so before loading the dashboard, collecting posts or detecting alerts the user has to designate this folder.
#' When a user wants to use episomer from the R console they will need to call this function for initialisation.
#' The 'data_folder' can also be given as a parameter for program launch functions \code{\link{episomer_app}}, \code{\link{search_loop}} or \code{\link{detect_loop}}, which will internally call this function.
#'
#' This call will fill (or refresh) a package scoped environment 'conf' that will store the settings. Settings stored in conf are:
#' \itemize{
#'   \item{General properties of the Shiny app (stored in properties.json)}
#'   \item{Download plans from the social media collection process (stored in topics.json merged with data from the topics.xlsx file}
#'   \item{Credentials for social media APIs and SMTP stored in the defined keyring}
#'   \item{The identity of the running instance: 'instance_id', a random identifier generated once
#'     per R process, and 'instance_host', the name of the machine. Both describe the current
#'     process rather than the shared configuration, so they are never written to properties.json.
#'     'instance_host' is taken from the environment variable 'EPISOMER_INSTANCE' when it is set,
#'     and falls back to the system host name otherwise. Setting 'EPISOMER_INSTANCE' is recommended
#'     when running episomer in containers, whose host names are random, and when several machines
#'     share the same data directory: it is the name the dashboard displays to tell which machine a
#'     service is running on.}
#' }
#'
#' When calling this function and the keyring is locked, a password will be prompted to unlock the keyring.
#' This behaviour can be changed by setting the environment variable 'ecdc_social_tool_kr_password' with the password.
#'
#' Changes made to conf can be stored permanently (except for 'data_dir') using:
#' \itemize{
#'   \item{\code{\link{save_config}}, or}
#'    \item{\code{\link{sm_api_set_auth_bluesky}}}
#'    \item{\code{\link{sm_api_set_auth_mastodon}}}
#' }
#' @examples
#' \dontrun{
#'    library(episomer)
#'    #loading system settings
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#' }
#' @seealso
#' \code{\link{save_config}}
#' \code{\link{sm_api_set_auth_bluesky}}
#' \code{\link{sm_api_set_auth_mastodon}}
#' \code{\link{episomer_app}}
#' \code{\link{search_loop}}
#' \code{\link{detect_loop}}
#' @rdname setup_config
#' @export
#' @importFrom jsonlite read_json
#' @importFrom tools md5sum
#' @importFrom readxl read_excel
setup_config <- function(
  data_dir = if (exists("data_dir", where = conf)) conf$data_dir else if (
    Sys.getenv("EPI_HOME") != ""
  )
    Sys.getenv("EPI_HOME") else file.path(tempdir(), "episomer"),
  ignore_keyring = FALSE,
  ignore_properties = FALSE,
  ignore_topics = FALSE,
  save_properties_first = FALSE,
  save_topics_first = list(),
  fs_remote =  if(Sys.getenv("EPI_HOST") != "") Sys.getenv("EPI_HOST") else "http://localhost:8080"
) {
  #setting the data_dir which is a read only property
  # The tilde is expanded here and not left to the callers: data_dir ends up inside the command
  # lines built in R/spark-bridge.R, which are handed to system()/shell(). On Linux those go
  # through a shell that expands "~" itself, but on Windows they go to cmd.exe, which does not:
  # a data_dir of "~/EPI_HOME" would produce a "-cp" entry matching no file at all and the Java
  # side would fail with a NoClassDefFoundError on a class coming from the downloaded jars.
  # The local variable is reassigned too so that everything derived from it below (the default
  # language vector paths, the properties written to disk) keeps a single, expanded form.
  data_dir <- path.expand(data_dir)
  # On Windows only, backslashes are turned into forward slashes: path.expand() returns the home
  # directory with backslashes, and data_dir is used as a regex in to_remote_if_needed()
  # (R/paths.R) as well as inside quoted command lines. Forward slashes are understood everywhere
  # on Windows and keep a single separator style in the paths derived from it. The substitution is
  # not done on the other platforms, where a backslash is a legal character in a file name.
  if (.Platform$OS.type == "windows") {
    data_dir <- gsub("\\\\", "/", data_dir)
  }
  conf$data_dir <- data_dir
  # Normalised the same way as everything get_scala_api_endpoint() hands out (see
  # prefer_loopback_ipv4() in R/fs.R). Not only for the speed it buys on Windows: is_remote()
  # (R/paths.R) decides whether a path is an API URL by comparing its host with the host of
  # conf$fs_remote, and the URLs it is compared against are built from
  # get_scala_api_endpoint(). Leaving "localhost" here while that function answers 127.0.0.1 would
  # make every remote path look local and send the file access back to a disk that, on an app-only
  # machine, holds nothing.
  conf$fs_remote <- if (is.null(fs_remote)) {
    fs_remote
  } else {
    prefer_loopback_ipv4(fs_remote)
  }
  # Identity of this process, see new_instance_id() above. It is deliberately set here, before
  # anything is read from disk, so every code path going through setup_config() has it available.
  #
  # instance_id is generated *once per process*: setup_config() is called again on every
  # configuration refresh (the Shiny app calls it on each change), and handing out a new identity
  # each time would make a running loop look like a different instance, i.e. lose the runner lease
  # it legitimately holds. inherits = FALSE keeps the lookup inside conf itself and does not walk
  # up to the package namespace, where an unrelated `instance_id` binding could exist.
  if (!exists("instance_id", envir = conf, inherits = FALSE)) {
    conf$instance_id <- new_instance_id()
  }
  # instance_host, on the other hand, is refreshed on each call: it costs nothing and it lets a
  # deployment fix a wrong EPISOMER_INSTANCE without restarting the process.
  conf$instance_host <- get_instance_host()
  # paths contains two files storing configuration data:
  # props which contains properties set on the Shiny App is stored on data_dir/properties.json
  # and data_dir/topics.json which stores search progress and is updated by the search loop
  props_path = get_properties_path()
  

  # save_first may be used by a function which is responsible for a part of the configuration to save changes on its perimeter before refreshing
  if (save_properties_first || length(save_topics_first) > 0) {
    save_config(
      data_dir = data_dir,
      properties = save_properties_first,
      sm_topics = save_topics_first,
      fs_remote = fs_remote
    )
  }
  #Loading last created configuration from json file on temp variable if exists or load default empty conf instead, this will ensure new settings are loaded with default values if missing
  temp <- get_empty_config(data_dir)
  if (!ignore_properties) {
    # Merging default values with those stored in the properties.json file. Precedence goes to
    # the last source of the list, so: defaults < EPISOMER_OPTION_* environment < properties.json.
    # An environment variable therefore only sets a property that properties.json does not
    # already carry - which, since save_config() writes them all, in practice means the first
    # start of a fresh data directory.
    # coerce_env_props() gives the environment values the type of the matching default, so that
    # a number stays a number whichever of the three sources it came from.
    # The env-var merge must run unconditionally, even when properties.json does not exist yet
    # (a brand new data directory) - that is precisely the case the paragraph above describes.
    # Only the properties.json read itself is conditional, to avoid erroring on a file that is
    # not there yet, locally or remotely (epifile.exists() is remote-aware, see R/fs.R).
    temp = merge_configs(list(
      temp,
      coerce_env_props(get_env_props(), temp),
      if (epifile.exists(props_path)) {
        jsonlite::read_json(
          props_path,
          simplifyVector = FALSE,
          auto_unbox = TRUE
        )
      } else {
        list()
      }
    ))
    #Setting config  variables filled only from json file
    # Symmetrically to save_config, the properties are copied one by one: an instance_id or an
    # instance_host left in properties.json by an older version, or added by hand, must never
    # overwrite the identity of the running process set at the top of this function.
    conf$keyring <- temp$keyring
    conf$collect_span <- temp$collect_span
    conf$schedule_span <- temp$schedule_span
    conf$schedule_start_hour <- temp$schedule_start_hour
    conf$languages <- temp$languages
    for (i in 1:length(conf$languages)) {
      conf$languages[[i]]$vectors <- file.path(
        conf$data_dir,
        "languages",
        paste(conf$languages[[i]]$code, "txt", "gz", sep = ".")
      )
    }
    conf$lang_updated_on <- temp$lang_updated_on
    conf$geonames_updated_on <- temp$geonames_updated_on
    conf$dep_updated_on <- temp$dep_updated_on
    conf$geotag_requested_on <- temp$geotag_requested_on
    conf$aggregate_requested_on <- temp$aggregate_requested_on
    conf$alerts_requested_on <- temp$alerts_requested_on
    conf$geonames_url <- temp$geonames_url
    conf$known_users <- temp$known_users
    conf$spark_cores <- temp$spark_cores
    conf$spark_memory <- temp$spark_memory
    conf$onthefly_api <- temp$onthefly_api
    conf$geolocation_threshold <- temp$geolocation_threshold
    conf$alert_alpha <- temp$alert_alpha
    conf$alert_alpha_outlier <- temp$alert_alpha_outlier
    conf$alert_k_decay <- temp$alert_k_decay
    conf$alert_history <- temp$alert_history
    conf$alert_same_weekday_baseline <- temp$alert_same_weekday_baseline
    conf$alert_with_quotes <- temp$alert_with_quotes
    conf$alert_with_bonferroni_correction <- temp$alert_with_bonferroni_correction
    conf$use_mkl <- temp$use_mkl
    conf$geonames_simplify <- temp$geonames_simplify
    conf$regions_disclaimer <- temp$regions_disclaimer
    conf$smtp_host <- temp$smtp_host
    conf$smtp_port <- temp$smtp_port
    conf$smtp_from <- temp$smtp_from
    conf$smtp_login <- temp$smtp_login
    conf$smtp_insecure <- temp$smtp_insecure
    conf$force_date_format <- temp$force_date_format
    conf$maven_repo <- temp$maven_repo
    conf$winutils_url <- temp$winutils_url
    conf$fs_url <- temp$fs_url
    conf$fs_port <- temp$fs_port
    conf$fs_setup_host <- temp$fs_setup_host
    conf$fs_setup_port <- temp$fs_setup_port
    conf$fs_batch_timeout <- temp$fs_batch_timeout
    conf$fs_query_timeout <- temp$fs_query_timeout
    # Runner lease tuning. runner_lease_ttl() and runner_claim_guard() (R/runners-lease.R) read
    # these from conf and fall back to their own hardcoded defaults when they are absent, so an
    # older properties.json - or a conf built without going through setup_config() - keeps working.
    conf$runner_heartbeat_span <- temp$runner_heartbeat_span
    conf$runner_ttl_search <- temp$runner_ttl_search
    conf$runner_ttl_detect <- temp$runner_ttl_detect
    conf$runner_ttl_fs <- temp$runner_ttl_fs
    conf$runner_claim_guard <- temp$runner_claim_guard
    conf$runner_wait_timeout <- temp$runner_wait_timeout
    conf$runner_wait_interval <- temp$runner_wait_interval
    conf$admin_email <- temp$admin_email
    conf$dismiss_past_request <- temp$dismiss_past_request
    conf$collect_history_days <- temp$collect_history_days
    conf$mastodon_max_empty_requests <- temp$mastodon_max_empty_requests
    # The properties declaring the social media are not a fixed list: a family added with
    # add_new_social_media() brings its own switches, and each federated family brings its list of
    # instances. We therefore copy whatever temp holds (the defaults merged with the properties
    # file) rather than a hardcoded set, dropping the ones currently in conf first so a family
    # removed from the file is really forgotten.
    rm(list = sm_config_properties(conf), envir = conf)
    for (sm_prop in sm_config_properties(temp)) {
      conf[[sm_prop]] <- temp[[sm_prop]]
    }
  }
  if (!ignore_topics) {
    #topics_path is the path to the excel file containing the topics provided by the user or episomer default ones
    user_topics_path <- get_user_topics_path()
    plans_path = get_plans_paths()
    # updating plans and topics if requested
    plans_exists <- FALSE
    topics_plans <- lapply(plans_path, function(p) {
      if (epifile.exists(p)) {
        plans_exists <- TRUE
        tops <- jsonlite::read_json(p, simplifyVector = FALSE, auto_unbox = TRUE)
	for(i in seq(1, length(tops$topics), along.with=tops$topics)) {
	  tops$topics[[i]]$topic = tolower(tops$topics[[i]]$topic)
	}
	tops
      }
      else
        list()
    })
    # merging topics from different SM
    merged_topics <- merge_config_lists(topics_plans, "topics")
    # merging initial + properties.json data with plans
    temp = merge_configs(list(temp, merged_topics))

    #Getting topics from excel topics files if it has changed since last load this is identified by checking the md5 signature
    #If user has not overrriden
    topics_changed <- FALSE
    topics <- {
      t <- list()
      t$md5 <- epimd5sum(user_topics_path)
      if (t$md5 != temp$topics_md5 || !plans_exists) {
        t$df <- epiread_excel(user_topics_path)
        topics_changed <- TRUE
      }
      t
    }

    #Merging topics from config json and topic excel topics if this last one has changed
    #Each time a topic is found on file, all its occurrences will be processed at the same time, to ensure consistent multi query topics updates based on position
    if (
      topics_changed &&
        exists("df", where = topics) &&
        length(active_social_media()) > 0
    ) {
      # changing topics to lowercase
      topics$df$Topic <- tolower(topics$df$Topic)
      distinct_topics <- as.list(unique(topics$df$Topic))
      adjusted_topics <- list()
      i_adjusted <- 1
      #For each distinct topic on Excel file
      for (i_topic in 1:length(distinct_topics)) {
        topic <- distinct_topics[[i_topic]]
        if (!grepl("^[A-Za-z_0-9][A-Za-z_0-9 \\-]*$", topic)) {
          stop(paste(
            "topic name",
            topic,
            "is invalid, it must contains only by alphanumeric letters, digits spaces '-' and '_' and not start with spaces, '-' or '_'",
            sep = " "
          ))
        }
        queries <- topics$df[topics$df$Topic == topic, ]
        active_sm <- active_social_media()
        # For each active social media
        for (sm in active_sm) {
          i_tmp <- 1
          #For each distinct query on Excel file on current topic
          for (i_query in 1:nrow(queries)) {
            for (translated_query in translate_query(
              sm,
              queries$Query[[i_query]]
            )) {
              #Looking for the next matching entry in json file
	      while (
                i_tmp <= length(temp$topics) &&
                  ((temp$topics[[i_tmp]]$topic != topic &&
                    temp$topics[[i_tmp]]$network == sm) ||
                    temp$topics[[i_tmp]]$network != sm)
              ) {
                i_tmp <- i_tmp + 1
              }
              if (i_tmp <= length(temp$topics)) {
                #reusing an existing query
                adjusted_topics[[i_adjusted]] <- temp$topics[[i_tmp]]
                adjusted_topics[[i_adjusted]]$network <- sm
                adjusted_topics[[i_adjusted]]$query <- translated_query
                adjusted_topics[[i_adjusted]]$label <- queries$Label[[i_query]]
                adjusted_topics[[i_adjusted]]$alpha <- if (
                  !is.null(queries$Alpha[[i_query]]) &&
                    !is.na(queries$Alpha[[i_query]])
                )
                  queries$Alpha[[i_query]] else conf$alert_alpha
                adjusted_topics[[i_adjusted]]$alpha_outlier <- (if (
                  !is.null(queries$`Outliers Alpha`[[i_query]]) &&
                    !is.na(queries$`Outliers Alpha`[[i_query]])
                )
                  queries$`Outliers Alpha`[[i_query]] else
                  conf$alert_alpha_outlier)
              } else {
                #creating a new query
                adjusted_topics[[i_adjusted]] <- list()
                adjusted_topics[[i_adjusted]]$network <- sm
                adjusted_topics[[i_adjusted]]$query <- translated_query
                adjusted_topics[[i_adjusted]]$topic <- queries$Topic[[i_query]]
                adjusted_topics[[i_adjusted]]$label <- queries$Label[[i_query]]
                adjusted_topics[[i_adjusted]]$alpha <- if (
                  !is.null(queries$Alpha[[i_query]]) &&
                    !is.na(queries$Alpha[[i_query]])
                )
                  queries$Alpha[[i_query]] else conf$alert_alpha
                adjusted_topics[[i_adjusted]]$alpha_outlier <- (if (
                  !is.null(queries$`Outliers Alpha`[[i_query]]) &&
                    !is.na(queries$`Outliers Alpha`[[i_query]])
                )
                  queries$`Outliers Alpha`[[i_query]] else
                  conf$alert_alpha_outlier)
              }
              i_tmp <- i_tmp + 1
              i_adjusted <- i_adjusted + 1
            }
          }
        }
      }
      temp$topics <- adjusted_topics
      temp$topics_md5 <- topics$md5
    }

    #Loading topic related information on config file
    conf$topics_md5 <- temp$topics_md5
    conf$topics <- temp$topics
    conf$dismiss_past_done <- temp$dismiss_past_done
    copy_plans_from(temp)
    # updating topic keywords, necessary for ignoring keywords in top words charts
    if (topics_changed && plans_exists) update_topic_keywords()
  }
  #Getting variables stored on keyring
  #Setting up keyring
  if (!ignore_keyring) {
    kr <- get_key_ring(conf$keyring)
    # Fetching and updating variables from keyring
    if (is_secret_set("smtp_password")) {
      conf$smtp_password <- get_secret("smtp_password")
    }
  }
}

# Copying plans from temporary file (non-typed) to conf, making sure plans have the right type
copy_plans_from <- function(temp) {
  #Copying plans
  if (length(temp$topics) > 0) {
    for (i in 1:length(temp$topics)) {
      if (
        !exists("plan", where = temp$topics[[i]]) ||
          length(temp$topics[[i]]$plan) == 0
      ) {
        conf$topics[[i]]$plan <- list()
      } else {
        conf$topics[[i]]$plan <- (lapply(
          1:length(temp$topics[[i]]$plan),
          function(j)
            do.call(parse_plan_attributes, args = temp$topics[[i]]$plan[[j]])
        ))
      }
    }
  }
}

# The indexes of the conf$topics entries collected by one network.
#
# conf$topics is flat: one entry per (topic, query, network) triple, every network together, built
# by the adjusted_topics loop of setup_config() above. A topic with several queries - "coronavirus
# OR covid-19" split by translate_query() - contributes one entry per query per network, so five
# topic entries per network is what three Excel topics look like, and fifteen is what they look
# like with three networks activated.
#
# Anything acting on behalf of a single network therefore has to select its own entries first, and
# the entry's own `network` field is the only authority on that. A *plan's* `network` is not: a
# plan is created by whichever worker first saw the entry without one, carrying that worker's
# network (update_plans_schedule(), R/plans.R). Filtering on the plan instead of the entry is what
# used to make every search worker collect every network's topics on a fresh start - three
# identical requests per topic with three networks activated - while the files on disk stayed
# clean, because this function is also what decides what gets written.
#
# Returns indexes rather than the entries so that callers can write back into conf$topics.
topics_of_network <- function(network) {
  which(vapply(
    conf$topics,
    function(t) identical(t$network, network),
    logical(1)
  ))
}

#' @title Save the configuration changes
#' @description Permanently saves configuration changes to the data folder (excluding social media credentials, but not SMTP credentials)
#' @param data_dir Path to a directory to save configuration settings if fs_remote is provided this value is ignored, Default: conf$data_dir
#' @param properties Whether to save the general properties to the properties.json file, default: TRUE
#' @param sm_topics Whether to save topic download plans to the topics.json file, default: TRUE
#' @param fs_remote Path to the remote master instance of episomer, default: conf$fs_remote"
#' @return Nothing
#' @details Permanently saves configuration changes to the data folder (excluding social media credentials, but not SMTP credentials)
#' to save social media credentials please use \code{\link{sm_api_set_auth_bluesky}} or
#' \code{\link{sm_api_set_auth_mastodon}}
#' @examples
#' \dontrun{
#'    library(episomer)
#'    #load configuration
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    #make some changes
#'    #conf$collect_span = 90
#'    #saving changes
#'    save_config()
#' }
#' @rdname save_config
#' @seealso
#' \code{\link{setup_config}}
#' \code{\link{sm_api_set_auth_bluesky}}
#' \code{\link{sm_api_set_auth_mastodon}}
#' @export
save_config <- function(
  data_dir = conf$data_dir,
  properties = TRUE,
  sm_topics = NULL,
  fs_remote = conf$fs_remote
) {
  # creating the data directory if it does not exists
  if (!epifile.exists(conf$data_dir)) {
    epidir.create(conf$data_dir, showWarnings = FALSE)
  }
  if (is.null(sm_topics)) {
    sm_topics <- active_social_media()
  }

  if (properties) {
    # saving properties to the properties.json file.
    # This list is built property by property on purpose: conf also holds session state describing
    # *this* process - data_dir, fs_remote, and the instance_id / instance_host of the running
    # instance - and properties.json is shared by every machine using the same data directory.
    # Writing an identity there would hand it to every other instance. Do not replace this by a
    # dump of conf.
    temp <- list()
    temp$collect_span <- conf$collect_span
    temp$schedule_span <- conf$schedule_span
    temp$schedule_start_hour <- conf$schedule_start_hour
    temp$languages <- conf$languages
    temp$dep_updated_on <- conf$dep_updated_on
    temp$lang_updated_on <- conf$lang_updated_on
    temp$geonames_updated_on <- conf$geonames_updated_on
    temp$geotag_requested_on <- conf$geotag_requested_on
    temp$aggregate_requested_on <- conf$aggregate_requested_on
    temp$alerts_requested_on <- conf$alerts_requested_on
    temp$geonames_url <- conf$geonames_url
    temp$keyring <- conf$keyring
    temp$known_users <- conf$known_users
    temp$spark_cores <- conf$spark_cores
    temp$spark_memory <- conf$spark_memory
    temp$onthefly_api <- conf$onthefly_api
    temp$geolocation_threshold <- conf$geolocation_threshold
    temp$geolocation_threshold <- conf$geolocation_threshold
    temp$alert_alpha <- conf$alert_alpha
    temp$alert_alpha_outlier <- conf$alert_alpha_outlier
    temp$alert_k_decay <- conf$alert_k_decay
    temp$alert_history <- conf$alert_history
    temp$alert_same_weekday_baseline <- conf$alert_same_weekday_baseline
    temp$alert_with_quotes <- conf$alert_with_quotes
    temp$alert_with_bonferroni_correction <- conf$alert_with_bonferroni_correction
    temp$use_mkl <- conf$use_mkl
    temp$geonames_simplify <- conf$geonames_simplify
    temp$regions_disclaimer <- conf$regions_disclaimer
    temp$smtp_host <- conf$smtp_host
    temp$smtp_port <- conf$smtp_port
    temp$smtp_from <- conf$smtp_from
    temp$smtp_login <- conf$smtp_login
    if (!is.null(conf$smtp_password) && conf$smtp_password != "")
      set_secret("smtp_password", conf$smtp_password)
    temp$smtp_insecure <- conf$smtp_insecure
    temp$force_date_format <- conf$force_date_format
    temp$maven_repo <- conf$maven_repo
    temp$winutils_url <- conf$winutils_url
    temp$fs_url <- conf$fs_url
    temp$fs_port <- conf$fs_port
    temp$fs_setup_host <- conf$fs_setup_host
    temp$fs_setup_port <- conf$fs_setup_port
    temp$fs_batch_timeout <- conf$fs_batch_timeout
    temp$fs_query_timeout <- conf$fs_query_timeout
    temp$runner_heartbeat_span <- conf$runner_heartbeat_span
    temp$runner_ttl_search <- conf$runner_ttl_search
    temp$runner_ttl_detect <- conf$runner_ttl_detect
    temp$runner_ttl_fs <- conf$runner_ttl_fs
    temp$runner_claim_guard <- conf$runner_claim_guard
    temp$runner_wait_timeout <- conf$runner_wait_timeout
    temp$runner_wait_interval <- conf$runner_wait_interval
    temp$admin_email <- conf$admin_email
    temp$dismiss_past_request <- conf$dismiss_past_request
    temp$dismiss_past_done <- conf$dismiss_past_done
    temp$collect_history_days <- conf$collect_history_days
    temp$mastodon_max_empty_requests <- conf$mastodon_max_empty_requests
    # saving every declared social media, see the symmetric read in setup_config
    for (sm_prop in sm_config_properties(conf)) {
      temp[[sm_prop]] <- conf[[sm_prop]]
    }
    temp$topics_md5 <- conf$topics_md5
    # writing the JSON file
    write_json_atomic(
      temp,
      get_properties_path(),
      pretty = TRUE,
      force = TRUE,
      auto_unbox = TRUE
    )
  }
  if (length(sm_topics) > 0) {
    # saving topics to the topics.json file
    paths = get_plans_paths()
    for (sm in sm_topics) {
      topics_in_sm = conf$topics[topics_of_network(sm)]
      temp <- list()
      temp$topics <- format_topics(topics_in_sm)
      # writing plan files for each social media
      write_json_atomic(
        temp,
        paths[[sm]],
        pretty = TRUE,
        force = TRUE,
        auto_unbox = TRUE
      )
    }
  }
}

format_topics <- function(topics) {
  lapply(topics, function(t) {
    t$plan <- lapply(t$plan, format_plan)
    t
  })
}

# Merging two or more configuration files as a list, the latest takes precedence over earlier ones
merge_configs <- function(configs) {
  if (length(configs) == 0)
    stop("No configurations provided for merging") else if (length(configs) == 1)
    configs[[1]] else {
    first <- configs[[1]]
    rest <- merge_configs(configs[-1])
    keys <- unique(c(names(first), names(rest)))
    as.list(setNames(
      mapply(
        function(x, y) if (is.null(y)) x else y,
        first[keys],
        rest[keys],
        # SIMPLIFY, not simplify: arguments placed after `...` are matched by their exact full
        # name, so a lowercase one would be swallowed into `...` and passed on to FUN instead.
        # Without it mapply() coerces the merged config to a character vector as soon as every
        # value is a length-1 scalar, turning numbers into strings.
        SIMPLIFY = FALSE
      ),
      keys
    ))
  }
}

merge_config_lists <- function(configs, property) {
  nested <- lapply(configs, function(cc) {
    if (exists(property, where = cc)) cc[[property]] else list()
  })
  ret <- list()
  ret[[property]] <- as.list(Reduce(c, nested))
  ret
}

# parse topic query and translate it to particular social media quert language
translate_query <- function(sm, q) {
  r = list(neg=list(), ors=list())
  q = trimws(q)
  q = gsub("^(or|and)\\s+", "", q, ignore.case = TRUE)
  q = gsub("\\s+(or|and)\\s*$", "", q, ignore.case = TRUE)
  q = gsub("\\s+or\\s+", "|", q, ignore.case = TRUE)
  q = gsub("\\s+and\\s+", "&", q, ignore.case = TRUE)
  q = gsub("^-| -|&-|\\|-", "+~", q)
  if (q == "") {
    r$ors[[1]] <- list()
  } else {
    ors <- trimws(strsplit(q, "\\|")[[1]])
    ors <- ors[ors != ""]
    if (length(ors) == 0) {
      r$ors[[1]] <- list()
    } else {
      for(i in seq_along(ors)) {
        r$ors[[i]] <- list()
        ands <- trimws(strsplit(ors[[i]], "&")[[1]])
        ands <- ands[ands != ""]
        for(j in seq_along(ands)) {
           parts <- trimws(strsplit(ands[[j]], "\\+")[[1]])
           parts <- parts[parts != ""]
           for(k in seq_along(parts)) {
              part <- trimws(parts[[k]])
              if(startsWith(part, "~") && nchar(part)>1) {
                 neg <- trimws(substr(part, 2, 1000))
                 r$neg <- c(r$neg,  neg)
	      } else if(nchar(part)>0) {
                 pos <- as.list(trimws(strsplit(part, "/")[[1]]))
                 r$ors[[i]] <- c(r$ors[[i]], list(pos))
              }
           }
        }
      }
    }
  }
  sm_api_translate_query(network = sm, r)
}


# Get topics data frame as displayed on the Shiny configuration tab
get_topics_df <- function() {
  df <- data.frame(
    Network = sapply(conf$topics, function(t) t$network),
    Topics = sapply(conf$topics, function(t) t$topic),
    Label = sapply(conf$topics, function(t) t$label),
    Query = sapply(conf$topics, function(t) t$query),
    QueryLength = sapply(conf$topics, function(t) nchar(t$query)),
    ActivePlans = sapply(conf$topics, function(t) length(t$plan)),
    Progress = sapply(conf$topics, function(t) {
      if (length(t$plan) > 0) {
        mean(unlist(lapply(t$plan, function(p) as.numeric(p$progress))))
      } else {
        0
      }
    }),
    Requests = sapply(conf$topics, function(t) {
      if (length(t$plan) > 0) {
        sum(unlist(lapply(t$plan, function(p) as.numeric(p$requests))))
      } else {
        0
      }
    }),
    Alpha = sapply(conf$topics, function(t) t$alpha),
    OutliersAlpha = sapply(conf$topics, function(t) t$alpha_outlier),
    stringsAsFactors = FALSE
  )
  if (length(df) == 0)
    stop("The list of topics is empty. Please check your configuration")
  df
}

#Get topic labels as named array that can be used for translation
get_topics_labels <- function() {
  `%>%` <- magrittr::`%>%`
  t <- (get_topics_df() %>%
    dplyr::group_by(.data$Topics) %>%
    dplyr::summarise(label = .data$Label[which(!is.na(.data$Label))[1]]) %>%
    dplyr::ungroup())
  setNames(t$label, t$Topics)
}

#Get topic alphas as named array that can be used for translation
get_topics_alphas <- function() {
  `%>%` <- magrittr::`%>%`
  t <- (get_topics_df() %>%
    dplyr::group_by(.data$Topics) %>%
    dplyr::summarise(alpha = .data$Alpha[which(!is.na(.data$Alpha))[1]]) %>%
    dplyr::ungroup())
  setNames(t$alpha, t$Topics)
}

#Get topic outliers alphas as named array that can be used for translation
get_topics_alpha_outliers <- function() {
  `%>%` <- magrittr::`%>%`
  t <- (get_topics_df() %>%
    dplyr::group_by(.data$Topics) %>%
    dplyr::summarise(
      alpha_outlier = .data$OutliersAlpha[which(!is.na(.data$OutliersAlpha))[1]]
    ) %>%
    dplyr::ungroup())
  setNames(t$alpha_outlier, t$Topics)
}

#Get topic k_decay as named array that can be used for translation
get_topics_k_decays <- function() {
  `%>%` <- magrittr::`%>%`
  t <- (get_topics_df() %>%
    dplyr::group_by(.data$Topics) %>%
    dplyr::summarise(k_decay = conf$alert_k_decay) %>%
    dplyr::ungroup())
  setNames(t$k_decay, t$Topics)
}

# Check config setup before continue
stop_if_no_config <- function(
  error_message = "Cannot continue without setting up a configuration."
) {
  if (!exists("data_dir", where = conf)) {
    stop(paste(
      error_message,
      "Please call setup_config('your data directory here')",
      sep = "\n"
    ))
  }
}

# Call config if necessary
setup_config_if_not_already <- function() {
  if (!exists("data_dir", where = conf)) {
    setup_config()
  }
}

# Get current available languages from the available language Excel file
get_available_languages <- function() {
  readxl::read_excel(get_available_languages_path())
}

# Remove language from the used languages on conf
remove_config_language <- function(code) {
  # Timestamping action
  conf$lang_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  # Removing the language
  conf$languages <- conf$languages[sapply(
    conf$languages,
    function(l) l$code != code
  )]
}

# Add a language to the used languages in conf
add_config_language <- function(code, name) {
  # Timestamping action
  conf$lang_updated_on <- strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
  index <- (1:length(conf$languages))[sapply(
    conf$languages,
    function(l) l$code == code
  )]
  if (length(index) > 0) {
    # Language code is already in the task list
    conf$languages[[index]]$code <- code
    conf$languages[[index]]$name <- name
    conf$languages[[index]]$vectors = paste(
      conf$data_dir,
      "/languages/",
      code,
      ".txt.gz",
      sep = ""
    )
    conf$languages[[index]]$modified_on <- strftime(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  } else {
    # Language code is not on the list
    conf$languages <- c(
      conf$languages,
      list(list(
        code = code,
        name = name,
        vectors = paste(
          conf$data_dir,
          "/languages/",
          code,
          ".txt.gz",
          sep = ""
        ),
        modified_on = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ))
    )
  }
}

# Get the current known users list from the important users files
get_known_users <- function() {
  gsub("@", "", readxl::read_excel(get_known_users_path())[[1]])
}

# The properties declaring the social media: the per family switches and, for federated families,
# the list of instances to collect from. The list of networks is derived from them.
sm_config_properties <- function(x) {
  # x is either the conf environment or a plain list read from properties.json. For an environment,
  # ls() rather than names(): names() follows the internal hash table order, which shifts as conf
  # grows and never goes back (see social_media_families() for the full story). The consequence is
  # milder here - it only shuffles the order of the sm_* keys written to properties.json, i.e.
  # spurious diffs on the file from one save to the next - but there is no reason to accept it.
  props <- if (is.environment(x)) ls(x, all.names = TRUE) else names(x)
  grep(
    "^sm_(activated|alerts)_|^[A-Za-z0-9]+_instances$",
    props,
    value = TRUE
  )
}

# the families declared in the configuration, i.e. those having activation switches
social_media_families <- function() {
  # ls() and not names(): names() on an environment is *not* sorted, it returns the keys in the
  # internal hash table order. That order depends on the size of the table, which grows as conf is
  # filled and never shrinks when it is emptied - so the families would come out in one order in a
  # fresh session and in another one after a setup_config(), and so would everything derived from
  # them: active_social_media(), alerts_social_media(), the collection order of the search loop and
  # the "known families are: ..." messages.
  props <- ls(conf, all.names = TRUE)
  sub("^sm_activated_", "", props[grepl("^sm_activated_", props)])
}

# the domains of the instances a federated family collects from, character(0) if none
sm_instances_of <- function(family) {
  instances <- unlist(conf[[sprintf("%s_instances", family)]])
  if (is.null(instances)) character(0) else instances
}

# the networks served by a family: one per instance when federated, the family itself otherwise
sm_networks_of <- function(family) {
  if (!sm_is_federated(family)) {
    return(family)
  }
  instances <- sm_instances_of(family)
  if (length(instances) == 0) {
    character(0)
  } else {
    sprintf("%s.%s", family, instances)
  }
}

# the networks of the families passing the given switch e.g. "sm_activated_"
sm_networks_where <- function(prefix) {
  families <- social_media_families()
  on <- families[vapply(
    families,
    function(family) isTRUE(conf[[sprintf("%s%s", prefix, family)]]),
    logical(1)
  )]
  sm_flatten_networks(lapply(on, sm_networks_of))
}

supported_social_media <- function() {
  sm_flatten_networks(lapply(social_media_families(), sm_networks_of))
}

# unlist() on a list of empty character vectors returns NULL, keep a character vector instead
sm_flatten_networks <- function(networks) {
  ret <- unlist(networks, use.names = FALSE)
  if (is.null(ret)) character(0) else ret
}

active_social_media <- function() {
  sm_networks_where("sm_activated_")
}

alerts_social_media <- function() {
  sm_networks_where("sm_alerts_")
}

#' @title Add an instance of a federated social media
#' @description Adds a domain to the instances a federated social media family collects from.
#' Each instance is collected as a network of its own, named <family>.<instance>, with its own
#' plan file, its own search process and its own access token.
#' @param family char, the federated social media family e.g. "mastodon"
#' @param instance char, the domain of the instance to collect from e.g. "mstdn.party"
#' @details The network identifier built from family and instance is a persistent key: it names
#' the plan file, the secrets and the network column of every post stored for it. Removing an
#' instance and adding it back resumes on its former history, but renaming it orphans that
#' history.
#' The credentials still have to be provided with \code{\link{sm_api_set_auth}}, and the
#' configuration is not written to disk, call \code{\link{save_config}} afterwards. The social
#' media menu of \code{\link{setup}} does all of it at once.
#' @return char, the identifier of the network collecting from this instance
#' @examples
#' \dontrun{
#'    library(episomer)
#'    setup_config(file.choose())
#'    network <- sm_add_instance("mastodon", "mstdn.party")
#'    sm_api_set_auth(network)
#'    save_config()
#' }
#' @seealso \code{\link{sm_remove_instance}}
#' @rdname sm_add_instance
#' @export
sm_add_instance <- function(family, instance) {
  if (!family %in% social_media_families()) {
    stop(sprintf(
      "There is no social media family '%s', known families are: %s",
      family,
      paste(social_media_families(), collapse = ", ")
    ))
  }
  if (!sm_is_federated(family)) {
    stop(sprintf(
      "The social media '%s' is not federated, it has no instances to choose from",
      family
    ))
  }
  instance <- tolower(instance)
  # the domain ends up in the network identifier, which is a file name, a secret name and a query
  # parameter, so it is checked before being stored
  if (
    !grepl(
      "^[a-z0-9]([a-z0-9-]*[a-z0-9])?(\\.[a-z0-9]([a-z0-9-]*[a-z0-9])?)+$",
      instance
    )
  ) {
    stop(sprintf("'%s' is not a valid instance domain", instance))
  }
  # adding an instance already collected is a no-op, so this can be called to refresh its token
  if (!instance %in% sm_instances_of(family)) {
    # stored as a list so it is serialised as a JSON array even when holding a single instance
    conf[[sprintf("%s_instances", family)]] <- as.list(c(
      sm_instances_of(family),
      instance
    ))
  }
  sprintf("%s.%s", family, instance)
}

#' @title Stop collecting from an instance of a federated social media
#' @description Removes a domain from the instances a federated social media family collects from.
#' @param family char, the federated social media family e.g. "mastodon"
#' @param instance char, the domain of the instance to stop collecting from
#' @details The posts already collected from this instance are NOT deleted, they stay stored under
#' the network name of the instance, as does its plan file topics.<family>.<instance>.json and its
#' access token. Adding the instance back therefore resumes on its former history.
#' The configuration is not written to disk, call \code{\link{save_config}} afterwards.
#' @return Nothing
#' @seealso \code{\link{sm_add_instance}}
#' @rdname sm_remove_instance
#' @export
sm_remove_instance <- function(family, instance) {
  instances <- sm_instances_of(family)
  if (!instance %in% instances) {
    stop(sprintf(
      "The instance '%s' is not among the %s instances collected",
      instance,
      family
    ))
  }
  conf[[sprintf("%s_instances", family)]] <- as.list(setdiff(
    instances,
    instance
  ))
  invisible(NULL)
}

is_sm_authenticated <- function(sm) {
    tryCatch(
      if(sm %in% supported_social_media()) {
        return(isTRUE(sm_api_has_credentials(sm)))
      },
      warning = function(m) {},
      error = function(e) {}
    )
    return(FALSE)
}

is_sm_activated <- function(sm) {
    prop = sprintf("sm_activated_%s", sm)
    exists(prop, where=conf) && conf[[prop]]
}
is_sm_with_alerts <- function(sm) {
    prop = sprintf("sm_alerts_%s", sm)
    exists(prop, where=conf) && conf[[prop]]
}
sm_activate <- function(sm) {
    prop = sprintf("sm_activated_%s", sm)
    if(exists(prop, where=conf))
        conf[[prop]] = TRUE
}
sm_deactivate <- function(sm) {
    prop = sprintf("sm_activated_%s", sm)
    if(exists(prop, where=conf))
        conf[[prop]] = FALSE
}

sm_set_with_alerts <- function(sm) {
    prop = sprintf("sm_alerts_%s", sm)
    if(exists(prop, where=conf))
        conf[[prop]] = TRUE
}
sm_set_without_alerts <- function(sm) {
    prop = sprintf("sm_alerts_%s", sm)
    if(exists(prop, where=conf))
        conf[[prop]] = FALSE
}

get_env_props <- function() {
  envs <- as.list(Sys.getenv())
  reframed_envs <- sapply(
    seq_along(1:length(envs)),
    function(i) {
      current_env_var_name <- names(envs)[i]
      if (!grepl("_INSTANCES", current_env_var_name)) {
        return(envs[[i]])
      }

      sm_instances_remove_semicolon_and_make_vector <- strsplit(
        envs[[i]],
        split = ";"
      )

      return(sm_instances_remove_semicolon_and_make_vector)
    },
    simplify = FALSE
  )

  names(reframed_envs) <- names(envs)

  reframed_envs <- reframed_envs[grepl(
    "^EPISOMER_OPTION_.*",
    names(reframed_envs)
  )]
  setNames(
    reframed_envs,
    tolower(gsub("^EPISOMER_OPTION_", "", names(reframed_envs)))
  )
}

# Give the properties read from the environment the type they are expected to have.
#
# Everything coming out of Sys.getenv() is a character string, while the same property read from
# properties.json comes back as a number or a boolean. Without this step,
# EPISOMER_OPTION_RUNNER_TTL_SEARCH=600 would put the *string* "600" in conf and every arithmetic
# comparison downstream would either be a silent string comparison ("600" < "90" is TRUE) or an
# error. The same latent trap already existed for fs_port, smtp_port and the alert thresholds.
#
# The expected type is taken from `defaults`, i.e. the list built by get_empty_config(): a property
# whose default is a number must be a number, one whose default is a boolean must be a boolean.
# That keeps the knowledge of types in one single place, and any property added there later is
# coerced for free.
#
# Left untouched on purpose:
#  * properties with no default (unknown name, or a social media declared at runtime) - there is
#    nothing to infer a type from;
#  * defaults that are NA, because NA is a *logical* in R and half a dozen defaults are NA-valued
#    dates (lang_updated_on and friends); coercing those to logical would destroy them;
#  * defaults that are lists (languages, topics, known_users...), which an environment variable
#    cannot express anyway.
#
# A value that cannot be converted is an error rather than a silent NA: an unusable
# EPISOMER_OPTION_* must be visible at startup, not three hours later in the middle of a loop.
coerce_env_props <- function(props, defaults) {
  for (prop in names(props)) {
    default <- defaults[[prop]]
    if (is.null(default) || length(default) != 1 || is.na(default)) next
    raw <- props[[prop]]
    if (is.numeric(default)) {
      value <- suppressWarnings(as.numeric(raw))
    } else if (is.logical(default)) {
      value <- as.logical(raw)
      # "1" / "0" are a natural way of writing a boolean in a compose file, and as.logical() maps
      # them to NA, so they get a second chance through the numeric route.
      if (is.na(value) && grepl("^[01]$", trimws(raw))) {
        value <- as.logical(as.numeric(raw))
      }
    } else {
      next
    }
    if (is.na(value)) {
      stop(sprintf(
        "Invalid value '%s' for environment variable EPISOMER_OPTION_%s: expected a %s",
        raw,
        toupper(prop),
        if (is.numeric(default)) "number" else "boolean"
      ))
    }
    props[[prop]] <- value
  }
  props
}



get_env_secrets <- function() {
  envs <- as.list(Sys.getenv())
  envs <- envs[grepl("^EPISOMER_SECRET_.*", names(envs))]
  envs <- setNames(envs, gsub("^EPISOMER_SECRET_", "", names(envs)))
  ret <- list() 
  # fetching values if provided on files
  for(p in names(envs)) {
     if(!grepl(".*_FILE$", p)) {
         ## Standard secret
         ret[[tolower(p)]] <- envs[[p]] 
     } else if(envs[[p]]!="" && file.exists(envs[[p]])) {
         ## Secret file
         con <- file(envs[[p]], "r", encoding = "UTF-8")
         val <- readLines(con, n=1, warn=FALSE)
         close(con)
         ret[[tolower(gsub("_FILE$", "", p))]] <- val 
     }
  }
  ret
}

# Whether the services must write their logs to the standard output instead of files under
# <data_dir>/jobs.
#
# One log per file is the right shape for a machine an operator logs into: the Shiny app offers
# them for download (see the "logs" section of build_dump(), R/check.R) and they survive the
# process that wrote them. It is the wrong shape for a container: nothing collects a file inside a
# pod, `docker logs` and `kubectl logs` read the standard output of PID 1 and nothing else, so a
# deployment that leaves the default on has services whose logs exist and are unreachable - which
# is what the Kubernetes deployment this option was written for reported (issue 149).
#
# A plain environment variable rather than an EPISOMER_OPTION_* property, for two reasons:
#
#  * where a process writes its log belongs to the process, not to the cluster. properties.json is
#    shared by every instance of a deployment, and the same data directory can perfectly well be
#    served by a container (stdout) and read by a desktop session (files);
#  * properties.json takes precedence over the environment (see setup_config()), so a knob stored
#    there would stop obeying the container the day it was first written - i.e. always, since
#    save_config() writes every property.
#
# "1"/"0" are accepted alongside "TRUE"/"FALSE", as coerce_env_props() does for the properties:
# writing a boolean as 1/0 in a compose file is common enough.
logs_to_stdout <- function() {
  raw <- trimws(Sys.getenv("EPISOMER_LOGS_TO_STDOUT", "FALSE"))
  if (raw == "") {
    return(FALSE)
  }
  value <- as.logical(raw)
  if (is.na(value) && grepl("^[01]$", raw)) {
    value <- as.logical(as.numeric(raw))
  }
  if (is.na(value)) {
    stop(sprintf(
      "Invalid value '%s' for environment variable EPISOMER_LOGS_TO_STDOUT: expected a boolean",
      raw
    ))
  }
  value
}
