# Runner leases ----------------------------------------------------------------------------------
#
# A "runner lease" is the right, granted for a limited time and renewable, to act as a given runner
# (fs, detect, search.bluesky, search.mastodon.piaille.fr...). It is what stops two machines
# sharing the same data directory from collecting the same posts twice.
#
# It complements, and does not replace, the PID files of R/runners.R:
#
#   PID file   -> local disk, answers "is there a process *on this machine* I can kill?"
#   lease file -> fs machine, answers "who, in the whole cluster, is allowed to be this runner?"
#
# One JSON file per runner, under <data_dir>/runners/, reached through epipath() so that the very
# same code works for a local fs (plain file access) and for a remote one (HTTP).
#
# Two design points worth keeping in mind while reading what follows:
#
#  * Freshness comes from the file mtime only, never from the timestamps stored inside the JSON.
#    The mtime is produced by the single machine hosting the fs, so no clock skew between runners
#    can make a lease look alive when it is not.
#  * This is a *cooperative* lease, not a distributed lock. The fs API has no compare-and-swap
#    (upload is always last-writer-wins), so claiming is "write, wait, re-read" and two instances
#    can in theory overlap for a few seconds. The real safety net is stop_if_lease_lost() called
#    before every meaningful write.
#
# Full rationale in inst/schema/multi_services_plan.md (sections 3 to 5).

# Default TTL per service, in seconds, from the table of the plan (section 3.5).
#
# A long TTL is not a correctness problem - the second instance simply waits longer before taking
# over - it only slows down failover. Correctness comes from stop_if_lease_lost().
#
#  fs      : also monitored by /ping, so a dead fs is detected by other means anyway
#  search  : the longest silence is a chunked sleep between two API rate-limit waits
#  detect  : one single task (generate_alerts(), geolocation, aggregation) can run for very long
#            and must not look dead
runner_lease_ttl_defaults <- function() {
  list(fs = 120, fs_mon = 120, search = 300, detect = 1800)
}

# TTL to use for an unknown runner name. Erring on the long side: a TTL that is too short causes
# false expiries (two runners collecting at once), a TTL that is too long only delays failover.
runner_lease_ttl_fallback <- 300

# Service a runner name belongs to: "search.mastodon.piaille.fr" -> "search", "detect" -> "detect".
runner_service <- function(name) {
  sub("\\..*$", "", name)
}

# TTL of a given runner, in seconds.
#
# The values are configuration (conf$runner_ttl_search and friends, added by plan issue 3); the
# defaults above apply as long as they are not there, and for runner names whose service has no
# property of its own. as.numeric() because a value coming from an EPISOMER_OPTION_* environment
# variable arrives as a string and is used in arithmetic here.
runner_lease_ttl <- function(name) {
  service <- runner_service(name)
  value <- conf[[paste0("runner_ttl_", service)]]
  if (is.null(value)) value <- runner_lease_ttl_defaults()[[service]]
  if (is.null(value)) value <- runner_lease_ttl_fallback
  as.numeric(value)
}

# Number of seconds to wait between the optimistic write of a claim and its confirmation read
# (see claim_runner_lease()). Configuration too (conf$runner_claim_guard, plan issue 3).
runner_claim_guard <- function() {
  value <- conf$runner_claim_guard
  if (is.null(value)) value <- 5
  as.numeric(value)
}

# Refuses a runner name that could escape the runners/ folder once pasted into a path. Runner names
# are built by the code, but part of them comes from the configuration (the Mastodon instance host
# in "search.mastodon.<host>"), so the check is cheap insurance rather than paranoia.
check_runner_name <- function(name) {
  if (
    length(name) != 1 ||
      is.na(name) ||
      !nzchar(name) ||
      !grepl("^[A-Za-z0-9._-]+$", name) ||
      grepl("\\.\\.", name)
  ) {
    stop(paste(
      "Invalid runner name:",
      paste(name, collapse = ", "),
      "- only letters, digits, dots, dashes and underscores are allowed"
    ))
  }
  invisible(name)
}

# Folder holding the live leases, one file per runner. Created on demand by the writers only:
# reading a lease happens far more often than writing one, and a reader must not create anything
# on the fs machine.
runner_lease_dir <- function(create = FALSE) {
  path <- epipath("runners")
  if (create && !epifile.exists(path)) {
    epidir.create(path, recursive = TRUE)
  }
  path
}

# Folder keeping the released leases (see release_runner_lease()). Nothing ever lists it: it is a
# trail an operator can read after the fact.
runner_lease_old_dir <- function(create = FALSE) {
  path <- epipath("runners", "old")
  if (create && !epifile.exists(path)) {
    epidir.create(path, recursive = TRUE)
  }
  path
}

# Path of the lease file of a runner, e.g. <data_dir>/runners/search.bluesky.json
runner_lease_path <- function(name, create_dir = FALSE) {
  check_runner_name(name)
  runner_lease_dir(create = create_dir)
  epipath("runners", paste0(name, ".json"))
}

# Path a lease is moved to when this instance releases it. The instance_id is part of the name so
# that successive releases of the same runner do not overwrite each other.
runner_lease_released_path <- function(name) {
  check_runner_name(name)
  epipath("runners", "old", paste0(name, ".", conf$instance_id, ".json"))
}

# Reads the lease of a runner: NULL when no live lease exists, otherwise its content plus a
# `seen_at` field holding the file mtime, i.e. the moment the holder last proved it was alive.
#
# Deliberate order: the content is read *before* the mtime. Reading the mtime first would allow
# this sequence - stale mtime of holder A read, A releases, B claims, B's content read - leaving us
# with "B holds it and looks expired", and we would steal a lease from a live instance. The other
# way round we may see "A holds it and looks fresh" and back off for nothing, which is safe.
read_runner_lease <- function(name) {
  path <- runner_lease_path(name)
  if (!epifile.exists(path)) {
    return(NULL)
  }
  # The lease may be moved away (released) or replaced between the test above and the two reads
  # below. A write is atomic (temp file + rename), so a half-written file is never seen, but the
  # file may simply be gone: treat any read failure as "no lease".
  lease <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(lease) || !is.list(lease) || is.null(lease$instance_id)) {
    return(NULL)
  }
  seen_at <- epifile.mtime(path)
  lease$seen_at <- if (length(seen_at) == 1) seen_at else as.POSIXct(NA)
  lease
}

# TRUE when nothing stops this instance from claiming `name`.
#
# A lease is free if it is absent (never claimed, or released and moved to runners/old/), if it has
# expired, or if it is already mine. "Expired by me" needs no special case: a new process has a new
# instance_id anyway, so recognising myself across a restart cannot work at instance level.
lease_is_free <- function(lease, name) {
  if (is.null(lease)) {
    return(TRUE)
  }
  if (identical(lease$instance_id, conf$instance_id)) {
    return(TRUE)
  }
  # seen_at comes from epifile.mtime(), never from the JSON (see the header of this file). NA means
  # the file disappeared while we were reading it, which is the free case too.
  if (length(lease$seen_at) != 1 || is.na(lease$seen_at)) {
    return(TRUE)
  }
  age <- as.numeric(difftime(Sys.time(), lease$seen_at, units = "secs"))
  age > runner_lease_ttl(name)
}

# Writes this instance's identity in the lease file of `name`, replacing whatever was there.
#
# write_json_atomic() makes the write atomic, so a concurrent reader never sees a half file - but
# it is still last-writer-wins, which is why claim_runner_lease() needs a confirmation read.
#
# Only the claim writes JSON: renewing is a touch, so `renewed_at` is refreshed only when the
# content is rewritten. Read it as "since when is this instance the holder", and read the file
# mtime for "last seen". Both timestamps, `pid` and `episomer_version` are diagnostics for the
# Shiny status table and for an operator reading runners/ - never inputs of the expiry decision.
write_runner_lease <- function(name) {
  stop_if_no_config(paste(
    "Cannot write the lease of runner",
    name,
    "without configuration setup"
  ))
  now <- strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  lease <- list(
    runner = name,
    service = runner_service(name),
    instance_id = conf$instance_id,
    host = conf$instance_host,
    pid = Sys.getpid(),
    episomer_version = tryCatch(
      as.character(utils::packageVersion(get_package_name())),
      error = function(e) NA_character_
    ),
    claimed_at = now,
    renewed_at = now
  )
  path <- runner_lease_path(name, create_dir = TRUE)
  write_json_atomic(lease, path, auto_unbox = TRUE)
  invisible(lease)
}

# Tries to become the holder of `name`. TRUE when this process is now the legitimate holder.
#
# Cooperative lease: the fs API has no "write only if unchanged", so two instances starting at the
# same moment can both write. The second write wins, and the loser notices it in the confirmation
# read below and backs off:
#
#   B: read -> absent            C: read -> absent
#   B: write -> holder = B       C: write -> holder = C     (C landed last, C wins)
#   B: re-read -> holder = C     C: re-read -> holder = C
#   B: FALSE, backs off          C: TRUE, collects
#
# `guard` has to be longer than the worst-case write latency, otherwise the loser could re-read
# before the winner's write lands and both would believe they hold the lease. The residual window
# is covered by stop_if_lease_lost() before every meaningful write.
claim_runner_lease <- function(name, guard = runner_claim_guard()) {
  stop_if_no_config(paste(
    "Cannot claim the lease of runner",
    name,
    "without configuration setup"
  ))
  current <- read_runner_lease(name)
  if (!lease_is_free(current, name)) {
    # somebody else is alive and holding it: no write at all, so the holder is left undisturbed
    return(FALSE)
  }
  if (!is.null(current) && identical(current$instance_id, conf$instance_id)) {
    # The live lease already names me: this is a re-registration, not an acquisition. There is
    # nothing to race against - a competing writer would have replaced my name in the read above -
    # so we skip both the rewrite and the guard wait, and only prove we are still alive.
    #
    # This matters because register_runner() is not always called once: the fs monitor calls
    # register_fs_monitor() on every iteration of its 2-second loop (R/fs.R), and paying a JSON
    # write plus a `guard`-second sleep there would be absurd.
    renew_runner_lease(name)
    return(TRUE)
  }
  write_runner_lease(name)
  runner_claim_wait(guard)
  i_hold_runner_lease(name)
}

# The guard wait of claim_runner_lease(), as a function of its own so that a test can simulate a
# competing write landing exactly there - which is the one thing the confirmation read exists for,
# and which cannot be reproduced by mocking Sys.sleep() (testthat refuses to mock base functions).
runner_claim_wait <- function(guard) {
  if (guard > 0) Sys.sleep(guard)
  invisible(NULL)
}

# Proves this instance is still alive, by bumping the mtime of its lease file.
#
# One tiny POST (Files.setLastModifiedTime on the fs side), no body: this is what the heartbeat
# calls, so it has to stay cheap.
#
# The existence test is not redundant: epifile.touch() *creates* a missing file, so touching a
# lease that has been released, or moved away by somebody else, would leave an empty non-JSON file
# under runners/ and pollute the listing the Shiny status table reads. Nothing is renewed when the
# lease is gone; the caller finds out through stop_if_lease_lost().
renew_runner_lease <- function(name = conf$runner_name) {
  if (is.null(name)) {
    return(invisible(FALSE))
  }
  path <- runner_lease_path(name)
  if (!epifile.exists(path)) {
    return(invisible(FALSE))
  }
  epifile.touch(path)
  invisible(TRUE)
}

# Gives the lease back, so that another instance can take over immediately instead of waiting for
# the whole TTL. Meant to be called from an on.exit() in the loops.
#
# The lease is *moved* to runners/old/<name>.<instance_id>.json rather than marked as released:
# the fs API has no delete action, and moving keeps "the file is absent" as the single meaning of
# "this runner is free". As a bonus, runners/ then lists live runners only, and the released files
# remain readable as a trail (their mtime being the release time).
#
# The i_hold_runner_lease() check right before the move is the important part: an instance that
# silently lost its lease and released anyway would take away the *new* holder's file. For the same
# reason, the handler of the episomer_lease_lost condition must not release anything.
release_runner_lease <- function(name = conf$runner_name) {
  if (is.null(name)) {
    return(invisible(FALSE))
  }
  if (!i_hold_runner_lease(name)) {
    message(paste(
      "Not releasing the lease of runner",
      name,
      "- it is not held by this instance anymore"
    ))
    return(invisible(FALSE))
  }
  runner_lease_old_dir(create = TRUE)
  epifile.move(runner_lease_path(name), runner_lease_released_path(name))
  invisible(TRUE)
}

# TRUE when the live lease of `name` names this instance.
#
# Freshness is deliberately not tested here: if the lease is mine but stale, nobody has taken it
# from me, so I am still the holder - I am just late renewing it. What matters for the guard points
# is "did somebody else take my place", and that is exactly an instance_id comparison.
i_hold_runner_lease <- function(name = conf$runner_name) {
  if (is.null(name)) {
    return(FALSE)
  }
  lease <- read_runner_lease(name)
  !is.null(lease) && identical(lease$instance_id, conf$instance_id)
}

# Read-only view of a runner, for the Shiny status table and the is_*_running() functions.
#
# status is one of:
#   "free"    : no live lease, the runner can be started anywhere
#   "running" : a lease renewed less than its TTL ago
#   "stale"   : a lease nobody has renewed for longer than its TTL - the holder is presumed dead
#               and the runner can be claimed again
runner_lease_status <- function(name) {
  lease <- read_runner_lease(name)
  if (is.null(lease)) {
    return(list(
      runner = name,
      status = "free",
      host = NA_character_,
      instance_id = NA_character_,
      pid = NA_integer_,
      seen_at = as.POSIXct(NA),
      age = NA_real_,
      ttl = runner_lease_ttl(name),
      mine = FALSE
    ))
  }
  age <- as.numeric(difftime(Sys.time(), lease$seen_at, units = "secs"))
  ttl <- runner_lease_ttl(name)
  list(
    runner = name,
    status = if (!is.na(age) && age > ttl) "stale" else "running",
    host = lease$host,
    instance_id = lease$instance_id,
    pid = lease$pid,
    seen_at = lease$seen_at,
    age = age,
    ttl = ttl,
    mine = identical(lease$instance_id, conf$instance_id)
  )
}

# Aborts when this instance is not the holder of `name` anymore. Called before every meaningful
# write (posting posts, saving topics.json, saving tasks.json), which is where losing a lease
# unnoticed would actually corrupt data or duplicate posts.
#
# A typed condition, not a bare stop(), so that the loops can tell "I lost my spot" - expected,
# exit quietly with a clear log line, and release nothing - from a real error. Same idiom as
# stop_if_no_config() / stop_if_no_fs(), which makes the call sites self-documenting.
stop_if_lease_lost <- function(name = conf$runner_name) {
  if (is.null(name)) {
    return(invisible(NULL))
  }
  if (!i_hold_runner_lease(name)) {
    rlang::abort(
      sprintf(
        "Runner lease '%s' was taken by another instance, stopping",
        name
      ),
      class = "episomer_lease_lost",
      runner = name
    )
  }
  invisible(NULL)
}

# Explains why a claim failed, naming the instance that holds the runner right now.
#
# This is what an operator reads in the logs of the container that refused to start, so it has to
# answer the only two questions worth asking at that moment: *where* is the runner already running
# (`host`, so one knows which machine to look at), and *since when* (`claimed_at`, plus the age of
# the last heartbeat, which tells a live holder from one that is about to expire).
#
# Called after claim_runner_lease() returned FALSE, so the lease is read a second time - it may
# have changed in between, and both outcomes are worth wording differently.
runner_lease_holder_message <- function(name) {
  lease <- read_runner_lease(name)
  if (is.null(lease)) {
    # The holder released, or was moved away, between the failed claim and this read. Nothing is
    # wrong with the cluster: starting again will succeed.
    return(sprintf(
      paste(
        "Runner '%s' could not be claimed: another instance was holding the lease and has just",
        "released it. Starting again should now succeed."
      ),
      name
    ))
  }
  age <- as.numeric(difftime(Sys.time(), lease$seen_at, units = "secs"))
  sprintf(
    paste(
      "Runner '%s' is already held by another instance (%s) running on host '%s'",
      "since %s, last seen %.0f seconds ago (lease TTL: %.0f seconds).",
      "This instance will not start it."
    ),
    name,
    lease$instance_id,
    lease$host,
    lease$claimed_at,
    age,
    runner_lease_ttl(name)
  )
}
