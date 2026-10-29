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
  if (is.null(value)) {
    value <- runner_lease_ttl_defaults()[[service]]
  }
  if (is.null(value)) {
    value <- runner_lease_ttl_fallback
  }
  as.numeric(value)
}

# Number of seconds to wait between the optimistic write of a claim and its confirmation read
# (see claim_runner_lease()). Configuration too (conf$runner_claim_guard, plan issue 3).
runner_claim_guard <- function() {
  value <- conf$runner_claim_guard
  if (is.null(value)) {
    value <- 5
  }
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

# TRUE when the lease of `name` is held by a process of *this very machine* that is not running
# anymore. In that case the lease is dead whatever its age, and it can be reclaimed at once.
#
# Why bother (plan sections 3.6 and 6.2): freshness alone makes a crashed runner look alive for up
# to a whole TTL - 30 minutes for detect. In the single-machine deployment, which is how episomer
# runs almost everywhere, that would be a plain regression compared to the PID file the lease is
# taking over from: the Shiny app would keep showing "running" long after the loop died, and
# restarting the service would have to wait the TTL out. When the holder is a process of our own
# machine we can do much better than waiting: ask the operating system whether it still exists.
#
# Two corroborating signals are required, and the second one is what makes this safe:
#
#   1. the lease names our host (conf$instance_host), and
#   2. our *local* PID file for that runner records exactly the PID written in the lease, and that
#      record still describes a live process of this namespace (recorded_pid_is_running()).
#
# Signal 1 alone would be enough between real machines, but not between containers:
# EPISOMER_INSTANCE is set per service in docker/compose.yml, so two containers of the same service
# (the standby of the multi-machine setup) share a host name while having separate PID namespaces.
# "PID 42 does not exist here" would then not mean "the holder is dead", and we would steal a live
# lease - the one thing this whole mechanism exists to prevent. Signal 2 closes that door: the PID
# file is written by the holder on the disk we are reading (conf$data_dir is local to the container,
# only jobs/ is shared), so a PID file naming the same PID means that PID really is ours to test -
# and the host and start time the record carries alongside the PID say whether it still does, which
# is what tells a restarted container from a live one.
#
# Both signals fail closed: anything missing, unreadable or merely different means "not my business,
# trust the mtime", i.e. the lease is treated as alive until it expires.
lease_holder_is_dead_locally <- function(lease, name) {
  if (is.null(lease) || is.null(lease$host) || is.null(lease$pid)) {
    return(FALSE)
  }
  # signal 1: the lease claims to be held here
  if (!identical(as.character(lease$host), as.character(conf$instance_host))) {
    return(FALSE)
  }
  lease_pid <- suppressWarnings(as.integer(lease$pid))
  if (length(lease_pid) != 1 || is.na(lease_pid)) {
    return(FALSE)
  }
  # signal 2: this machine's own PID file agrees on which process that is
  rec <- read_running_task_pid_record(name)
  if (!identical(rec$pid, lease_pid)) {
    return(FALSE)
  }
  # recorded_pid_is_running() rather than is_r_pid_running(): the record carries the host and the
  # start time of the process it describes, and both are needed here. The host tells a PID file
  # written in this namespace from one a container of ours left in the same mounted directory, and
  # the start time tells the holder from a *different* process that has since been handed its
  # number - which is exactly what kept a restarted fs container from reclaiming its own lease, its
  # new main Rscript being PID 9 again (inst/schema/multi_services_plan.md, section 9 item 6).
  !recorded_pid_is_running(rec)
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
  # The fast reclaim of section 3.6: a holder that crashed on this very machine is dead now, there
  # is nothing to wait for. This is what makes "crash, restart, collect again" immediate instead of
  # TTL long, and it is the only shortcut allowed - it needs proof from the local OS, not a guess.
  if (lease_holder_is_dead_locally(lease, name)) {
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
  if (guard > 0) {
    Sys.sleep(guard)
  }
  invisible(NULL)
}

# Waiting for a busy lease -------------------------------------------------------------------------
#
# claim_runner_lease() answers "may I be this runner *right now*". For everything that starts a
# service that question is asked too early, and issue 14 is what it costs: `docker compose down`
# runs no R code at all - the entrypoint is a bash script that does not forward SIGTERM, and an
# Rscript receiving one dies without running a single on.exit() (issue 13) - so a `down`
# immediately followed by an `up` finds the previous leases still there, alive, held by containers
# that no longer exist. The claim is refused, register_runner() raises, the service exits, and
# nothing is left running to try again when the lease finally expires TTL seconds later. The
# deployment stays down until somebody types `make compose-up` a second time.
#
# Waiting is what the plan already described as the standby behaviour (issue 9), only done inside
# the process instead of by exiting and letting Docker restart the container:
#
#   * it costs the same as a restart loop - one read of one small JSON file per attempt - without
#     the container churn, and it works identically outside Compose: the setup() wizard, a service
#     started by hand on a laptop, a systemd unit;
#   * it makes the wait readable. One log line naming the holder and the interval, repeated at most
#     once a minute, instead of the same fatal error every few seconds in a container that keeps
#     dying and taking its logs with it;
#   * it does not weaken exclusivity by one bit. A wait only ever ends on a successful
#     claim_runner_lease(), guard wait and confirmation read included.
#
# `restart: on-failure` stays in the Compose files as the net below this one: it catches what a
# wait cannot, i.e. a service dying for a reason that has nothing to do with a lease.
runner_wait_timeout_default <- Inf
runner_wait_interval_default <- 15

# How long a service waits for a busy lease before giving up, in seconds.
#
# Infinite by default, because "the lease is taken" is not an incident to report but a role to
# hold: a second machine pointed at the same database is *meant* to sit there until the first one
# stops, and a service that waits says so in its log and holds no lease in the status table of
# issue 8. A finite value turns the wait back into the historical refusal once it runs out, for a
# deployment that would rather have its supervisor notice.
runner_wait_timeout <- function() {
  value <- conf$runner_wait_timeout
  if (is.null(value)) {
    value <- runner_wait_timeout_default
  }
  value <- suppressWarnings(as.numeric(value))
  # a negative or unreadable timeout would mean "give up before trying", which no caller wants
  if (length(value) != 1 || is.na(value) || value < 0) {
    value <- runner_wait_timeout_default
  }
  value
}

# Seconds between two attempts. Short enough that a service restarted right after its predecessor
# was killed starts collecting within seconds of the TTL expiring, long enough that waiting costs
# nothing measurable: one read of one lease file, i.e. one small GET on the fs machine.
runner_wait_interval <- function() {
  value <- conf$runner_wait_interval
  if (is.null(value)) {
    value <- runner_wait_interval_default
  }
  value <- suppressWarnings(as.numeric(value))
  # zero or negative would turn the retry loop into a busy loop hammering the fs
  if (length(value) != 1 || is.na(value) || value <= 0) {
    value <- runner_wait_interval_default
  }
  value
}

# Seconds between two "still waiting" lines. Deliberately much larger than the retry interval: the
# attempts are cheap and frequent, but the log has to stay readable over a 30-minute detect TTL.
runner_wait_log_span <- 60

# Claims `name`, waiting for it to become free. TRUE when this process is now the holder.
#
# FALSE only when `timeout` ran out, and the caller then refuses exactly as it always did. With
# timeout = 0 this *is* claim_runner_lease(), which is what register_runner() does by default: only
# the three service loops wait. Everything else has to answer at once - the Shiny app deciding
# whether to spawn a task, and above all the fs monitor, which re-registers on every iteration of
# its 2-second loop.
#
# No heartbeat here, unlike every other wait of this file: we hold no lease while waiting, so there
# is nothing to renew and nothing to lose. A plain sleep is the honest thing to do.
wait_for_runner_lease <- function(
  name,
  timeout = runner_wait_timeout(),
  interval = runner_wait_interval()
) {
  if (claim_runner_lease(name)) {
    return(TRUE)
  }
  started <- Sys.time()
  waited <- function() {
    as.numeric(difftime(Sys.time(), started, units = "secs"))
  }
  # Time of the last line written, not of the last attempt: NULL means one is due right now, so an
  # operator opening the log finds out why the service has not started yet without waiting a minute
  # for the first message.
  last_log <- NULL
  repeat {
    left <- timeout - waited()
    if (left <= 0) {
      return(FALSE)
    }
    due <- is.null(last_log) ||
      as.numeric(difftime(Sys.time(), last_log, units = "secs")) >=
        runner_wait_log_span
    if (due) {
      msg(runner_lease_wait_message(name, timeout, interval))
      last_log <- Sys.time()
    }
    Sys.sleep(min(interval, left))
    if (claim_runner_lease(name)) {
      msg(sprintf(
        "Runner '%s' has been claimed after waiting %s for its previous holder.",
        name,
        format_lease_duration(waited())
      ))
      return(TRUE)
    }
  }
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
  # The attempt is recorded, not its success: it is what renew_runner_lease_if_due() reads to keep
  # a heartbeat placed in a busy loop from costing one fs request per iteration. A lease that is
  # gone will not come back by itself, so retrying the existence test below sooner would buy
  # nothing - losing the lease is what stop_if_lease_lost() is for, at the guard points.
  assign(name, Sys.time(), envir = lease_last_renewal)
  path <- runner_lease_path(name)
  if (!epifile.exists(path)) {
    return(invisible(FALSE))
  }
  epifile.touch(path)
  invisible(TRUE)
}

# Heartbeat ----------------------------------------------------------------------------------------
#
# Renewing is what proves a runner is alive, and it is the only thing standing between it and
# another instance taking its place once the TTL has passed. The hard part is not the renewal
# itself - one touch - it is *where* to put it: a loop spends most of its time doing something
# other than looping, i.e. sleeping until the next collection slot, waiting for a rate limit to
# reset, or running one long detect task.
#
# Two shapes, two helpers (plan section 3.3, issue 5):
#
#   sleep_with_heartbeat()      "I am about to do nothing for a while": the wait is chopped into
#                               heartbeat-sized slices, with a renewal between two slices.
#   renew_runner_lease_if_due() "I am busy but I pass here often": renews at most once per
#                               heartbeat span, so it can be called liberally, even in a loop
#                               iterating over every topic and every plan.
#
# What we deliberately do NOT do is beat from a background process (future, the way fs_loop() runs
# fs_mon): a background beater keeps shouting "alive" while the main loop is deadlocked, which is
# precisely the failure a lease is supposed to expose. Here, a renewal only ever happens because
# the loop itself reached the line that renews.

# Fallback when conf holds no runner_heartbeat_span (a process that never called setup_config(),
# or a configuration written before plan issue 3). Same value as get_empty_config().
runner_heartbeat_span_default <- 30

# Seconds between two renewals. as.numeric() for the same reason as in runner_lease_ttl(): the
# value is used in arithmetic and may come from an EPISOMER_OPTION_* environment variable.
runner_heartbeat_span <- function() {
  value <- conf$runner_heartbeat_span
  if (is.null(value)) {
    value <- runner_heartbeat_span_default
  }
  value <- suppressWarnings(as.numeric(value))
  # A span of zero, negative or unreadable would turn sleep_with_heartbeat() into a busy loop
  if (length(value) != 1 || is.na(value) || value <= 0) {
    value <- runner_heartbeat_span_default
  }
  value
}

# When this process last tried to renew each of its leases, one entry per runner name.
#
# Purely local bookkeeping, and never an input to the expiry decision: whether a lease is alive is
# decided by the mtime on the fs machine (see the header of this file), never by what a runner
# believes about itself.
lease_last_renewal <- new.env(parent = emptyenv())

# Renews the lease of `name`, unless it has already been renewed less than `span` seconds ago.
#
# This is the version to call from inside a loop: what it costs, most of the time, is one lookup
# in an environment. It returns TRUE only when it actually renewed, which is what the tests read.
renew_runner_lease_if_due <- function(
  name = conf$runner_name,
  span = runner_heartbeat_span()
) {
  if (!heartbeat_is_due(name, span)) {
    return(invisible(FALSE))
  }
  renew_runner_lease(name)
}

# TRUE when `name` has not been renewed by this process for at least `span` seconds.
#
# A runner name of NULL means this process holds no lease at all (the Shiny app, a sandboxed loop,
# a function called from the console): nothing is ever due, and that is not an error.
heartbeat_is_due <- function(
  name = conf$runner_name,
  span = runner_heartbeat_span()
) {
  if (is.null(name)) {
    return(FALSE)
  }
  last <- lease_last_renewal[[name]]
  is.null(last) ||
    as.numeric(difftime(Sys.time(), last, units = "secs")) >= span
}

# Beats for the fs runner, from the fs monitor.
#
# fs is the one service that cannot beat for itself: fs_loop() claims the lease and then hands the
# process over to the Scala service (spark_job(), R/fs.R), which never gives the control back to R
# until it dies. Nothing renews the lease, so it goes stale after runner_ttl_fs seconds while the
# service is perfectly healthy.
#
# fs is not the service several machines are meant to run - that is search, split per network. It
# runs on exactly one machine by deployment, and its lease is there as a safety net for the day
# somebody points a second machine at the same data directory: a stale lease would let that second
# fs open the same Lucene storage. It is also what the services status table of plan issue 8 will
# read, and showing a running fs as dead is not much better than the mistake itself.
#
# The monitor future started by fs_loop() is the only part of the fs service still running R code,
# so it beats on its behalf. Two precautions make this an honest heartbeat rather than the
# background beater the plan rejects (section 3.3):
#
#   * it beats only while the service really answers /ping, so a dead or wedged fs still lets its
#     lease expire;
#   * the /ping call itself only happens when a beat is due, not on every 2-second iteration of
#     the monitor loop.
renew_fs_runner_lease <- function() {
  if (!heartbeat_is_due("fs")) {
    return(invisible(FALSE))
  }
  if (!is_fs_running()) {
    return(invisible(FALSE))
  }
  renew_runner_lease("fs")
}

# Sleeps for `seconds`, proving along the way that this runner is still alive, and gives up early
# if the lease was taken in the meantime.
#
# Drop-in replacement for Sys.sleep() in the loops. The point is that `seconds` is regularly much
# larger than the TTL: the search loop sleeps until its next collection slot, which is bounded only
# by collect_span (60 minutes by default) against a 300-second TTL. A plain Sys.sleep() there makes
# a perfectly healthy runner look dead, and hands its lease to the standby machine.
#
# stop_if_lease_lost() inside the loop matters as much as the renewal: waking up 40 minutes later
# to discover that somebody else has been collecting for 35 of them is not a good outcome. Losing
# the lease raises episomer_lease_lost, which the loops let through - the runner exits, it does not
# release anything (the lease is not ours anymore).
#
# With no lease at all (conf$runner_name NULL, i.e. a sandboxed loop or the Shiny app) both calls
# are no-ops and this degrades to a plain, slightly chopped, Sys.sleep().
sleep_with_heartbeat <- function(
  seconds,
  name = conf$runner_name,
  span = runner_heartbeat_span()
) {
  seconds <- suppressWarnings(as.numeric(seconds))
  if (length(seconds) != 1 || is.na(seconds) || seconds <= 0) {
    return(invisible(NULL))
  }
  # The deadline is computed once: the renewals happening between two slices take their own time,
  # and accumulating them would make every long wait drift longer than it was asked to be.
  deadline <- Sys.time() + seconds
  repeat {
    left <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (left <= 0) {
      break
    }
    Sys.sleep(min(left, span))
    # if_due rather than a plain renewal: a two-second sleep in the middle of an active loop has
    # no reason to cost an extra request when the lease was renewed a moment ago.
    renew_runner_lease_if_due(name, span = span)
    stop_if_lease_lost(name)
  }
  invisible(NULL)
}

# Heartbeat hook for the retry callbacks of the social media APIs (the `after =` argument of
# httr2::req_retry(), R/sm-bluesky-api.R and R/sm-mastodon-api.R).
#
# This is the one long wait sleep_with_heartbeat() cannot reach: when an API answers 429, httr2
# sleeps *inside* req_perform(), and our loop only gets the control back once the request has
# finally succeeded. A Mastodon rate-limit window is 5 minutes, i.e. exactly the search TTL, so
# that silence alone is enough to lose the lease (plan section 3.3, issue 5).
#
# `after` is the only place httr2 hands us the control back before sleeping, so that is where the
# heartbeat has to live. Two cases:
#
#   * a real rate-limit wait (`seconds` known and long): we sleep it ourselves, chopped and with
#     renewals, and return 0 so that httr2 retries straight away. Doing the waiting in a callback
#     is unusual enough to be worth this comment - the alternative would be to let httr2 sleep
#     blindly for minutes;
#   * anything else (`seconds` NA on a transient 500/502/503, or a short wait): httr2 falls back on
#     its own exponential backoff, capped at 60 seconds per try - far below every TTL. One renewal
#     before returning is enough, and the backoff is left untouched.
#
# Note for issue 6: losing the lease while waiting raises episomer_lease_lost from inside
# req_perform(), so the handler that catches it must be above the tryCatch() of the search loop.
heartbeat_before_retry <- function(seconds) {
  span <- runner_heartbeat_span()
  wait <- suppressWarnings(as.numeric(seconds))
  if (length(wait) == 1 && !is.na(wait) && wait > span) {
    sleep_with_heartbeat(wait, span = span)
    return(0)
  }
  renew_runner_lease_if_due(span = span)
  seconds
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

# Gives back the lease of a runner whose holder is a process of *this* machine that has stopped.
#
# release_runner_lease() above cannot do this job: it gives back the lease of the calling process,
# and the whole point here is that the holder is somebody else - a process this machine has just
# killed, or one that died on its own.
#
# It is needed because R runs no exit handler on a signal: an Rscript receiving SIGTERM dies without
# its on.exit(unregister_runner()) ever running. Every service stopped through kill_task()
# (R/runners.R), i.e. from the setup wizard and from the Shiny app, therefore left its lease under
# runners/ until the TTL expired - up to 30 minutes for detect - and a restart in between was
# refused by a runner nobody was running.
#
# Safety is entirely lease_holder_is_dead_locally()'s: the lease has to name this host *and* the PID
# our own PID file records for that runner, and that PID has to be gone. A lease of another machine,
# one of a container sharing our host name but not our PID namespace, or one of a holder still
# alive, is left strictly alone - which is what lets the caller sweep the whole of runners/ without
# knowing what it will find in there.
release_dead_local_runner_lease <- function(name) {
  lease <- read_runner_lease(name)
  if (is.null(lease) || !lease_holder_is_dead_locally(lease, name)) {
    return(invisible(FALSE))
  }
  runner_lease_old_dir(create = TRUE)
  epifile.move(
    runner_lease_path(name),
    runner_lease_released_path(name, instance_id = lease$instance_id)
  )
  # Both layers go, exactly as in unregister_runner(): the PID file names the very process we have
  # just proved to be gone, and leaving it behind is what made the release useless where it was
  # needed most - a container restart.
  #
  # PIDs are handed out from the same small range at every start of a container, so the number a
  # dead PID file records is very likely to belong to a live, unrelated R process of the new run -
  # `future::plan(multisession)` alone takes some twenty of them for its idle PSOCK workers. The
  # local check of register_runner() then reads that file, finds the PID alive, and refuses to start
  # a runner nobody is running, with an error naming a process that has nothing to do with episomer.
  # That is what stopped the fs monitor from ever registering again after a `docker compose down`
  # followed by a `docker compose up`: its lease was correctly released here, its PID file was not,
  # and a PSOCK worker of the new run had been given PID 119 back.
  #
  # Guarded, because the release above frees the runner: a local process may claim it and write its
  # own PID file between the two, and that file is not ours to delete.
  if (identical(read_running_task_pid_file(name), as.integer(lease$pid))) {
    remove_running_task_pid(name)
  }
  message(sprintf(
    "Released the lease of runner %s: its holder (PID %s) is not running on this machine anymore",
    name,
    lease$pid
  ))
  invisible(TRUE)
}

# Sweeps runners/ and gives back every lease held by a stopped process of this machine. Returns the
# names of the runners that were released.
#
# Called when stopping services (R/runners.R): the runner that was just killed is released, and so
# is any lease a previous crash left behind - each of them under the same proof as the one above, so
# there is nothing to gain from restricting the sweep to the runner being stopped. Listing runners/
# rather than naming the runners also covers the ones this process cannot enumerate: the per-network
# search leases of the workers, and fs_mon.
#
# Nothing here may raise: it runs while shutting services down, and a listing that fails (no
# runners/ folder yet, fs unreachable in a remote deployment) must not turn a stop into an error.
release_dead_local_runner_leases <- function() {
  names <- tryCatch(runner_lease_names(), error = function(e) character(0))
  released <- vapply(
    names,
    function(name) {
      tryCatch(release_dead_local_runner_lease(name), error = function(e) FALSE)
    },
    logical(1)
  )
  invisible(unname(names[released]))
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
#               and the runner can be claimed again. A lease whose holder is a *dead process of this
#               machine* is stale too, whatever its age (lease_holder_is_dead_locally()); the
#               `holder_dead_locally` flag says which of the two happened, so that a status table
#               can word it as "crashed here" rather than "not renewed".
runner_lease_status <- function(name) {
  lease <- read_runner_lease(name)
  if (is.null(lease)) {
    return(list(
      runner = name,
      status = "free",
      host = NA_character_,
      instance_id = NA_character_,
      pid = NA_integer_,
      claimed_at = NA_character_,
      seen_at = as.POSIXct(NA),
      age = NA_real_,
      ttl = runner_lease_ttl(name),
      mine = FALSE,
      holder_dead_locally = FALSE
    ))
  }
  age <- as.numeric(difftime(Sys.time(), lease$seen_at, units = "secs"))
  ttl <- runner_lease_ttl(name)
  dead_locally <- lease_holder_is_dead_locally(lease, name)
  list(
    runner = name,
    status = if (dead_locally || (!is.na(age) && age > ttl)) {
      "stale"
    } else {
      "running"
    },
    host = lease$host,
    instance_id = lease$instance_id,
    pid = lease$pid,
    # From the JSON, not from the file: "since when is this instance the holder". Renewing only
    # touches the file, so claimed_at is not refreshed by a heartbeat - which is exactly what the
    # "since" column of the status table wants to show.
    claimed_at = lease$claimed_at,
    seen_at = lease$seen_at,
    age = age,
    ttl = ttl,
    mine = identical(lease$instance_id, conf$instance_id),
    holder_dead_locally = dead_locally
  )
}

# TRUE when somebody, anywhere in the cluster, is currently acting as runner `name`.
#
# This is the cluster-wide question, the one the Shiny app and the check_*() functions ask, and the
# lease is the only thing that can answer it: the PID file only ever knew about this machine (plan
# section 6.2). Its local counterpart is is_runner_running_locally() in R/runners.R - "is there
# something *here* to kill?" - and everything that stops a process must keep using that one.
#
# "running" already folds in the local cross-check, so a runner whose process crashed on this very
# machine reads as stopped immediately instead of for one TTL (see runner_lease_status()).
is_runner_running <- function(name) {
  identical(runner_lease_status(name)$status, "running")
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

# Runs `expr` and turns the loss of the runner lease into a clean, explained exit.
#
# This is the other half of stop_if_lease_lost(): the guard points scattered before the meaningful
# writes raise the condition, the top of each loop catches it here. Losing a lease is not a failure
# of this process - another instance legitimately took over after this one stopped proving it was
# alive - so the loop has to end the way it ends on a clean shutdown: one explicit log line, and no
# error propagating to a supervisor that would read it as a crash worth restarting immediately.
#
# What this deliberately does NOT do is release the lease. The file under runners/ now belongs to
# the new holder, and releasing would move *its* file away. The on.exit(release_runner_lease()) the
# loops already carry is harmless for the same reason: it checks i_hold_runner_lease() first and
# refuses (see release_runner_lease()), so there is nothing to undo here.
#
# Returns the value of `expr`, or the sentinel "lease-lost" when the lease was taken - which is how
# search_loop() tells a worker that stepped aside from one that ended for an unknown reason.
run_until_lease_lost <- function(expr) {
  tryCatch(
    # `expr` is evaluated here, inside the tryCatch(): the handler below is in place for the whole
    # run of the loop, including the waits nested deep inside req_perform() (heartbeat_before_retry)
    expr,
    episomer_lease_lost = function(cnd) {
      msg(conditionMessage(cnd))
      msg(paste(
        "Another instance is now in charge of this runner: exiting cleanly and leaving the lease",
        "to its new holder."
      ))
      "lease-lost"
    }
  )
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
  paste(
    runner_lease_holder_sentence(lease, name),
    "This instance will not start it."
  )
}

# The sentence the refusal above and the wait below both open with: who holds the runner, on which
# machine, since when, and how long ago it last proved it was alive.
#
# Shared rather than written twice so that a container that waits and a container that gives up
# describe the same situation in the same words - an operator grepping the logs of two machines
# reads one story, not two.
runner_lease_holder_sentence <- function(lease, name) {
  age <- as.numeric(difftime(Sys.time(), lease$seen_at, units = "secs"))
  sprintf(
    paste(
      "Runner '%s' is already held by another instance (%s) running on host '%s'",
      "since %s, last seen %.0f seconds ago (lease TTL: %.0f seconds)."
    ),
    name,
    lease$instance_id,
    lease$host,
    lease$claimed_at,
    age,
    runner_lease_ttl(name)
  )
}

# The same thing said by a service that is going to wait instead of giving up
# (wait_for_runner_lease()).
#
# It has to answer one more question than the refusal does - "is anything going to happen, and
# when?" - because the alternative is a container that looks hung. Naming the interval and, when
# the wait is bounded, its deadline is what turns a silent process into an explained one. The age
# of the last heartbeat in the shared sentence gives the rest: a holder last seen 200 seconds ago
# against a 300-second TTL will be gone in about a hundred.
runner_lease_wait_message <- function(
  name,
  timeout = runner_wait_timeout(),
  interval = runner_wait_interval()
) {
  lease <- read_runner_lease(name)
  if (is.null(lease)) {
    # Released between the failed claim and this read, or taken over by a third instance whose
    # write has not landed yet. Either way there is nothing to describe but the retry.
    return(sprintf(
      paste(
        "Runner '%s' could not be claimed, and the lease is already gone. Trying again in",
        "%.0f seconds."
      ),
      name,
      interval
    ))
  }
  deadline <- if (is.finite(timeout)) {
    sprintf(", for at most %s", format_lease_duration(timeout))
  } else {
    ""
  }
  paste(
    runner_lease_holder_sentence(lease, name),
    sprintf(
      "This instance is waiting for it to be released or to expire, trying again every %.0f seconds%s.",
      interval,
      deadline
    )
  )
}

# Explains why a runner must not be started on *this* machine right now, or "" when nothing stands
# in the way.
#
# This is the distinction plan issue 8 is about: "not running here" and "not running at all" used to
# be the same sentence, and they are not the same situation anymore. A service whose lease is held,
# alive, by another machine sharing this database is running - starting a second one here would be
# refused by register_runner() after the fact, so the app says so before spawning anything.
#
# Two cases deliberately do *not* produce a message:
#
#  * a stale lease held by another machine: taking over is exactly what the standby is for, and the
#    claim is going to succeed;
#  * a lease held by this very host: starting is a local matter then, and the local PID check of
#    register_runner() already refuses a duplicate with a message naming the PID to kill.
runner_running_elsewhere_message <- function(name) {
  status <- runner_lease_status(name)
  if (!identical(status$status, "running")) {
    return("")
  }
  host <- if (length(status$host) != 1 || is.na(status$host)) {
    NA_character_
  } else {
    as.character(status$host)
  }
  if (identical(host, as.character(conf$instance_host))) {
    return("")
  }
  sprintf(
    paste(
      "Runner '%s' is already running on host '%s' (holding its lease since %s, last seen %s).",
      "Only one machine sharing this episomer database may run it at a time, so it has not been",
      "started here. Stop it on that machine, or wait for its lease to expire (TTL: %s), and try",
      "again."
    ),
    name,
    if (is.na(host)) "unknown" else host,
    runner_lease_since_label(status),
    runner_lease_last_seen_label(status),
    format_lease_duration(status$ttl)
  )
}

# The status table of the Shiny app --------------------------------------------------------------
#
# Everything below is read-only and is meant for a human: the Shiny status panel (plan section 8)
# and an operator reading runners/ by hand. No decision of the lease mechanism depends on it.
#
# The whole table is one listing of runners/ plus one read per file. runners/old/ is deliberately
# left out: a released runner is one whose file is gone (section 3.4), so it has no row at all,
# which is what "free" looks like here. A runner that has never been claimed has no row either -
# the table says who is *holding* something, not what could be started.

# Names of the runners holding a live lease right now, in display order.
#
# The listing is not recursive, so runners/old/ appears in it as a directory entry: the .json
# pattern is what keeps it out (and the fs API answers an empty list for a missing folder, so a
# deployment that has never started a runner needs no special case).
runner_lease_names <- function() {
  files <- epilist.files(runner_lease_dir(), pattern = "\\.json$")
  if (length(files) == 0) {
    return(character(0))
  }
  names <- sub("\\.json$", "", basename(files))
  sort_runner_names(names)
}

# Display order of the runners: services in the order an operator thinks about them, then
# alphabetical inside a service. Alphabetical puts the parent lease of search ("search", taken by
# search_loop()) right before its per-network children ("search.bluesky", ...), which is the
# reading order that makes the split of a collection between machines obvious.
sort_runner_names <- function(names) {
  services <- c("fs", "fs_mon", "search", "detect")
  rank <- match(runner_service(names), services)
  # an unknown service sorts last rather than dropping out of the table
  rank[is.na(rank)] <- length(services) + 1L
  names[order(rank, names)]
}

# Status of every runner holding a live lease, as a list of runner_lease_status() entries.
runner_leases <- function() {
  stop_if_no_config("Cannot list the runner leases without configuration setup")
  lapply(runner_lease_names(), runner_lease_status)
}

# Human-readable name of the service a runner belongs to, matching the wording used for the same
# services in the status panel above the table ("episomer database", "Data collection &
# processing", "Alerts"). The raw runner name is kept alongside it (runner_leases_df() puts it in
# brackets) so a "search.bluesky" row is still traceable back to what it is.
#
# "fs_mon" has no entry here on purpose: it is the watchdog that keeps "fs" alive, not a service of
# its own, and the app filters its row out of the table before this is ever called on it.
runner_service_display_name <- function(name) {
  labels <- c(
    fs = "episomer database",
    search = "Data collection & processing",
    detect = "Alerts"
  )
  label <- unname(labels[runner_service(name)])
  if (is.na(label)) name else label
}

# "Data collection & processing (search.bluesky)": the display name above with the raw runner name
# in brackets, or just the raw name when it has no display name of its own.
runner_display_name <- function(name) {
  label <- runner_service_display_name(name)
  if (identical(label, name)) name else sprintf("%s (%s)", label, name)
}

# The same thing as a data.frame, ready to be displayed: one row per runner, the five columns of
# plan issue 8.
#
# A lease can disappear between the listing and its read (its holder released it, or another
# instance took over and moved the file): runner_lease_status() then reports it as "free", and such
# a row is dropped instead of being shown as a runner nobody holds.
runner_leases_df <- function() {
  leases <- Filter(
    function(lease) !identical(lease$status, "free"),
    runner_leases()
  )
  if (length(leases) == 0) {
    # An empty table with its columns, not an empty data.frame: the app renders the header either
    # way and the caller does not have to special-case the "nothing running anywhere" deployment.
    return(data.frame(
      Runner = character(0),
      Status = character(0),
      Host = character(0),
      `Last seen` = character(0),
      Since = character(0),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Runner = vapply(leases, function(l) l$runner, character(1)),
    Status = vapply(leases, runner_lease_status_label, character(1)),
    Host = vapply(leases, runner_lease_host_label, character(1)),
    `Last seen` = vapply(leases, runner_lease_last_seen_label, character(1)),
    Since = vapply(leases, runner_lease_since_label, character(1)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# How a status reads in the table.
#
# "stale" covers two very different situations and an operator needs to tell them apart:
# a lease nobody renewed (the holder's machine is unreachable, or its clock-independent heartbeat
# stopped) and a lease whose holder was a process of *this* machine that is not running anymore
# (holder_dead_locally, see lease_holder_is_dead_locally()). The second one is a crash we can be
# sure about, and it is reclaimable right now instead of at the end of the TTL.
# "supervisor" is the third case, and it is not a failure at all. The parent "search" runner is
# claimed by search_loop(), which then blocks in parallel::parLapply() for the whole life of the
# service: it has no iteration left to renew its lease on, so that lease goes stale after
# runner_ttl_search on a perfectly healthy machine and stays that way (R/runners.R,
# is_search_running(), and plan section 9.5). Wording it "stale" made every reader of the table
# report a dead search loop that was collecting normally, so it is named for what it is, with a
# pointer to the rows that do carry the truth - the per-network workers, which renew every
# heartbeat span and which sort_runner_names() has just placed right underneath.
#
# The order of the branches is what keeps this honest: a parent that really died *here* is caught
# by holder_dead_locally above and still reads "crashed here", and a parent that died elsewhere
# shows up as its workers going stale in the rows this label sends the reader to.
runner_lease_status_label <- function(lease) {
  if (identical(lease$status, "running")) {
    return("active")
  }
  if (isTRUE(lease$holder_dead_locally)) {
    return("stopped on this machine")
  }
  if (identical(lease$runner, "search")) {
    return(
      "coordinates collection, sends no activity signal by design: see the search.* rows for its real state"
    )
  }
  # fs cannot beat for itself (renew_fs_runner_lease()): only its monitor does. A stale fs lease
  # on a database that still answers /ping is a monitor that stopped reporting, not a dead
  # database, and "probably stopped" would tell the reader the opposite of the status panel above.
  if (identical(lease$runner, "fs") && is_fs_running()) {
    # return("running, but its monitor stopped reporting")
    return("active")
  }
  sprintf(
    "no activity for %s (probably stopped)",
    format_lease_duration(lease$ttl)
  )
}

# Which machine holds the runner. The marker matters in the single-machine deployment, where every
# row is local and an unmarked host name would leave the reader wondering.
runner_lease_host_label <- function(lease) {
  host <- if (is.null(lease$host) || is.na(lease$host)) {
    "unknown"
  } else {
    as.character(lease$host)
  }
  if (identical(host, as.character(conf$instance_host))) {
    return(paste(host, "(this machine)"))
  }
  host
}

# When the holder last proved it was alive, i.e. the age of the lease file's mtime - the only
# quantity the expiry decision is based on (section 3.2).
runner_lease_last_seen_label <- function(lease) {
  age <- format_lease_duration(lease$age)
  if (is.na(age)) "unknown" else paste(age, "ago")
}

# Since when this holder has been the holder, read from the JSON. It does not move when the lease
# is renewed, so a long "since" next to a short "last seen" is the picture of a healthy runner.
#
# Displayed in UTC, like the stored value: the holder may sit in another timezone than the reader,
# and the point of this column is to be comparable with the runners/*.json files and with the logs
# of the other machines. Anything unparseable is shown as it was written rather than hidden.
runner_lease_since_label <- function(lease) {
  if (is.null(lease$claimed_at) || is.na(lease$claimed_at)) {
    return("unknown")
  }
  claimed_at <- as.character(lease$claimed_at)
  parsed <- suppressWarnings(as.POSIXct(
    claimed_at,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  ))
  if (length(parsed) != 1 || is.na(parsed)) {
    return(claimed_at)
  }
  paste(strftime(parsed, format = "%Y-%m-%d %H:%M", tz = "UTC"), "UTC")
}

# A duration in seconds, written for a human reading a status line.
format_lease_duration <- function(seconds) {
  if (length(seconds) != 1 || is.na(seconds)) {
    return(NA_character_)
  }
  seconds <- max(0, round(as.numeric(seconds)))
  if (seconds < 60) {
    sprintf("%d s", seconds)
  } else if (seconds < 3600) {
    sprintf("%d min", round(seconds / 60))
  } else if (seconds < 86400) {
    sprintf("%.1f h", seconds / 3600)
  } else {
    sprintf("%.1f days", seconds / 86400)
  }
}
