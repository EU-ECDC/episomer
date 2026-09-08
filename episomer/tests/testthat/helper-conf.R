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

# Configuration of a local episomer instance owning a fresh, empty data directory: fs_remote = NULL
# so epipath() resolves to that directory and every fs call takes its local branch, no network.
#
# `guard = 0` is not set here on purpose - a test that goes through claim_runner_lease() has to say
# so itself (runner_claim_guard = 0), which keeps the guard wait visible where it matters.
local_lease_conf <- function(
  instance_id = "instance-A",
  instance_host = "machine-A",
  ...,
  .local_envir = parent.frame()
) {
  data_dir <- withr::local_tempdir(.local_envir = .local_envir)
  local_heartbeats(.local_envir = .local_envir)
  local_conf(
    data_dir = data_dir,
    fs_remote = NULL,
    instance_id = instance_id,
    instance_host = instance_host,
    ...,
    .local_envir = .local_envir
  )
  data_dir
}

# Empties the record of the last lease renewals (lease_last_renewal, R/runners-lease.R) and puts
# it back when the calling test exits.
#
# That environment lives in the package namespace, so it survives from one test to the next: a
# renewal noted while testing "detect" would otherwise make the next test believe no heartbeat is
# due for that runner. It is session state belonging to the configuration, so local_lease_conf()
# resets it too.
local_heartbeats <- function(.local_envir = parent.frame()) {
  saved <- as.list(lease_last_renewal, all.names = TRUE)
  withr::defer(
    {
      rm(list = ls(lease_last_renewal, all.names = TRUE), envir = lease_last_renewal)
      list2env(saved, envir = lease_last_renewal)
    },
    envir = .local_envir
  )
  rm(list = ls(lease_last_renewal, all.names = TRUE), envir = lease_last_renewal)
  invisible(lease_last_renewal)
}

# Same thing for the record of what the fs monitor has already logged (fs_monitor_last_log, R/fs.R),
# which fs_monitor_log_once() reads to drop the repetitions of a message. It lives in the package
# namespace too, so a message logged by one test would be dropped by the next one.
local_monitor_logs <- function(.local_envir = parent.frame()) {
  saved <- as.list(fs_monitor_last_log, all.names = TRUE)
  withr::defer(
    {
      rm(list = ls(fs_monitor_last_log, all.names = TRUE), envir = fs_monitor_last_log)
      list2env(saved, envir = fs_monitor_last_log)
    },
    envir = .local_envir
  )
  rm(list = ls(fs_monitor_last_log, all.names = TRUE), envir = fs_monitor_last_log)
  invisible(fs_monitor_last_log)
}

# Patches the pid recorded in a lease: write_runner_lease() can only record Sys.getpid(), so this is
# the only way to describe a *dead* holder - which lease_holder_is_dead_locally() (R/runners-lease.R)
# is all about - without spawning and killing a real process.
set_lease_pid <- function(name, pid) {
  path <- runner_lease_path(name)
  lease <- jsonlite::read_json(path, simplifyVector = TRUE)
  lease$pid <- pid
  write_json_atomic(lease, path, auto_unbox = TRUE)
  invisible(pid)
}

# A PID file holding nothing but the number, which is what the Scala side writes for the JVMs when
# EPISOMER_INSTANCE is unset (ProcessID.writePID) and what every version before the record format
# wrote. Kept as the default in the tests that only care about the number: the fields it does not
# carry read as "unknown" and fail open, so this is the previous behaviour exactly.
write_local_pid_file <- function(name, pid) {
  write(pid, file = get_running_task_pid_path(name), append = FALSE)
  invisible(pid)
}

# A PID file as another PID namespace would have left it in a directory we also read: the same path
# and a plausible number, but a `host` or a `start_time` of its own. This is the state a
# `make compose-down` leaves in $EPI_HOME - the fs container mounts the data directory, so its
# fs.PID is written there and read back by the host, where the number means something else
# entirely (see write_running_task_pid(), R/runners.R).
write_foreign_pid_file <- function(
  name,
  pid,
  host = "fs",
  start_time = "0"
) {
  writeLines(
    c(as.character(pid), host, start_time),
    con = get_running_task_pid_path(name)
  )
  invisible(pid)
}

# A PID no process can have: is_r_pid_running() must answer FALSE for it.
dead_pid <- 4194303L

# Says "this very process is alive, and no other PID is", for the tests that need a *live* holder.
#
# The live holder cannot be described the way dead_pid describes a dead one: only a real running
# process will do, and the only one a test is sure to have is itself. Sys.getpid() alone is not
# enough though, because is_r_pid_running() (R/runners.R) asks the operating system for the *name*
# of the process and accepts R, Rscript, Rterm and rsession: an R session hosted under a name of its
# own - ark for Positron, python for radian - is not recognised, so those tests would pass or fail
# depending on what started them. Mocking the probe is what test-runners_wait.R already does for
# the same reason; what is under test here is what lease_holder_is_dead_locally() concludes from a
# live PID, not how liveness is established.
local_live_this_process <- function(.local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    is_r_pid_running = function(pid) identical(as.integer(pid), Sys.getpid()),
    .package = "episomer",
    .env = .local_envir
  )
}

# What is_r_pid_running() / is_java_pid_running() conclude from a process whose `ps -o comm=` name
# is `name`, whatever PID it is asked about.
#
# The command name is the only input of the decision under test here, and it is the one thing a
# test cannot arrange for real: it would have to start a process actually called "ark" or
# "sparkR". Mocking the probe rather than the process is what the tests of this layer already do
# (see local_live_this_process() above), and pid_command_name() is the single call every liveness
# question passes through.
named_pid_is_r <- function(name) {
  named_pid_is_of_type(name, is_r_pid_running)
}

named_pid_is_java <- function(name) {
  named_pid_is_of_type(name, is_java_pid_running)
}

named_pid_is_of_type <- function(name, probe) {
  testthat::with_mocked_bindings(
    probe(1L),
    pid_command_name = function(pid) name,
    .package = "episomer"
  )
}

# Makes the lease of `name` belong to another machine, as if it had been taken over while this
# process was busy collecting.
steal_lease <- function(name) {
  with_instance("instance-B", host = "machine-B", code = write_runner_lease(name))
}

# The one-plan, one-topic configuration search_loop_worker() needs to reach its inner loop:
# requests = 0 and an expected_end in the future keep update_plans_schedule() from rebuilding
# anything, so the plan below is the plan the worker will work on.
#
# `network` appears twice on purpose, and both are needed. The one on the topic entry is what
# topics_of_network() selects on (R/config.R) - conf$topics holds every network's topics, and a
# worker with none of its own would just wait; the one on the plan is what the round robin of
# search_loop_worker() tests before making a request.
a_topic_with_one_plan <- function(network = "bluesky") {
  list(list(
    topic = "topic-1",
    query = "something",
    network = network,
    plan = list(list(
      network = network,
      requests = 0,
      expected_end = Sys.time() + 3600,
      end_on = NULL
    ))
  ))
}
