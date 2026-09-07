# The PID file layer (R/runners.R): what a `<runner>.PID` file records, and when it is evidence
# about a process rather than just a number on disk.
#
# This is the local half of the two exclusion layers register_runner() has to satisfy - the lease,
# the cluster-wide half, is tested in test-runners_lease.R. What is under test here is the one
# question the layer answers, "is the process this file describes still running here?", and the two
# ways it used to get it wrong:
#
#   * a PID file written in *another PID namespace*. The fs container mounts the data directory, so
#     $EPI_HOME/fs.PID is written by its main Rscript - PID 9 - and read back by the host, where
#     that number belongs to whatever started ninth at boot. `make compose-down` runs no R code on
#     the way out, so the file stays, and the wizard then refused to start the fs with
#     "Runner fs PID: 9 is already running";
#   * a PID *recycled* on this very machine. A container restarted under the same EPISOMER_INSTANCE
#     is the same host with the same small PID range, so the new run's PID 9 is a different process
#     wearing the holder's number (inst/schema/multi_services_plan.md, section 9 item 6).
#
# Liveness itself is mocked here, through local_live_this_process() and the named probes: the only
# live process a test is sure to have is itself, and it is not necessarily recognised by name (ark
# for Positron, python for radian - see helper-conf.R).

no_record <- list(
  pid = NA_integer_,
  host = NA_character_,
  start_time = NA_character_
)

# What the file holds ------------------------------------------------------------------------------

test_that("write_running_task_pid() records the PID, the host and the start time", {
  local_lease_conf(instance_host = "machine-A")

  write_running_task_pid("detect")

  # The PID stays alone on the first line: the Scala side writes fs.java.PID and detect.java.PID in
  # that form (ProcessID.writePID) and kill_task() has to keep reading both.
  expect_identical(
    readLines(get_running_task_pid_path("detect"))[[1]],
    as.character(Sys.getpid())
  )
  expect_identical(
    read_running_task_pid_record("detect"),
    list(
      pid = Sys.getpid(),
      host = "machine-A",
      start_time = pid_start_time(Sys.getpid())
    )
  )
})

test_that("read_running_task_pid_record() answers NA for what it cannot read", {
  local_lease_conf()

  # no file at all: the runner has never been registered here
  expect_identical(read_running_task_pid_record("fs"), no_record)

  path <- get_running_task_pid_path("fs")
  # a file whose first line is not a number is no more usable than an absent one, and neither is an
  # interrupted write that got as far as the newline
  writeLines("not a pid", path)
  expect_identical(read_running_task_pid_record("fs")$pid, NA_integer_)
  writeLines(c("", "machine-A"), path)
  expect_identical(read_running_task_pid_record("fs")$pid, NA_integer_)
  writeLines(character(0), path)
  expect_identical(read_running_task_pid_record("fs"), no_record)

  expect_false(recorded_pid_is_running(read_running_task_pid_record("fs")))
  expect_identical(get_running_task_pid("fs"), -1)
})

test_that("a PID file holding nothing but a number falls back to trusting the number", {
  # The Scala side's form when EPISOMER_INSTANCE is unset, and the form every version before the
  # record wrote. A field that is not there means "unknown", never "somebody else's": both extra
  # signals fail open, so such a file behaves exactly as it did before.
  local_lease_conf(instance_host = "machine-A")
  write_local_pid_file("search", Sys.getpid())
  local_live_this_process()

  expect_identical(
    read_running_task_pid_record("search"),
    list(pid = Sys.getpid(), host = NA_character_, start_time = NA_character_)
  )
  expect_true(recorded_pid_is_running(read_running_task_pid_record("search")))
  expect_identical(get_running_task_pid("search"), Sys.getpid())
})

# A record written somewhere else --------------------------------------------------------------

test_that("a PID file of another host is not believed, however alive its number is here", {
  # The state every `make compose-down` leaves behind. The number is deliberately our own live PID,
  # and liveness is mocked to say so: even then the record must not be believed, because it was not
  # written in this namespace and says nothing about our process table.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_claim_guard = 0
  )
  write_foreign_pid_file("fs", Sys.getpid(), host = "fs")
  local_live_this_process()

  expect_false(recorded_pid_is_running(read_running_task_pid_record("fs")))
  expect_identical(get_running_task_pid("fs"), -1)
  expect_false(is_runner_running_locally("fs"))
})

test_that("register_runner() starts a runner whose PID file was left by a container", {
  # The end of the bug, stated as the operator meets it: the fs is started again from the wizard
  # instead of being refused with "Runner fs PID: 9 is already running", and the file it leaves
  # behind describes *this* machine.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_claim_guard = 0
  )
  write_foreign_pid_file("fs", Sys.getpid(), host = "fs")
  local_live_this_process()

  expect_silent(register_runner("fs"))

  expect_identical(read_running_task_pid_record("fs")$host, "machine-A")
  expect_true(i_hold_runner_lease("fs"))
})

# A PID recycled on this host -------------------------------------------------------------------

test_that("a record whose start time is not that of the live PID is not believed", {
  # Same host, same number, different process - the restarted container of section 9 item 6.
  # EPISOMER_INSTANCE is set per service, so both runs are "machine-A" here and the host signal
  # cannot help; the start time recorded next to the PID is what tells them apart.
  local_lease_conf(instance_host = "machine-A")
  write_foreign_pid_file(
    "detect",
    Sys.getpid(),
    host = "machine-A",
    start_time = "a previous run"
  )
  local_live_this_process()

  expect_false(recorded_pid_is_running(read_running_task_pid_record("detect")))
  expect_identical(get_running_task_pid("detect"), -1)
})

test_that("a record of a live process of this host is believed", {
  # The other side of the same test: nothing above may be read as "a record is never trusted".
  local_lease_conf(instance_host = "machine-A")
  write_running_task_pid("detect")
  local_live_this_process()

  expect_true(recorded_pid_is_running(read_running_task_pid_record("detect")))
  expect_identical(get_running_task_pid("detect"), Sys.getpid())
  expect_true(is_runner_running_locally("detect"))
})

test_that("a lease whose holder's PID has been recycled here is free again", {
  # What the two signals buy the lease layer. The lease names our host and our PID file agrees on
  # the number, so lease_holder_is_dead_locally() used to conclude "still alive" and the restarted
  # service waited out its whole TTL. The start time disproves the record.
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_fs = 120
  )
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("fs"))
  set_lease_pid("fs", Sys.getpid())
  write_foreign_pid_file(
    "fs",
    Sys.getpid(),
    host = "machine-A",
    start_time = "a previous run"
  )
  local_live_this_process()

  expect_true(lease_holder_is_dead_locally(read_runner_lease("fs"), "fs"))
  expect_true(lease_is_free(read_runner_lease("fs"), "fs"))
})

# Never signalling a PID that is not ours --------------------------------------------------------

test_that("kill_task() does not signal a PID recorded on another host", {
  # kill(1) takes a number. A container's fs.PID holds a number that belongs to an unrelated
  # process of this host, so a stop issued here used to be able to signal it.
  local_lease_conf(instance_host = "machine-A")
  write_foreign_pid_file("fs", Sys.getpid(), host = "fs")
  local_live_this_process()

  expect_message(expect_false(kill_task("fs", "R")), "not running anymore")
})

test_that("kill_task() has nothing to do without a PID file or without a usable PID", {
  local_lease_conf(instance_host = "machine-A")

  expect_false(kill_task("fs", "R"))
  writeLines("not a pid", get_running_task_pid_path("fs"))
  expect_message(expect_false(kill_task("fs", "R")), "does not hold a usable PID")
})

# The probes the whole layer rests on ------------------------------------------------------------

test_that("is_r_pid_running() is exact on the PID, not a substring of a ps line", {
  # Regression on the pipeline this replaced:
  #   ps -cax | grep 'R\\|rsession' | grep <pid>
  # "some line mentioning R also contains these digits somewhere". Any R process whose ps line held
  # the digits of `pid` answered on its behalf, so PID 9 read as alive as soon as an unrelated R
  # session held PID 2990 - and that, on top of a PID file carrying no host, is what turned a
  # leftover container fs.PID into a refusal to start.
  skip_on_os("windows")
  local_mocked_bindings(
    pid_command_name = function(pid) {
      if (identical(as.integer(pid), 2990L)) "R" else character(0)
    },
    .package = "episomer"
  )

  expect_true(is_r_pid_running(2990L))
  expect_false(is_r_pid_running(9L))
  expect_false(is_r_pid_running(299L))
  expect_false(is_r_pid_running(29900L))
})

test_that("pid_command_name() names this process, and nothing for a PID no process has", {
  skip_on_os("windows")
  # Deliberately not asserting *which* name: Rscript execs the R binary and reports "R", an IDE
  # host reports its own (ark for Positron). That is precisely why the tests above mock this.
  expect_length(pid_command_name(Sys.getpid()), 1)
  expect_identical(pid_command_name(dead_pid), character(0))
})

test_that("pid_start_time() is stable for a live process and NA when there is none", {
  skip_on_os("windows")
  started <- pid_start_time(Sys.getpid())

  expect_false(is.na(started))
  # Same process, same answer - the whole point: it is only ever compared with itself.
  expect_identical(pid_start_time(Sys.getpid()), started)
  expect_identical(pid_start_time(dead_pid), NA_character_)
  expect_identical(pid_start_time(NA_integer_), NA_character_)
})
