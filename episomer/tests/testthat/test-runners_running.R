# Asking whether a runner is running (multi-instance services, plan issue 7).
#
# is_search_running(), is_detect_running() and missing_search_jobs() used to read the local PID
# files, which could only ever answer "is it running *here*". They now read the runner leases, i.e.
# the cluster-wide question, which is what makes the Shiny app usable in a multi-machine deployment.
#
# Same setup as test-runners_lease.R: fs_remote = NULL so epipath() resolves to a temp directory, and
# a "second instance" is just another conf$instance_id borrowed with with_instance().

# Cluster-wide status ------------------------------------------------------------------------------

test_that("is_detect_running() is FALSE when no instance holds the runner", {
  local_lease_conf()

  expect_false(is_detect_running())
  expect_identical(runner_lease_status("detect")$status, "free")
})

test_that("is_detect_running() is TRUE while another machine holds a fresh lease", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_detect = 1800)
  # this is the whole point of issue 7: nothing runs on this machine, no PID file here, and the
  # answer is still "running"
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))

  expect_true(is_detect_running())
  expect_identical(get_running_task_pid("detect"), -1)
})

test_that("is_detect_running() is FALSE once the lease went stale", {
  local_lease_conf(instance_id = "instance-A", runner_ttl_detect = 300)
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))
  # aging the file itself rather than mocking: freshness comes from the mtime, which is the fs
  # machine's clock (plan section 3.2)
  Sys.setFileTime(runner_lease_path("detect"), Sys.time() - 600)

  expect_false(is_detect_running())
  expect_identical(runner_lease_status("detect")$status, "stale")
})

test_that("missing_search_jobs() reports the networks no instance collects", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_ttl_search = 300,
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    mastodon_instances = "piaille.fr"
  )
  with_instance(
    "instance-B",
    host = "machine-B",
    code = write_runner_lease("search.bluesky")
  )

  # bluesky is collected by machine B, so it is not missing - only mastodon is
  expect_identical(missing_search_jobs(), "mastodon.piaille.fr")
})

test_that("missing_search_jobs() is empty when every network is collected somewhere", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_ttl_search = 300,
    sm_activated_bluesky = TRUE,
    sm_activated_mastodon = TRUE,
    mastodon_instances = "piaille.fr"
  )
  # the split of plan section 7, case 7: two machines sharing the collection
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search.bluesky"))
  with_instance(
    "instance-C",
    host = "machine-C",
    code = write_runner_lease("search.mastodon.piaille.fr")
  )

  expect_length(missing_search_jobs(), 0)
})

# The parent "search" lease ------------------------------------------------------------------------

test_that("is_search_running() is TRUE on the just-claimed parent lease alone", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_ttl_search = 300,
    sm_activated_bluesky = TRUE
  )
  # the startup window: search_loop() has claimed "search" and the workers have not registered yet
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("search"))

  expect_true(is_search_running())
})

test_that("is_search_running() is TRUE on a worker lease while the parent one is stale", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_ttl_search = 300,
    sm_activated_bluesky = TRUE
  )
  with_instance("instance-B", host = "machine-B", code = {
    write_runner_lease("search")
    write_runner_lease("search.bluesky")
  })
  # search_loop() blocks in parLapply() for the life of the service and cannot renew the parent
  # lease, so this is what a *healthy* search service looks like after runner_ttl_search seconds
  # (plan section 9.5). Reading the parent lease alone would report "stopped" here.
  Sys.setFileTime(runner_lease_path("search"), Sys.time() - 600)

  expect_identical(runner_lease_status("search")$status, "stale")
  expect_true(is_search_running())
})

test_that("is_search_running() is FALSE when neither the parent nor the workers are fresh", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_ttl_search = 300,
    sm_activated_bluesky = TRUE
  )
  with_instance("instance-B", host = "machine-B", code = {
    write_runner_lease("search")
    write_runner_lease("search.bluesky")
  })
  Sys.setFileTime(runner_lease_path("search"), Sys.time() - 600)
  Sys.setFileTime(runner_lease_path("search.bluesky"), Sys.time() - 600)

  expect_false(is_search_running())
})

# Local cross-check on the PID (plan sections 3.6 and 6.2) -----------------------------------------

test_that("a holder that crashed on this machine reads as stopped at once", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 1800
  )
  # a previous process of *this* machine claimed the lease, wrote its PID file, and died without
  # releasing anything. The lease mtime is fresh, so freshness alone would say "running" for the
  # next 30 minutes.
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("detect"))
  set_lease_pid("detect", dead_pid)
  write_local_pid_file("detect", dead_pid)

  expect_false(is_detect_running())
  status <- runner_lease_status("detect")
  expect_identical(status$status, "stale")
  expect_true(status$holder_dead_locally)
  expect_lt(status$age, status$ttl)
})

test_that("the crashed holder's lease can be reclaimed without waiting for the TTL", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 1800,
    runner_claim_guard = 0
  )
  with_instance("instance-old", host = "machine-A", code = write_runner_lease("detect"))
  set_lease_pid("detect", dead_pid)
  write_local_pid_file("detect", dead_pid)

  # the fast reclaim of plan section 3.6: restarting after a crash collects again immediately
  expect_true(lease_is_free(read_runner_lease("detect"), "detect"))
  expect_true(claim_runner_lease("detect", guard = 0))
  expect_true(i_hold_runner_lease("detect"))
})

test_that("the cross-check does not fire on a live process of this machine", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 1800
  )
  # another R process of this machine holds it, and the testthat process stands in for it: its PID
  # is the only one a test can be sure names a live process (see local_live_this_process())
  local_live_this_process()
  with_instance("instance-B", host = "machine-A", code = write_runner_lease("detect"))
  write_local_pid_file("detect", Sys.getpid())

  expect_true(is_detect_running())
  expect_false(runner_lease_status("detect")$holder_dead_locally)
})

test_that("the cross-check needs the local PID file to agree, so a live lease is never stolen", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 1800
  )
  # Two containers of the same service share EPISOMER_INSTANCE, hence the same host name, while
  # having separate PID namespaces: "this PID does not exist here" must not be read as "the holder
  # is dead". Here nothing local corroborates the lease - no PID file at all - so the lease is
  # trusted until it expires.
  with_instance("instance-B", host = "machine-A", code = write_runner_lease("detect"))
  set_lease_pid("detect", dead_pid)

  expect_true(is_detect_running())
  expect_false(lease_is_free(read_runner_lease("detect"), "detect"))
})

test_that("the cross-check ignores a lease held by another host", {
  local_lease_conf(
    instance_id = "instance-A",
    instance_host = "machine-A",
    runner_ttl_detect = 1800
  )
  with_instance("instance-B", host = "machine-B", code = write_runner_lease("detect"))
  set_lease_pid("detect", dead_pid)
  # a PID file left over here by a former local runner must not be used to judge machine B's process
  write_local_pid_file("detect", dead_pid)

  expect_true(is_detect_running())
  expect_false(lease_is_free(read_runner_lease("detect"), "detect"))
})

# is_r_pid_running(), the check both cross-checks rest on ------------------------------------------

test_that("is_r_pid_running() is FALSE for a PID no process has, and for a missing one", {
  # The other half of the contract - "TRUE for a live R process" - is deliberately not asserted
  # here: is_r_pid_running() recognises a process by *name* (R, Rscript, Rterm, rsession), so it
  # answers FALSE for the R session of an IDE running under a name of its own, ark for Positron or
  # python for radian, and a test of the live case would then say more about what started it than
  # about the code. That is also why the tests above go through local_live_this_process().
  expect_false(is_r_pid_running(dead_pid))
  expect_false(is_r_pid_running(NA_integer_))
  # what read_running_task_pid_file() returns for an empty or unreadable PID file, and a vector of
  # PIDs, which is neither a process nor an answerable question
  expect_false(is_r_pid_running(integer(0)))
  expect_false(is_r_pid_running(c(Sys.getpid(), dead_pid)))
})

# fs is the exception ------------------------------------------------------------------------------

test_that("is_fs_running() answers on /ping and ignores the fs lease", {
  local_lease_conf(
    instance_id = "instance-A",
    runner_ttl_fs = 120,
    # nothing listens there, so /ping fails
    fs_url = "http://127.0.0.1",
    fs_port = 1
  )
  # a perfectly fresh lease, held by this very instance: a lease-based implementation would say
  # "running". fs deliberately keeps /ping - instantaneous, no TTL lag, and it tells whether the
  # service *answers* (plan section 6.2).
  write_runner_lease("fs")

  expect_true(i_hold_runner_lease("fs"))
  expect_false(is_fs_running())
})
