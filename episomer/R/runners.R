# registers the data collection task (PID file + runner lease) for the current process or stops if
# no configuration has been set or if it is already running, here or on another machine
register_search_runner <- function(network = NULL, wait = FALSE) {
  stop_if_no_config(paste(
    "Cannot check running status for search without configuration setup"
  ))
  name <- "search"
  if (!is.null(network)) {
    name <- sprintf("search.%s", network)
  }
  register_runner(name, wait = wait)
}


# registers the fs service (PID file + runner lease) for the current process or stops if no
# configuration has been set or if it is already running, here or on another machine
register_fs_runner <- function(wait = FALSE) {
  stop_if_no_config(paste(
    "Cannot check running status for fs without configuration setup"
  ))
  register_runner("fs", wait = wait)
}

# registers the detect runner (PID file + runner lease) for the current process or stops if no
# configuration has been set or if it is already running, here or on another machine
register_detect_runner <- function(wait = FALSE) {
  stop_if_no_config(paste(
    "Cannot check running status for fs without configuration setup"
  ))
  register_runner("detect", wait = wait)
}

#' @title Registers the fs_monitor for the current process or exits
#' @description registers the fs_monitor (by writing fs.monitor.PID file) for the current process or stops if no configuration has been set or if it is already running
#' @return Nothing
#' @details Registers the fs_monitor (by writing fs.monitor.PID file) for the current process or stops if no configuration has been set or if it is already running
#' This function has been exported so it can be properly called from the future package on the database runner, but it is not intended to be directly called by end users.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    register_fs_monitor()
#' }
#' @rdname runners
#' @export
register_fs_monitor <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for fs monitor without configuration setup"
  ))
  register_runner("fs_mon")
}

# Registers a task runner for the current process, or stops if no configuration has been set or if
# the runner is already running somewhere.
#
# Two independent layers have to agree before this process may act as `name`:
#
#   1. the PID file, on the local disk - "is there another process *on this machine* doing this?"
#      It is also what gives kill_task() / stop_runner_task() something to kill, and what tells a
#      lease left behind by a process of this machine that died from one a live machine still holds;
#   2. the runner lease, on the fs machine - "am I allowed to be this runner in the whole cluster?"
#      This is the layer that stops two machines sharing the same data directory from collecting
#      the same posts twice (R/runners-lease.R, inst/schema/multi_services_plan.md section 6.1).
#
# The local check comes first on purpose: it is cheap, it needs no network, and failing on it does
# not disturb the fs machine at all. The local *write*, on the other hand, comes last, after the
# lease has been granted - see below.
register_runner <- function(name, wait = FALSE) {
  stop_if_no_config(paste(
    "Cannot register ",
    name,
    " without configuration setup"
  ))
  # getting current pid
  pid <- Sys.getpid()
  # getting last running PID
  last_pid <- get_running_task_pid(name)
  # This one is never waited for, whatever `wait` says. A live process of *this* machine already
  # being the runner is an operator mistake - the same service started twice - not the standby
  # situation the lease is about, and the answer to it is a PID to kill rather than a delay to sit
  # through (plan issue 14).
  if (last_pid >= 0 && last_pid != pid) {
    stop(paste("Runner ", name, "PID:", last_pid, "is already running"))
  }

  # Second layer: claiming the lease. Failing here is a normal, expected situation - it is the
  # standby machine of a two-machine deployment discovering that the other one already collects.
  #
  # `wait` is what the three service loops pass (plan issue 14). A service is not a question asked
  # once: the lease it finds held is, far more often than not, the one its own predecessor left
  # behind on a `docker compose down`, which no R code was alive to release. Waiting turns "exit
  # and let somebody restart me" into "start as soon as the runner is free", and it is the whole
  # standby behaviour of issue 9 with no container churn. Everything else claims once and answers
  # immediately: the Shiny app deciding whether to spawn a task must not block its session, and the
  # fs monitor calls this on every iteration of a 2-second loop.
  claimed <- if (isTRUE(wait)) {
    wait_for_runner_lease(name)
  } else {
    claim_runner_lease(name)
  }
  if (!claimed) {
    stop(runner_lease_holder_message(name))
  }

  # If here means that the current process has to register as the runner service.
  #
  # The PID file is written *after* the claim, and that order matters. The PID file left behind by
  # the previous holder is the second of the two signals lease_holder_is_dead_locally()
  # (R/runners-lease.R) needs to recognise a lease whose holder died on this very machine: the lease
  # names a PID, and only our own PID file can prove that PID was ours to test. Writing our PID over
  # it first destroyed exactly that proof, so the fast reclaim of a crashed - or killed - runner
  # could never happen where it matters most, and starting a service again meant waiting out its
  # whole TTL (two minutes for fs, thirty for detect) with an error naming a dead instance as the
  # holder. For the same reason the refusal above leaves the file alone instead of removing it: it
  # is not ours, it describes the process that ran before us, and the next attempt needs to read it.
  #
  # The price is that two processes of this machine starting the same runner within the guard wait
  # are now separated by the lease rather than by the PID file. That is what the lease is for: the
  # loser of the confirmation read exits before collecting anything (see claim_runner_lease()), and
  # in exchange the far more common case - restarting a service whose previous process is gone -
  # stops being refused.
  write_running_task_pid(name, pid)

  # Session state, not configuration: it lets renew_runner_lease(), stop_if_lease_lost() and
  # release_runner_lease() be called with no argument from anywhere in this process. It is set
  # last, so it only ever names a runner this process really holds.
  conf$runner_name <- name
  invisible(name)
}

# Undoes register_runner(), one layer at a time. Meant to be the on.exit() of every loop.
#
# Both layers have to go, and they go for different reasons:
#
#   * the lease, so that another machine may take this runner over immediately instead of waiting
#     out the TTL. release_runner_lease() refuses if we are not the holder anymore, which is what
#     keeps a runner that was taken over from moving away the *new* holder's file;
#   * the PID file, because it describes this very process, which is ending. Leaving it behind was
#     the state observed after a search worker died: the lease said "free" while the PID file still
#     named a live process (a PSOCK node idling in its parent's cluster), so the two layers
#     answered differently about the same runner and a restart of that single runner was refused
#     locally by a runner nobody was running.
#
# The PID file is only removed when it still names this process: if another local process took the
# registration over, the file is its own and not ours to delete.
unregister_runner <- function(name = conf$runner_name) {
  if (is.null(name)) {
    return(invisible(FALSE))
  }
  released <- release_runner_lease(name)
  if (identical(read_running_task_pid_file(name), as.integer(Sys.getpid()))) {
    remove_running_task_pid(name)
  }
  invisible(released)
}

#' @title Registers the social media message collection task
#' @description registers the social media message collection task or stops if no configuration has been set or if it is already running
#' @return Nothing
#' @details Registers the message collextion task or stops if no configuration has been set or if it is already running.
#' Bluesky authentication needs to be previously set on the shiny app or by calling set_auth().
#' You can test if authentication is working on the shiny app troubleshot page or by calling (with dplyr): episomer::check_all() %>% filter(check == 'bluesky_auth')
#' This function will use the task scheduler on windows and will fall back to launching the runner as a separate process (attached to this session) on Linux.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    register_search_runner_task()
#' }
#' @rdname runners
#' @export
register_search_runner_task <- function() {
  register_runner_task("search")
}

#' @title Stops the post collection task
#' @description stops the post collection task
#' @return Nothing
#' @details Stops the post collection task if it is already running
#' This function will try also deactivate the respective scheduled task on Windows.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    stop_search_runner_task()
#' }
#' @rdname runners
#' @export
stop_search_runner_task <- function() {
  stop_runner_task("search")
}


#' @title Registers the alert detection task
#' @description registers the alert detection task or stops if no configuration has been set or if it is already running
#' @return Nothing
#' @details Registers the alert detection task or stops if no configuration has been set or if it is already running.
#' To generate alerts, this task needs the post collection to had successfully run since the last time it ran.
#' This function will use the task scheduler on windows and will fall back to launching the runner as a separate process (attached to this session) on Linux.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    register_detect_runner_task()
#' }
#' @rdname runners
#' @export
register_detect_runner_task <- function() {
  register_runner_task("detect")
}

#' @title Stops the alert detection task
#' @description stops the alert detection task
#' @return Nothing
#' @details Stops the alert detection task if it is already running
#' This function will try also deactivate the respective scheduled task on Windows.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    stop_detect_runner_task()
#' }
#' @rdname runners
#' @export
stop_detect_runner_task <- function() {
  stop_runner_task("detect")
}

#' @title Registers the episomer database task
#' @description registers the episomer database task or stops if no configuration has been set or if it is already running
#' @param wait_until_response Logical, whether to wait until the fs runner answers, default FALSE
#' @param bind_host Character, the interface the database has to bind for this run instead of the
#'   configured \code{conf$fs_url}, with or without a scheme ("localhost" and "http://localhost"
#'   are both accepted), default: NULL
#' @param bind_port Numeric, the port the database has to bind for this run instead of the
#'   configured \code{conf$fs_port}, default: NULL
#' @param timeout Numeric, how many seconds to wait for the database to answer before giving up
#'   with an error, default: Inf
#' @return Nothing
#' @details Registers the episomer database task or stops if no configuration has been set or if it is already running.
#' This task need the dependencies, geonames and languages steps to have been successfully ran. This can be done on the shiny app configuration page or by manually running the detect_runner_task.
#' This function will try to use the task scheduler on windows and will fall back to launching the runner as a separate process (attached to this session) on Linux.
#'
#' \code{bind_host} and \code{bind_port} start a database that is deliberately *not* on the
#' configured address, for a caller that needs one for itself: the language model retraining of
#' \code{\link{setup}} needs a running database, and it must not be mistaken for the real service
#' by whatever waits for that service to become healthy before starting (\code{depends_on:
#' service_healthy} in docker/compose.yml, a readiness probe in Kubernetes). They travel to the
#' database as command line arguments - jobs/fs.R, then \code{\link{fs_loop}}, then the Scala
#' \code{API.run()} - and nothing about them is written to properties.json, so a process killed
#' outright leaves no trace behind. Every call the current process makes to the API goes to that
#' address until \code{clear_fs_endpoint_override()} is called or the process ends.
#'
#' \code{"localhost"} is the value to give \code{bind_host} in a container orchestrator: an
#' address reachable from inside the pod only, needing no exposed port and no service routing, and
#' therefore reachable even while the pod is still unready.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    register_fs_runner_task()
#'
#'    # a database only this process can see, on a port of its own
#'    register_fs_runner_task(wait_until_response = TRUE, bind_host = "localhost", bind_port = 9080)
#' }
#' @rdname runners
#' @export
register_fs_runner_task <- function(
  wait_until_response = FALSE,
  bind_host = NULL,
  bind_port = NULL,
  timeout = Inf
) {
  runner_args <- character(0)
  if (!is.null(bind_host)) {
    runner_args <- c(runner_args, "bindHost", fs_bind_host_only(bind_host))
  }
  if (!is.null(bind_port)) {
    runner_args <- c(runner_args, "bindPort", as.character(bind_port))
  }
  # Set *before* the service is started, because the wait below is is_fs_running(), i.e. a GET on
  # get_scala_ping_url(): asking the configured address about a database bound somewhere else would
  # wait for ever. See set_fs_endpoint_override() in R/fs.R.
  if (length(runner_args) > 0) {
    set_fs_endpoint_override(
      if (is.null(bind_host)) conf$fs_url else bind_host,
      if (is.null(bind_port)) conf$fs_port else bind_port
    )
  }
  register_runner_task("fs", runner_args = runner_args)
  message(sprintf("Waiting until the fs runner answers on %s", get_scala_ping_url()))
  # Bounded when the caller asks for it. The wait used to be unbounded, which is survivable for an
  # operator watching a console and not at all for an unattended deployment: a database that cannot
  # bind its address - a Kubernetes service name resolving to a cluster IP the pod does not hold is
  # the case that happened, see Settings.fsBindHost - never answers, and setup() then hung on a row
  # of dots instead of failing with something to read.
  deadline <- Sys.time() + timeout
  while (TRUE) {
     if(is_fs_running()) {
         message("fs is running")
         return(invisible(NULL))
     }
     if (Sys.time() >= deadline) {
       stop(sprintf(
         "The episomer database did not answer on %s after %s seconds. Its log is in %s.",
         get_scala_ping_url(),
         timeout,
         file.path(conf$data_dir, "jobs", "fs.log")
       ))
     }
     cat(".")
     Sys.sleep(2)
  }
}

#' @title Stops all episomer runner tasks
#' @description stops all episomer runner tasks
#' @return Nothing
#' @details Stops the detect loop, the search loop and the fs loop
#' This function will try also deactivate the respective scheduled task on Windows.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    stop_all_runner_tasks()
#' }
#' @rdname runners
#' @export
stop_all_runner_tasks <- function() {
  # Deliberately reading the *local* PID files and not is_detect_running() / is_search_running():
  # stopping a runner means killing a process on this machine (stop_runner_task() -> kill_task()),
  # so the question to ask is "is there something here to kill?", never "is this runner running
  # somewhere in the cluster?". Now that is_*_running() reads the lease (plan issue 7), a runner
  # held by another machine would otherwise send us killing a process that does not exist here.
  if(is_runner_running_locally("detect")) {
    message("Detect loop is running, stopping it")
    stop_detect_runner_task()
  }
  if(search_has_local_process()) {
    message("Search loop is running, stopping it")
    stop_search_runner_task()
  }
  # Not only what was stopped just above: a service that had already died on its own - killed by
  # hand, or crashed - left its lease behind exactly the same way, and never went through
  # stop_runner_task() to have it released. "Stop all services" is where an operator expects
  # runners/ to be left clean, so what the two tests above found nothing to kill for is swept here.
  #
  # Before the fs and not at the very end, for the reason given in stop_runner_task(): a deployment
  # reaching runners/ over HTTP cannot give a lease back once the fs has stopped answering.
  release_dead_local_runner_leases()
  # fs keeps its /ping check: it is not lease-based today and stays that way (plan section 6.2),
  # and it is the only reliable way to know whether the Scala service itself has stopped.
  if(is_fs_running()) {
    message("Fs loop is running, stopping it")
    stop_fs_runner_task()
  }
}

#' @title Starts all episomer runner tasks
#' @description starts all episomer runner tasks
#' @return Nothing
#' @details Starts the detect loop, the search loop and the fs loop
#' This function will try also register the respective scheduled task on Windows.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    start_all_runner_tasks()
#' }
#' @rdname runners
#' @export
start_all_runner_tasks <- function() {
  stop_all_runner_tasks()
  register_fs_runner_task(wait_until_response = TRUE)
  register_search_runner_task()
  register_detect_runner_task()
}

#' @title Stops the episomer database task
#' @description stops the episomer database task
#' @return Nothing
#' @details Stops the episomer database task if it is already running
#' This function will try also deactivate the respective scheduled task on Windows.
#' @param timeout Numeric how many seconds to wait for the database service to stop answering
#'   before giving up, default: 60
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    stop_fs_runner_task()
#' }
#' @rdname runners
#' @export
stop_fs_runner_task <- function(timeout = 60) {
  # Asked *before* the stop: once the PID files have been used and deleted, there is no telling
  # apart a fs that refuses to die from one that was never ours to kill.
  was_local <- fs_has_local_process()
  stop_runner_task("fs")
  if (!is_fs_running()) {
    return(invisible(NULL))
  }
  # Something answers /ping and this machine has no process to kill for it: the fs belongs to
  # somebody else. On a development box that is nearly always a port forward - a WSL distribution
  # or a container publishing 8080, which Windows then serves on its own localhost - and waiting
  # for it to stop is waiting for something no episomer command here will ever stop. Say whose it
  # is and let the caller carry on: an operator reading "Waiting until the fs runner stops" under a
  # growing row of dots has no way to guess any of this (the wait used to be unbounded).
  if (!was_local) {
    message(sprintf(
      "%s still answers but no episomer process of this machine is registered for it: it is not ours to stop. If a WSL distribution or a container publishes that port, stop the service there, or give this machine another fs_port.",
      get_scala_ping_url()
    ))
    return(invisible(NULL))
  }
  message("Waiting until the fs runner stops")
  deadline <- Sys.time() + timeout
  while (Sys.time() < deadline) {
    if (!is_fs_running()) {
      message("fs has stopped")
      return(invisible(NULL))
    }
    cat(".")
    Sys.sleep(2)
  }
  # Bounded on purpose. This runs from stop_all_runner_tasks(), which the setup wizard calls before
  # every step it takes: a fs that will not stop must not be able to hold the whole wizard.
  message(sprintf(
    "fs still answers on %s after %s seconds, giving up on waiting for it",
    get_scala_ping_url(),
    timeout
  ))
  invisible(NULL)
}

# TRUE when this machine holds a PID file for any of the processes making up the database service.
#
# The three of them are asked, and not just fs.PID: the R loop, the fs monitor and the JVM the loop
# spawns (fs.java.PID, written by the Scala API itself, see scala/src/main/scala/API.scala) are
# stopped together by stop_runner_task("fs"), and any one of them being recorded here means the
# service answering /ping is this machine's.
fs_has_local_process <- function() {
  # The *record*, not the mere presence of the file. This is the one caller for which the difference
  # is the whole point: the case it exists to catch is a fs published on this port by somebody else,
  # and the commonest "somebody else" is a container of this very machine - which mounts the data
  # directory and therefore leaves its own fs.PID sitting right where we look. Believing the file
  # made every `make compose-down` leftover read as "the containerised fs is ours to stop", i.e. the
  # exact opposite of what the message below says.
  any(vapply(
    c("fs", "fs_mon"),
    function(name) recorded_pid_is_running(read_running_task_pid_record(name)),
    logical(1)
  )) ||
    recorded_pid_is_running(read_running_task_pid_record("fs.java"), "java")
}

# TRUE when this machine has a search process to kill - the parent loop, or any of its per-network
# workers.
#
# Asked instead of is_runner_running_locally("search") alone, because the two can outlive each
# other: search_loop() blocks in parLapply() for the whole life of the service and its workers are
# separate R processes, so a parent killed by hand (or crashed) leaves them collecting with no
# search.PID left to show for it. That is the state "stop all services" has to be able to get out
# of, and reading the parent only made it silently do nothing.
search_has_local_process <- function() {
  any(vapply(
    c("search", local_search_worker_runner_names()),
    is_runner_running_locally,
    logical(1)
  ))
}

# Path of the Rscript executable to hand to taskscheduleR, in a form cmd.exe can parse.
#
# taskscheduler_create() builds the command line of the scheduled task with
#   sprintf("cmd /c %s %s %s %s >> %s 2>&1", Rexe, ...)
# i.e. it quotes the script and the log file but *not* Rexe, and its default value is
# file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"). When R is installed under
# "C:/Program Files/R/...", which is the default of the Windows installer, R_HOME contains a space
# and cmd.exe stops reading the program name at it: the task then writes nothing but
# "'C:/Program' is not recognized as an internal or external command" into jobs/<task>.log.
# R_HOME is not always in that form - a session started from Rscript.exe on the command line
# reports the 8.3 short name, which has no space - so the failure only shows up for some ways of
# starting the session, RStudio among them.
#
# The short name is asked for first because it removes the space instead of quoting around it, and
# so does not depend on how the nested quotes of "schtasks /TR" are read. 8.3 name generation can
# be disabled on a volume, in which case shortPathName() gives the long name back unchanged and
# quoting is the only option left.
rscript_exe_for_schtasks <- function() {
  exe <- file.path(R.home("bin"), "Rscript.exe")
  short <- utils::shortPathName(exe)
  if (grepl(" ", short, fixed = TRUE)) shQuote(short, type = "cmd") else short
}

# This task is currently Windows only
#
# runner_args are extra command line arguments appended after the data directory, read by the
# jobs/<task>.R script the runner is started with. Only the fs uses them today, to be told which
# interface and port to bind (see register_fs_runner_task()).
register_runner_task <- function(task_name, runner_args = character(0)) {
  # Making sure configuration has been set
  stop_if_no_config(paste(
    "Cannot register scheduled task without configuration setup"
  ))

  # Getting the embedded script name to be called in order to make the script run
  script_name <- paste(task_name, "R", sep = ".")
  run_attached <- FALSE
  # Getting source script
  script_base <- system.file(
    "extdata",
    script_name,
    package = get_package_name()
  )
  # Getting destination folder which is going
  script_folder <- file.path(conf$data_dir, "jobs")
  if (!file.exists(script_folder)) {
    dir.create(script_folder, showWarnings = FALSE)
  }
  script <- paste(script_folder, script_name, sep = "/")
  # Copying the script
  file.copy(from = script_base, to = script, overwrite = TRUE)
  taskname = paste("episomer", task_name, "loop", sep = "_")
  rscript_args = paste(
    c(paste("\"", conf$data_dir, "\"", sep = ""), runner_args),
    collapse = " "
  )

  if (.Platform$OS.type == "windows") {
    # testing if taskscheduleR can be loaded
    if (requireNamespace("taskscheduleR", quietly = TRUE)) {
      # Registering the task as a schedule task using taskscheduleR
      # Note that the date format is being guess using %DATE% shell variable
      # This value can be overridden by conf$fore_date_format setting
      message(sprintf("Registering and running %s as a Windows scheduler task", task_name))
      message(sprintf("Logs written to %s", gsub(".R", ".log", script)))
      taskscheduleR::taskscheduler_create(
        taskname,
        rscript = script,
        rscript_args = rscript_args,
        Rexe = rscript_exe_for_schtasks(),
        schedule = "HOUR",
        startdate = if (conf$force_date_format == "")
          tail(strsplit(shell("echo %DATE%", intern = TRUE), " ")[[1]], 1) else
          strftime(Sys.time(), conf$force_date_format),
        schtasks_extra = "/F"
      )
      taskscheduleR::taskscheduler_runnow(taskname)
    } else {
      message(
        "Please install taskscheduleR package in order to permanently activate the task. This will run the task attached to this process and will end when you close this session."
      )
      run_attached <- TRUE
    }
  } else {
    message(
      "Permanent Task scheduling only work in Windows. The task will not automatically trigger on server startup"
    )
    run_attached <- TRUE
  }

  if (run_attached) {
      # running the requested task as a separate thread. This is useful for testing episomer from the shiny app but it does not permanently schedule the task.
      # calculating alerts per topic
      message(sprintf("Running %s as a service attached to this session (for permanent scheduling add the following code to startup tasks)", task_name))
      cm <- paste(script, rscript_args)
      script_log <- paste(task_name, "log", sep = ".")
      log <- file.path(script_folder, script_log)
      message(sprintf("Rscript '%s' %s > %s", script, rscript_args, log))
      message(sprintf("Logs written to %s", log))
      system2(
        "Rscript",
        args = c(script, conf$data_dir, runner_args),
        stdout = log,
        stderr = log,
        wait = FALSE
      )
  }
}

#Stopping a running episomer task
stop_runner_task <- function(task_name) {
  stop_if_no_config(paste(
    "Cannot stop scheduled task without configuration setup"
  ))
  message(paste("Stopping", task_name))
  # Filtering by OS, currently only Windows is supported
  if (.Platform$OS.type == "windows") {
    taskname = paste("episomer", task_name, "loop", sep = "_")
    # Asked before /End and /Change: a runner started attached to a session - which is what happens
    # whenever taskscheduleR is not installed, see register_runner_task() - has no scheduled task at
    # all, and schtasks then writes "ERROR: The system cannot find the file specified." and
    # "ERROR: The specified task name ... does not exist in the system." on the console. Nothing is
    # wrong there, the process is still killed below, but those two lines read exactly like a
    # failure of the stop and are the first thing the setup wizard shows on Windows.
    if (windows_scheduled_task_exists(taskname)) {
      # Stopping the task if running
      cmd <- sprintf("schtasks /End /TN %s", shQuote(taskname, type = "cmd"))
      system(cmd, intern = FALSE)
      # Disabling the task
      cmd <- sprintf(
        "schtasks /Change /TN %s /Disable",
        shQuote(taskname, type = "cmd")
      )
      system(cmd, intern = FALSE)
    } else {
      message(sprintf(
        "No Windows scheduled task named %s, %s was started attached to a session",
        taskname,
        task_name
      ))
    }
  } else {
  
  }
  # If processes are still running then forcing stop by killing the processes.
  #
  # A killed runner releases nothing by itself: R runs no exit handler on a signal, so the
  # on.exit(unregister_runner()) of the loop never runs and the lease stays under runners/ until it
  # expires. Giving it back here is what lets the service be started again straight away - and, in a
  # multi-machine deployment, what lets the standby take the runner over without waiting out the TTL
  # (inst/schema/multi_services_plan.md section 3.4).
  if (task_name == "fs") {
    kill_task("fs", "R")
    kill_task("fs_mon", "R")
    # Before the Scala service, deliberately. Where the leases are reached over HTTP - conf$fs_remote
    # set, which is what setup_config() does by default - runners/ lives behind the very API this
    # java process serves, so the lease of the fs runner can only be handed back while it still
    # answers. Killing the monitor first also spares us the health_check() alert it would send while
    # watching the fs it is about to lose.
    release_stopped_runner_leases(c("fs", "fs_mon"))
    kill_task("fs.java", "java")
  } else if (task_name == "search") {
    # The per-network workers before the parent, and not instead of it: search_loop() does no
    # collection itself, it claims the "search" runner and then blocks in parallel::parLapply()
    # while one PSOCK worker per social media runs search_loop_worker() (R/search.R). Those workers
    # are R processes of their own, they are not children of the parent in any way the OS will clean
    # up for us, and killing the parent alone leaves them collecting for ever: the parent dies on
    # SIGTERM without running its on.exit(parallel::stopCluster(cl)), and a worker only notices the
    # closed master socket when it next asks the cluster for work - which it never does, its loop
    # is infinite. That is exactly what "search.PID is gone but jobs/search.<network>.log keeps
    # growing" was.
    #
    # Nothing of this is needed on the docker side, which is why `make compose-down` never showed
    # the problem: the container's whole PID namespace goes with it, workers included.
    workers <- local_search_worker_runner_names()
    for (worker in workers) {
      kill_task(worker, "R")
    }
    kill_task("search", "R")
    # The workers are waited for too, not just the parent: each of them holds the lease that really
    # matters (exclusivity is per network, plan section 2), and each of them died on a signal
    # without releasing it. Sweeping while they are still in the process table would release none.
    release_stopped_runner_leases(c("search", workers))
  } else if (task_name == "detect") {
    kill_task("detect", "R")
    kill_task("detect.java", "java")
    release_stopped_runner_leases("detect")
  }
}

# The runner names of the per-network search workers this machine has a PID file for.
#
# Read from the disk rather than built from active_social_media(): a network deactivated - or an
# instance URL changed - while the search loop was running still has a worker collecting for it, and
# that worker is precisely the one a stop must not miss. The names carry dots of their own
# ("search.mastodon.piaille.fr"), so the parent "search.PID" is the only thing to exclude.
#
# A file left behind by a process that has already gone is harmless here: kill_task() refuses to
# signal a PID it cannot prove alive, and is_runner_running_locally() answers FALSE for it at once.
local_search_worker_runner_names <- function() {
  files <- list.files(
    conf$data_dir,
    pattern = "^search\\..+\\.PID$",
    full.names = FALSE
  )
  names <- sub("\\.PID$", "", files)
  # Every name here is signalled as an R process, so the ".java" convention of the Scala side
  # (fs.java.PID, detect.java.PID) is kept out. The search service spawns no JVM today; this is so
  # that the day it does, its PID file is not quietly handed to a check that will always say no.
  names[!endsWith(names, ".java")]
}

# Gives back the leases of the runners this machine has just stopped.
#
# The decision to release is release_dead_local_runner_lease()'s (R/runners-lease.R) and needs the
# holder PID to be gone from this machine. kill(1) returns as soon as the signal has been delivered,
# not when the process has died, so the PIDs of the runners we killed are waited for first: sweeping
# too early would release nothing and put us back to waiting out the TTL. The wait is bounded, and
# a runner that outlives it simply keeps its lease - which is the safe outcome, it may still be
# writing.
#
# The sweep itself is not restricted to `names`: it also picks up the leases of the per-network
# search workers, of fs_mon, and whatever an earlier crash left behind. Each of them is released
# under the same proof, so there is nothing to gain from being narrower here.
release_stopped_runner_leases <- function(names, timeout = 10) {
  deadline <- Sys.time() + timeout
  while (length(names) > 0 && Sys.time() < deadline) {
    names <- names[vapply(names, is_runner_running_locally, logical(1))]
    if (length(names) > 0) Sys.sleep(1)
  }
  release_dead_local_runner_leases()
}

# TRUE when a Windows scheduled task of that name is registered.
#
# /Query answers with a non zero exit status for an unknown task, which is all we need: its output
# is dropped so that the "cannot find the file specified" of a runner that has no scheduled task
# never reaches the console (see stop_runner_task()).
windows_scheduled_task_exists <- function(taskname) {
  cmd <- sprintf("schtasks /Query /TN %s", shQuote(taskname, type = "cmd"))
  status <- suppressWarnings(tryCatch(
    system(cmd, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE),
    error = function(e) 1L
  ))
  identical(as.integer(status), 0L)
}

# Forcing the stop of a PID file by killing the underlying process.
#
# The record has to be believed before a signal is sent, not just read: kill(1) takes a number, and
# a PID file written in another PID namespace holds a number that belongs to somebody else here. A
# leftover container fs.PID naming PID 9 used to be enough for a local stop to signal whatever
# process of this host had been given PID 9 - recorded_pid_is_running() is what refuses that, on
# exactly the same grounds as the local check of register_runner().
kill_task <- function(pidfile, type = "R") {
  path <- file.path(conf$data_dir, paste0(pidfile, ".PID"))
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }
  rec <- read_running_task_pid_record(pidfile)
  if (is.na(rec$pid)) {
    message(paste("PID file", path, "does not hold a usable PID"))
    return(invisible(FALSE))
  }
  if (!recorded_pid_is_running(rec, type)) {
    message(paste("registered pid ", rec$pid, "not running anymore"))
    return(invisible(FALSE))
  }
  message(paste("Forcing process", rec$pid, "to stop"))
  if (.Platform$OS.type == "windows") {
    system(paste("taskkill /F /pid", rec$pid), intern = TRUE)
  } else {
    system(paste("kill ", rec$pid), intern = TRUE)
  }
  invisible(TRUE)
}


#' @title Missing search jobs if any
#' @description check if search jobs for each activated social media are running and return the list names of social media with missing jobs
#' @return char list of social media with no search job running
#' @details returns a char vector with the name of missing social media.
#' The answer is cluster-wide: it is read from the runner leases (`<data_dir>/runners/`), so a
#' social media collected by another machine sharing the same database is not reported as missing.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    missing_search_jobs()
#'} 
#' @rdname runners
#' @export
missing_search_jobs <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for search without configuration setup"
  ))
  active <- active_social_media()
  # One lease per social media, exactly as there used to be one PID file per social media: the
  # granularity of exclusivity is search.<network>, which is what lets machine B collect bluesky
  # while machine C collects mastodon (plan section 2). Reading the leases rather than the local PID
  # files is what makes this list right in that setup - a network collected elsewhere is not missing.
  not_running <- active[sapply(
    active,
    function(sm) !is_runner_running(sprintf("search.%s", sm))
  )]
  not_running
}

#' @title Check whether the post collection task is running
#' @description gets the post collection execution status
#' @return logical Whether the post collection is running
#' @details returns a logical value being TRUE if the post collection is running, on this machine
#' *or on any other machine* sharing the same episomer database: the status is read from the runner
#' leases and not from the local PID files. A service that stopped without releasing its lease is
#' reported as running until its lease expires (`runner_ttl_search` seconds), unless it crashed on
#' this very machine, which is detected at once.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    is_search_running()
#' }
#' @rdname runners
#' @export
is_search_running <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for search without configuration setup"
  ))
  # The parent "search" runner is claimed by search_loop(), which then blocks in
  # parallel::parLapply() for the whole life of the service and has no iteration left to renew its
  # lease on: on a perfectly healthy machine that lease goes stale after runner_ttl_search (plan
  # section 9.5, and the issue 5 notes). Reading it alone would therefore report "stopped" five
  # minutes after every start, so the per-network workers - which do renew, every
  # runner_heartbeat_span - are asked as well. The service is running if any of those entries is
  # fresh, which also covers the startup window where the workers have not registered yet and only
  # the just-claimed parent lease exists.
  runners <- c("search", sprintf("search.%s", active_social_media()))
  any(vapply(runners, is_runner_running, logical(1)))
}

#' @title Check whether the database is running
#' @description gets the database runner execution status
#' @return logical Whether the database is running
#' @details returns a logical value being TRUE if the database runner is actually running.
#' Unlike the other services this one is not answered by the runner lease but by an HTTP call to
#' the database `/ping` endpoint: it is instantaneous, it needs no TTL, and it tells whether the
#' service *answers* rather than whether some process holds its lease. The `fs` runner does take a
#' lease, for a different purpose - stopping a second database from opening the same storage.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    is_fs_running()
#' }
#' @rdname runners
#' @export
is_fs_running <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for fs without configuration setup"
  ))
  tryCatch(
    httr::GET(url = get_scala_ping_url(), httr::timeout(1.0))$status_code ==
      200,
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
}

# stop with an error if the embedded database service is not running.
stop_if_no_fs <- function(msg = NULL) {
  if (!is_fs_running()) {
    if (!is.null(msg)) stop(msg) else
      stop(
        "This function needs the episomer database to be running.\nYou can activate it for this session with the command register_fs_runner_task() or on a separate session with fs_loop().\nBe aware that for data to be displayed, the collect task must have ran previously"
      )
  }
}


#' @title Check whether the alert detection task is running
#' @description gets the alert detection runner execution status
#' @return logical Whether the alert detection task is running
#' @details returns a logical value being TRUE if the alert detection task is actually running, on
#' this machine or on any other machine sharing the same episomer database - see
#' \code{is_search_running} for what the runner leases can and cannot tell.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    is_detect_running()
#' }
#' @rdname runners
#' @export
is_detect_running <- function() {
  stop_if_no_config(paste(
    "Cannot check detect batch status for search without configuration setup"
  ))
  # detect has a single runner name and renews it on every iteration of detect_loop(), so its lease
  # answers directly. Its TTL is long on purpose (30 minutes): one task can run that long without
  # writing anything, and looking dead while working would be far worse than answering late.
  is_runner_running("detect")
}

remove_running_task_pid <- function(name) {
  pid_path <- get_running_task_pid_path(name)
  if(file.exists(pid_path))
    unlink(pid_path)
}

# TRUE when a process of this machine is registered as `name` and still alive.
#
# This is the *local* answer, the one the PID file gives. Keep it apart from is_search_running() /
# is_detect_running(), which answer the cluster-wide question ("is anybody, anywhere, running this
# runner?") and are about to be backed by the lease. Everything that kills a process locally must
# use this one.
is_runner_running_locally <- function(name) {
  get_running_task_pid(name) >= 0
}

# Get runner active PID this function will return -1 if no process was detected
get_running_task_pid <- function(name) {
  rec <- read_running_task_pid_record(name)
  if (recorded_pid_is_running(rec)) rec$pid else -1
}

# Contents of a runner PID file: one field per line, PID first.
#
# The PID *has* to stay alone on the first line, because we are not the only writer of these files:
# the Scala side writes fs.java.PID and detect.java.PID itself, as a bare number
# (ProcessID.writePID, scala/src/main/scala/ProcessId.scala). The two fields below are ours, and
# they are what makes a PID file describe a *process* instead of just a number:
#
#   * `host` is conf$instance_host, the same value the lease records. A PID only means something in
#     the namespace it was handed out in, and the fs container writes its PID file into a data
#     directory the host reads too: $EPI_HOME/fs.PID naming PID 9 describes the container's main
#     Rscript, while on the host that number belongs to whatever started ninth at boot. This field
#     is what tells the two apart, and without it every `make compose-down` left behind a file that
#     refused the next local start with "Runner fs PID: 9 is already running";
#   * `start_time` closes the same hole *within* one host, where the host name cannot help: a
#     container restarted under the same EPISOMER_INSTANCE hands out its PIDs from the same small
#     range, so the new run's PID 9 is a different process wearing the same number. This is the
#     signal inst/schema/multi_services_plan.md section 9 item 6 asks for.
#
# Both are optional on read: a file holding nothing but a number is either the Scala side's or one
# written by an older version, and it falls back to the previous behaviour - believe the number.
write_running_task_pid <- function(name, pid = Sys.getpid()) {
  writeLines(
    c(
      as.character(pid),
      pid_field_or_blank(conf$instance_host),
      pid_field_or_blank(pid_start_time(pid))
    ),
    con = get_running_task_pid_path(name)
  )
  invisible(pid)
}

# An unknown field is written as an empty line rather than as "NA", which is a plausible host name.
pid_field_or_blank <- function(value) {
  if (length(value) != 1 || is.na(value)) "" else as.character(value)
}

# The three fields of the PID file of `name`, each NA when the file does not carry it.
read_running_task_pid_record <- function(name) {
  path <- get_running_task_pid_path(name)
  record <- function(pid = NA_integer_, host = NA_character_, start = NA_character_) {
    list(pid = pid, host = host, start_time = start)
  }
  if (!file.exists(path)) {
    return(record())
  }
  lines <- trimws(tryCatch(
    readLines(path, warn = FALSE),
    error = function(e) character(0),
    warning = function(w) character(0)
  ))
  field <- function(i) {
    if (length(lines) >= i && nzchar(lines[[i]])) lines[[i]] else NA_character_
  }
  pid <- suppressWarnings(as.integer(field(1)))
  record(
    pid = if (length(pid) == 1) pid else NA_integer_,
    host = field(2),
    start = field(3)
  )
}

# The PID this machine wrote when it registered as `name`, or NA when it never did (no PID file).
#
# Kept apart from get_running_task_pid(), which folds "no PID file" and "the recorded process is
# gone" into the same -1: telling those two apart is precisely what the local cross-check of
# lease_holder_is_dead_locally() needs.
read_running_task_pid_file <- function(name) {
  read_running_task_pid_record(name)$pid
}

# TRUE when the process a PID file record describes is still running here, as a process of `type`.
#
# This is the question every caller of the PID layer actually asks, and it is not "is that number
# in the process table": a PID file is only evidence about a process if it was written in the same
# PID namespace, by the run we are still in. The two extra fields of write_running_task_pid() are
# checked before the process table is even looked at, and each one fails *open* when absent, so a
# bare number - the Scala side's, or an older version's - behaves exactly as it used to.
recorded_pid_is_running <- function(rec, type = "R") {
  if (length(rec$pid) != 1 || is.na(rec$pid)) {
    return(FALSE)
  }
  # Written somewhere else: another machine, or a container of this one. Its numbering says nothing
  # about our process table, so this record is stale by construction - not "alive", not "unknown".
  if (
    !is.na(rec$host) &&
      !identical(rec$host, as.character(conf$instance_host))
  ) {
    return(FALSE)
  }
  # Same number, different process: the PID has been recycled since the record was written. Only a
  # start time we can actually read now disproves the record; when the OS will not tell us (see
  # pid_start_time()), the signal is dropped rather than guessed at.
  if (!is.na(rec$start_time)) {
    started <- pid_start_time(rec$pid)
    if (!is.na(started) && !identical(started, rec$start_time)) {
      return(FALSE)
    }
  }
  # Through the named probes rather than pid_is_running_of_type() directly, so that the seam the
  # tests already mock stays the seam every path goes through (see helper-conf.R: liveness cannot be
  # established for real in a test, an R session hosted under a name of its own - ark for Positron -
  # not being recognised by name).
  if (identical(type, "java")) {
    is_java_pid_running(rec$pid)
  } else {
    is_r_pid_running(rec$pid)
  }
}

# Start time of `pid`, as an opaque string only ever compared with another value of this function
# taken on this machine. NA when the OS will not say, in which case the caller drops the signal.
pid_start_time <- function(pid) {
  if (length(pid) != 1 || is.na(pid)) {
    return(NA_character_)
  }
  if (.Platform$OS.type == "windows") {
    # There is no cheap, non deprecated way to ask for it: wmic is gone from recent Windows and
    # shelling out to PowerShell costs about a second, on a function the fs monitor calls every two
    # seconds. Windows is not a container host in this deployment, so the `host` field already
    # covers what this signal is here for, and returning NA keeps the previous behaviour.
    return(NA_character_)
  }
  # Linux: field 22 of /proc/<pid>/stat, the start time in clock ticks since boot. Read from the
  # file rather than through ps, so it costs no process. Field 2 is the command name, which may
  # itself contain spaces and parentheses, so the fields are counted from the last ")" - after
  # which the first one is field 3, and field 22 is therefore the 20th.
  stat_path <- file.path("/proc", pid, "stat")
  if (file.exists(stat_path)) {
    line <- tryCatch(
      readLines(stat_path, warn = FALSE)[[1]],
      error = function(e) NA_character_,
      warning = function(w) NA_character_
    )
    if (is.na(line)) {
      return(NA_character_)
    }
    fields <- strsplit(trimws(sub("^.*\\)", "", line)), "[[:space:]]+")[[1]]
    return(if (length(fields) >= 20) fields[[20]] else NA_character_)
  }
  # macOS and the other unices: ps knows it, to the second. That resolution is plenty - what is
  # being told apart is two processes, and a PID is not recycled within the second it was freed.
  out <- suppressWarnings(tryCatch(
    system(paste("ps -o lstart= -p", pid), intern = TRUE, ignore.stderr = TRUE),
    error = function(e) character(0)
  ))
  out <- trimws(out)
  out <- out[nzchar(out)]
  if (length(out) == 1) out else NA_character_
}

# Path of the PID file of a runner, on the *local* disk of this machine.
#
# Named rather than pasted in five places: the lease code needs to read it too (plan issue 7, the
# local cross-check of is_runner_running()), and it must read exactly the same file the PID layer
# writes.
get_running_task_pid_path <- function(name) {
  paste(conf$data_dir, "/", name, ".PID", sep = "")
}

# TRUE when `pid` is still a running R process *of this machine*.
#
# This check makes a system dependent call to get the processes launched by R* or rsession binaries:
# if `pid` is found in that list then we assume it is running.
#
# Extracted from get_running_task_pid() so that the very same test can be applied to a PID that does
# not come from a PID file - namely the one recorded inside a runner lease, which is what lets a
# crashed runner be detected on its own host without waiting for the lease TTL (R/runners-lease.R,
# lease_holder_is_dead_locally()).
is_r_pid_running <- function(pid) {
  pid_is_running_of_type(pid, "R")
}

# TRUE when `pid` is still a running JVM *of this machine* - the database service the fs loop spawns,
# recorded in fs.java.PID / detect.java.PID by the Scala side itself.
#
# Named next to is_r_pid_running() for the same reason: it is the single seam through which every
# liveness question about a java runner passes, and therefore the one a test can mock.
is_java_pid_running <- function(pid) {
  pid_is_running_of_type(pid, "java")
}

# TRUE when `pid` is a running process of this machine whose command *name* matches `type`.
#
# Exact on the PID, which the pipeline this replaced was not. It used to be
#   ps -cax | grep 'R\|rsession' | grep <pid>
# i.e. "some line mentioning R also contains these digits somewhere", so any R process whose ps line
# happened to contain the digits of `pid` answered yes on its behalf: PID 9 reads as alive as soon
# as an unrelated R session holds PID 2990. Together with a PID file carrying no host, that is what
# made a leftover container fs.PID refuse the start of a local fs with
# "Runner fs PID: 9 is already running", and what could have had kill_task() signal a host process
# that had nothing to do with episomer.
pid_is_running_of_type <- function(pid, type = "R") {
  if (length(pid) != 1 || is.na(pid)) {
    return(FALSE)
  }
  image <- runner_process_image(type)
  if (.Platform$OS.type == "windows") {
    # tasklist filters on the PID itself, so only the image name is left to match here.
    out <- suppressWarnings(tryCatch(
      system(sprintf('tasklist /nh /fi "pid eq %s"', pid), intern = TRUE),
      error = function(e) character(0)
    ))
    any(grepl(image, out))
  } else {
    any(grepl(image, pid_command_name(pid)))
  }
}

# The command names a runner process of that type is allowed to have. The former "mac" branch was
# character for character the same as the unix one, so the two are a single branch.
#
# The unix patterns are matched against `ps -o comm=`, which is a bare name on Linux ("R", "java")
# but the *full path* of the executable on macOS ("/.../Resources/bin/exec/R"). The `(^|/)...$`
# frame is what accepts both while still matching a whole name: a plain "R|rsession" matched
# anywhere in the string, so any command whose name merely *contains* an upper case R answered yes
# on a runner's behalf - the same class of accident on the process name that
# pid_is_running_of_type() removed on the process number.
#
# `ark` is the binary Positron hosts its R console in. A loop started by hand from a Positron
# console is an ordinary R runner in every respect, but ps calls it "ark", so leaving it out made
# recorded_pid_is_running() answer FALSE for a process that was very much alive: the lease of a
# running search_loop() read as "crashed here" in the status table, and the setup() menu offered to
# start a second one. The containers are unaffected either way - devscripts/run.sh starts them with
# Rscript, which execs the R binary and reports "R".
#
# radian is deliberately *not* here, although it is an R console too: it hosts R inside python, so
# accepting it would mean accepting "python", i.e. calling every python process of the machine a
# live R runner. That is the one direction this list must never widen in.
runner_process_image <- function(type) {
  if (.Platform$OS.type == "windows") {
    # tasklist filters on the PID itself and prints a whole row, of which the image name is only
    # one field: these patterns have to stay unanchored.
    switch(
      type,
      R = "R\\.exe|Rscript\\.exe|rsession\\.exe|Rterm\\.exe|ark\\.exe",
      java = "java\\.exe",
      stop("Unsupported type of process")
    )
  } else {
    switch(
      type,
      R = "(^|/)(R|Rscript|Rterm|rsession|ark)$",
      java = "(^|/)java$",
      stop("Unsupported type of process")
    )
  }
}

# Command name of `pid` on a unix, character(0) when there is no such process. Rscript execs the R
# binary, so a runner started by `Rscript run.R` reports "R" here.
pid_command_name <- function(pid) {
  out <- suppressWarnings(tryCatch(
    system(paste("ps -p", pid, "-o comm="), intern = TRUE, ignore.stderr = TRUE),
    error = function(e) character(0)
  ))
  out <- trimws(out)
  out[nzchar(out)]
}

loop_run_issues <- function(loop_name) {
  # A service whose lease is alive on another machine sharing this database *is* running, and no
  # amount of fixing the local tasks below would make a second one start here (plan issue 8). Note
  # that the runner name of each of the three loops is the loop name itself, and that a lease held
  # on this very host is not a case for this message: register_*_runner_task() is a local action,
  # and the local PID check refuses a duplicate with a message naming the process to stop.
  if (loop_name %in% c("fs", "search", "detect")) {
    running_elsewhere <- runner_running_elsewhere_message(loop_name)
    if (running_elsewhere != "") {
      return(running_elsewhere)
    }
  }
  t <- get_tasks()
  if (loop_name == "fs") {
    if (
      !is.na(t$dependencies$status) &&
        t$dependencies$status %in% c("success", "running")
    ) {
      ""
    } else if (!is_detect_running()) {
      "Please activate 'Requirement & alerts' task. To run the embedded database, the dependencies task needs to be running or have been completed successfully"
    } else {
      "Please manually run the 'Run dependencies' step. To run the embedded database, the dependencies task needs to be running or have been completed successfully"
    }
  } else if (loop_name == "search") {
    auth_ok <- check_sm_auth()
    if(!auth_ok) {
      "episomer could not create an authentication token, please make sure you have entered valid credentials in the configuration page and click on 'Save settings'"
    } else if (is.na(t$dependencies$status) || t$dependencies$status != "success") {
      "Please make sure 'Requirement & alerts' task has been activated and successfully completed the tasks for 'Dependencies'"
    } else if (is.na(t$geonames$status) || t$geonames$status != "success") {
      "Please make sure 'Requirement & alerts' task has been activated and successfully completed the tasks for 'Geonames'"
    } else if (is.na(t$languages$status) || t$languages$status != "success") {
      "Please make sure 'Requirement & alerts' task has been activated and successfully completed the tasks for 'Languages'"
    }
      else ""
  } else if (loop_name == "detect") {
    ""
  } else stop(paste("Unlnown loop", loop_name))
}
