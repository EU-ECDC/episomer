# registers the data collection task (PID file + runner lease) for the current process or stops if
# no configuration has been set or if it is already running, here or on another machine
register_search_runner <- function(network = NULL) {
  stop_if_no_config(paste(
    "Cannot check running status for search without configuration setup"
  ))
  name <- "search"
  if (!is.null(network)) {
    name <- sprintf("search.%s", network)
  }
  register_runner(name)
}


# registers the fs service (PID file + runner lease) for the current process or stops if no
# configuration has been set or if it is already running, here or on another machine
register_fs_runner <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for fs without configuration setup"
  ))
  register_runner("fs")
}

# registers the detect runner (PID file + runner lease) for the current process or stops if no
# configuration has been set or if it is already running, here or on another machine
register_detect_runner <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for fs without configuration setup"
  ))
  register_runner("detect")
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
#      It is also what gives kill_task() / stop_runner_task() something to work with, so it stays
#      exactly as it was;
#   2. the runner lease, on the fs machine - "am I allowed to be this runner in the whole cluster?"
#      This is the layer that stops two machines sharing the same data directory from collecting
#      the same posts twice (R/runners-lease.R, inst/schema/multi_services_plan.md section 6.1).
#
# The local check comes first on purpose: it is cheap, it needs no network, and failing on it does
# not disturb the fs machine at all.
register_runner <- function(name) {
  stop_if_no_config(paste(
    "Cannot register ",
    name,
    " without configuration setup"
  ))
  # getting current pid
  pid <- Sys.getpid()
  # getting last running PID
  last_pid <- get_running_task_pid(name)
  if (last_pid >= 0 && last_pid != pid) {
    stop(paste("Runner ", name, "PID:", last_pid, "is already running"))
  }
  # If here means that the current process has to register as the runner service
  pid_path <- get_running_task_pid_path(name)
  write(pid, file = pid_path, append = FALSE)

  # Second layer: claiming the lease. Failing here is a normal, expected situation - it is the
  # standby machine of a two-machine deployment discovering that the other one already collects.
  if (!claim_runner_lease(name)) {
    # The PID file written a few lines above must not survive the refusal: it would make this
    # machine look like it hosts a running runner, which is exactly what is_*_running() and
    # stop_all_runner_tasks() read locally.
    remove_running_task_pid(name)
    stop(runner_lease_holder_message(name))
  }

  # Session state, not configuration: it lets renew_runner_lease(), stop_if_lease_lost() and
  # release_runner_lease() be called with no argument from anywhere in this process. It is set
  # last, so it only ever names a runner this process really holds.
  conf$runner_name <- name
  invisible(name)
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
#' @return Nothing
#' @details Registers the episomer database task or stops if no configuration has been set or if it is already running.
#' This task need the dependencies, geonames and languages steps to have been successfully ran. This can be done on the shiny app configuration page or by manually running the detect_runner_task.
#' This function will try to use the task scheduler on windows and will fall back to launching the runner as a separate process (attached to this session) on Linux.
#' @examples
#' \dontrun{
#'    #getting tasks statuses
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    register_fs_runner_task()
#' }
#' @rdname runners
#' @export
register_fs_runner_task <- function(wait_until_response=FALSE) {
  register_runner_task("fs")
  message("Waiting until the fs runner answers")
  answered <- FALSE
  while(!answered) {
     if(is_fs_running()) {
         message("fs is running")
         answered <- TRUE
     } else {
         cat(".")
         Sys.sleep(2)
     }
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
  if(is_runner_running_locally("search")) {
    message("Search loop is running, stopping it")
    stop_search_runner_task()
  }
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
stop_fs_runner_task <- function() {
  stop_runner_task("fs")
  if(is_fs_running()) {
      message("Waiting until the fs runner stops")
      answered <- FALSE
      while(!answered) {
         if(!is_fs_running()) {
             message("fs has stopped")
             answered <- TRUE
         } else {
             cat(".")
             Sys.sleep(2)
         }
     }
  }
  
}

# This task is currently Windows only
register_runner_task <- function(task_name) {
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
  rscript_args = paste("\"", conf$data_dir, "\"", sep = "")

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
      message(sprintf("Rscript '%s' '%s' > %s", script, conf$data_dir, log))
      message(sprintf("Logs written to %s", log))
      system2(
        "Rscript",
        args = c(script, conf$data_dir),
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
    # Stopping the task if running
    taskname = paste("episomer", task_name, "loop", sep = "_")
    cmd <- sprintf("schtasks /End /TN %s", shQuote(taskname, type = "cmd"))
    system(cmd, intern = FALSE)
    # Disabling the task
    cmd <- sprintf(
      "schtasks /Change /TN %s /Disable",
      shQuote(taskname, type = "cmd")
    )
    system(cmd, intern = FALSE)
  } else {
  
  }
  # If processes are still running then forcing stop by killing the processes
  if (task_name == "fs") {
    kill_task("fs", "R")
    kill_task("fs.java", "java")
    kill_task("fs_mon", "R")
  } else if (task_name == "search") {
    kill_task("search", "R")
  } else if (task_name == "detect") {
    kill_task("detect", "R")
    kill_task("detect.java", "java")
  }
}
# Forcing the stop of a PID file by killing the underlying process
kill_task <- function(pidfile, type = "R") {
  path = file.path(conf$data_dir, paste0(pidfile, ".PID"))
  if (file.exists(path)) {
    f = file(description = path, open = "r")
    last_pid = readLines(f)
    close(f)

    if (.Platform$OS.type == "windows") {
      if (type == "R")
        image = "R\\.exe|Rscript\\.exe|rsession\\.exe|Rterm\\.exe" else if (
        type == "java"
      )
        image = "java.exe" else stop("Unsupported type of process")
      if (
        length(grep(
          image,
          system(
            paste('tasklist /nh /fi "pid eq ', last_pid, '"'),
            intern = TRUE
          )
        )) >
          0
      ) {
        message(paste("Forcing process", last_pid, "to stop"))
        system(paste('taskkill /F /pid', last_pid), intern = TRUE)
      } else {
        message(paste("registered pid ", last_pid, "not running anymore"))
      }
    } else {
      if (type == "R") image = "R\\|rsession" else if (type == "java")
        image = "java" else stop("Unsupported type of process")
      if (
        system(
          paste(
            "ps -cax | grep ",
            paste0("'", image, "'"),
            " | grep ",
            last_pid
          ),
          ignore.stdout = TRUE
        ) ==
          0
      ) {
        message(paste("Forcing process", last_pid, "to stop"))
        system(paste('kill ', last_pid), intern = TRUE)
      } else {
        message(paste("registered pid ", last_pid, "not running anymore"))
      }
    }
  }
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
  last_pid <- read_running_task_pid_file(name)
  if (!is.na(last_pid) && is_r_pid_running(last_pid)) last_pid else -1
}

# The PID this machine wrote when it registered as `name`, or NA when it never did (no PID file).
#
# Kept apart from get_running_task_pid(), which folds "no PID file" and "the recorded process is
# gone" into the same -1: telling those two apart is precisely what the local cross-check of
# lease_holder_is_dead_locally() needs.
read_running_task_pid_file <- function(name) {
  pid_path <- get_running_task_pid_path(name)
  if (!file.exists(pid_path)) {
    return(NA_integer_)
  }
  tryCatch(
    as.integer(readChar(pid_path, file.info(pid_path)$size)),
    error = function(e) NA_integer_,
    warning = function(w) NA_integer_
  )
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
# lease_holder_is_dead_locally()). Behaviour is unchanged: the former "mac" branch was character for
# character the same command as the unix one, so the two are now a single branch.
is_r_pid_running <- function(pid) {
  if (length(pid) != 1 || is.na(pid)) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    length(grep(
      "R\\.exe|Rscript\\.exe|rsession\\.exe|Rterm\\.exe",
      system(paste('tasklist /nh /fi "pid eq ', pid, '"'), intern = TRUE)
    )) >
      0
  } else {
    system(
      paste("ps -cax | grep 'R\\|rsession' | grep ", pid),
      ignore.stdout = TRUE
    ) ==
      0
  }
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
