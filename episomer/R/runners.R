# registers the data collection task (by writing search.PID file) for the current process or stops if no configuration has been set or if it is already running
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


# registers the fs_monitor (by writing fs.PID file) for the current process or stops if no configuration has been set or if it is already running
register_fs_runner <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for fs without configuration setup"
  ))
  register_runner("fs")
}

# registers the detect_runner (by writing detect.PID file) for the current process or stops if no configuration has been set or if it is already running
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

# Registers the a task runner (by writing "name".PID file) for the current process
# or stops if no configuration has been set or if it is already running
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
  pid_path <- paste(conf$data_dir, "/", name, ".PID", sep = "")
  write(pid, file = pid_path, append = FALSE)
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
  if(is_detect_running()) {
    message("Detect loop is running, stopping it")
    stop_detect_runner_task()
  }
  if(is_search_running()) {
    message("Search loop is running, stopping it")
    stop_search_runner_task()
  }
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
  register_fs_runner_task(wait = TRUE)
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
      message(sprintf("Logs written to %s", gsub(".R", ".log", rscript)))
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
      warning(
        "Please install taskscheduleR package in order to permanently activate the task. This will run the task attached to this process and will end when you close this session."
      )
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
#' @details returns a char vector with the name of missing social media
#' @examples
#' \dontrun{
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    missing_search_jobs()
#' 
#' @rdname runners
#' @export
missing_search_jobs <- function() {
  stop_if_no_config(paste(
    "Cannot check running status for search without configuration setup"
  ))
  active <- active_social_media()
  not_running <- active[sapply(
    active,
    function(sm) get_running_task_pid(sprintf("search.%s", sm)) < 0
  )]
  not_running
}

#' @title Check whether the post collection task is running
#' @description gets the post collection execution status
#' @return logical Whether the post collection is running
#' @details returns a logical value being TRUE if the post collection is running
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
  get_running_task_pid("search") >= 0
}

#' @title Check whether the database is running
#' @description gets the database runner execution status
#' @return logical Whether the database is running
#' @details returns a logical value being TRUE if the database runner is actually running
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
#' @details returns a logical value being TRUE if the alert detection task is actually running
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
  get_running_task_pid("detect") >= 0
}

# Get runner active PID this function will return -1 if no process was detected
get_running_task_pid <- function(name) {
  # Getting PID file name based on task name which stored the PID of last time this task was running
  pid_path <- paste(conf$data_dir, "/", name, ".PID", sep = "")
  if (file.exists(pid_path)) {
    # Getting last runner pid from PID file if exists
    last_pid <- as.integer(readChar(pid_path, file.info(pid_path)$size))
    # Checking if last_pid is still running
    # this check makes a system dependent call to get running processes launch by R* or rsession binaries
    # if the last_pid is found on the list of processes returned then we assume is running
    pid_running <- (if (.Platform$OS.type == "windows") {
      length(grep(
        "R\\.exe|Rscript\\.exe|rsession\\.exe|Rterm\\.exe",
        system(paste('tasklist /nh /fi "pid eq ', last_pid, '"'), intern = TRUE)
      )) >
        0
    } else if (.Platform$OS.type == "mac")
      system(
        paste("ps -cax | grep 'R\\|rsession' | grep ", last_pid),
        ignore.stdout = TRUE
      ) ==
        0 else
      system(
        paste("ps -cax | grep 'R\\|rsession' | grep ", last_pid),
        ignore.stdout = TRUE
      ) ==
        0)
    if (pid_running) last_pid else -1
  } else -1
}
