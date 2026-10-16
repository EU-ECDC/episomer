# check if Java is installed
check_java_present <- function() {
  if (is_java_present()) {
    TRUE
  } else {
    warning(paste(
      "episomer could not detect Java. Please install it and make sure the Java binary is on the path or that JAVA_HOME is set",
      "episomer requires 64-bit Java between ",
      java_min_version(),
      " and ",
      java_max_version()
    ))
  }
}

# check if Java is installed
is_java_present <- function() {
  programs <- "java"
  # If JAVA_HOME is not set, checks if the system can found Java on the path
  if (Sys.getenv("JAVA_HOME") == "" || is.null(Sys.getenv("JAVA_HOME"))) {
    length(grep(programs, Sys.which(programs))) == length(programs)
  } else {
    #checking if the Java binary can be found from JAVA_HOME
    if (.Platform$OS.type == "windows") {
      file.exists(file.path(Sys.getenv("JAVA_HOME"), "bin", "java.exe"))
    } else {
      file.exists(file.path(Sys.getenv("JAVA_HOME"), "bin", "java"))
    }
  }
}

# get Java version vector
get_java_version_vector <- function() {
  if (.Platform$OS.type == "windows") {
    tmp <- tempfile()
    shell(paste(java_path(), "-version >", tmp, "2>&1"))
    con <- file(tmp)
    jver <- readLines(con)
    close(con)
    file.remove(tmp)
    jver
  } else {
    con <- pipe(paste(java_path(), "-version 2>&1"))
    jver <- readLines(con)
    close(con)
    jver
  }
}

# check if Java is 64 bits
check_java_64 <- function() {
  if (any(grepl("64-Bit", get_java_version_vector()))) {
    TRUE
  } else {
    warning(paste(
      "Your current Java version does not appear to be 64-bit. episomer requires 64-bit Java between ",
      java_min_version(),
      " and ",
      java_max_version()
    ))
    FALSE
  }
}

# get Java version
get_java_version <- function() {
  jver <- get_java_version_vector()
  index <- grep("version", jver)
  if (length(index) == 0) {
    NULL
  } else {
    j_version_parts <- strsplit(
      gsub("[^0-9]", ".", jver[[index[[1]]]]),
      "\\.+"
    )[[1]]
    j_version_parts <- j_version_parts[grepl("[0-9]", j_version_parts)]
    if (length(j_version_parts) > 1 && as.integer(j_version_parts[[1]]) == 1) {
      as.integer(j_version_parts[[2]])
    } else {
      as.integer(j_version_parts[[1]])
    }
  }
}

java_min_version <- function() 21
java_max_version <- function() 25
java_tested_version <- function() 25
# check Java version
check_java_version <- function() {
  java_version <- get_java_version()
  if (is.null(java_version)) {
    warning("episomer cannot identify your Java version")
    FALSE
  } else if (
    java_version >= java_min_version() && java_version <= java_max_version()
  ) {
    TRUE
  } else if (java_version <= java_tested_version()) {
    warning(paste(
      "Your current Java version is",
      java_version,
      ". episomer needs Java versions between ",
      java_min_version(),
      " and ",
      java_max_version(),
      ". Some users have reported that your version also works, but it is recommended to use a previously compatible Java version"
    ))
    FALSE
  } else {
    warning(paste(
      "Your current Java version is",
      java_version,
      ". episomer needs Java versions between ",
      java_min_version(),
      " and ",
      java_max_version()
    ))
    FALSE
  }
}


is_64bit <- function() {
  if (.Platform$OS.type == "windows") {
    Sys.getenv("PROCESSOR_ARCHITECTURE") == "AMD64"
  } else {
    grepl("64", system("uname -a", intern = TRUE))
  }
}

# check 64 architecture
check_64 <- function() {
  if (is_64bit()) {
    TRUE
  } else {
    warning("Your R seems to be 32-bit, episomer needs 64-bit R")
    FALSE
  }
}


# check msvcr100 on Windows machines
check_winmsvcr100 <- function() {
  # inspired from sparklyr https://github.com/sparklyr/sparklyr/blob/master/R/install_spark_windows.R
  if (.Platform$OS.type == "windows") {
    systemRoot <- Sys.getenv("SystemRoot")
    systemDir <- "system32"
    msvcr100Path <- normalizePath(
      file.path(systemRoot, systemDir, "msvcr100.dll"),
      winslash = "/",
      mustWork = FALSE
    )
    if (file.exists(msvcr100Path)) {
      TRUE
    } else {
      warning(paste(
        "Running Spark on Windows requires the",
        "Microsoft Visual C++ 2010 SP1 Redistributable Package.",
        "Please download and install from: \nhttps://www.microsoft.com/download/en/details.aspx?id=13523",
        "Then restart R after the installation is completed"
      ))
      FALSE
    }
  } else {
    TRUE
  }
}

# check winutils is installed
check_winutils <- function() {
  if (.Platform$OS.type == "windows") {
    if (epifile.exists(get_winutils_path())) {
      TRUE
    } else {
      warning(
        "To run SPARK on Windows, you need winutils which is a HADOOP binary. You can download it by running the update dependencies task"
      )
      FALSE
    }
  } else {
    TRUE
  }
}

# check download Java dependencies has been executed
check_java_deps <- function() {
  if (!epifile.exists(get_jars_dest_path())) {
    warning(
      "episomer requires the dependencies task to run and download all Java dependencies"
    )
    FALSE
  } else {
    con <- file(get_sbt_file_dep_path())
    deps <- readLines(con)
    close(con)
    downloaded <- epilist.files(get_jars_dest_path())
    if (length(deps) != length(downloaded)) {
      warning(
        "episomer requires the dependencies task to run and download all Java dependencies"
      )
      FALSE
    } else {
      TRUE
    }
  }
}

# check geonames is downloaded and indexed
check_geonames <- function() {
  if (!epifile.exists(get_geonames_txt_path())) {
    warning(
      "episomer needs GeoNames to be downloaded and unzipped for geolocating posts. Please run the GeoNames task in the requirements and alert pipeline"
    )
    FALSE
  } else {
    dir <- get_geonames_index_path()
    ret <- (if (!epifile.exists(dir)) {
      FALSE
    } else {
      dirs <- epilist.dirs(dir)
      ind_dir <- dirs[grepl(".*/lucIndex[^/]*", dirs)]
      if (length(ind_dir) == 0) {
        FALSE
      } else {
        length(epilist.files(ind_dir, "_.*\\.cfs")) > 0
      }
    })
    if (ret) {
      TRUE
    } else {
      warning("GeoNames has not been indexed. Please execute the geonames task")
      FALSE
    }
  }
}


# check that languages are downloaded and indexed
check_languages <- function() {
  lang_files <- sapply(conf$languages, function(l) {
    to_remote_if_needed(l$vectors)
  })
  if (
    length(lang_files) == 0 ||
      !all(sapply(lang_files, function(f) epifile.exists(f)))
  ) {
    warning(
      "episomer requires languages models to be downloaded for geolocating posts. Please activate at least one language and launch the languages task"
    )
    FALSE
  } else {
    dir <- get_lang_index_path()
    ret <- (if (!epifile.exists(dir)) {
      FALSE
    } else {
      dirs <- epilist.dirs(dir)
      ind_dir <- dirs[grepl(".*/lucIndex[^/]*", dirs)]
      if (length(ind_dir) == 0) {
        FALSE
      } else {
        files <- epilist.files(ind_dir, ".*\\.cfs")
        length(files) > 0
      }
    })
    if (ret) {
      TRUE
    } else {
      warning(
        "The language vector index has not been built. Please activate at least one language and launch the languages task"
      )
      FALSE
    }
  }
}


# check if aggregated files are present (aggregate has successfully run)
check_series_present <- function(
  series = c("country_counts", "geolocated", "topwords", "tags", "urls")
) {
  for (s in series) {
    found <- FALSE
    dirs <- tryCatch(
      suppressWarnings(epipath("fs", s)),
      error = function(e) {
        warning(
          sprintf(
            "No aggregated data found for %s. Please run the data collection (if not running yet) and the database with episomer::setup()",
            s
          )
        )
        NULL
      }
    )
    if (is.null(dirs)) {
      return(FALSE)
    }
    for (p in dirs) {
      #  for(w in epilist.dirs(p)) {
      # a small index can hold only compound segments (.cfs), with no .fdt outside them
      if (length(epilist.files(p, "\\.(fdt|cfs)$", recursive = TRUE)) > 0) {
        found <- TRUE
        break
      }
      #  }
      if (found) {
        break
      }
    }
    if (!found) {
      warning(
        sprintf(
          "No data was found for %s. Please ensure the search loop is properly running",
          s
        )
      )
      return(FALSE)
    }
  }
  return(TRUE)
}

# check if post files are present & post collection has run
check_posts_present <- function() {
  if (!is.na(last_fs_updates("posts"))) {
    TRUE
  } else {
    warning(
      "No posts found on database. Please run the data collection with episomer::setup()"
    )
    FALSE
  }
}

# check if alert files are present
check_alerts_present <- function() {
  dirs <- tryCatch(
    suppressWarnings(epilist.dirs(path = epipath("alerts"))),
    error = function(e) {
      warning("No alert files found. Please execute the alerts task")
      NULL
    }
  )
  if (is.null(dirs)) {
    return(FALSE)
  }
  dirs <- dirs[!grepl(".*NA$", dirs)]
  if (length(dirs) == 0) {
    warning("No alert files found. Please execute the alerts task")
    return(FALSE)
  }
  last_year <- tail(dirs, n = 1)
  last_file <- tryCatch(
    suppressWarnings(tail(
      epilist.files(path = last_year, pattern = ".*.json"),
      n = 1
    )),
    error = function(e) {
      warning("No alert files found. Please execute the alerts task")
      character(0)
    }
  )

  if (length(last_file) > 0) {
    TRUE
  } else {
    warning("No alert files found. Please execute the alerts task")
    FALSE
  }
}

# check if taskScheduleR is installed on Windows
check_scheduler <- function() {
  if (.Platform$OS.type == "windows") {
    installed <- tryCatch(
      {
        func <- taskscheduleR::taskscheduler_create
        TRUE
      },
      warning = function(w) FALSE,
      error = function(e) FALSE
    )
    if (installed) {
      TRUE
    } else {
      warning(
        "To activate tasks, you need to install the taskscheduleR package manually. Please run install.packages('taskscheduleR') on an R session and restart episomer"
      )
      FALSE
    }
  } else {
    TRUE
  }
}

# check if Pandoc is installed
check_pandoc <- function() {
  if (unname(Sys.which("pandoc")) == "") {
    warning(
      "To export the PDF report from the dashboard, Pandoc must be installed."
    )
    FALSE
  } else {
    TRUE
  }
}

# check if Pandoc is installed
check_tex <- function() {
  if (unname(Sys.which("pdflatex")) == "") {
    warning(paste(
      "For exporting the PDF report from the dashboard, a tex distribution has to be installed.",
      "We suggest installing tinytex with the following commands on an R session:",
      "install.packages('tinytex'); tinytex::install_tinytex()"
    ))
    FALSE
  } else {
    TRUE
  }
}

# check search is running
check_search_running <- function() {
  if (is_search_running()) {
    TRUE
  } else {
    warning(paste(
      "Data collection & processing pipeline is not running on any machine sharing this database. ",
      "Please run episomer::setup() to start the search loop."
    ))
    FALSE
  }
}

# check detect is running
check_detect_running <- function() {
  if (is_detect_running()) {
    TRUE
  } else {
    warning(paste(
      "Requirements & alerts pipeline is not running on any machine sharing this database. ",
      "Please run episomer::setup() to start the detect loop."
    ))
    FALSE
  }
}

# check fs is running
check_fs_running <- function() {
  if (is_fs_running()) {
    TRUE
  } else {
    warning(paste(
      "Embedded database is not running",
      "Please run episomer::setup() to start the database."
    ))
    FALSE
  }
}

# check sm authentication
last_check_sm_auth <- new.env()

check_sm_auth <- function() {
  unauthenticated_sm <- "No active SM"

  all_authenticated <- tryCatch(
    {
      ok <- FALSE
      for (sm in active_social_media()) {
        unauthenticated_sm <- sm
        ok <- is_sm_authenticated(sm)
        if (!ok) break
      }
      ok
    },
    warning = function(m) FALSE,
    error = function(e) FALSE
  )

  last_check_sm_auth$value <- all_authenticated

  if (!all_authenticated) {
    warning(
      sprintf(
        "Could not find an authentication for %s, please define it using episomer::setup() if you want to collect messages",
        unauthenticated_sm
      )
    )
  }

  all_authenticated
}
check_last_sm_auth <- function() {
  if (exists("value", last_check_sm_auth) && last_check_sm_auth$value) {
    TRUE
  } else {
    warning(
      "Cannot create a social media token, please choose an authentication method using episomer::setup()"
    )
    FALSE
  }
}

# can move from temp to data directory
#
# What it guards against is a data directory on another file system than TMPDIR, where
# file.rename() fails across the device boundary - a Windows hazard above all. An app running
# away from the fs has no local data directory to move anything into: it reads and writes through
# the API, the hazard cannot occur, and running the test anyway reported "ko" with a message
# about TMPDIR and drive letters that named a tempdir() placeholder nobody had configured.
#
# It passes rather than being reported as skipped because check_all() has no third state: a check
# answers TRUE or FALSE, and nothing here is broken. The test is deliberately left on the raw
# file.rename()/file.remove() and not moved to epifile.*, which would measure a move on the fs
# machine instead - another thing entirely.
check_move_from_temp <- function() {
  if (!has_local_data_dir()) {
    return(TRUE)
  }
  ok <- tryCatch(
    {
      temp <- tempfile()
      fileConn <- file(temp)
      writeLines(c("hello temp"), fileConn)
      close(fileConn)
      file.rename(temp, file.path(conf$data_dir, "deleteme.temp"))
      file.remove(file.path(conf$data_dir, "deleteme.temp"))
      TRUE
    },
    warning = function(m) {
      FALSE
    },
    error = function(e) {
      FALSE
    }
  )
  if (ok) {
    TRUE
  } else {
    warning(paste(
      "Cannot move files from temporary folder to data_dir, possible cross link device.",
      "Please set TMPDIR, TMP or TEMP environment variable to the same drive as the data folder",
      conf$data_dir
    ))
    FALSE
  }
}


# can tar gz
check_tar_gz <- function() {
  ok <- tryCatch(
    {
      temp <- tempdir()
      p <- file.path(temp, "test")
      pgz <- file.path(temp, "test.tar.gz")
      fileConn <- file(p)
      writeLines(c("hello temp"), fileConn)
      close(fileConn)

      result <- tryCatch(
        {
          processx::run(
            "tar",
            c(
              "-czf",
              normalizePath(pgz, winslash = "/", mustWork = FALSE),
              normalizePath(p, winslash = "/"),
              "--force-local"
            ),
            wd = temp
          )
        },
        error = function(e) {
          processx::run(
            "tar",
            c(
              "-czf",
              normalizePath(pgz, winslash = "/", mustWork = FALSE),
              normalizePath(p, winslash = "/")
            ),
            wd = temp
          )
        }
      )

      TRUE
    },
    warning = function(m) {
      FALSE
    },
    error = function(e) {
      FALSE
    }
  )
  if (ok) {
    TRUE
  } else {
    warning(paste(
      "Cannot build tar.gz. Please ensure that Tar and Gzip are available in the system path and make sure you can execute 'tar -czf test.tar.gz somefile' on a terminal.",
      "The latest versions of Windows 10 may include these tools; otherwise, you would need to install them to create snapshots."
    ))
    FALSE
  }
}

#' @title Episomer can run
#' @description check whether conditions are met for episomer app to run
#' @return logical, whether episomer can run or not
#' @details Perform configuration checks concerning, Java, bitness, task status, dependencies and social media authentication to determine whether episomer can run.
#' @examples
#' \dontrun{
#'    #importing episomer
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    #check if episomer can run
#'    can_run()
#' }
#' @rdname can_run
#' @export
can_run <- function() {
  return(tryCatch(
    check_64() &&
      check_java_64() &&
      check_java_deps() &&
      check_java_present() &&
      check_sm_auth() &&
      check_winmsvcr100() &&
      check_winutils() &&
      check_languages() &&
      check_geonames() &&
      check_move_from_temp(),
    warning = function(m) {
      message(m)
      FALSE
    }
  ))
}

#' @title Episomer setup can run
#' @description check whether conditions are met for episomer setup to run
#' @return logical, whether episomer can run or not
#' @details Perform configuration checks concerning, Java, bitness whether episomer setup can run.
#' @examples
#' \dontrun{
#'    #importing episomer
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    #check if episomer can run
#'    can_run()
#' }
#' @rdname can_run
#' @export
setup_can_run <- function() {
  return(tryCatch(
    check_64() &&
      check_java_64() &&
      check_java_present() &&
      check_java_version() &&
      check_jar_version(),
    warning = function(m) {
      message(m)
      FALSE
    }
  ))
}

# stops spark_job()/spark_df() from silently building a broken classpath ("-cp ;jars/*", missing
# the app jar segment entirely) when inst/java ships no jar matching the versioned pattern -- the
# failure mode that surfaced as "Could not find or load main class org.ecdc.episomer.Episomer"
check_app_jar_present <- function() {
  jar_path <- get_app_jar_path()
  if (identical(jar_path, "") || !epifile.exists(jar_path)) {
    stop(
      "episomer could not find its embedded java bundle jar in inst/java. ",
      "The R package installation is incomplete or corrupted, please reinstall episomer."
    )
  }
  invisible(TRUE)
}

# check that the embedded jar's own version (encoded in its filename) matches the R package
# version -- catches a stale jar left behind by a partial rebuild (e.g. sbt package skipped)
check_jar_version <- function() {
  jar_path <- get_app_jar_path()
  if (identical(jar_path, "") || !epifile.exists(jar_path)) {
    warning("episomer could not find the embedded java bundle jar in inst/java")
    return(FALSE)
  }
  jar_version <- sub(
    "^ecdc-episomer-bundle_2\\.13-(.*)\\.jar$",
    "\\1",
    basename(jar_path)
  )
  pkg_version <- as.character(utils::packageVersion(get_package_name()))
  if (jar_version != pkg_version) {
    warning(sprintf(
      "episomer java bundle version (%s) does not match the package version (%s), please rebuild the jar",
      jar_version,
      pkg_version
    ))
    return(FALSE)
  }
  TRUE
}

#' @title Run automatic sanity checks
#' @description It runs a set of automated sanity checks for helping the user to troubleshot issues
#' @return Data frame containing the statuses of all realized checks
#' @details This function executes a series of sanity checks, concerning, Java, bitness, task status, dependencies and social media authentication.
#' @examples
#' \dontrun{
#'    #importing episomer
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    #running all tests
#'    check_all()
#' }
#' @rdname check_all
#' @export
check_all <- function() {
  check_sm_auth()
  checks <- list(
    scheduler = check_scheduler,
    sm_auth = check_last_sm_auth,
    search_running = check_search_running,
    database_running = check_fs_running,
    posts = check_posts_present,
    os64 = check_64,
    java = check_java_present,
    java64 = check_java_64,
    java_version = check_java_version,
    winmsvc = check_winmsvcr100,
    detection_running = check_detect_running,
    winutils = check_winutils,
    java_deps = check_java_deps,
    move_from_temp = check_move_from_temp,
    tar_gz = check_tar_gz,
    geonames = check_geonames,
    languages = check_languages,
    aggregate = check_series_present,
    alerts = check_alerts_present,
    pandoc = check_pandoc,
    tex = check_tex
  )
  results <- sapply(
    checks,
    function(check) {
      tryCatch(
        if (check()) "ok" else "ko",
        warning = function(m) {
          as.character(m$message)
        },
        error = function(e) {
          as.character(e)
        }
      )
    }
  )
  data.frame(
    check = names(checks),
    passed = results == "ok",
    message = ifelse(results == "ok", "", results)
  )
}

# Environment for storing last admin email
checks <- new.env()

#' @title Send email to administrator if a failure of episomer is detected
#' @description It validates if episomer is not collecting posts, aggregating posts or not calculating alerts
#' @param send_mail Boolean. Whether an email should be sent to the defined administrator, default: TRUE
#' @param one_per_day Boolean. Whether a limit of one email per day will be applied, default: TRUE
#' @return A list of health check errors found
#' @details This function sends an email to the defined administrator if episomer is not collecting posts, aggregating posts or not calculating alerts
#' @examples
#' \dontrun{
#'    #importing episomer
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    #sending the email to the administrator if episomer components are not properly working
#'    health_check()
#' }
#' @rdname health_check
#' @export
health_check <- function(send_mail = TRUE, one_per_day = TRUE) {
  #health check is only done if one_per_day limit is disabled or if no email was already sent since yesterday and it is after 8AM.
  start_of_day <- strptime(strftime(Sys.time(), "%Y-%m-%d"), "%Y-%m-%d")
  alerts <- list()
  if (
    !one_per_day ||
      !exists("last_check", checks) ||
      (checks$last_check < start_of_day) &&
        as.numeric(strftime(Sys.time(), "%H")) >= 8
  ) {
    # check 01: if last post collected date is more than 1 hour
    last_collected <- epifile.mtime(get_search_touch_path())
    if (
      is.na(last_collected) ||
        as.numeric(Sys.time() - last_collected, unit = "hours") >= 1
    ) {
      alerts <- append(
        alerts,
        paste(
          "Posts have not been collected for more than ",
          as.integer(as.numeric(Sys.time() - last_collected, unit = "hours")),
          "hours"
        )
      )
    }

    # Check 02: if geolocated posts has not been done during more than one hour
    collection_times <- last_fs_updates("posts")
    last_geo <- collection_times$posts
    if (
      is.na(last_geo) || as.numeric(Sys.time() - last_geo, unit = "hours") >= 1
    ) {
      alerts <- append(
        alerts,
        paste(
          "Posts have not been geolocated for more than ",
          as.integer(as.numeric(Sys.time() - last_geo, unit = "hours")),
          "hours"
        )
      )
    }
    # Check 03: if any time series has not been processed in more than one hour
    collection_times <- last_fs_updates(c(
      "topwords",
      "country_counts",
      "geolocated"
    ))
    for (i in 1:length(collection_times)) {
      serie <- names(collection_times)[[i]]
      last_collect <- collection_times[[serie]]
      if (
        is.na(last_collect) ||
          as.numeric(Sys.time() - last_collect, unit = "hours") >= 1
      ) {
        alerts <- append(
          alerts,
          paste(
            "Serie,",
            serie,
            "has not been produced for more than ",
            as.integer(as.numeric(Sys.time() - last_collect, unit = "hours")),
            "hours"
          )
        )
      }
    }

    # Check 04: if alert detection has not finished in more than 2 hours + schedule span
    tasks <- get_tasks()
    last_detect_end <- tasks$alert$end_on
    if (
      is.na(last_detect_end) ||
        as.numeric(Sys.time() - last_detect_end, unit = "hours") >=
          (2 + conf$schedule_span / 60)
    ) {
      alerts <- append(
        alerts,
        paste(
          "The alerts tasks has not been finished since",
          as.integer(as.numeric(Sys.time() - last_detect_end, unit = "hours")),
          "hours"
        )
      )
    }

    # Check 05: if any task is on aborted status
    for (i in 1:length(tasks)) {
      if (!is.na(tasks[[i]]$status) && tasks[[i]]$status == "aborted") {
        alerts <- append(
          alerts,
          paste("Task,", tasks[[i]]$task, "is on aborted status")
        )
      }
    }
    if (
      send_mail &&
        length(alerts) > 1 &&
        !is.null(conf$admin_email) &&
        conf$admin_email != "" &&
        conf$smtp_host != ""
    ) {
      # creating the message to send
      msg <- (emayili::envelope() %>%
        emayili::from(conf$smtp_from) %>%
        emayili::to(conf$admin_email) %>%
        emayili::subject("episomer may not be properly running") %>%
        emayili::html(
          paste(
            "<p>The following errors were found during episomer health check, please check your episomer installation</p><p><ul><li>",
            paste(alerts, collapse = "</li><li>"),
            "</li></ul></p>"
          )
        ))
      smtp <- (if (
        is.na(conf$smtp_password) ||
          is.null(conf$smtp_password) ||
          conf$smtp_password == ""
      ) {
        emayili::server(
          host = conf$smtp_host,
          port = conf$smtp_port,
          insecure = conf$smtp_insecure,
          reuse = FALSE
        )
      } else {
        emayili::server(
          host = conf$smtp_host,
          port = conf$smtp_port,
          username = conf$smtp_login,
          insecure = conf$smtp_insecure,
          password = conf$smtp_password,
          reuse = FALSE
        )
      })
      message(sprintf(
        "Health check failed, sending email from %s to %s\n%s",
        conf$smtp_from,
        conf$admin_email,
        paste(alerts, collapse = "\n")
      ))
      tryCatch(
        smtp(msg),
        warning = function(e) {
          message(paste(
            "Warning: Cannot send email. The following error occured",
            e
          ))
        },
        error = function(e) {
          message(paste(
            "Warning: Cannot send email. The following error occured",
            e
          ))
        }
      )

      checks$last_check <- Sys.time()
    }
  }
  alerts
}

update_session_info <- function() {
  tf <- tempfile("episomer-session")
  con <- file(tf)
  writeLines(utils::capture.output(utils::sessionInfo()), con)
  close(con)
  epifile.copy(tf, get_session_info_path())
  unlink(tf)
}


#' @title Snapshot of your episomer installation
#' @description Creates a snapshot file of your episomer installation folder. This can include all or a subset of the data files.
#' @param destination_dir, character(1) vector with the path of the destination folder to produce the snapshot
#' @param types, character vector indicating the types of data to include on a snapshot. Some of: "settings", "dependencies", "machine-learning", "aggregations", "posts", "logs"
#' @param posts_period, date(2) start and end dates to filter posts to include on snapshot (if selected)
#' @param aggregated_period, date(2) start and end dates to filter time series to include on snapshot (if selected)
#' @param compress, logical(1) whether to compress or not the output file
#' @param progress, function to report progress during execution.
#' @return Nothing
#' @details
#' This function can be used to create a a portable file to move your episomer installation in a single file, to backup your data, to archive your old data or to send information to technical team in order to reproduce an observed issue.
#' Different kinds of data can be included on the snapshot depending on the type of parameter. Possible values are:
#' - 'settings': Including all setting files of your installation (excluding passwords)
#' - 'dependencies': All jars and winutils.exe on windows installations
#' - 'machine-learning': All trained models and vectors and training data (this can include post text which is personal data)
#' - 'aggregations': Episomer aggregated time series
#' - 'posts': Posts collected by episomer
#' - 'logs': Log files produced automatically on windows task scheduler tasks.
#' @examples
#' \dontrun{
#'    #importing episomer
#'    library(episomer)
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'    message('Please choose a destination directory')
#'    dest <- file.choose()
#'    #creating a compressed snapshot for settings and logs
#'    create_snapshot(dest, c("settings","dependencies"), compress = TRUE)
#' }
#' @rdname create_snapshot
#' @export
create_snapshot <- function(
  destination_dir,
  types = c(
    "settings",
    "dependencies",
    "machine-learning",
    "aggregations",
    "posts",
    "logs"
  ),
  posts_period = get_aggregated_period(),
  aggregated_period = get_aggregated_period(),
  compress = TRUE,
  progress = function(v, m) message(paste(round(v * 100, 2), m))
) {
  if (!file.exists(destination_dir)) {
    dir.create(destination_dir)
  }
  destination <- normalizePath(
    file.path(
      destination_dir,
      paste0(
        "episomer-snapshot",
        strftime(Sys.time(), "%Y.%m.%d-%H.%M.%S"),
        if (compress) ".tar.gz" else ".tar"
      )
    ),
    winslash = "/",
    mustWork = FALSE
  )
  update_session_info()
  items <- list()
  if ("settings" %in% types) {
    items <- c(
      items,
      episomer_files("", recurse = FALSE),
      episomer_files("collections")
    )
  }
  if ("dependencies" %in% types) {
    items <- c(
      items,
      list(
        "hadoop/bin/winutils.exe" = get_winutils_path()
      ),
      as.list(setNames(
        list.files(get_jars_dest_path(), full.names = TRUE),
        file.path("jars", list.files(get_jars_dest_path()))
      ))
    )
  }
  if ("machine-learning" %in% types) {
    items <- c(
      items,
      list(
        "alert-training.xlsx" = get_alert_training_path(),
        "geo-training.xlsx" = get_geotraining_path(),
        "geo-training-evaluation.json" = get_geotraining_evaluation_path(),
        "geo/forced-geo.json" = get_forced_geo_path(),
        "geo/forced-geo-codes.json" = get_forced_geo_codes_path(),
        "geo/allCountries.txt" = get_geonames_txt_path()
      ),
      episomer_files(get_cities_parquet_path(relative = TRUE)),
      episomer_files(get_geonames_parquet_path(relative = TRUE)),
      episomer_files(get_geonames_index_path(relative = TRUE)),
      episomer_files(get_lang_index_path(relative = TRUE)),
      episomer_files(get_lang_index_path(relative = TRUE)),
      episomer_files("languages"),
      episomer_files("alert-ml")
    )
  }
  if ("aggregations" %in% types) {
    items <- c(
      items,
      episomer_files("alerts", aggregated_period[[1]], aggregated_period[[2]]),
      episomer_files(
        "fs",
        aggregated_period[[1]],
        aggregated_period[[2]]
      ),
      episomer_files("series", aggregated_period[[1]], aggregated_period[[2]]),
      episomer_files("stats", aggregated_period[[1]], aggregated_period[[2]])
    )
  }
  if ("posts" %in% types) {
    items <- c(
      items,
      list(
        "geo/togeolocate.json" = get_post_togeo_path(),
        "geo/geolocating.json" = get_post_geoing_path(),
        "geo/toaggregate.json" = get_post_toaggr_path(),
        "geo/aggregating.json" = get_post_aggring_path()
      ),
      episomer_files("fs/posts", posts_period[[1]], posts_period[[2]])
    )
  }
  if ("logs" %in% types) {
    jobs_dir <- epipath("jobs")
    items <- c(
      items,
      list(
        "detect.log" = file.path(jobs_dir, "detect.log"),
        "fs.log" = file.path(jobs_dir, "fs.log"),
        "search.log" = file.path(jobs_dir, "search.log")
      )
    )
  }
  nb <- length(items)
  i <- 0
  progress(0, paste("Building archive for", nb, "files"))
  last_message <- as.integer(Sys.time())
  dump <- file.path(destination_dir, "dump")
  if (file.exists(dump)) {
    unlink(dump, recursive = TRUE)
  }
  dir.create(dump)
  if (file.exists(destination)) {
    unlink(destination)
  }
  if (nb > 0) {
    relpaths <- names(items)
    paths <- unname(items)
    for (i in 1:nb) {
      progress(
        round(0.7 * i / nb, 2),
        paste("Downloading", i, "of", nb, "files")
      )
      last_message <<- as.integer(Sys.time())
      if (epifile.exists(paths[[i]])) {
        epifile.copy(
          paths[[i]],
          file.path(dump, relpaths[[i]]),
          allow_partial = grepl("\\.log$", relpaths[[i]]),
          tail_lines = if (grepl("\\.log$", relpaths[[i]])) {
            conf$snapshot_log_lines
          } else {
            0
          }
        )
      }
    }
    progress(0.8, "Compacting and archiving")
    comp <- if (compress) "z" else ""
    tar_args <- c(sprintf("-cv%sf", comp), destination, ".")
    # GNU tar needs --force-local to accept a "C:/..." destination, but the bsdtar shipped with
    # Windows rejects that option (see the same fallback in the tar availability check).
    # processx's own error only says "System command 'tar' failed": keep tar's stderr in it
    run_tar <- function(args) {
      res <- processx::run("tar", args, wd = dump, error_on_status = FALSE)
      if (res$status != 0) {
        stop(
          sprintf(
            "tar %s failed (status %s): %s",
            paste(args, collapse = " "),
            res$status,
            trimws(res$stderr)
          ),
          call. = FALSE
        )
      }
      res
    }
    tryCatch(
      run_tar(c("--force-local", tar_args)),
      error = function(e) {
        warning(conditionMessage(e), call. = FALSE)
        run_tar(tar_args)
      }
    )
  }
  if (file.exists(dump)) {
    unlink(dump, recursive = TRUE)
  }
  progress(1, "Archive done")
}

episomer_files <- function(rel_path, dmin = NULL, dmax = NULL, recurse = TRUE) {
  if (!is.null(dmin) && is.na(dmin)) {
    dmin <- NULL
  }
  if (!is.null(dmax) && is.na(dmax)) {
    dmax <- NULL
  }

  ret <- list()
  path <- epipath(rel_path)
  if (!epifile.exists(path)) {
    return(ret)
  }
  files <- trimws(epilist.files(path, full.names = FALSE))
  dirs <- trimws(epilist.dirs(path, full.names = FALSE))
  file_names <- setdiff(
    files,
    dirs
  )
  # Adding files
  if (length(file_names) > 0) {
    files <- as.list(setNames(
      epipath(rel_path, file_names),
      file.path(rel_path, file_names)
    ))
    #fixing issues with relativa paths starting with /
    files <- setNames(files, gsub("^/+", "", names(files)))
    dates <- as.Date(strptime(
      regmatches(
        names(files),
        gregexpr("[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\.[0-9][0-9]", names(files))
      ),
      "%Y.%m.%d"
    ))
    fd <- ifelse(
      sapply(dates, function(v) is.null(dmin)),
      TRUE,
      ifelse(is.na(dates), FALSE, dates >= dmin)
    ) &
      ifelse(
        sapply(dates, function(v) is.null(dmax)),
        TRUE,
        ifelse(is.na(dates), FALSE, dates <= dmax)
      )
    weeks <- regmatches(
      names(files),
      gregexpr("[0-9][0-9][0-9][0-9]\\.[0-9][0-9]", names(files))
    )
    weeks <- ifelse(is.na(dates), weeks, NA)
    wmin <- if (is.null(dmin)) NULL else strftime(dmin, "%G.%V")
    wmax <- if (is.null(dmax)) NULL else strftime(dmax, "%G.%V")
    fw <- ifelse(
      sapply(weeks, function(v) is.null(wmin)),
      TRUE,
      ifelse(is.na(weeks), FALSE, weeks >= wmin)
    ) &
      ifelse(
        sapply(weeks, function(v) is.null(wmax)),
        TRUE,
        ifelse(is.na(weeks), FALSE, weeks <= wmax)
      )
    ret <- c(ret, files[fd | fw])
  }
  # Adding subfolders
  if (recurse) {
    for (d in dirs) {
      ret <- c(
        ret,
        episomer_files(file.path(rel_path, d), dmin = dmin, dmax = dmax)
      )
    }
  }
  ret
}
