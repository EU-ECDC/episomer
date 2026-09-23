# get the Java path using JAVA_HOME if set
java_path <- function() {
  if (Sys.getenv("JAVA_HOME") == "") "java" else if (
    .Platform$OS.type == "windows"
  )
    paste("\"", Sys.getenv("JAVA_HOME"), "\\bin\\", "java\"", sep = "") else
    paste("\"", Sys.getenv("JAVA_HOME"), "/bin/", "java\"", sep = "")
}

# Setting the blas environment variables depending on chosen configuration and OS
set_blas_env <- function() {
  if (.Platform$OS.type == "windows") {
    if (
      conf$use_mkl &&
        file.exists(
          "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/redist/intel64_win/mkl"
        )
    ) {
      mkl_path = "C:\\Program Files (x86)\\IntelSWTools\\compilers_and_libraries\\windows\\redist\\intel64_win\\mkl"
      Sys.setenv(PATH = paste(mkl_path, Sys.getenv("PATH"), sep = ";"))
    }
  }
}

# Telling the JVM which R to run, and where that R finds its packages.
#
# The fs API starts `Rscript jobs/get_secret.R` to reach the keyring (API.scala), and until it was
# told otherwise it ran whatever "Rscript" the PATH offered - see rscript_exe_for_scala()
# (R/runners.R) for what that costs.
#
# The environment is the channel because it is the only one that carries all the way down: the JVM
# inherits this process's environment through system(), and ProcessBuilder starts its own children
# with a copy of System.getenv(). So R_LIBS reaches the Rscript sub process on its own, two levels
# down, and the Scala side only has to read EPISOMER_RSCRIPT.
#
# R_LIBS rather than R_LIBS_USER: it is the one an R sub process prepends to .libPaths() whatever
# else it reads, and .libPaths() is precisely the list that just worked here. A container needs
# neither - the Dockerfile sets R_LIBS_USER for the whole image and holds one R - but setting them
# there changes nothing and keeps a single code path.
set_scala_r_env <- function() {
  Sys.setenv(
    EPISOMER_RSCRIPT = rscript_exe_for_scala(),
    R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep)
  )
}

# Getting Java options for using MKL BLAS if setup
get_blas_java_opt <- function() {
  if (.Platform$OS.type == "windows") {
    if (
      conf$use_mkl &&
        file.exists(
          "C:/Program Files (x86)/IntelSWTools/compilers_and_libraries/windows/redist/intel64_win/mkl"
        )
    ) {
      "-Dcom.github.fommil.netlib.NativeSystemBLAS.natives=mkl_rt.dll"
    } else ""
  } else ""
}

# getting a string for passing languages as parameter for system calls
conf_languages_as_arg <- function() {
  paste(
    "langCodes \"",
    paste(lapply(conf$languages, function(l) l$code), collapse = ","),
    "\" ",
    "langNames \"",
    paste(lapply(conf$languages, function(l) l$name), collapse = ","),
    "\" ",
    "langPaths \"",
    paste(lapply(conf$languages, function(l) l$vectors), collapse = ","),
    "\" ",
    sep = ""
  )
}

# getting a string for passing geonames as paramater for system calls
conf_geonames_as_arg <- function() {
  return(
    paste(
      "geonamesSource",
      paste(
        "\"",
        file.path(conf$data_dir, "geo", "allCountries.txt"),
        "\"",
        sep = ""
      ),
      "geonamesDestination",
      paste("\"", paste(conf$data_dir, "geo", sep = "/"), "\"", sep = ""),
      "geonamesSimplify",
      paste("\"", conf$geonames_simplify, "\"", sep = "")
    )
  )
}


# making a spark call via system call without returning any value
spark_job <- function(args) {
  check_app_jar_present()
  # Setting BLAS environmebt variables
  set_blas_env()
  # Setting the R the JVM runs the secret scripts with
  set_scala_r_env()
  cmd <- paste(
    if (.Platform$OS.type != "windows") {
      #Default non windows environment variables within the command
      "export OPENBLAS_NUM_THREADS=1"
    } else {
      Sys.setenv(
        OPENBLAS_NUM_THREADS = 1,
        HADOOP_HOME = get_winutils_hadoop_home_path(),
        PATH = paste(file.path(get_winutils_hadoop_home_path(), "bin"), Sys.getenv("PATH"), sep = ";")
      ) #Default windows environment variables
      ""
    },
    paste(
      java_path(), #Java executable
      paste(
        "-cp \"",
        get_app_jar_path(), #episomer embedded jar path with
        if (.Platform$OS.type != "windows") ":" else ";",
        get_jars_dest_path(), #Java class path with all dependencies
        "/*\"",
        sep = ""
      ),
      paste("-Xmx", conf$spark_memory, sep = ""), #memory limits
      paste("--add-opens=java.base/java.nio=ALL-UNNAMED"), #allowing proper serialisation in spark of ByteArray
      paste("--add-opens=java.base/java.lang.invoke=ALL-UNNAMED"), #allowing proper serialisation in spark of ByteArray
      paste("--add-opens=java.base/java.util=ALL-UNNAMED"), #allowing proper serialisation in spark of ByteArray
      paste(get_blas_java_opt()), #Java blas specific options
      "org.ecdc.episomer.Episomer", #episomer scala main class
      args #arguments to the call
    ),
    sep = if (.Platform$OS.type == "windows") '\r\n' else '\n'
  )

  #message(cmd) # uncomment this to see the command sent
  res <- system(cmd)
  if (res != 0) stop(paste("Error encountered while executing: ", cmd))
}


# getting a spark data frame by piping a system call
spark_df <- function(args, handler = NULL) {
  check_app_jar_present()
  # Setting BLAS environmebt variables
  set_blas_env()
  # Setting the R the JVM runs the secret scripts with
  set_scala_r_env()
  # Setting the memory to the half of allocated since R will use memory for loading the dataframe
  half_mem <- paste(
    ceiling(as.integer(gsub("[^0-9]*", "", conf$spark_memory)) / 2),
    gsub("[0-9]*", "", conf$spark_memory),
    sep = ""
  )
  cmd <- paste(
    if (.Platform$OS.type != "windows")
      #Default non windows environment variables
      "export OPENBLAS_NUM_THREADS=1" else {
      Sys.setenv(
        OPENBLAS_NUM_THREADS = 1,
        HADOOP_HOME = get_winutils_hadoop_home_path()
      ) # Setting windowd default environment variables
      ""
    },
    paste(
      paste(
        if (.Platform$OS.type == "windows") "call " else "",
        java_path(), # Java executable
        sep = ""
      ),
      paste(
        "-cp \"",
        get_app_jar_path(), #episomer embedded jar path with
        if (.Platform$OS.type != "windows") ":" else ";",
        get_jars_dest_path(), #Java class path with all dependencies
        "/*\"",
        sep = ""
      ),
      "-Dfile.encoding=UTF8", #Setting Java encoding to UTF-8
      paste("-Xmx", half_mem, sep = ""), #Setting Java memory
      paste(get_blas_java_opt()),
      "org.ecdc.episomer.Episomer", #episomer scala main class
      args
    ),
    sep = '\n'
  )
  #message(cmd) # uncomment this to see the command sent
  con <- if (.Platform$OS.type == "windows") {
    # on windows we send the results to a temporary files
    t <- tempfile()
    tcmd <- paste(cmd, shQuote(t), sep = " >")
    shell(tcmd)
    file(t, encoding = "UTF-8")
  } else {
    # on other OS we get the results directly from the command line
    pipe(cmd, encoding = "UTF-8")
  }
  # here we have a connection ro read returning json lines
  df <- if (is.null(handler)) {
    #If no custom transformation will be done on data we pass the json lines to jsonlite to interprete it as an R dataframe
    jsonlite::stream_in(con, pagesize = 10000, verbose = FALSE)
  } else {
    # If a custom transformation is going to be done (a handler function has been set)
    # this function will be called on pages of 10k lines using the jsonlite stream_in function
    # the transformed results will be stored on a temporary file that will be read by stream_in
    tmp_file <- tempfile(pattern = "episomer", fileext = ".json")
    #message(tmp_file)
    con_tmp <- file(tmp_file, open = "w", encoding = "UTF-8")
    jsonlite::stream_in(
      con,
      pagesize = 10000,
      verbose = FALSE,
      function(df) handler(df, con_tmp)
    )
    close(con_tmp)
    con_tmp <- file(tmp_file, open = "r", encoding = "UTF-8")
    ret <- jsonlite::stream_in(con_tmp, pagesize = 10000, verbose = FALSE)
    close(con_tmp)
    unlink(tmp_file)
    ret
  }
  if (.Platform$OS.type == "windows") unlink(t)
  df
}

#' @title Updates Java dependencies
#' @description Download Java dependencies of the application mainly related to Apache SPARK, Apache Lucene, and Apache Pekko
#' @param jars, whether to download maven jars default: TRUE
#' @param winutils, whether to download winutils binary default: TRUE
#' @return The list of tasks updated with produced messages
#' @details Run a one-shot task consisting of downloading Java and Scala dependencies, this is separated by the following subtasks
#' \itemize{
#'   \item{Download jar dependencies from configuration maven repo to project data folder. This includes, scala, spark, lucene. Packages to be downloaded are defined in package file 'sbt-deps.txt'}
#'   \item{Download winutils from configuration URL to project data folder. For more details on winutils please see
#'     \url{https://issues.apache.org/jira/browse/HADOOP-13223} and \url{https://issues.apache.org/jira/browse/HADOOP-16816}
#'   }
#' }
#'
#' The URLs to download the JAR dependencies (maven package manager) and Winutils are on the configuration tab of the Shiny app.
#'
#' Normally this function is not called directly by the user but from the \code{\link{detect_loop}} function.
#' @examples
#' \dontrun{
#'    library(episomer)
#'    # setting up the data folder
#'    message('Please choose the episomer data directory')
#'    setup_config(file.choose())
#'
#'    # geolocating last posts
#'    tasks <- download_dependencies()
#' }
#' @rdname download_dependencies
#' @seealso
#'  \code{\link{detect_loop}}
#'
#'  \code{\link{get_tasks}}
#'
#' @export
#' @importFrom utils download.file
download_dependencies <- function(jars = TRUE, winutils = TRUE) {
    if(jars)
        download_sbt_dependencies()
    
    if (.Platform$OS.type == "windows" && winutils) {
        download_winutils()
    }
}

# Download SBT dependencies from maven (scala, spark, lucene, pekko, netlib (BLAS), httpclient) into package folder
download_sbt_dependencies <- function() {
  # reading the dependencies to download from embedded list
  con <- file(get_sbt_file_dep_path())
  deps <- readLines(con)
  close(con)

  # transforming the dependencies listed into maven URLs there are two possible formats that will be stored on urls and urls2
  parts <- strsplit(deps, "/")
  versions <- sapply(parts, function(p) {
    l <- nchar(p[[4]])
    substr(p[[5]], l + 2, nchar(p[[5]]))
  })
  versions <-  gsub("\\.([^\\.]*)$", "", versions)
  versions2 <- gsub("-(.*)$", "", versions)
  urls <- paste(
    conf$maven_repo,
    gsub("\\.", "/", sapply(parts, `[[`, 3)),
    sapply(parts, `[[`, 4),
    versions,
    sapply(parts, `[[`, 5),
    sep = "/"
  )
  urls2 <- paste(
    conf$maven_repo,
    gsub("\\.", "/", sapply(parts, `[[`, 3)),
    sapply(parts, `[[`, 4),
    versions2,
    sapply(parts, `[[`, 5),
    sep = "/"
  )

  #Creating destination directory
  if (!dir.exists(get_jars_dest_path())) dir.create(get_jars_dest_path()) else {
    #Deleting existing files
    existing <- list.files(get_jars_dest_path(), full.names = TRUE)
    tryCatch(
      {
        file.remove(existing)
      },
      warning = function(c) {
        warning(paste(
          "Warning during removing JAR files in ",
          get_jars_dest_path(),
          c
        ))
      },
      error = function(c) {
        stop(paste(
          "episomer cannot remove JAR fils files in ",
          get_jars_dest_path(),
          " please remove the files manually.",
          c
        ))
      }
    )
  }

  # destination file
  dest_file <- file.path(
    get_jars_dest_path(),
    paste(
      sapply(parts, `[[`, 3),
      sapply(parts, `[[`, 4),
      sapply(parts, `[[`, 5),
      sep = "_"
    )
  )

  # iterating over all files
  tryCatch2 <- function(exp, handler) {tryCatch(exp, error=handler, warning=handler)}
  for (i in 1:length(urls)) {
    tryCatch2({
        # downloading files on first format
        message(paste("downloading", urls[[i]]), "-->", dest_file[[i]])
        download.file(url = urls[[i]], destfile = dest_file[[i]], mode = "wb")
      },
      handler = function(c) {
	tryCatch2({
            # downloading files on second format
            message(paste("downloading v2(w)", urls2[[i]]), "-->", dest_file[[i]])
            download.file(url = urls2[[i]], destfile = dest_file[[i]], mode = "wb")
        },
        function(e) {
            stop(e)
        })
      }
    )
  }
}

# Download winutils (necessary for spark on Windows,
# for more details on winutils please see https://issues.apache.org/jira/browse/HADOOP-13223 and https://issues.apache.org/jira/browse/HADOOP-16816
download_winutils <- function() {
  if (!dir.exists(get_winutils_hadoop_home_path()))
    dir.create(get_winutils_hadoop_home_path())
  if (!dir.exists(file.path(get_winutils_hadoop_home_path(), "bin")))
    dir.create(file.path(get_winutils_hadoop_home_path(), "bin"))
  # downloading winutils
  url = conf$winutils_url
  dest = get_winutils_path()
  download.file(url = url, destfile = dest, mode = "wb" )
  url <- sub("winutils.exe", "hadoop.dll", url)
  dest <- sub("winutils.exe", "hadoop.dll", dest) 
  # downloading hadoop dll
  download.file(url = url, destfile = dest, mode = "wb" )
}
