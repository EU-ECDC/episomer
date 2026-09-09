#!/usr/bin/env Rscript
# script for running the episomer database (fs) loop
args = commandArgs(trailingOnly=TRUE)

# test if the data directory path has been provided
if (length(args)==0) {
  stop("The config file has not been provided", call.=FALSE)
} 

data_dir <- args[1]

# Anything after the data directory is a sequence of 'key' 'value' pairs, the same shape the Scala
# side reads its own arguments in (Episomer.main). register_fs_runner_task() uses them to hand over
# a bind interface and a bind port for this run only, so that a database started for the exclusive
# use of one caller - the language model retraining of setup() - is not found on the configured
# address by the services waiting for the real one to become healthy.
extra <- args[-1]
if (length(extra) %% 2 != 0) {
  stop(
    paste(
      "Extra arguments must be given as 'key' 'value' pairs, got:",
      paste(extra, collapse = " ")
    ),
    call. = FALSE
  )
}
named <- if (length(extra) > 0)
    setNames(as.list(extra[c(FALSE, TRUE)]), extra[c(TRUE, FALSE)])
  else
    list()

episomer::fs_loop(
  data_dir,
  bind_host = named$bindHost,
  bind_port = if (is.null(named$bindPort)) NULL else as.numeric(named$bindPort)
)
