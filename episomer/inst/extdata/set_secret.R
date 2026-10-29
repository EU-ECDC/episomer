#!/usr/bin/env Rscript
# script for running the search loop
args = commandArgs(trailingOnly=TRUE)

# test if the data directory path has been provided
if (length(args)==0) {
  stop("The config file has not been provided", call.=FALSE)
} 

secret <- args[1]
kr <-episomer:::get_key_ring('file')
f <- file("stdin")
open(f)
val <- readLines(f, n=1)
episomer:::set_secret(secret, val)
write(sprintf("%s has been set", secret), stderr())

