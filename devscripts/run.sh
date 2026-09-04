#!/bin/bash

mkdir -p "/episomer-data/jobs"
mkdir -p "/episomer-data/tmp"
mkdir -p "${XDG_CONFIG_HOME:-${HOME}/.config}/r-keyring"
Rscript /episomer/devscripts/run.R
