#!/bin/bash
set -e

# creating package and installing it
export cdir=`pwd`
export OPENBLAS_NUM_THREADS=1

if [ -z ${JAR+x} ]; then 
    echo "Skipping SBT jar generation"; 
else 
    cd "$cdir/scala"
    sbt package
    mkdir -p "$cdir/episomer/inst/java"
    cp "$cdir/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-1.0.jar" "$cdir/episomer/inst/java"
    cd "$cdir"
fi

if [ -z ${RENV+x} ]; then 
    echo "Skipping RENV"; 
else 
    Rscript devscripts/renv.R;
fi

if [ -z ${PACK+x} ] && [ -z ${DEVT+x} ] && [ -z ${DEPS+x} ]; then 
    echo "Skipping packaging / R dependencies"
else
    if [ ! -z ${PACK+x}  ] ; then 
        git archive --format zip --output "$cdir/episomer/java/ecdc-episomer-bundle_2.13-1.0-source.zip" HEAD 
    fi
    Rscript devscripts/package.R
fi

if [ -z ${INSTALL+x} ]; then
    echo "Not installing"; 
else
    Rscript devscripts/install.R
fi


cd "$cdir"
