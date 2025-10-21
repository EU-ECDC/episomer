# creating package and installing it
export cdir=`pwd`
export OPENBLAS_NUM_THREADS=1

cd "$cdir/scala"
sbt package

mkdir -p "$cdir/episomer/inst/java"
cp "$cdir/scala/target/scala-2.13/ecdc-episomer-bundle_2.13-1.0.jar" "$cdir/episomer/inst/java"

git archive --format zip --output "$cdir/episomer/java/ecdc-episomer-bundle_2.13-1.0-source.zip" HEAD 

cd "$cdir"
#Rscript devscripts/renv.R
#Rscript devscripts/package.R

cd "$cdir"
