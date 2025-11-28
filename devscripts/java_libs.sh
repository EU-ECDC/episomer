# updating scala depedencies file
export cdir=`pwd`

cd "$cdir/scala"

rm -r "$cdir/scala/lib_managed"
sbt package
cd "$cdir/scala/lib_managed"

find . -type f > "$cdir/episomer/inst/extdata/sbt-deps.txt"

cd "$cdir"

