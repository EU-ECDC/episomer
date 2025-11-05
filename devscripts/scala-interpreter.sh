#!/bin/bash
export OPENBLAS_NUM_THREADS=1
#export PATH=$JAVA_HOME/bin:$PATH

if [ -z ${EPI_HOME+x} ]; then echo "please set EPI_HOME is unset"; exit 1; fi
export TEMPDIR=$EPI_HOME/tmp
export SBT_OPTS="-Xmx16G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xss2M -Duser.timezone=GMT -Djava.io.tmpdir=$TEMPDIR"
export SBT_OPTS="-Xmx16G -Xss2M -Duser.timezone=GMT -Djava.io.tmpdir=$TEMPDIR"
cd scala
expect -c '
spawn sbt consoleQuick
expect "scala>"
send "import org.apache.spark.sql.SparkSession\r"
send "import org.apache.spark.sql.functions._\r"
send "implicit val spark = SparkSession.builder().master(\"local\[*\]\").appName(\"test\").getOrCreate()\r"
send "import spark.implicits._\r"
send "spark.sparkContext.setLogLevel(\"WARN\")\r"
interact'

cd ..
