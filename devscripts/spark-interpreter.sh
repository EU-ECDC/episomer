#!/bin/bash
export OPENBLAS_NUM_THREADS=1
export SPARK_VERSION=3.0.3
export PATH=$JAVA_HOME/bin:$PATH

if [ -z ${EPI_HOME+x} ]; then echo "please set EPI_HOME is unset"; exit 1; fi
export TEMPDIR=$EPI_HOME/tmp
export SBT_OPTS="-Xmx16G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xss2M -Duser.timezone=GMT -Djava.io.tmpdir=$TEMPDIR"
export SBT_OPTS="-Xmx16G -Xss2M -Duser.timezone=GMT -Djava.io.tmpdir=$TEMPDIR"
cd scala
expect -c '
spawn sbt console
expect "scala>"
send "import org.ecdc.episomer.Settings\r"
send "implicit val conf = Settings.apply(\"'$EPI_HOME'\")\r"
send "conf.load\r"
send "import org.ecdc.episomer.fs.LuceneActor\r"
send "import org.apache.spark.sql.SparkSession\r"
send "import org.apache.spark.sql.functions._\r"
send "org.ecdc.episomer.API.run(conf.epiHome)\r"
interact'

cd ..
