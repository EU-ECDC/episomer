package org.ecdc.episomer

import sun.management.VMManagement;
import java.lang.management.ManagementFactory;
import java.lang.management.RuntimeMXBean;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

import java.io.FileWriter; 
import java.io.IOException;  

object ProcessID {

	def PID = {
		val runtime = ManagementFactory.getRuntimeMXBean();
                runtime.getPid()
	}

  // Name of the machine this JVM runs on, in the same form the R side records in conf$instance_host
  // (R/config.R, get_instance_host()).
  //
  // Only EPISOMER_INSTANCE is read, and the field is left blank when it is unset. R falls back to
  // Sys.info()[["nodename"]] there, and the JVM has no way to reproduce that value it can be
  // trusted on: InetAddress.getLocalHost().getHostName() goes through a reverse lookup and may
  // answer an FQDN, "localhost", or throw. A blank field means "unknown" to the reader, which fails
  // open to the previous behaviour - whereas a *different* spelling of the same host would read as
  // a foreign namespace and have stop_fs_runner_task() leave the JVM running.
  //
  // Nothing is lost by that: the namespace this field exists to identify is a container's, and a
  // container is precisely where EPISOMER_INSTANCE is set (docker/compose.yml, one value per
  // service).
  def instanceHost = {
    val instance = System.getenv("EPISOMER_INSTANCE")
    if(instance != null) instance else ""
  }

  // PID file layout, shared with the R side (R/runners.R, write_running_task_pid): the PID alone on
  // the first line, then the host, then the process start time - which we leave blank, the JVM has
  // no cheap way to obtain it and R treats a missing field as "signal unavailable".
  //
  // The host line is what stops these files from being read as ours by another PID namespace. The
  // data directory is mounted into the fs container, so $EPI_HOME/fs.java.PID is written there and
  // read on the host, where the number it holds belongs to an unrelated process: without the host
  // line a `make compose-down` leftover has the host believe the containerised database is its own
  // to stop, and kill_task() signal whichever local process was handed that number.
  def writePID(path:String) = {
     val writer = new FileWriter(path)
     val pid = PID
     print(s"running with PID: ${pid}")
     writer.write(s"${pid.toString()}\n${instanceHost}\n\n")
     writer.close()
  }
}
