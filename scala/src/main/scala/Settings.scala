package org.ecdc.episomer

import spray.json._
import java.nio.file.{Paths, Files}
import org.ecdc.episomer.{JavaBridge, Language}
import org.ecdc.episomer.geo.Geonames
import org.ecdc.episomer.fs.EpiSerialisation
import org.apache.spark.sql.{SparkSession}
import demy.storage.{Storage, FSNode}

case class Settings(epiHome:String) {
  var _properties:Option[JsObject] = None
  def load() = {
    val psource = scala.io.Source.fromFile(Paths.get(epiHome, "properties.json").toString)
    val plines = try psource.mkString finally psource.close() 
    _properties = Some(plines.parseJson.asJsObject)
    this
  }
  def loadJson(relativePath:String) = {
    val path = Paths.get(epiHome, relativePath)
    if(Files.exists(path)) {
      val psource = scala.io.Source.fromFile(path.toString, "UTF-8")
      val plines = try psource.mkString finally psource.close() 
      Some(plines.parseJson.asJsObject)
    }
    else None
  }

  def properties = _properties match {
    case Some(p) => p
    case None => throw new Exception("Please call load() before calling properties on these settings")
  } 

  def geonamesSimplify = properties.fields.get("geonames_simplify").map(v => v.asInstanceOf[JsBoolean].value)
  def knownUsers = 
    properties.fields.get("known_users").map(v => 
      v.asInstanceOf[JsArray]
        .elements
        .map(v => v.asInstanceOf[JsString].value)
    )
  def languages =  
    properties.fields.get("languages").map(v => 
      v.asInstanceOf[JsArray]
        .elements
        .map(v => v.asInstanceOf[JsObject].fields
          .mapValues(f => f.asInstanceOf[JsString].value)
        ).map(f => Language(f("name"), f("code"), f("vectors")))
    )

  def sparkCores = properties.fields.get("spark_cores").map(v => v.asInstanceOf[JsNumber].value.toInt)
  def sparkMemory =  properties.fields.get("spark_memory").map(v => v.asInstanceOf[JsString].value)
  def winutils =  System.getProperty("os.name").toLowerCase.contains("windows") match {
    case true => Some(Paths.get(this.epiHome, "hadoop").toString)
    case fakse => None
  }
  def langIndexPath =  Paths.get(epiHome, "geo","lang_vectors.index").toString 
  def togeolocatePath =  Paths.get(epiHome, "geo","togeolocate.json").toString 
  def geolocatingPath =  Paths.get(epiHome, "geo","geolocating.json").toString 
  def toaggregatePath =  Paths.get(epiHome, "geo","toaggregate.json").toString 
  def aggregatingPath =  Paths.get(epiHome, "geo","aggregating.json").toString 
  def collectionPath =  Paths.get(epiHome, "collections").toString 
  def geolocationThreshold = properties.fields.get("geolocation_threshold").map(v => if(v.isInstanceOf[JsString]) v.asInstanceOf[JsString].value.toInt else v.asInstanceOf[JsNumber].value.toInt)
  def geolocationStrategy = Some("demy.mllib.index.PredictStrategy")
  def geoNBefore = 10
  def geoNAfter = 4
  def geonames = 
    Geonames(
      Paths.get(epiHome, "geo", "allCountries.txt").toString,  
      Paths.get(epiHome, "geo").toString,  
      properties.fields.get("geonames_simplify").map(v => v.asInstanceOf[JsBoolean].value).get
    )
  def pidPath(name:String) = Paths.get(epiHome, s"$name.PID").toString
  def fsRoot = Paths.get(epiHome, "fs").toString
  def fsBatchTimeout = properties.fields.get("fs_batch_timeout").map(v => v.asInstanceOf[JsNumber].value.toInt).getOrElse(60*60)
  def fsQueryTimeout = properties.fields.get("fs_query_timeout").map(v => v.asInstanceOf[JsNumber].value.toInt).getOrElse(60)
  def fsLongBatchTimeout = properties.fields.get("fs_long_batch_timeout").map(v => v.asInstanceOf[JsNumber].value.toInt).getOrElse(60*60*24)
  def fsPort = properties.fields.get("fs_port").map(v => v.asInstanceOf[JsNumber].value.toInt).getOrElse(8080)
  def fsUrl = properties.fields.get("fs_url").map(v => v.asInstanceOf[JsString].value).getOrElse("http://localhost")

  // The interface the API actually binds to (API.scala, Http().newServerAt).
  //
  // fs_url is a *bind* address, and it is stored in properties.json - a file every instance sharing
  // the data directory reads. R/fs.R carries the same paragraph at its top for the client half of
  // this problem: a containerised fs writes there the name it binds to *inside* the Docker network
  // ("http://fs", EPISOMER_OPTION_FS_URL in docker/compose.yml), and that name exists nowhere else.
  // Any other process starting the database from that same directory - the R wizard on the host is
  // the one that actually happened - then bound a name nothing could resolve and died with
  //     java.net.SocketException: Unresolved address
  // before serving a single request, having already taken the fs lease and written its PID file.
  //
  // So the host is checked before it is used, and localhost stands in when it does not resolve
  // here. That is the mirror of rule 4 of get_scala_api_endpoint() (R/fs.R), which falls back to
  // localhost:fs_port for exactly the same reason - so both ends land on the same address, and in
  // the container case the port is published on the host anyway.
  //
  // Only *resolvability* is tested, deliberately. A name that resolves to an address this machine
  // does not hold is a different mistake with a diagnosis of its own (BindException: cannot assign
  // requested address), and silently binding localhost instead of an interface the operator named
  // on purpose would hide it.
  def fsBindHost = {
    val configured = fsUrl.replaceAll("^https?://", "").replaceAll("/+$", "")
    if(hostResolvesHere(configured))
      configured
    else {
      println(s"fs_url host '$configured' cannot be resolved on this machine: binding 'localhost' instead. It was most likely written in properties.json by another instance - a container binds its Docker service name, which means nothing outside that network.")
      "localhost"
    }
  }

  def hostResolvesHere(host:String) =
    host.nonEmpty && (
      try { java.net.InetAddress.getByName(host); true }
      catch { case _:Throwable => false }
    )
  
  def splitter = properties.fields.get("postSplitterRegex").map(v => v.asInstanceOf[JsString].value).getOrElse(Settings.defaultSplitter)
  def forcedGeo = this.loadJson("geo/forced-geo.json").map(json => EpiSerialisation.forcedGeoFormat.read(json)) 
  def forcedGeoCodes = this.loadJson("geo/forced-geo-codes.json").map(json => EpiSerialisation.forcedGeoCodesFormat.read(json)) 
  def topicKeyWords = this.loadJson("geo/topic-keywords.json").map(json => EpiSerialisation.topicKeyWordsFormat.read(json)) 
  def getSparkSession() = {
    Settings.synchronized {
      this.load()
      if(Settings.spark.isEmpty) {
        Settings.spark =  Some(JavaBridge.getSparkSession(this.sparkCores.getOrElse(0))) 
      }
    }
    Settings.spark.get
  }
  def closeSparkSession() = {
    Settings.spark match{
      case Some(spark) =>
        spark.stop()
      case _ =>
    }
  }
  def getSparkStorage = Storage.getSparkStorage

}
object Settings {
  var spark:Option[SparkSession] = None
  def defaultSplitter = "(http|https|HTTP|HTTPS|ftp|FTP)://(\\S)+|[^\\p{L}]|@+|#+|\\bRT\\b+|\\bvia\\b+|\\bvía\\b+|((?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z]))+"
}
