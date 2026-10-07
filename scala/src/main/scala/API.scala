package org.ecdc.episomer

import org.ecdc.episomer.fs.{LuceneActor, TopicPosts, AlertClassification, TaggedAlert, AlertRun}
import org.ecdc.episomer.geo.{GeonamesActor, GeoTrainings }
import org.ecdc.episomer.alert.{AlertActor}
import org.apache.pekko.actor.{ActorSystem, Actor, Props}
import org.apache.pekko.stream.ActorMaterializer
import org.apache.pekko.pattern.{ask, pipe}
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{HttpEntity, ContentType, ContentTypes, HttpResponse, StatusCodes, StatusCode, MediaTypes}
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.server.directives.FileInfo
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.util.{Timeout}
import spray.json.{JsValue}
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.{Map => MMap}
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.Done
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.CompletionStrategy
import org.apache.pekko.stream.scaladsl._
import org.apache.pekko.http.scaladsl.model.HttpEntity.{Chunked, Strict}
import org.apache.pekko.util.ByteString
import org.apache.pekko.stream.scaladsl.FileIO
import java.time.Instant
import java.nio.file.{Paths, Files, Path, FileAlreadyExistsException, FileSystemException}
import java.nio.file.attribute.FileTime
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.io.{File, FileInputStream, InputStreamReader, BufferedReader, OutputStreamWriter, BufferedWriter}
import java.util.zip.GZIPInputStream
import java.nio.file.{StandardCopyOption, StandardOpenOption}
import java.nio.charset.StandardCharsets
import java.security.{DigestInputStream, MessageDigest}

import spray.json.{JsonParser, JsString, JsArray, JsObject, JsNumber}
import org.apache.log4j.Logger
import org.apache.log4j.Level

object EpisomerActor {
  case class Failure(msg:String)
  case class Success(msg:String)
}
object API {

  private def touchExistingFile(path:Path):Unit = {
    try {
      Files.setLastModifiedTime(path, FileTime.from(Instant.now()))
    } catch {
      case _:FileSystemException =>
        val channel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE)
        try {
          val firstByte = ByteBuffer.allocate(1)
          if(channel.read(firstByte, 0) == 1) {
            firstByte.flip()
            channel.write(firstByte, 0)
          } else {
            channel.write(ByteBuffer.wrap(Array[Byte](0)), 0)
            channel.truncate(0)
          }
        } finally {
          channel.close()
        }
    }
  }

  var actorSystemPointer:Option[ActorSystem] = None
  var oLuceneRunner:Option[ActorRef] = None
  var oConf:Option[Settings] = None
  val secrets = MMap[String, String]()
  // The R executable the secret scripts of jobs/ are run with.
  //
  // "Rscript" off the PATH is not necessarily the R this episomer is installed into. On Windows
  // R's bin directory is not on the PATH of a session started by the installer at all, and where
  // some Rscript *is* on the PATH it can belong to another installation: the R user library is per
  // version (win-library/4.4 against win-library/4.6), so the sub process starts, finds nothing in
  // its library and dies on "there is no package called 'episomer'". The /secret endpoint then
  // answers 406, i.e. it reports a keyring failure for something that never reached the keyring.
  //
  // The R side knows which R it is running and puts it here before starting this JVM
  // (rscript_exe_for_scala() and set_scala_r_env(), R/runners.R and R/spark-bridge.R). It puts its
  // .libPaths() in R_LIBS as well, which needs no forwarding of ours: ProcessBuilder starts a sub
  // process with a copy of this JVM's environment. The fallback keeps a JVM started by hand, with
  // neither variable set, working as before.
  def rscriptExecutable =
    sys.env.get("EPISOMER_RSCRIPT").filter(_.trim.nonEmpty).getOrElse("Rscript")

  // The sub process running one of the secret scripts of jobs/. Reading and writing set it up the
  // same way, only the script and the extra `value` parameter differ.
  def secretProcess(conf:Settings, script:String, secret:String, passphrase:String) = {
    val pb = new ProcessBuilder(
      rscriptExecutable,
      Paths.get(conf.epiHome, "jobs", script).toString,
      secret,
      conf.epiHome
    )
    sys.env.get("R_EPIHOME") match {
      case Some(path) =>
        pb.directory(new File(path))
      case _ =>
    }
    pb.environment().put("EPISOMER_KR_PASSWORD", passphrase)
    pb
  }
  // oBindHost / oBindPort override what properties.json says, for this run only.
  //
  // They arrive as the optional 'bindHost' / 'bindPort' arguments of the fsService command
  // (Episomer.main), put there by register_fs_runner_task(bind_host =, bind_port =) on the R side.
  // The case they exist for: the R setup wizard needs a database of its own while it retrains the
  // language models, and that database must not be found on the configured address by the services
  // that wait for the real one to become healthy before they start. Writing the temporary address
  // in properties.json - which is what used to happen - meant a shared configuration file left
  // wrong whenever the wizard was killed before it could put it back.
  def run(epiHome:String, oBindHost:Option[String] = None, oBindPort:Option[Int] = None) {
    import fs.EpiSerialisation._
    Logger.getLogger("pekko").setLevel(Level.DEBUG)
    Logger.getLogger("pekko.http").setLevel(Level.DEBUG)
    implicit val actorSystem = ActorSystem("episomer")
    actorSystemPointer = Some(actorSystem)
    implicit val executionContext = actorSystem.dispatcher
    implicit val conf = Settings(epiHome)
    conf.load
    oConf = Some(conf)
    
    val luceneRunner = actorSystem.actorOf(Props(classOf[LuceneActor], conf))
    val geonamesRunner = actorSystem.actorOf(Props(classOf[GeonamesActor], conf))
    val alertRunner = actorSystem.actorOf(Props(classOf[AlertActor], conf))
    oLuceneRunner = Some(luceneRunner)
    val pid = ProcessID.writePID(conf.pidPath("fs.java"))

    removeLockFiles()
    // posts the previous run indexed without committing are indexed again before anything else
    luceneRunner ! LuceneActor.RestoreJournalRequest()
    val route =
      extractUri { uri =>
        path("ping") {
          complete("pong")
        } ~
        path("posts") { // checks if path/url starts with model
          get {
            parameters("q".?, 
              "topic".?,
              "from".?, 
              "to".?, 
              "country_code".?, 
              "mentioning".?, 
              "user".?, 
              "estimatecount".as[Boolean]?false, 
              "hide_users".as[Boolean]?false, 
              "action".?, 
              "by_relevance".as[Boolean]?false, 
              "jsonnl".as[Boolean]?false, 
              "max".as[Int]?100
            ) { (query, topic, oFromStr, oToStr, countryCode, mentioning, user, estimatecount, hideUsers, action, byRelevance, jsonnl, max) => 
              implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
              val mask = "YYYY-MM-DDT00:00:00.000Z"
              val from = oFromStr.map(fromStr => Instant.parse(s"$fromStr${mask.substring(fromStr.size)}"))
              val to =  oToStr.map(toStr => Instant.parse(s"$toStr${mask.substring(toStr.size)}"))
              val source: Source[org.apache.pekko.util.ByteString, ActorRef] = Source.actorRefWithBackpressure(
                ackMessage = ByteString("ok")
                , completionMatcher = {case Done => CompletionStrategy.immediately}
                , failureMatcher = PartialFunction.empty
              )
              val topics = topic.map(cc => cc.toLowerCase.split("(\\s|;)+").toSeq)
              val countryCodes = countryCode.map(cc => cc.split("(\\s|;)+").toSeq)
              val mentions = mentioning.map(cc => cc.split("(\\s|;|@)+").toSeq)
              val users = user.map(cc => cc.split("(\\s|;|@)+").toSeq)
              val (actorRef, matSource) = source.preMaterialize()
              luceneRunner ! LuceneActor.SearchRequest(query, topics, from, to, countryCodes, mentions, users, estimatecount, hideUsers, action, max, byRelevance, jsonnl, actorRef) 
              complete(Chunked.fromData(ContentTypes.`application/json`, matSource.map{m =>
                if(m.startsWith(ByteString(s"[Stream--error]:"))) 
                  throw new Exception(m.decodeString("UTF-8"))
                else 
                  m
              }))
            }
          } ~
          post {
            withRequestTimeout(conf.fsQueryTimeout.seconds) {
              parameters("topic", "network", "geolocate".as[Boolean]?true) { (topic, network, geolocate) =>
                entity(as[JsValue]) { json  =>
                  implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
                  Try(postsFormat.read(json)) match {
                    case Success(posts) =>
                       val fut = (luceneRunner ? TopicPosts(network, topic.toLowerCase, posts))
                         .map{
                            case LuceneActor.DatesProcessed(m, dates) => 
                              if(geolocate) {
                                if(LuceneActor.tooBigToGeolocate())
                                  (StatusCodes.InternalServerError, 
                                    LuceneActor.DatesProcessed("Geolocate or aggregate is too slow. One of the geolocating or aggregating files in geo folder is bigger than the predefined limit of 500MB, stopping for safety reasons")
                                  )
                                else {
                                  luceneRunner ! LuceneActor.GeolocatePostsRequest(TopicPosts(network, topic.toLowerCase, posts))
                                  (StatusCodes.OK, LuceneActor.DatesProcessed(m, dates))
                                }
                              }
                              else (StatusCodes.OK, LuceneActor.DatesProcessed(m, dates))
                            case EpisomerActor.Failure(m) => (StatusCodes.NotAcceptable, LuceneActor.DatesProcessed(m))
                            case o => (StatusCodes.InternalServerError, LuceneActor.DatesProcessed(s"Cannot interpret $o as a message"))
                          }
                       complete(fut) 
                     case Failure(e) =>
                       println(s"Cannot interpret the provided body as an standardized post 1:\n ${json} $e\n, ${e.getStackTrace.mkString("\n")}") 
                       complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Cannot interpret the provided body an stantardised post:\n $e")) 
                  } 
                } ~ 
                  entity(as[String]) { value  => 
                  logThis(value)
                  complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"This endpoint expects json, got this instead: \n$value")) 
                }
              } ~ {
                complete(StatusCodes.NotImplemented, EpisomerActor.Failure(s"Missing expected parameter topic")) 
              }
            }
          }
        } ~ path("aggregate") { // checks if path/url starts with model
          get {
            parameters("serie", 
              "topic".?,
              "from"?(Instant.now.toString().take(10)), 
              "to"?(Instant.now.toString().take(10)), 
              "filter".as[String].*,
              "topField".?,
              "topFrequency".?,
              "jsonnl".as[Boolean]?false 
            ) { (collection, oTopic, fromStr, toStr, filtersStr, topField, topFrequency, jsonnl) => 
              implicit val timeout: Timeout = conf.fsBatchTimeout.seconds //For ask property
              val mask = "YYYY-MM-DDT00:00:00.000Z"
              val from = Instant.parse(s"$fromStr${mask.substring(fromStr.size)}")
              val to =  Instant.parse(s"$toStr${mask.substring(toStr.size)}")
              val filters = filtersStr.toSeq.map(fv => Some(fv).map(v => v.split(":")).map{case Array(v1, v2) => (v1, v2) case _ => throw new Exception(s"cannot parse $fv as column:value")}.get)
              val source: Source[org.apache.pekko.util.ByteString, ActorRef] = Source.actorRefWithBackpressure(
                ackMessage = ByteString("ok")
                , completionMatcher = {case Done => CompletionStrategy.immediately}
                , failureMatcher = PartialFunction.empty
              )
              val (actorRef, matSource) = source.preMaterialize()
              luceneRunner ! LuceneActor.AggregatedRequest(collection, oTopic.map(_.toLowerCase), from, to, filters, topField.map(tfi => topFrequency.map(tfr => (tfi, tfr))).flatten, jsonnl, actorRef) 
              complete(Chunked.fromData(ContentTypes.`application/json`, matSource.map{m =>
                if(m.startsWith(ByteString(s"[Stream--error]:"))) 
                  throw new Exception(m.decodeString("UTF-8"))
                else 
                  m
              }))
            }
          } ~ post {
            entity(as[String]) { json  =>
              Try(collectionFormat.read(JsonParser(json))) match {
                case Success(collection) =>
                  val path = Paths.get(conf.collectionPath)
                  Files.createDirectories(path)
                  Files.write(Paths.get(path.toString, s"${collection.name}.json"), json.getBytes(StandardCharsets.UTF_8)) 
                  complete(StatusCodes.OK, EpisomerActor.Success("OK"))
                case Failure(e) =>
                  println(s"Cannot interpret the provided body as an aggregation request:\n $e, ${e.getStackTrace.mkString("\n")}\n${json}") 
                  complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Cannot interpret the provided JSON body as an aggregate request:\n $e ${json}")) 
              }
            } ~  entity(as[String]) { value  => 
               complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"This endpoint expects json, got this instead: \n$value")) 
            }
          }
        } ~ path("period") { // checks if path/url starts with model
          get {
            parameters("serie") { (collection) => 
              implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
              val fut = (luceneRunner ? LuceneActor.PeriodRequest(collection))
                .map{
                   case LuceneActor.PeriodResponse(min, max, h) => (StatusCodes.OK, LuceneActor.PeriodResponse(min, max, h))
                   case org.apache.pekko.actor.Status.Failure(f)  => 
                     println(s"error found $f")
                     (StatusCodes.InternalServerError, LuceneActor.PeriodResponse(None, None, None))
                 }
              complete(fut) 
            }
          }
        } ~ path("recalculate-hash") { // checks if path/url starts with model
          withRequestTimeout(conf.fsBatchTimeout.seconds) {
          post {
              implicit val timeout: Timeout = conf.fsBatchTimeout.seconds //For ask property
              val fut = (luceneRunner ? LuceneActor.RecalculateHashRequest())
                .map{v =>
                   println(v)
                   v.toString
                 }
              complete(fut) 
            }
          }
        } ~ path("geolocated-posts") { 
          post {
            parameters("created".as[String].*) { createdStr => 
              val mask = "YYYY-MM-DDT00:00:00.000Z"
              val dates =  createdStr.map(d => Instant.parse(s"$d${mask.substring(d.size)}"))
              if(createdStr.size > 0) {
                entity(as[JsValue]) { json  =>
                  implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
                  Try(geolocatedPostsFormat.read(json)) match {
                    case Success(geolocateds) =>
                       val fut = (luceneRunner ? LuceneActor.GeolocatedPostsCreated(geolocateds, dates.toSeq))
                         .map{
                            case EpisomerActor.Success(m) => (StatusCodes.OK, EpisomerActor.Success(m))
                            case EpisomerActor.Failure(m) => (StatusCodes.NotAcceptable, EpisomerActor.Success(m))
                            case o => (StatusCodes.InternalServerError, EpisomerActor.Success(s"Cannot interpret $o as a message"))
                          }
                       complete(fut) 
                     case Failure(e) =>
                       println(s"Cannot interpret the provided body as geolocated array:\n $e, ${e.getStackTrace.mkString("\n")}") 
                       complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Cannot interpret the provided body as a gelocated array:\n $e")) 
                  } 
                } ~ entity(as[String]) { value  => 
                  logThis(value)
                  complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"This endpoint expects json, got this instead: \n$value")) 
                }
              } else {
                complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Missing parameter created with reference dates of geolocated posts")) 
              }
            }
          }
        } ~ path("commit") { // commit the posts sent
          post {
            withRequestTimeout(conf.fsBatchTimeout.seconds) {
            implicit val timeout: Timeout = conf.fsBatchTimeout.seconds //For ask property
            val fut =  (luceneRunner ? LuceneActor.CommitRequest())
              .map{
                 case EpisomerActor.Success(m) => (StatusCodes.OK, EpisomerActor.Success(m))
                 case EpisomerActor.Failure(m) => (StatusCodes.NotAcceptable, EpisomerActor.Success(m))
                 case o => (StatusCodes.InternalServerError, EpisomerActor.Success(s"Cannot interpret $o as a message"))
               }
             complete(fut)
            }
          }
        } ~ path("geotraining-set") {
          get {
            parameters("excludedLangs".as[String].*, "locationSamples".as[Boolean]?true, "jsonnl".as[Boolean]?false) { (excludedLangs, locationSamples, jsonnl) => 
              implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
              val source: Source[org.apache.pekko.util.ByteString, ActorRef] = Source.actorRefWithBackpressure(
                ackMessage = ByteString("ok")
                , completionMatcher = {case Done => CompletionStrategy.immediately}
                , failureMatcher = PartialFunction.empty
              )
              val (actorRef, matSource) = source.preMaterialize()
              geonamesRunner ! GeonamesActor.TrainingSetRequest(excludedLangs.toSeq, locationSamples, jsonnl, actorRef) 
              complete(Chunked.fromData(ContentTypes.`application/json`, matSource.map{m =>
                if(m.startsWith(ByteString(s"[Stream--error]:"))) 
                  throw new Exception(m.decodeString("UTF-8"))
                else 
                  m
              }))
            }
          } ~ 
          post {
            withRequestTimeout(conf.fsLongBatchTimeout.seconds) {
              parameters("jsonnl".as[Boolean]?false) { (jsonnl) => 
                entity(as[JsValue]) { json  =>
                  implicit val timeout: Timeout = conf.fsLongBatchTimeout.seconds //For ask property
                  
                  Try(geoTrainingsFormat.read(json)) match {
                    case Success(GeoTrainings(trainingSet)) =>
                      val source: Source[org.apache.pekko.util.ByteString, ActorRef] = Source.actorRefWithBackpressure(
                        ackMessage = ByteString("ok")
                        , completionMatcher = {case Done => CompletionStrategy.immediately}
                        , failureMatcher = PartialFunction.empty
                      )//.timeout(timeout)
                      val (actorRef, matSource) = source.preMaterialize()
                      geonamesRunner ! GeonamesActor.TrainLanguagesRequest(trainingSet, jsonnl, actorRef) 
                      complete(Chunked.fromData(ContentTypes.`application/json`, matSource.map{m =>
                        if(m.startsWith(ByteString(s"[Stream--error]:"))) 
                          throw new Exception(m.decodeString("UTF-8"))
                        else 
                          m
                      }))
                    case Failure(e) =>
                      println(s"Cannot interpret the provided body as a value train geolocation algorithm:\n $e, ${e.getStackTrace.mkString("\n")}") 
                      complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Cannot interpret the provided body as a value to train geolocation algorithm:\n $e")) 
                    } 
                } ~ 
                  entity(as[String]) { value  => 
                  logThis(value)
                  complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"This endpoint expects json, got this instead: \n$value")) 
                }
              }
            }
          }
        } ~ path("geolocate-text") {
          post {
            parameters("minScore".as[Int].?, "jsonnl".as[Boolean]?false) { (minScore, jsonnl) => 
              entity(as[JsValue]) { json  =>
                implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
                Try(textsToGeoFormat.read(json)) match {
                  case Success(toGeo) =>
                    val source: Source[org.apache.pekko.util.ByteString, ActorRef] = Source.actorRefWithBackpressure(
                      ackMessage = ByteString("ok")
                      , completionMatcher = {case Done => CompletionStrategy.immediately}
                      , failureMatcher = PartialFunction.empty
                    )
                    val (actorRef, matSource) = source.preMaterialize()
                    geonamesRunner ! GeonamesActor.GeolocateTextsRequest(
                      toGeo.items, 
                      minScore, 
                      conf.forcedGeo.map(_.items), 
                      conf.forcedGeoCodes.map(_.items), 
                      conf.topicKeyWords.map(_.items.flatMap(e => e._2).toSet), 
                      jsonnl, 
                      actorRef
                    ) 
                    complete(Chunked.fromData(ContentTypes.`application/json`, matSource.map{m =>
                      if(m.startsWith(ByteString(s"[Stream--error]:"))) 
                        throw new Exception(m.decodeString("UTF-8"))
                      else 
                        m
                    }))
                  case Failure(e) =>
                    println(s"Cannot interpret the provided body as a value to geolocate:\n $e, ${e.getStackTrace.mkString("\n")}") 
                    complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Cannot interpret the provided body as a value to geolocate:\n $e")) 
                } 
              } ~ 
                entity(as[String]) { value  => 
                logThis(value)
                complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"This endpoint expects json, got this instead: \n$value")) 
              }
            }
          }
        } ~ path("evaluate-alerts") {
          post {
            withRequestTimeout(conf.fsBatchTimeout.seconds) {
              entity(as[JsValue]) { json  =>
                implicit val timeout: Timeout = conf.fsBatchTimeout.seconds //For ask property
                Try(alertClassificationFormat.read(json)) match {
                  case Success(AlertClassification(alerts, runs)) =>
                     val fut = (alertRunner ? AlertActor.ClassifyAlertsRequest(alerts, runs))
                       .map{
                         case ac:AlertClassification => ac
                         case EpisomerActor.Failure(m) => {
                           throw new Exception(m)
                         }
                       }
                     complete(fut) 
                  case Failure(e) =>
                    println(s"Cannot interpret the provided body as a value to train classifier:\n $e, ${e.getStackTrace.mkString("\n")}") 
                    complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Cannot interpret the provided body as a value to train classifier:\n $e")) 
                } 
              } ~ 
                entity(as[String]) { value  => 
                logThis(value)
                complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"This endpoint expects json, got this instead: \n$value")) 
              }
            }  
          }
        } ~ pathPrefix("secret") {
          get {
            withRequestTimeout(conf.fsQueryTimeout.seconds) {
                parameters("secret".as[String], "passphrase".as[String]) { (secret, passphrase) =>
                  secrets.get(secret) match {
                      case Some(s) =>
                          complete(JsObject(Map(("status",JsString("ok")), ("value",JsString(s)), ("code",JsNumber(0)))).toString())
                      case None =>
		          val pb = secretProcess(conf, "get_secret.R", secret, passphrase)
                          val p = pb.start();
                          val stdout = new BufferedReader(new InputStreamReader(p.getInputStream()))
                          val stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()))
                          val out = stdout.readLine
                          val berr = new StringBuilder()
                          var line = stderr.readLine
                          while (line != null) {
                            berr.append(s"$line\n")
                            line = stderr.readLine
                          }
                          val err = berr.toString
                          val exitCode = p.waitFor()
                          if(out !=null && exitCode == 0) { 
                            secrets(secret) = out
                            complete(JsObject(Map(("status",JsString("ok")), ("value",JsString(out)), ("code",JsNumber(exitCode)))).toString())
                          }
                          else if(exitCode == 0) { 
                            complete(JsObject(Map(("status",JsString("ko")), ("message",JsString(err)), ("code",JsNumber(exitCode)))).toString())
                          } else {
                            complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Could not get the secret exit($exitCode):$err")) 
                          }
                  }
                }
            }
          } ~ post {
            withRequestTimeout(conf.fsQueryTimeout.seconds) {
                parameters("secret".as[String], "passphrase".as[String], "value".as[String]) { (secret, passphrase, value) =>
                  val pb = secretProcess(conf, "set_secret.R", secret, passphrase)
                  val p = pb.start();
                  val stdout = new BufferedReader(new InputStreamReader(p.getInputStream()))
                  val stderr = new BufferedReader(new InputStreamReader(p.getErrorStream()))
                  val stdin = new BufferedWriter(new OutputStreamWriter(p.getOutputStream()))
                  stdin.write(value)
                  stdin.newLine()
                  stdin.close()
                  val out = stdout.readLine
                  val berr = new StringBuilder()
                  var line = stderr.readLine
                  while (line != null) {
                    berr.append(s"$line\n")
                    line = stderr.readLine
                  }
                  val err = berr.toString
                  val exitCode = p.waitFor()
                  if(exitCode == 0) {
                    secrets(secret)=value
                    complete(JsObject(Map(("status",JsString("ok")), ("code",JsNumber(exitCode)))).toString())
                  } else {
                    complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"Could not set the secret exit($exitCode):$err")) 
                  }
                }
            }
          }
        } ~ pathPrefix("stopWords") {
          get {
            withRequestTimeout(conf.fsQueryTimeout.seconds) {
                parameters("code".as[String], "n".as[Int]?500) { (code, n) =>
                   val ret = ArrayBuffer[String]()
                   (conf.languages match {
                     case Some(ll) => ll.filter(l => l.code == code).headOption
                     case None => None
                   }).flatMap { lang => 
                        if(Files.exists(Paths.get(lang.vectorsPath)))
                            Some(lang.vectorsPath)
                        else None
                   }.map{ vectorPath => 
                         val fileStream = new FileInputStream(vectorPath)
                         try {
                           val gzipStream = new GZIPInputStream(fileStream)
                           val decoder = new InputStreamReader(gzipStream)
                           val buffered = new BufferedReader(decoder)
                           var i = 0
                           var line = buffered.readLine()
                           line = buffered.readLine() //ignoring first line as is the title
                           while(i < n && line != null) {
                               ret.append(line.split(" ")(0))
                               i = i + 1
                               line = buffered.readLine()
                           } 
                         } finally {
                           fileStream.close()
                         }
                         None
                    }
                  val jsonValues = ret.toVector.map(JsString(_))
                  complete(JsArray(jsonValues).toString())
                } 
            } 
          }
        } ~ pathPrefix("file") {
          get {
            withRequestTimeout(conf.fsQueryTimeout.seconds) {
              extractUnmatchedPath { path  =>
                val localPath = Paths.get(conf.epiHome, path.toString)
                val url = uri.toString
                val base = url.substring(0,url.lastIndexOf("?") match {case v if v <0 => url.length case v => v})
                parameters("action".as[String]?"get", "fullname".as[Boolean].?, "recursive".as[Boolean]?false) { (action, fullname, recursive) => 
                  if(action == "exists") {
                    complete(Files.exists(localPath).toString())
                  } else if(action == "md5sum") {
                    if(Files.isRegularFile(localPath)) {
                      val dig = MessageDigest.getInstance("MD5")
                      val is = Files.newInputStream(localPath)
                      val dis = new DigestInputStream(is, dig)
                      while(dis.read()>=0){}
                      val res = dig.digest()
                      val b = new StringBuilder()
                      b.append("\"")
                      res.foreach(v => b.append(String.format("%02x", v)))
                      b.append("\"")
                      complete(b.toString)
                    } else {
                      complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$localPath' is not something you can get the md5sum from")) 
                    }
                  } else if(action == "mtime") {
                    complete(recursiveLastModifiedTime(localPath) match {
                        case Some(v) => s"\"$v\""
                        case None => "null"
                    })
                  } else if(action == "list") {
                    val full = fullname.getOrElse(false)
                    val jsonValues = recursiveFileList(localPath, recursive = recursive)
                        .map{p =>
                            if(full) {
                                p.toString().replace(localPath.toString, base)
                            } else if(recursive) {
                                //recursive but not full
                                p.toString().replace(localPath.toString, path.toString).replaceAll("^/+", "")
                            } else {
                                //not recursive
                                (new java.io.File(p.toString)).getName
                            }
                        } 
                        .toVector
                        .map(JsString(_))
                    complete(JsArray(jsonValues).toString())
                  } else if(action == "listdir") {
                    val full = fullname.getOrElse(true)
                    val jsonValues = Files.list(localPath)
                        .filter(p => (new java.io.File(p.toString)).isDirectory)
                        .map(p => (new java.io.File(p.toString)).getName)
                        .map(p => if(full) f"$base/$p" else p)
                        .toList.asScala.toVector
                        .map(JsString(_))
                    complete(JsArray(jsonValues).toString())
                  } else { //get
                    if(Files.isRegularFile(localPath)) {
                      complete(HttpEntity.fromFile(MediaTypes.`application/octet-stream`, new File(localPath.toString)))
                    } else {
                      complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$localPath' is not something you can download")) 
                    }
                  }
                }
              }
            }
          } ~ 
          post {
            withRequestTimeout(conf.fsQueryTimeout.seconds) {
              parameters("action".as[String]?"upload", "createDirs".as[Boolean]?false, "ignoreIfExists".as[Boolean]?false, "append".as[Boolean]?false) { (action, createDirs, ignoreIfExists, append) => 
                extractUnmatchedPath { path  =>
                    val localPath = Paths.get(conf.epiHome, path.toString)
                    if(action == "createDir") {
                      if(Files.isDirectory(localPath) && ignoreIfExists) {
                        complete(s"Ok")
                      } else {
                        if(!Files.exists(localPath.getParent) && createDirs)
                          Files.createDirectories(localPath.getParent)
                        Files.createDirectory(localPath)
                        complete(s"Directory created")
                      }
                    } else if(action == "touch") {
                       try {
                          Files.createFile(localPath);
                       } catch  { case e:FileAlreadyExistsException =>
                          touchExistingFile(localPath)
                       }
                       complete("ok") 
                    } else if(action == "copy") {
                      entity(as[JsValue]) { 
                        case JsString(dest)  =>
                            if(dest.contains("..") || (new java.io.File(dest)).isAbsolute())
                                complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$dest' is not something you can copy to")) 
                            else { 
                                val localDest = Paths.get(conf.epiHome, dest.toString)
                                Files.copy(localPath, localDest, StandardCopyOption.REPLACE_EXISTING )
                                complete("ok") 
                            }
                        case dest => 
                            complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$dest' is not something you can copy to")) 
                      }
                    } else if(action == "move") {
                      entity(as[JsValue]) { 
                        case JsString(dest)  =>
                            if(dest.contains("..") || (new java.io.File(dest)).isAbsolute())
                                complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$dest' is not something you can copy to")) 
                            else { 
                                val localDest = Paths.get(conf.epiHome, dest.toString)
                                Files.move(localPath, localDest, StandardCopyOption.REPLACE_EXISTING )
                                complete("ok") 
                            }
                        case dest => 
                            complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$dest' is not something you can copy to")) 
                      }
                    } else { //upload
                      val tempPath = Paths.get(conf.epiHome, f"${path.toString}.part")
                      if(!Files.exists(localPath.getParent) && createDirs)
                        Files.createDirectories(localPath.getParent)
                      if(Files.isDirectory(localPath.getParent()) && (!Files.exists(localPath) || Files.isRegularFile(localPath))) {
                        fileUpload("data") { case(metadata, byteSource) => 
                          if(Files.exists(tempPath))
                              Files.delete(tempPath)
                          val sink = if(!append) FileIO.toPath(tempPath) else  FileIO.toPath(localPath,  Set(StandardOpenOption.WRITE, StandardOpenOption.APPEND, StandardOpenOption.CREATE)) 
                          val writeResult = byteSource.runWith(sink)
                          onSuccess(writeResult) { result =>
                              result.status match {
                                case Success(_) => {
                                    if(!append)
                                        Files.move(tempPath, localPath, StandardCopyOption.REPLACE_EXISTING,  StandardCopyOption.ATOMIC_MOVE )
                                    complete(s"Successfully written ${result.count} bytes")
                                }
                                case Failure(e) => 
                                    complete(StatusCodes.InternalServerError, EpisomerActor.Failure(s"Error while uploading the file '$e'\n ${e.getStackTrace.mkString("\n")}"))
                            }
                          } 
                        }
                      } else {
                        complete(StatusCodes.NotAcceptable, EpisomerActor.Failure(s"'$localPath' cannot is not a file that can be created")) 
                      }
                    }
                }
              }
            }
          }
        } ~ {
          complete(StatusCodes.NotAcceptable,  EpisomerActor.Failure(s"Cannot find a route for uri $uri")) 
        }
      }
    

    // The arguments win over the configuration file: a caller naming an interface for this one run
    // has said something more specific than a properties.json every instance of the data directory
    // shares. Absent them, fs_url is bound verbatim (conf.fsBindHost is that value with its scheme
    // removed) - if it is not an address this machine holds, the bind fails and says so, which is
    // the diagnosis the operator needs rather than a service listening somewhere unasked for.
    //
    // The scheme is stripped off the argument as well, so that it may be written either way round -
    // fs_url carries one ("http://localhost") and newServerAt() takes an interface, not a URL.
    val bindHost = oBindHost
      .map(h => h.replaceAll("^https?://", "").replaceAll("/+$", ""))
      .filter(_.nonEmpty)
      .getOrElse(conf.fsBindHost)
    val bindPort = oBindPort.getOrElse(conf.fsPort)
    println(s"Binding the episomer API on $bindHost:$bindPort")
    Http().newServerAt(bindHost, bindPort).bind(route)
  }
  // Same two hazards as recursiveLastModifiedTime() below, in the same three lines: a live index
  // directory can vanish mid-walk, and Files.list() holds an open directory handle until it is
  // closed.
  def recursiveFileList(path:Path, recursive:Boolean = true):Seq[Path] = {
    if(!Files.exists(path))
        Seq()
    else if(Files.isDirectory(path)) {
        var stream:java.util.stream.Stream[Path] = null
        try {
            stream = Files.list(path)
            if(recursive)
                stream.map(recursiveFileList(_)).toList().asScala.flatMap(d => d).toSeq
            else
                stream.toList().asScala.toSeq
        } catch {
            case _:java.nio.file.NoSuchFileException => Seq()
        } finally {
            if(stream != null) stream.close()
        }
    } else {
        Seq(path)
    }
  }
  // The latest modification time under `path`, or None when there is nothing to date.
  //
  // Nothing here can be atomic: the directories being walked are live Lucene indices, and Lucene
  // deletes a segment file the moment it has merged it into a bigger one. So a file listed a
  // moment ago may be gone by the time it is asked for its attributes, and that used to escape as
  //     java.nio.file.NoSuchFileException: ...\fs\posts\2026.36\_kd_Lucene103_0.tip
  // which Pekko's default handler turned into a 500. R does not survive that: health_check() asks
  // for this on fs/posts every five seconds from the sleep of the detect scheduler
  // (last_fs_updates() -> epifile.mtime()), so one segment merge landing between the listing and
  // the stat killed the whole detect loop with "Execution halted".
  //
  // A file that no longer exists has no modification time, which is exactly what None means, and
  // dropping it is also the right answer: it cannot be the newest thing under a directory that
  // outlived it.
  //
  // The stream is closed, which it was not. Files.list() holds an open directory handle until it
  // is, one per directory per request, and on Windows an open handle on a directory also stands in
  // the way of deleting what is inside it - so leaking them on a path polled every five seconds
  // worked against the very merges above.
  def recursiveLastModifiedTime(path:Path):Option[java.nio.file.attribute.FileTime] = {
    if(!Files.exists(path))
        None
    else if(Files.isDirectory(path)) {
        var stream:java.util.stream.Stream[Path] = null
        try {
            stream = Files.list(path)
            stream.map(recursiveLastModifiedTime(_)).toList().asScala.flatMap(d => d).maxOption
        } catch {
            // the directory itself went away while it was being walked
            case _:java.nio.file.NoSuchFileException => None
        } finally {
            if(stream != null) stream.close()
        }
    } else {
        try Some(Files.getLastModifiedTime(path))
        catch { case _:java.nio.file.NoSuchFileException => None }
    }
  }
  def logThis(log:String) = {
    import java.nio.file.StandardOpenOption
    Files.write(Paths.get(s"${System.getProperty("user.home")}/pekko-epi.json"), log.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
  }
  def completeTry(tryed:Try[ToResponseMarshallable], uri:String) = {
    tryed match {
      case Success(entity) =>
        complete(entity)
      case Failure(e) =>
        complete(defaultException(e, uri))
    }
  }
  def defaultException(e:Throwable, uri:String) = {
    val message = s"Request to $uri could not be handled normally" + "\n" + e.toString() + "\n" + e.getStackTrace.mkString("\n")
    println(message)
    HttpResponse(StatusCodes.InternalServerError, entity = message)
  }
  def stop() {
    oLuceneRunner.map{lr =>
      implicit val timeout: Timeout =oConf.get.fsQueryTimeout.seconds //For ask property
      implicit val executionContext = actorSystemPointer.get.dispatcher
      (lr ? LuceneActor.CloseRequest)
        .onComplete{_ => 
          actorSystemPointer.map(as => as.terminate)
          actorSystemPointer = None
          oLuceneRunner = None
      }
    }
  }
  def removeLockFiles()(implicit conf:Settings) { 
    if(Files.exists(Paths.get(conf.fsRoot)))
      Files.walk(Paths.get(conf.fsRoot))
        .iterator.asScala.foreach{p =>
          if(p.endsWith("write.lock"))
            Files.delete(p)
        }
  }
} 
