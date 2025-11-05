package org.ecdc.episomer

import org.ecdc.episomer.fs.{LuceneActor, TopicPosts, AlertClassification, TaggedAlert, AlertRun}
import org.ecdc.episomer.geo.{GeonamesActor, GeoTrainings }
import org.ecdc.episomer.alert.{AlertActor}
import org.apache.pekko.actor.{ActorSystem, Actor, Props}
import org.apache.pekko.stream.ActorMaterializer
import org.apache.pekko.pattern.{ask, pipe}
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.{HttpEntity, ContentType, ContentTypes, HttpResponse}
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.http.scaladsl.model.{StatusCodes, StatusCode}
import org.apache.pekko.http.scaladsl.marshalling.ToResponseMarshallable
import org.apache.pekko.util.{Timeout}
import spray.json.{JsValue}
import scala.concurrent.duration._
import scala.util.{Try, Success, Failure}
import scala.collection.JavaConverters._
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.Done
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.stream.OverflowStrategy
import org.apache.pekko.stream.CompletionStrategy
import org.apache.pekko.stream.scaladsl._
import org.apache.pekko.http.scaladsl.model.HttpEntity.{Chunked, Strict}
import org.apache.pekko.util.ByteString
import java.time.Instant
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets
import spray.json.JsonParser

object EpisomerActor {
  case class Failure(msg:String)
  case class Success(msg:String)
}
object API {

  var actorSystemPointer:Option[ActorSystem] = None
  var oLuceneRunner:Option[ActorRef] = None
  var oConf:Option[Settings] = None
  def run(epiHome:String) {
    import fs.EpiSerialisation._
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
            parameters("jsonnl".as[Boolean]?false) { (jsonnl) => 
              withRequestTimeout(conf.fsLongBatchTimeout.seconds) {
                entity(as[JsValue]) { json  =>
                  implicit val timeout: Timeout = conf.fsLongBatchTimeout.seconds //For ask property
                  Try(geoTrainingsFormat.read(json)) match {
                    case Success(GeoTrainings(trainingSet)) =>
                      val source: Source[org.apache.pekko.util.ByteString, ActorRef] = Source.actorRefWithBackpressure(
                        ackMessage = ByteString("ok")
                        , completionMatcher = {case Done => CompletionStrategy.immediately}
                        , failureMatcher = PartialFunction.empty
                      )
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
        } ~ {
          complete(StatusCodes.NotAcceptable,  EpisomerActor.Failure(s"Cannot find a route for uri $uri")) 
        }
      }
    

    Http().newServerAt("localhost", conf.fsPort).bind(route)
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
