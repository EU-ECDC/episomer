package org.ecdc.episomer.fs

import org.ecdc.episomer.EpisomerActor
import org.ecdc.episomer.{Settings}
import akka.pattern.{ask, pipe}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import akka.actor.{ActorSystem, Actor, ActorLogging, Props}
import akka.pattern.ask
import akka.stream.scaladsl.{Source}
import akka.actor.ActorRef
import akka.Done
import akka.util.ByteString
import akka.util.{Timeout}
import java.time.LocalDateTime
import scala.collection.JavaConverters._
import scala.collection.mutable.{HashMap, ArrayBuffer, HashSet}
import java.time.temporal.{IsoFields, ChronoUnit}
import java.time.{Instant, ZoneOffset, LocalDateTime}
import java.nio.file.{Paths, Files}
import org.apache.spark.sql.{SparkSession, Column, Dataset, Row, DataFrame}
import org.apache.spark.sql.functions.{col, udf, input_file_name, explode, coalesce, when, lit, concat, struct, expr, lower}
import demy.storage.{Storage, FSNode, WriteMode}
import org.apache.lucene.document.{Document, TextField, StringField, IntPoint, BinaryPoint, LongPoint, DoublePoint, FloatPoint, Field, StoredField}
import org.apache.lucene.search.{Query, TermQuery, BooleanQuery, PrefixQuery, TermRangeQuery}
import org.apache.lucene.search.BooleanClause.Occur
import org.apache.lucene.index.Term
import org.apache.lucene.search.spell.LuceneDictionary
import demy.util.{log => l}
import scala.concurrent.ExecutionContext
import org.ecdc.episomer.geo.Geonames.Geolocate
import scala.util.{Try, Success, Failure}
import java.nio.charset.StandardCharsets
import spray.json.JsonParser
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool


class LuceneActor(conf:Settings) extends Actor with ActorLogging {
  implicit val executionContext = context.system.dispatchers.lookup("lucene-dispatcher")
  implicit val cnf = conf
  def receive = {
    case TopicPosts(network, topic, ts) =>
      implicit val holder = LuceneActor.holder
      Future{
        val dateMap = ts.items.groupBy(t => t.created_at.toString.take(10))
        dateMap.foreach{case (date, posts) =>
          val index = LuceneActor.getIndex("posts", posts(0).created_at)
          posts.foreach{t => index.indexPost(t, topic, network)}
        }
        dateMap.keys.toSeq
      }
      .map{dates =>
        LuceneActor.DatesProcessed("ok", dates)
      }
      .pipeTo(sender())
    case LuceneActor.GeolocatePostsRequest(TopicPosts(network, topic, ts)) =>
      implicit val holder = LuceneActor.holder
      Future{
        implicit val spark = conf.getSparkSession()
        implicit val storage = conf.getSparkStorage
        LuceneActor.add2Geolocate(TopicPosts(network, topic, ts), conf.forcedGeo.map(_.items), conf.forcedGeoCodes.map(_.items), conf.topicKeyWords.map(_.items(topic)))  
      }.onComplete {
        case Success(_) =>
        case Failure(t) => l.msg(s"Error during geolocalisation: ${t.getMessage}: ${t.getStackTrace.mkString("\n")}")
      }
    case LuceneActor.GeolocatedPostsCreated(GeolocatedPosts(items), dates) =>
      implicit val holder = LuceneActor.holder
      Future{
        val indexes = dates.flatMap(d => LuceneActor.getReadIndexes("posts", Some(d), Some(d)))
        items.foreach{g =>
          var found = false 
          for(index <- indexes if !found) {
            found = index.indexGeolocated(g)
          }
        }
        items.size
      }
      .map{c =>
        EpisomerActor.Success(s"$c geolocated properly processed")
      }
      .pipeTo(sender())
    case ts:LuceneActor.CommitRequest =>
      implicit val holder = LuceneActor.holder
      Future{
        LuceneActor.commitRequest()
      }
      .map{c =>
        EpisomerActor.Success(s"$c Commit done processed")
      }
      .pipeTo(sender())
    case LuceneActor.CloseRequest =>
      implicit val holder = LuceneActor.holder
      Future {
        LuceneActor.commit(closeDirectory = true)
        LuceneActor.closeSparkSession()
      }
      .map{c =>
        EpisomerActor.Success(s"$c Commit done processed")
      }
      .pipeTo(sender())
    case LuceneActor.SearchRequest(query, topics, from, to, countryCodes, mentions, users, estimatecount, hideUsers, action, max, byRelevance, jsonnl, caller) => 

      implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
      implicit val holder = LuceneActor.holder 

      val indexes = LuceneActor.getReadIndexes("posts", from, to).toSeq
      var sep = ""
      Future {
        if(!jsonnl) { 
          Await.result(caller ?  ByteString("[", ByteString.UTF_8), Duration.Inf)
        }
        var left = max
        indexes
          .iterator
          .flatMap{case i => 
            i.refreshReader()
            val qb = new BooleanQuery.Builder()
            query.map{q => 
              qb.add(i.parseQuery(q), Occur.MUST)
            }
            topics.map{tts =>
              val tb = new BooleanQuery.Builder()
              tts.map(t => tb.add(new TermQuery(new Term("topic", t)), Occur.SHOULD))
              qb.add(tb.build, Occur.MUST)
            }
            countryCodes.map{ccds =>
              val cb = new BooleanQuery.Builder()
              ccds.map(cc => cb.add(new TermQuery(new Term("text_loc.geo_country_code", cc.toUpperCase)), Occur.SHOULD)) 
              qb.add(cb.build, Occur.MUST)
            }
            if(!mentions.isEmpty || !users.isEmpty) {
              val ub = new BooleanQuery.Builder()
              mentions.map{mtns =>
                mtns.map{u => 
                  ub.add(new TermQuery(new Term("text", s"${u.toLowerCase}")), Occur.SHOULD) 
                  ub.add(new TermQuery(new Term("quoted_text", s"${u.toLowerCase}")), Occur.SHOULD)
                }
              }
              users.map{us =>
                us.map{u => 
                  ub.add(new TermQuery(new Term("user_name", s"$u")), Occur.SHOULD) 
                }
              }
              qb.add(ub.build, Occur.MUST)
            }
            qb.add(TermRangeQuery.newStringRange("created_date", from.map(d => d.toString.take(10)).getOrElse("0"), to.map(d => d.toString.take(10)).getOrElse("9"), true, true), Occur.MUST)
            val q = qb.build

            val noFilter = i.searchPosts(qb.build, Some(left), doCount = estimatecount, if(byRelevance) QuerySort.relevance else QuerySort.index).toSeq
            
            val posts = if(!mentions.isEmpty) {
              val reg = ("(?i)" + mentions.get.map(m => s"@$m\\b").mkString("|")).r
              val filtered = noFilter.filter{case (doc, totalCount) => 
                Seq(doc.getField("text").stringValue(), if(doc.getField("quoted_text") ==null) null else doc.getField("quoted_text").stringValue()).exists(t => t != null && !reg.findFirstIn(t).isEmpty)
              }
              filtered.map{case(doc, count) => (doc, (1.0*count*filtered.size/noFilter.size).toLong)}
            }
            else
              noFilter

            left = left - posts.size
            if(left < 0)
              posts.take(posts.size + left).map(p => (p, i))
            else 
              posts.map(p => (p, i))
          }
          .map{case ((doc, totalCount), index) => 
            val anoMap = Map(
              "text" -> ("@(\\w){1,15}", "@user"),
              "quoted_text" -> ("@(\\w){1,15}", "@user"),
              "user_name" -> (".+", "user")
            )
            val ret = EpiSerialisation.luceneDocFormat.customWrite(
              doc, 
              forceString=Set("id"), 
              if(estimatecount) Some(totalCount) else None, 
              transform = ( 
                if(hideUsers)
                 anoMap 
                else Map[String, (String, String)]()
              ),
              asArray = Set("tags","urls", "categories")
            )
            if(action == Some("delete")) {
              index.deleteDoc(doc, "topic_post_id")  
            } else if(action == Some("anonymise")) {
              index.searchReplaceInDoc(doc, pkName = "topic_post_id", updateMap = anoMap, textFields = Set("text", "quoted_text"))
            }
            ret
          }
          .take(max)
          .foreach{line => 
            Await.result(caller ? ByteString(
              s"${sep}${line.toString}\n", 
              ByteString.UTF_8
            ), Duration.Inf)
            if(!jsonnl) sep = ","
          }
        if(!action.isEmpty) {
          LuceneActor.commitRequest()
        }
        if(!jsonnl) {
          Await.result(caller ?  ByteString("]", ByteString.UTF_8), Duration.Inf)
        }
        caller ! Done
      }.recover {
        case e: Exception => 
          caller ! ByteString(s"[Stream--error]: ${e.getCause} ${e.getMessage}: ${e.getStackTrace.mkString("\n")}", ByteString.UTF_8)
      }.onComplete { case  _ =>
      }
    case LuceneActor.AggregatedRequest(collection, oTopic, from, to, filters, topFieldLimit, jsonnl, caller) => 
      implicit val timeout: Timeout = conf.fsBatchTimeout.seconds //For ask property
      implicit val holder = LuceneActor.holder
      val indexes = LuceneActor.getReadIndexes(collection, Some(from), Some(to)).toSeq
      var sep = ""
      val chunkSize = 500
                
   
      Future {
        if(!jsonnl) { 
          Await.result(caller ?  ByteString("[", ByteString.UTF_8), Duration.Inf)
        }

        val (parSeq, parallelism) = 
          Some(indexes.toSeq)
           .map(s => (s.par, s.size))
           .get
        if(parSeq.size > 0) {
          val pool = new ForkJoinPool(parallelism)
          parSeq.tasksupport = new ForkJoinTaskSupport(pool)
          parSeq.tasksupport = new ForkJoinTaskSupport(java.util.concurrent.ForkJoinPool.commonPool)
          parSeq
            .foreach{case i =>
              val builder = StringBuilder.newBuilder
              var toAdd = chunkSize
              val qb = new BooleanQuery.Builder()
              oTopic.map{topic => 
                val tqb = new BooleanQuery.Builder()
                topic.split(";").foreach(t => tqb.add(new TermQuery(new Term("topic", t)), Occur.SHOULD))
                qb.add(tqb.build, Occur.MUST)
              }
              qb.add(TermRangeQuery.newStringRange("created_date", from.toString.take(10), to.toString.take(10), true, true), Occur.MUST) 
              filters.foreach{
                case(field, value) => 
                  val fqb = new BooleanQuery.Builder()
                  value.split(";").foreach(v => fqb.add(new TermQuery(new Term(field, v)), Occur.SHOULD))
                  qb.add(fqb.build, Occur.MUST)
              }
              var first = true
              var z = 0
              i.searchPosts(
                qb.build, 
                doCount = false, 
                sort = topFieldLimit match {case Some(_) => QuerySort.indexReverse case None => QuerySort.index}, 
                topFieldLimit = topFieldLimit
              )
                .map{case (doc, totalCount) => EpiSerialisation.luceneDocFormat.write(doc)}
                .foreach{line =>
                  if(first == true) {
                    first = false
                  }
                  builder ++= s"${sep}${line.toString}\n"
                  toAdd = toAdd -1
                  if(toAdd == 0) {
                    pool.synchronized {
                      Await.result(caller ? ByteString(builder.toString, ByteString.UTF_8), Duration.Inf)
                    }
                    toAdd = chunkSize
                    builder.clear
                  }
                  if(!jsonnl) sep = ","
                }

              if(builder.size > 0) {
                pool.synchronized {
                  Await.result(caller ? ByteString(builder.toString, ByteString.UTF_8), Duration.Inf)
                }
                builder.clear
              }
            }
          pool.shutdown
        }
        if(!jsonnl) { 
          Await.result(caller ?  ByteString("]", ByteString.UTF_8), Duration.Inf)
        }
        caller ! Done
      }.recover {
        case e: Exception => 
          caller ! ByteString(s"[Stream--error]: ${e.getMessage}: ${e.getStackTrace.mkString("\n")}", ByteString.UTF_8)
      }.onComplete { case  _ =>
      }
    case LuceneActor.PeriodRequest(collection) =>
      implicit val holder = LuceneActor.holder
      
      Future{
        val sPath = Paths.get(conf.fsRoot, collection)
        if(!Files.exists(sPath))
          LuceneActor.PeriodResponse(None, None, None)
        else {
          val keys = Files.list(sPath).iterator.asScala.toSeq.map(p => p.getFileName.toString).distinct.sorted
          var firstDay = None.asInstanceOf[Option[String]]
          var lastDay = None.asInstanceOf[Option[String]]
          var lastHour = None.asInstanceOf[Option[Int]]
          Range(0, keys.length).flatMap{case i =>
            val iFirst = if(firstDay == None) Some(i) else None
            val iLast = if(lastDay == None) Some(keys.length - 1 - i) else None
            Seq(iFirst, iLast)
              .flatMap{j => j}
              .map{j =>
                  Try (LuceneActor.getIndex(collection, keys(j))) match {
                    case Success(index) => 
                      val searcher = index.useSearcher()
                      val dates = ArrayBuffer[String]()
                      val dateIter = (new LuceneDictionary(searcher.getIndexReader, "created_date").getEntryIterator())
                      var date = dateIter.next
                      while(date != null) {
                          if(date.utf8ToString.contains("-"))
                              dates += date.utf8ToString
                          date = dateIter.next
                      }
                      index.releaseSearcher(searcher)
                      if(dates.length > 0) {
                          // At this point we know we have found maximum or minimum dates
                          if(Some(j)==iFirst && firstDay == None) {
                              firstDay = Some(dates.min)
                          }
                          if(Some(j)==iLast && lastDay == None) {
                              lastDay = Some(dates.max)
                          }
                          if(Some(j)==iLast && lastHour == None && collection == "country_counts" ) { 
                            //finding out what is the last aggregated hour
                            lastHour = (
                                Iterator.range(23, -1, -1)
                                  .map(k => s"0$k".takeRight(2))
                                  .map{hour =>
                                    val q = "created_date:[\""+dates.max+"\" TO \""+dates.max+"\"] AND created_hour:'"+hour+"'"
                                    index.parseAndSearchPosts(
                                      query = q, 
                                      max = Some(1), 
                                      sort = QuerySort.index
                                    ).size match {
                                      case 0 => None 
                                      case _ => Some(hour)
                                    }
                                  }
                                  .flatMap(e => e)
                                  .toSeq
                                  .headOption
                                  .map(_.toInt)
                            )
                          }
                      }
                    case _ => None
                  }
              }
          }
          LuceneActor.PeriodResponse(firstDay, lastDay, lastHour)
        }
      }
      .pipeTo(sender())
    case LuceneActor.RecalculateHashRequest() =>
      implicit val holder = LuceneActor.holder
      Future {
        LuceneActor.recalculateHash()
        "Done!" 
      }.pipeTo(sender())
    case b => 
      Future(EpisomerActor.Failure(s"Cannot understund $b of type ${b.getClass.getName} as message")).pipeTo(sender())
  }
}

case class IndexHolder(
  var spark:Option[SparkSession] = None,
  val dirs:HashMap[String, String] = HashMap[String, String](),
  val indexes:HashMap[String, PostIndex] = HashMap[String, PostIndex](),
  val writeEnabled:Boolean = false,
  var toAggregate:HashMap[String, DataFrame] = HashMap[String, DataFrame](),
  var geolocating:Boolean = false,
  var aggregating:Boolean = false,
  var commiting:Boolean = false,
  var toGeolocate:Option[Dataset[Int]] = None,
  val commitRequests:HashSet[String] = HashSet[String]()
)
 
object LuceneActor {
  lazy val holder = LuceneActor.getHolder(writeEnabled = true)
  case class DatesProcessed(msg:String, dates:Seq[String] = Seq[String]())
  case class GeolocatedPostsCreated (geolocated:GeolocatedPosts, dates:Seq[Instant])
  case class GeolocatePostsRequest(items:TopicPosts)
  case class CommitRequest()
  case class CloseRequest()
  case class SearchRequest(
    query:Option[String], 
    topics:Option[Seq[String]], 
    from:Option[Instant], 
    to:Option[Instant], 
    countryCodes:Option[Seq[String]], 
    mentions:Option[Seq[String]], 
    users:Option[Seq[String]], 
    estimatecount:Boolean, 
    hideUsers:Boolean,
    action:Option[String],
    max:Int, 
    byRelevance:Boolean, 
    jsonnl:Boolean, 
    caller:ActorRef
  )
  case class AggregateRequest(items:TopicPosts)
  case class AggregatedRequest(collection:String, topic:Option[String], from:Instant, to:Instant, filters:Seq[(String, String)], topFieldLimit:Option[(String, String)], jsonnl:Boolean, caller:ActorRef)
  case class PeriodRequest(collection:String)
  case class PeriodResponse(first:Option[String], last:Option[String], last_hour:Option[Int])
  case class RecalculateHashRequest()
  def getHolder(writeEnabled:Boolean) = IndexHolder(writeEnabled = writeEnabled)
  def commitRequest()(implicit holder:IndexHolder, ec:ExecutionContext) {
    var dismissCommit = false
    holder.dirs.synchronized {
      if(holder.commiting)
        dismissCommit = true
      else
        holder.commiting = true
    }
    if(!dismissCommit) {
      val thread = new Thread {
        override def run {
          Future{
            var geoStopped = false
            var aggrStopped = false
            var messageShown = false
            while(!geoStopped || !aggrStopped) {
              holder.dirs.synchronized {
                if(!holder.geolocating) {
                  holder.geolocating = true
                  geoStopped = true
                }
                if(geoStopped && !holder.aggregating) {
                  holder.aggregating = true
                  aggrStopped = true
                }
              }
              if(!geoStopped || !aggrStopped) {
                if(!messageShown) {
                  l.msg(s"Delaying commit request to finish before. Waiting geolocation: ${!geoStopped}. Waiting aggregation ${!aggrStopped}")
                  messageShown = true
                }
                Thread.sleep(1000)
              }
            }
            LuceneActor.commit()
            holder.geolocating = false
            holder.aggregating = false
            holder.commiting = false
          }.onComplete {
              case Success(_) => 
                l.msg("Commit request finished successfully")
              case Failure(f) => 
                l.msg(s"Commit request failed with ${f.getMessage} \n ${f.getStackTrace.mkString("\n")} ")
          }
        }
      }
      thread.start
    } else {
      l.msg("Commit request dismissed since already requesteed")
    }
    Future("In progress")
  }

  def commit()(implicit holder:IndexHolder)  {
    commit(closeDirectory = true)
  }

  def commit(closeDirectory:Boolean= true)(implicit holder:IndexHolder)  {
    val now = System.nanoTime
    holder.dirs.synchronized {
      holder.dirs.foreach{ case (key, path) =>
          val i = holder.indexes(key)
          if(i.writeEnabled && i.writer.get.isOpen) {
            i.writer.get.commit()
            if(closeDirectory)
              i.writer.get.close()
          }
          if(closeDirectory) {
            i.reader.close()
            i.index.close()
          }
        case _ =>
      }
      if(closeDirectory) {
        holder.indexes.clear
        holder.dirs.clear
      }
    }
    if(closeDirectory)
      l.msg(s"commit done on ${(System.nanoTime - now) / 1000000000} secs")
  }
  def closeSparkSession()(implicit holder:IndexHolder) = {
    holder.spark match{
      case Some(spark) =>
        spark.stop()
      case _ =>
    }
  }

  def getIndex(collection:String, forInstant:Instant)(implicit conf:Settings, holder:IndexHolder):PostIndex = {
    getIndex(collection, getIndexKey(collection, forInstant))
  }
  def getIndex(collection:String, key:String)(implicit conf:Settings, holder:IndexHolder):PostIndex = {
    /*println("indexes")
    println(s"$collection - $key - ${holder.writeEnabled}")
    holder.dirs.foreach(println _)*/
    val path = s"$collection.$key"
    holder.dirs.synchronized {
      if(!holder.dirs.contains(path)) {
        conf.load()
        holder.dirs(path) = Paths.get(conf.epiHome, "fs", collection,  key).toString()
      }
      if(holder.indexes.get(path).isEmpty || !holder.indexes(path).isOpen) {
        holder.indexes(path) = PostIndex(holder.dirs(path), holder.writeEnabled)
      }
    }
    holder.indexes(path)
  }
  def getIndexKey(collection:String, forInstant:Instant) = {
    val timing = collection match {
      case "posts" => "week"
      case "country_counts" => "week"
      case "topwords" => "week"
      case "geolocated" => "week"
      case _ => "week"
    }
    timing match {
      case "week" =>
        Some(LocalDateTime.ofInstant(forInstant, ZoneOffset.UTC))
          .map(utc => s"${utc.get(IsoFields.WEEK_BASED_YEAR)*100 + utc.get(IsoFields.WEEK_OF_WEEK_BASED_YEAR)}")
          .map(s => s"${s.take(4)}.${s.substring(4)}")
          .get
      case _ => throw new Exception(s"Unknon grouping for collection $collection")
    }
  }    

  def getReadKeys(collection:String, from:Option[Instant], to:Option[Instant])(implicit conf:Settings, holder:IndexHolder) = 
     (from, to) match {
       case(Some(f), Some(t)) => 
         Range(0, ChronoUnit.DAYS.between(f, t.plus(1, ChronoUnit.DAYS)).toInt)
           .map(i => f.plus(i,  ChronoUnit.DAYS))
           .map(i => getIndexKey(collection, i))
           .distinct
           .filter(key => Paths.get(conf.epiHome, "fs", collection ,key).toFile.exists())
           .sortWith(_ > _)
        case _ =>
          val sPath = Paths.get(conf.fsRoot, collection)
          if(!Files.exists(sPath))
            Seq[String]()
          else 
            Files.list(sPath)
              .iterator
              .asScala
              .toSeq.map(p => p.getFileName.toString)
              .takeWhile{p =>
                (from.isEmpty || p >= getIndexKey(collection, from.get)) &&
                (to.isEmpty || p <= getIndexKey(collection, to.get))
              }
              .sortWith(_ > _)
     }
  
  def getReadIndexes(collection:String, from:Option[Instant], to:Option[Instant])(implicit conf:Settings, holder:IndexHolder) = 
    getReadKeys(collection, from, to).map(key => getIndex(collection, key))

  def add2Geolocate(posts:TopicPosts, forcedGeo:Option[Map[String, String]], forcedGeoCodes:Option[Map[String, String]], topics:Option[Set[String]])
    (implicit conf:Settings, holder:IndexHolder, ec: ExecutionContext) = 
  {
    var goGeo = false
    implicit val st = conf.getSparkStorage
    holder.dirs.synchronized {
      val toGeo = st.getNode(conf.togeolocatePath)
      if(posts.posts.items.size > 0) {
        if(toGeo.exists) toGeo.setContent("\n", WriteMode.append)
        toGeo.setContent(EpiSerialisation.topicPostsFormat.write(posts).toString, WriteMode.append)
      }
      val geolocating = st.getNode(conf.geolocatingPath)

      if(toGeo.exists && !geolocating.exists) {
        toGeo.move(geolocating, WriteMode.failIfExists)
      }
      if(!holder.geolocating && !holder.commiting) {
        holder.geolocating = true
        goGeo = true
      }
    } 
    Future{
      if(goGeo) {
        implicit val spark = conf.getSparkSession()
        LuceneActor.geolocatePosts(forcedGeo, forcedGeoCodes, topics)
        LuceneActor.add2Aggregate() 
       
        val geolocating = st.getNode(conf.geolocatingPath)
        geolocating.delete()
        holder.geolocating = false
      }
    }.onComplete{
       case scala.util.Success(_) =>
       case scala.util.Failure(t) =>
         l.msg(s"Error during geolocalisation: Retrying on next request ${t.getMessage} ${t.getStackTrace.mkString("\n")}" )
         holder.geolocating = false
    }
  }
  def tooBigToGeolocate()(implicit conf:Settings) = {
    val st = conf.getSparkStorage
    val biggest = Seq(
        Try(st.getNode(conf.toaggregatePath).getFileSize).toOption,
        Try(st.getNode(conf.aggregatingPath).getFileSize).toOption,
        Try(st.getNode(conf.geolocatingPath).getFileSize).toOption,
        Try(st.getNode(conf.togeolocatePath).getFileSize).toOption
      ).flatMap(e => e)
      .reduceOption((a, b) => if(a > b) a else b)
      .getOrElse(0L)
    biggest > 1000 * 1024 * 1024
  }
  def add2Aggregate()(implicit conf:Settings, holder:IndexHolder, ec: ExecutionContext) = {
    var goAggr = false
    implicit val st = conf.getSparkStorage
    val toAggr = st.getNode(conf.toaggregatePath)
    val aggregating = st.getNode(conf.aggregatingPath)
    val geolocating = st.getNode(conf.geolocatingPath)
    
    holder.dirs.synchronized { 
      if(geolocating.exists) {
        if(toAggr.exists) toAggr.setContent("\n", WriteMode.append)
        var firstLine = true
        for(line <- geolocating.getContentAsLines) {
          toAggr.setContent(s"${if(!firstLine) "\n" else ""}$line", WriteMode.append)
          firstLine = false
        }
      }

      if(toAggr.exists && !aggregating.exists) {
        toAggr.move(aggregating, WriteMode.failIfExists)
      }

      if(!holder.aggregating) {
        holder.aggregating = true
        goAggr = true
      }
    }
    
    Future{
      if(goAggr) {
        implicit val spark = conf.getSparkSession()
        Files.list(Paths.get(conf.collectionPath)).iterator.asScala.foreach{p =>
           val content = Files.lines(p, StandardCharsets.UTF_8).iterator.asScala.mkString("\n")
           Some(EpiSerialisation.collectionFormat.read(JsonParser(content)))
            .map{
             case collection =>
               LuceneActor.aggregatePosts(collection)
           }
        } 
        val aggregating = st.getNode(conf.aggregatingPath)
        aggregating.delete()
        holder.aggregating = false
      }
    }.onComplete{
       case scala.util.Success(_) => 
       case scala.util.Failure(t) =>
         holder.toAggregate.synchronized { 
           l.msg(s"Error during aggregating: Retrying on next request ${t.getMessage} ${t.getStackTrace.mkString("\n")} ")
           holder.aggregating = false
       }
    }
  }

  val defaultTextLangCols =
    Map(
      "text"->Some("lang")
      , "quoted_text"->Some("quoted_lang")
      //, "user_description"->Some("lang")
      //, "linked_user_description"->Some("linked_lang")
      //, "user_location"->Some("lang")
      //, "linked_user_location"->Some("linked_lang")
      //, "place_full_name"->None.asInstanceOf[Option[String]]
      //, "linked_place_full_name"->None.asInstanceOf[Option[String]]
    )
  def geolocatePosts( 
    forcedGeo:Option[Map[String, String]], 
    forcedGeoCodes:Option[Map[String, String]], 
    topics:Option[Set[String]]
  )(implicit spark:SparkSession, conf:Settings, storage:Storage, holder:IndexHolder) {
    import spark.implicits._
    val sc = spark.sparkContext
    val startTime = System.nanoTime
    holder.toGeolocate = {
      holder.toGeolocate match {
        case Some(df) => holder.toGeolocate
        case None => 
          val par =  50//conf.sparkCores.get
          Some(
            spark.sparkContext.parallelize(Seq.range(0, par)).repartition(par).unpersist()
              .mapPartitions{iter => iter.flatMap{ i =>
                val st = conf.getSparkStorage
                val geoNode = st.getNode(conf.geolocatingPath)
                val posts  = if(geoNode.exists) {
                  geoNode.getContentAsLines.map(json => EpiSerialisation.topicPostsFormat.read(JsonParser(json)))
                } else {
                  Iterator[TopicPosts]()
                }
                val r = posts.flatMap{case TopicPosts(network, topic, ts) => ts.items.filter(t => Math.abs(t.id.hashCode).toInt % par == i).map(t => (network, topic, t))}
                r
              }}
              .toDF
              .as[(String, String, Post)].toDF("network", "topic", "post")
              .select(col("network"), col("topic"), col("post.*"))
              .geolocate(
                textLangCols = defaultTextLangCols
                , minScore = conf.geolocationThreshold.get
                , maxLevDistance = 0
                , nBefore = conf.geoNBefore
                , nAfter = conf.geoNAfter
                , tokenizerRegex = conf.splitter
                , langs = conf.languages.get
                , geonames = conf.geonames
                , reuseGeoIndex = true
                , langIndexPath=conf.langIndexPath
                , reuseLangIndex = true
                , forcedGeo = forcedGeo
                , forcedGeoCodes = forcedGeoCodes
                , closestTo = topics
              )
              .select((
                //TODO: why we do not use the quoted_lang here!!!????
                Seq(col("network"), col("topic"), col("lang"), col("id").as("id"), col("created_at"))
                  ++  defaultTextLangCols.keys.toSeq
                      .map(c => s"${c}_loc")
                      .map(c => when(col(s"$c.geo_id").isNotNull, struct(Seq("geo_id","geo_name", "geo_code","geo_type","geo_country_code", "geo_longitude", "geo_latitude").map(cc => col(s"$c.$cc")):_*)).as(c))
                ):_*
              )
              .withColumn("is_geo_located"
                , when(defaultTextLangCols.keys.toSeq.map(c => col(s"${c}_loc").isNull).reduce( _ && _)
                 , false
                 ).otherwise(true)
              )
              .select(struct(
                  col("network"), 
                  col("topic"), 
                  col("id"), 
                  col("is_geo_located"), 
                  col("lang"),
                  col("text_loc"), 
                  col("quoted_text_loc"), 
                ).as("geo"), 
                col("created_at")
              )
              .as[(GeolocatedPost, Instant)]
              .mapPartitions{iter =>
                val indexes = HashMap[String, PostIndex]()
                iter.map{case (geo, createdAt) =>
                  val iKey = LuceneActor.getIndexKey("posts", createdAt)
                  implicit val holder = LuceneActor.holder
                  if(!indexes.contains(iKey)) {
                    indexes(iKey) = LuceneActor.getIndex("posts", iKey)
                    indexes(iKey).refreshReader()
                  }
                  indexes(iKey).indexGeolocated(geo)
                  1
                }
              }
          )
      }
    }

    val midTime = System.nanoTime
    val numGeo = holder.toGeolocate.get.unpersist.collect.sum
    val endTime = System.nanoTime
    LuceneActor.commit(closeDirectory = false)
    l.msg(s"${(endTime - midTime)/1e9d} secs for geolocating ${numGeo} posts")
  }


  def aggregatePosts(collection:Collection)(implicit spark:SparkSession, conf:Settings) {
    import spark.implicits._
    val startTime = System.nanoTime
    val sc = spark.sparkContext
    val holder = LuceneActor.holder
    
    val epiHome = conf.epiHome
    val aggr = collection.aggregation
    val columns = aggr.columns.map(v => aggr.params.map(qPars => qPars.foldLeft(v)((curr, iter) => curr.replace(s"@${iter._1}", iter._2))).getOrElse(v))
    val groupBy = aggr.groupBy.getOrElse(Seq[String]()).map(v => aggr.params.map(qPars => qPars.foldLeft(v)((curr, iter) => curr.replace(s"@${iter._1}", iter._2))).getOrElse(v))
    val filterBy = aggr.filterBy.getOrElse(Seq[String]()).map(v => aggr.params.map(qPars => qPars.foldLeft(v)((curr, iter) => curr.replace(s"@${iter._1}", iter._2))).getOrElse(v))
    val sortBy = aggr.sortBy.getOrElse(Seq[String]()).map(v => aggr.params.map(qPars => qPars.foldLeft(v)((curr, iter) => curr.replace(s"@${iter._1}", iter._2))).getOrElse(v))
    val sourceExp = aggr.sourceExpressions.getOrElse(Seq[String]()).map(v => aggr.params.map(qPars => qPars.foldLeft(v)((curr, iter) => curr.replace(s"@${iter._1}", iter._2))).getOrElse(v))
    val pks = collection.pks
    val collName = collection.name
    val dateCol = collection.dateCol
    val par =  conf.sparkCores.get
    if(!holder.toAggregate.contains(collection.name)) {
      holder.toAggregate(collection.name) = 
        Some(
           spark.sparkContext.parallelize(Seq.range(0, par))
            .mapPartitions{iter => iter.flatMap{ i =>
               val st = conf.getSparkStorage
               val aggrNode = st.getNode(conf.aggregatingPath)
               val posts  = if(aggrNode.exists) {
                 aggrNode.getContentAsLines.map(json => EpiSerialisation.topicPostsFormat.read(JsonParser(json)))
               } else {
                 Iterator[TopicPosts]()
               }
               posts
                 .flatMap{case TopicPosts(network, topic, tts) => tts.items.filter(t => Math.abs(t.id.hashCode).toInt % par == i).map(t => (network, topic, t))}
                 .map{case (network, topic, t) => (network, topic, t.id, LuceneActor.getIndexKey(collection.name, t.created_at))}
               
            }}
          )
          .map{rdd =>
            rdd.mapPartitions{iter => 
              val h = LuceneActor.holder
              val c = Settings(epiHome)
              conf.load()
              var lastKey = ""
              var index:PostIndex=null 
              (for((network, topic, id, key) <- iter) yield {  
                if(lastKey != key) {
                  index = LuceneActor.getIndex("posts", key)(c, h)
                  index.refreshReader
                }
                index.searchPostAsJSON(id, topic, network) match {
                  case Some(s) => Some(s)
                  case _ => 
                    l.msg(s"Cannot find post to aggregate $key, $id, $topic")
                    None
                }
              }).flatMap(ot => ot)
            }
          }
          .map(rdd => spark.read.schema(schemas.geoLocatedPostSchema).json(spark.createDataset(rdd)))
          .map{df => df.where(col("lang").isin(conf.languages.get.map(_.code):_*))}
          .map{
            case df if(sourceExp.size > 0) => 
              df.select(sourceExp.map(s => expr(s) ):_*)
            case df => df
          }
          .map{
            case df if(filterBy.size > 0) => 
              df.where(filterBy.map(w => expr(w)).reduce(_ && _))
            case df => df
          }
          .get
    }   
    val ds = Some(holder.toAggregate(collection.name))
          .map(df => df.unpersist)
          .map{
            case df if(groupBy.size == 0) => 
              df.select(columns.map(c => expr(c)):_*) 
            case df => 
              df.groupBy(groupBy.map(gb => expr(gb)):_*)
                .agg(expr(columns.head), columns.drop(1).map(c => expr(c)):_*)
          }
          .map{
            case df if(sortBy.size == 0) => 
              df 
            case df =>
              df.orderBy(sortBy.map(ob => expr(ob)):_*)
          }
          .map{df =>
            df.mapPartitions{iter => 
              implicit val holder = LuceneActor.holder
              implicit val conf = Settings(epiHome)
              conf.load()
              var lastKey = ""
              var index:PostIndex=null 
              var s = System.nanoTime
              (for(row <- iter) yield { 
                val key =  Instant.parse(s"${row.getAs[String](dateCol)}T00:00:00.000Z")
                if(lastKey != key)
                  index = LuceneActor.getIndex(collName, key)
                index.indexSparkRow(row = row, pk = pks, aggr = collection.aggr)
                1
              })
            }
          }.get
    val numAggr = ds.collect.sum 
    LuceneActor.commit(closeDirectory = false)(holder)
    l.msg(s"${(System.nanoTime - startTime)/1e9d} secs for aggregating ${numAggr} posts in ${collection.name}")
  }

  def recalculateHash()(implicit holder:IndexHolder, conf:Settings, ec:ExecutionContext){
    Files.list(Paths.get(conf.collectionPath)).iterator.asScala.foreach{p =>
      Some(Files.lines(p, StandardCharsets.UTF_8).iterator.asScala.mkString("\n"))
         .map(content => EpiSerialisation.collectionFormat.read(JsonParser(content)))
         .map{ case col =>
           l.msg(f"Recalculating hashes for ${col.name}")
           val pkName = col.pks.mkString("_")
           val ind  = LuceneActor.getReadIndexes(collection = col.name, from = None, to = None)
           ind.foreach{i => 
             i.searchPosts(new org.apache.lucene.search.MatchAllDocsQuery(), max = None, doCount = false, sort = QuerySort.index)
               .foreach{ case (doc, totalHits) =>
                  i.updateHash(doc = doc, pkName = pkName) 
               }
           }
           l.msg(f"Recalculating hashes for ${col.name} done!!")

         }

    }
    LuceneActor.commitRequest()
  }

}

