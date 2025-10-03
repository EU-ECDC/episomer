package org.ecdc.episomer.fs

import org.ecdc.episomer.EpisomerActor
import org.ecdc.episomer.geo.{GeoTrainings, GeoTraining, GeoTrainingSource, GeoTrainingPart}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{JsString, JsNull, JsValue, JsNumber, DefaultJsonProtocol, JsonFormat, RootJsonFormat, JsObject, JsArray, JsBoolean, JsField, JsonParser}
import java.time.Instant
import java.time.format.DateTimeFormatter
import java.util.Locale 
import scala.util.Try
import org.apache.lucene.document.{Document, TextField, StringField, IntPoint, BinaryPoint, LongPoint, DoublePoint, FloatPoint, Field, StoredField, DoubleDocValuesField}
import org.apache.lucene.index.IndexableField
import scala.collection.JavaConverters._
import java.net.URLDecoder
import org.apache.spark.sql.types._

object schemas {
  val geoLocationSchema = StructType(Seq(
     StructField("geo_code",StringType,true)
     , StructField("geo_country_code",StringType,true)
     , StructField("geo_id",LongType,true)
     , StructField("geo_latitude",DoubleType,true)
     , StructField("geo_longitude",DoubleType,true)
     , StructField("geo_name",StringType,true)
     , StructField("geo_type",StringType,true)
    ))

  val geoLocatedPostSchema = {
      StructType(Seq(
        StructField("network",StringType,true)
        , StructField("topic",StringType,true)
        , StructField("id",StringType,true)
        , StructField("uri",StringType,true)
        , StructField("created_at",TimestampType,true)
        , StructField("created_date",StringType,true)
        , StructField("user_name",StringType,true)
        , StructField("text",StringType,true)
        , StructField("lang",StringType,true)
        , StructField("quoted_text",StringType,true)
        , StructField("quoted_lang",StringType,true)
        , StructField("is_geo_located",BooleanType,true)
        , StructField("is_quote",BooleanType,true)
        , StructField("text_loc", geoLocationSchema,true)
        , StructField("quoted_text_loc", geoLocationSchema,true)
        , StructField("tags", ArrayType(StringType, true),true)
        , StructField("urls", ArrayType(StringType, true),true)
        , StructField("categories", ArrayType(StringType, true),true)
        ))
    }
}

case class Posts(items:Seq[Post])
case class TopicPosts(netowrk:String, topic:String, posts:Posts)
case class Post(
    id:String, 
    uri:String, 
    created_at:Instant, 
    user_name:String,  
    text:String, 
    lang:String, 
    is_quote:Boolean, 
    quoted_text:Option[String], 
    quoted_lang:Option[String], 
    tags:Option[Seq[String]], 
    urls:Option[Seq[String]], 
    categories:Option[Seq[String]]
)

case class GeolocatedPosts(items:Seq[GeolocatedPost])
case class Location(geo_code:String, geo_country_code:String,geo_id:Long, geo_latitude:Double, geo_longitude:Double, geo_name:String, geo_type:String)
case class GeolocatedPost(network:String, var topic:String, id:String, is_geo_located:Boolean, lang:String, text_loc:Option[Location], quoted_text_loc:Option[Location]) {
  def fixTopic = {
    this.topic = URLDecoder.decode(this.topic, "UTF-8").toLowerCase
    this
  }
}

case class AlertClassification(alerts:Seq[TaggedAlert], runs:Option[Seq[AlertRun]])
case class TaggedAlert(
  id:String,
  date:String,
  topic:String,
  country:String,
  number_of_posts:Int, 
  topwords:Option[String],
  topposts:Map[String, Seq[String]],
  given_category:Option[String],
  episomer_category:Option[String],
  test:Option[Boolean],
  augmented:Option[Boolean],
  deleted:Option[Boolean],

) {
  def setEpisomerCategory(cat:String) =  
    TaggedAlert(
      id = id,
      date = date,
      topic = topic,
      country = country,
      number_of_posts = number_of_posts, 
      topwords = topwords,
      topposts = topposts,
      given_category = given_category,
      episomer_category = Some(cat),
      test = test,
      augmented = augmented,
      deleted = deleted
    )
}

case class AlertRun(
  ranking:Int,
  models:String,
  alerts:Option[Int],
  runs:Int,
  f1score:Double,
  accuracy:Double, 
  precision_by_class:String,
  sensitivity_by_class:String,
  fscore_by_class:String,
  last_run:Option[String],
  balance_classes:Option[Boolean],
  force_to_use:Option[Boolean],
  active:Option[Boolean],
  documentation:Option[String],
  custom_parameters:Option[Map[String, String]] 
) {
  def setRanking(rank:Int) = 
     AlertRun(
       ranking = rank,
       models = models,
       alerts = alerts,
       runs = runs,
       f1score = f1score,
       accuracy = accuracy, 
       precision_by_class = precision_by_class,
       sensitivity_by_class = sensitivity_by_class,
       fscore_by_class = fscore_by_class,
       last_run = last_run,
       balance_classes = balance_classes,
       force_to_use = force_to_use,
       active = active,
       documentation = documentation,
       custom_parameters = custom_parameters 
     ) 
  def setCount(count:Int) = 
     AlertRun(
       ranking = ranking,
       models = models,
       alerts = Some(count),
       runs = runs,
       f1score = f1score,
       accuracy = accuracy, 
       precision_by_class = precision_by_class,
       sensitivity_by_class = sensitivity_by_class,
       fscore_by_class = fscore_by_class,
       last_run = last_run,
       balance_classes = balance_classes,
       force_to_use = force_to_use,
       active = active,
       documentation = documentation,
       custom_parameters = custom_parameters 
     ) 
}

case class TopicKeyWords(items:Map[String, Set[String]])
case class ForcedGeo(items:Map[String, String])
case class ForcedGeoCodes(items:Map[String, String])


case class TextsToGeo(items:Seq[TextToGeo])
case class TextToGeo(id:String, text:String, lang:Option[String])



case class Collection(
  name:String,
  dateCol:String,
  pks:Seq[String],
  aggr:Map[String, String],
  aggregation:Aggregation 
)

case class Aggregation(
  columns:Seq[String], 
  groupBy:Option[Seq[String]], 
  filterBy:Option[Seq[String]], 
  sortBy:Option[Seq[String]], 
  sourceExpressions:Option[Seq[String]], 
  params:Option[Map[String, String]],
)


object EpiSerialisation
  extends SprayJsonSupport
    with DefaultJsonProtocol {
    implicit val luceneSuccessFormat = jsonFormat1(EpisomerActor.Success.apply)
    implicit val luceneFailureFormat = jsonFormat1(EpisomerActor.Failure.apply)
    implicit val datesProcessedFormat = jsonFormat2(LuceneActor.DatesProcessed.apply)
    implicit val periodResponseFormat = jsonFormat3(LuceneActor.PeriodResponse.apply)
    implicit val commitRequestFormat = jsonFormat0(LuceneActor.CommitRequest.apply)
    implicit val locationRequestFormat = jsonFormat7(Location.apply)
    implicit val geolocatedPostFormat = jsonFormat7(GeolocatedPost.apply)
    implicit val aggregationFormat = jsonFormat6(Aggregation.apply)
    implicit val collectionFormat = jsonFormat5(Collection.apply)
    implicit val textToGeoFormat = jsonFormat3(TextToGeo.apply)
    implicit val taggedAlertsFormat = jsonFormat12(TaggedAlert.apply)
    implicit val alertRunFormat = jsonFormat15(AlertRun.apply)
    implicit val alertClassificationFormat = jsonFormat2(AlertClassification.apply)


    implicit object postFormat extends RootJsonFormat[Post] {
      def write(t: Post) =
        JsObject(Map(
          "id" -> JsString(t.id), 
          "uri" -> JsString(t.uri), 
          "created_at" -> JsString(t.created_at.toString), 
          "user_name" -> JsString(t.user_name), 
          "text" -> JsString(t.text), 
          "lang" -> JsString(t.lang), 
          "is_quote" -> JsBoolean(t.is_quote),
          "quoted_text" -> t.quoted_text.map(t => JsString(t)).getOrElse(JsNull),
          "quoted_lang" -> t.quoted_lang.map(t => JsString(t)).getOrElse(JsNull), 
          "tags" -> t.tags.map(tags => JsArray(tags.map(t => JsString(t)):_*)).getOrElse(JsNull), 
          "urls" -> t.urls.map(urs => JsArray(urs.map(t => JsString(t)):_*)).getOrElse(JsNull), 
          "categories" -> t.categories.map(conts => JsArray(conts.map(t => JsString(t)):_*)).getOrElse(JsNull) 
      ))
      
      def read(value: JsValue) = {
        value match {
          case JsObject(fields) =>
              Post(
                id= fields("id").asInstanceOf[JsString].value, 
                uri= fields("uri").asInstanceOf[JsString].value, 
                created_at= Instant.parse(fields("created_at").asInstanceOf[JsString].value), 
                user_name= fields("user_name").asInstanceOf[JsString].value, 
                text= fields("text").asInstanceOf[JsString].value, 
                lang = fields("lang").asInstanceOf[JsString].value, 
                is_quote = fields("is_quote").asInstanceOf[JsBoolean].value, 
                quoted_text= fields.get("quoted_text").map(f => if(f == JsNull) None else Some(f.asInstanceOf[JsString].value)).flatten, 
                quoted_lang= fields.get("quoted_lang").map(f => if(f == JsNull) None else Some(f.asInstanceOf[JsString].value)).flatten, 
                tags = fields.get("tags").map(f => if(f == JsNull) None else Some(f.asInstanceOf[JsArray].elements.toSeq.map(v => v.asInstanceOf[JsString].value))).flatten, 
                urls = fields.get("urls").map(f => if(f == JsNull) None else Some(f.asInstanceOf[JsArray].elements.toSeq.map(v => v.asInstanceOf[JsString].value))).flatten, 
                categories = fields.get("categories").map(f => if(f == JsNull) None else Some(f.asInstanceOf[JsArray].elements.toSeq.map(v => v.asInstanceOf[JsString].value))).flatten
              )
          case _ =>
            throw new Exception(s"@epi cannot deserialize as Post \n ${value}")
        }
      }
    }
    implicit object textsToGeoFormat extends RootJsonFormat[TextsToGeo] {
      def write(t: TextsToGeo) = JsArray(t.items.map(c => textToGeoFormat.write(c)):_*)
      def read(value: JsValue) = value match {
        case JsArray(items) => TextsToGeo(items = items.map(t => textToGeoFormat.read(t)).toSeq)
        case JsObject(fields) =>
          fields.get("items") match {
            case Some(JsArray(items)) => TextsToGeo(items = items.map(t => textToGeoFormat.read(t)).toSeq)
            case _ => throw new Exception("@epi cannot find expected items field to get to geos")
          }
        case _ => throw new Exception(s"@epi cannot deserialize $value to ToGeos")
      }
    }
    implicit object geoTrainingsFormat extends RootJsonFormat[GeoTrainings] {
      def write(t: GeoTrainings) = JsArray(t.items.map(c => geoTrainingFormat.write(c)):_*)
      def read(value: JsValue) = value match {
        case JsArray(items) => GeoTrainings(items = items.map(t => geoTrainingFormat.read(t)).toSeq)
        case JsObject(fields) =>
          fields.get("items") match {
            case Some(JsArray(items)) => GeoTrainings(items = items.map(t => geoTrainingFormat.read(t)).toSeq)
            case _ => throw new Exception("@epi cannot find expected items field to get geoTrainingd")
          }
        case _ => throw new Exception(s"@epi cannot deserialize $value to GeoTrainings")
      }
    }
    implicit object geoTrainingFormat extends RootJsonFormat[GeoTraining] {
      def write(t: GeoTraining) = throw new NotImplementedError("Writing GeoTraining is as json is not implemented")
      def read(value: JsValue) = {
        value match {
          case JsObject(fields) =>
              GeoTraining(
                category = fields("Type").asInstanceOf[JsString].value,  
                text = fields("Text").asInstanceOf[JsString].value,  
                locationInText = fields.get("Location in text").map(v => v.asInstanceOf[JsString].value), 
                isLocation = fields.get("Location yes/no")
                  .map(v => v.asInstanceOf[JsString].value.toLowerCase match { 
                    case "ok" => Some(true)
                    case "ko" => Some(false)
                    case "yes" => Some(true)
                    case "no" => Some(false)
                    case _ => None
                  })
                  .flatten,  
                forcedLocationCode = fields.get("Associate country code").map(v => v.asInstanceOf[JsString].value), 
                forcedLocationName =fields.get("Associate with").map(v => v.asInstanceOf[JsString].value) , 
                source = fields.get("Source")
                  .map(v => v.asInstanceOf[JsString].value)
                  .map(v => GeoTrainingSource(v))
                  .getOrElse(GeoTrainingSource.manual), 
                postId = fields.get("Post Id").map(v => v.asInstanceOf[JsString].value),
                lang = fields.get("Lang")
                  .map(v => v.asInstanceOf[JsString].value.toLowerCase)
                  .map{
                    case null => None
                    case "all" => None
                    case v => Some(v)
                  }.flatten,
                postPart = fields.get("Post part")
                  .map(v => v.asInstanceOf[JsString].value)
                  .map(v => GeoTrainingPart(v)), 
                foundLocation = fields.get("Episomer match").map(v => v.asInstanceOf[JsString].value) , 
                foundLocationCode = fields.get("Episomer country match").map(v => v.asInstanceOf[JsString].value), 
                foundCountryCode = fields.get("Episomer country code match").map(v => v.asInstanceOf[JsString].value)
              )
          case _ =>
            throw new Exception(s"@epi cannot deserialize $value to GeoTraining")
        }
      }
    }
    implicit object postsFormat extends RootJsonFormat[Posts] {
      def write(t: Posts) = JsArray(t.items.map(c => postFormat.write(c)):_*)
      def read(value: JsValue) = value match {
        case JsArray(items) => Posts(items = items.map(t => postFormat.read(t)).toSeq)
        /*case JsObject(fields) =>
          (fields.get("statuses"), fields.get("data"), fields.get("meta")) match {
            case (Some(JsArray(items)), _, _) => Posts(items = items.map(t => postV1Format.read(t)).toSeq)
            case (None, Some(JsArray(posts)), _)  => postsV2Format.read(value).toV1
            case (None, None, Some(_))  =>  Posts(items = Seq[Post]()) 
            case _ => throw new Exception("@epi cannot find expected statuses array on search Json result")
          }*/
        case _ => throw new Exception(s"@epi cannot deserialize $value to Posts")
      }
    }
    implicit val topicPostsFormat = jsonFormat3(TopicPosts.apply)
    implicit object luceneDocFormat extends RootJsonFormat[Document] {
      def write(doc: Document) = writeField(fields = doc.getFields.asScala.toSeq, totalCount = None, transform  = Map[String, (String, String)](), asArray =Set[String]())
      def customWrite(
        doc: Document, 
        forceString:Set[String]=Set[String](), 
        totalCount:Option[Long]=None, 
        transform:Map[String, (String, String)] = Map[String, (String, String)](), 
        asArray:Set[String]//=Set[String]()
      ) = writeField(fields = doc.getFields.asScala.toSeq, forceString=forceString, totalCount = totalCount, transform = transform, asArray = asArray)
      
      def writeField(
        fields:Seq[IndexableField], 
        forceString:Set[String] = Set[String](), 
        path:Option[String] = None, 
        totalCount:Option[Long] = None, 
        transform:Map[String, (String, String)],
        asArray:Set[String]
      ):JsValue = {
        val (baseFields, childrenNames) = path match {
          case Some(p) => (
            fields.filter(f => f.name.startsWith(p) && !f.name.substring(p.size).contains(".")), 
            fields
              .map(_.name)
              .filter(n => n.startsWith(p) && n.substring(p.size).contains("."))
              .map(n => n.substring(p.size).split("\\.")(0))
              .distinct
          ) 
          case None => (
            fields.filter(f => !f.name.contains(".")), 
            fields
              .map(_.name)
              .filter(n => n.contains("."))
              .map(n => n.split("\\.")(0))
              .distinct

          )
        }
        JsObject(Map(
          (baseFields.map{f =>
            val fname = f.name.substring(path.getOrElse("").size)
            if(asArray.contains(f.name) && f.stringValue != null)
              (fname, JsArray(f.stringValue.split("\n").map(v => JsString(transform.get(fname).map{case (s, r) =>v.replaceAll(s, r)}.getOrElse(v))):_*))
            else if(forceString.contains(f.name) && f.stringValue != null)
              (fname, JsString(transform.get(fname).map{case (s, r) =>f.stringValue.replaceAll(s, r)}.getOrElse(f.stringValue)))
            else if(f.numericValue != null && !forceString.contains(f.name))
              (fname, f.numericValue match {
                case v:java.lang.Integer => JsNumber(v)
                case v:java.lang.Float => JsNumber(v)
                case v:java.lang.Double => JsNumber(v)
                case v:java.lang.Long => JsNumber(v)
                case v:java.math.BigDecimal => JsNumber(BigDecimal(v.toString))
                case _ => throw new NotImplementedError("I do not know how convert ${v.getClass.getName} into JsNumber")
              })
            else if(f.stringValue != null)
              (fname, JsString(transform.get(fname).map{case (s, r) =>f.stringValue.replaceAll(s, r)}.getOrElse(f.stringValue)))
            else if(f.binaryValue != null && f.binaryValue.length == 1)
              (fname, JsBoolean(f.binaryValue.bytes(0) == 1.toByte))
            else 
              throw new NotImplementedError(f"Serializing lucene field is only supported for boolean, number and string so far and got $f")
          } ++
            childrenNames.map(c => (c, writeField(fields = fields, path = Some(f"${path.map(p => p+".").getOrElse("")}$c."), totalCount = None, transform = transform, asArray = asArray))) ++
            (if(path.isEmpty && !totalCount.isEmpty) Seq("totalCount" -> JsNumber(totalCount.get)) else Seq[(String, JsNumber)]())
          ):_*
        ))
      }
      def read(value: JsValue) = {
        throw new NotImplementedError(f"Deserializing lucene documebts is not supported") 
      }
    }
    implicit object geolocatedPostsFormat extends RootJsonFormat[GeolocatedPosts] {
      def write(t: GeolocatedPosts) = JsArray(t.items.map(c => geolocatedPostFormat.write(c)):_*)
      def read(value: JsValue) = value match {
        case JsArray(items) => GeolocatedPosts(items = items.map(t => geolocatedPostFormat.read(t)).map(g => g.fixTopic).toSeq)
        case JsObject(fields) =>
          fields.get("items") match {
            case Some(JsArray(items)) => GeolocatedPosts(items = items.map(t => geolocatedPostFormat.read(t)).map(g => g.fixTopic).toSeq)
            case _ => throw new Exception("@epi cannot find expected itmes array on search Json result")
          }
        case _ => throw new Exception(s"@epi cannot deserialize $value to GeolocatedPosts")
      }
    }
    implicit object topicKeyWordsFormat extends RootJsonFormat[TopicKeyWords] {
      def write(t: TopicKeyWords) = JsObject(t.items.map{case (k, s) => new JsField(k, JsArray(s.toSeq.map(v => JsString(v)):_*))})
      def read(value: JsValue) = value match {
        case JsObject(fields) =>
          TopicKeyWords(
            fields.map{
              case (name, JsArray(items)) => 
                (name.toLowerCase, 
                  items.map{ 
                    case JsString(value) => value
                    case e => throw new Exception(s"@epi1 TopicKeyWords is expected to have only a map of string sets, but got ${e}")
                  }.toSet
                )
              case (name, JsString(value)) => 
                (name.toLowerCase, Set(value))
              case e => throw new Exception(s"@epi2 TopicKeyWords is expected to have only a map of string sets, but got ${e}")
            }
            .toMap
          )
        case e => throw new Exception(s"@epi3 TopicKeyWords is expected to have only a map of string sets, but got ${e}")
      }
    }
    implicit object forcedGeoFormat extends RootJsonFormat[ForcedGeo] {
      def write(t: ForcedGeo) = JsObject(t.items.map{case (k, v) => new JsField(k, JsString(v))})
      def read(value: JsValue) = value match {
        case JsObject(fields) =>
          ForcedGeo(
            fields.map{
              case (name, JsString(value)) => (name, value)
              case _ => throw new Exception("@epi ForcedGeo is expected to have only a map of values")
            }
            .toMap
          )
        case _ => throw new Exception("@epi ForcedGeo is expected to have only a map of values")
      }
    }
    implicit object forcedGeoCodesFormat extends RootJsonFormat[ForcedGeoCodes] {
      def write(t: ForcedGeoCodes) = JsObject(t.items.map{case (k, v) => new JsField(k, JsString(v))})
      def read(value: JsValue) = value match {
        case JsObject(fields) =>
          ForcedGeoCodes(
            fields.map{
              case (name, JsString(value)) => (name, value)
              case e => throw new Exception(s"@epi ForcedGeoCodes is expected to have only a map of values but got ${e}")
            }
            .toMap
          )
        case e => throw new Exception(s"@epi ForcedGeo is expected to have only a map of values, but got ${e}")
      }
    }
}

