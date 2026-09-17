package org.ecdc.episomer.alert

import org.ecdc.episomer.fs.{AlertClassification}
import org.apache.spark.ml.classification.{Classifier, ClassificationModel, OneVsRest, OneVsRestModel}
import org.apache.spark.ml.feature.{IndexToString, StringIndexer, VectorIndexer, StringIndexerModel, VectorAssembler}
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import demy.util.{log => l, util}
import demy.mllib.linalg.implicits._
import org.ecdc.episomer.{Settings, EpisomerActor}
import org.ecdc.episomer.{Language}
import org.ecdc.episomer.Language.LangTools
import org.ecdc.episomer.fs.{TextToGeo, TaggedAlert, AlertRun}
import org.apache.pekko.pattern.{ask, pipe}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.util.Try
import org.apache.pekko.actor.{ActorSystem, Actor, ActorLogging, Props}
import org.apache.pekko.pattern.ask
import org.apache.pekko.actor.ActorRef
import org.apache.pekko.Done
import org.apache.pekko.util.{ByteString, Timeout}
import scala.collection.JavaConverters._
import org.apache.spark.sql.{SparkSession, Column, Dataset, Row, DataFrame}
import org.apache.spark.sql.functions.{col, lit, array_join, udf, concat}
import org.apache.spark.ml.linalg.{Vectors, DenseVector, Vector=>MLVector}
import demy.storage.{Storage, FSNode, WriteMode}
import scala.util.Random

class AlertActor(conf:Settings) extends Actor with ActorLogging {
  implicit val executionContext = context.system.dispatchers.lookup("lucene-dispatcher")
  implicit val cnf = conf
  def receive = {
    case AlertActor.ClassifyAlertsRequest(alerts, oRuns) => 
      implicit val timeout: Timeout = conf.fsQueryTimeout.seconds //For ask property
      implicit val s = conf.getSparkSession
      implicit val st = conf.getSparkStorage
      import s.implicits._
      //Ensuring languages models are up to date

      var sep = ""
      l.msg(s"Alert classification requested for ${alerts.size} alerts, ${if(oRuns.isEmpty) "classifying with the stored model" else s"retraining the ${oRuns.get.size} declared runs"}")
      Future {
        implicit val s = conf.getSparkSession
        implicit val st = conf.getSparkStorage
        import s.implicits._
        
        // A classification reads the model set that is currently published; a retraining fits its
        // models in a directory of its own and publishes it only once both are in place, so the
        // search loop classifying at the same time keeps reading the previous set whole
        val modelSet =
          if(oRuns.isEmpty) AlertActor.activeModelSetPath()
          else AlertActor.newModelSetPath()
        l.msg(s"Alert model set in use for this request: $modelSet (${if(oRuns.isEmpty) "published" else "staged by this retraining"})")

	    val alertsdf =  AlertActor.alerts2modeldf(alerts = alerts, reuseLabels = oRuns.isEmpty, modelSet = modelSet)
      val retRuns = 
	      oRuns.map{case runs =>
	        val newRuns = 
	          runs
	            .map{r =>
	              if(!r.active.getOrElse(true))
	                r
	              else {
                  val trainingSize = (
	                if(r.balance_classes.get) {
                    val v = alerts.filter(a => !a.deleted.get).size
                    l.msg(s"Running ${r.models} on ${v} balanced alerts, customized with ${r.custom_parameters}") 
                    v
	                } else {
                    val v = alerts.filter(a => !a.augmented.get).size
                    l.msg(s"Running ${r.models} on ${v} ${r.runs} times, customized with ${r.custom_parameters}") 
                    v
                  })
                  AlertActor.trainTest(alertsdf = alertsdf, run = r, trainRatio = 0.75, modelSet = modelSet)
	                  .setCount(alerts.size)
	              }
              }
	            .sortWith{(a, b) => 
	              (a.f1score - (if(!a.active.getOrElse(true)) 1.0 else 0.0) + (if(a.force_to_use.getOrElse(false)) 3.0 else 0.0)) > 
                (b.f1score - (if(!b.active.getOrElse(true)) 1.0 else 0.0) + (if(b.force_to_use.getOrElse(false)) 3.0 else 0.0))
	            }
	            .zipWithIndex
	            .map{case (r, i) => r.setRanking(i)}

          if(newRuns.size > 0 && newRuns(0).active.getOrElse(true)) {
            if(newRuns(0).force_to_use.getOrElse(false))
              l.msg(s"Retraining with full data for forced run ${newRuns(0).models}, balanced(${newRuns(0).balance_classes}), ${newRuns(0).custom_parameters}")
            else
              l.msg(s"Retraining with full data for best run ${newRuns(0).models}, balanced(${newRuns(0).balance_classes}) ${newRuns(0).custom_parameters}")
	          AlertActor.finalTrain(alertsdf = alertsdf, run = newRuns(0), modelSet = modelSet)
            AlertActor.publishModelSet(modelSet)
	        } else {
            // Nothing can be published and nothing can be classified either: the label indexer
            // fitted above belongs to this training set only, and the previously published
            // classifier predicts against the labels of its own indexer. Answering with categories
            // read through mismatched labels is what used to happen here, silently
            throw new Exception(s"No training was performed since no active runs where declared. The published alert models were left untouched")
	        }
	        newRuns
	      }

        val retAlerts = AlertActor.classifyAlerts(alertsdf, modelSet = modelSet).collect.toSeq
	      AlertClassification(retAlerts, retRuns)
      }.recover {
        case e: Exception =>
          EpisomerActor.Failure(s"[Job-error]: ${e.getCause} ${e.getMessage}: ${e.getStackTrace.mkString("\n")}")
      }.pipeTo(sender())
    case b => 
      Future(EpisomerActor.Failure(s"Cannot understund $b of type ${b.getClass.getName} as message")).pipeTo(sender())
  }
}

 
object AlertActor {
  case class ClassifyAlertsRequest(
    alerts:Seq[TaggedAlert], 
    runs:Option[Seq[AlertRun]], 
  )
  def getModel[F, E <: Classifier[F, E, M], M <: ClassificationModel[F, M]](run:AlertRun) = {
    val classifier = 
      Try(
        Class.forName(s"org.apache.spark.ml.classification.${run.models}")
          .getConstructor()
          .newInstance()
          .asInstanceOf[Classifier[F, E, M]]
      ).getOrElse(
        throw new Exception(s"Cannot create an alert classifier for class org.apache.spark.ml.classification.${run.models}. The selected class must inherit from https://spark.apache.org/docs/latest/api/scala/org/apache/spark/ml/classification/Classifier.html")
      )
    val params = classifier.params.map{p => ( 
      p.name,
      (p.getClass.getName.split("\\.").last match {case "Param" => "String" case a => a }).replaceAll("Param", ""),
      if(classifier.hasDefault(p)) classifier.getDefault(p) else "",
      p.doc
    )}
    //Setting custom parameters
    run.custom_parameters.map(p => p.foreach{case (name, strVal) => 
      if(classifier.hasParam(name))
        classifier.getParam(name) match {
          case param =>
           if((param.getClass.getName.endsWith("DoubleParam") && Try(classifier.set(param, strVal.toDouble)).isSuccess)
               || (param.getClass.getName.endsWith("IntParam") && Try(classifier.set(param, strVal.toInt)).isSuccess)
               || (param.getClass.getName.endsWith("param.Param") && Try(classifier.set(param, strVal)).isSuccess)
           ) {} else
             throw new Exception(
               s"Could not set the value $strVal to parameter $name. This is the doc\n: ${params.filter(_._1 == name).map{
                 case (name, dtype, default, doc) => s"$name[$dtype]($default):$doc"}.mkString(",")}"
             )
      } else {
        throw new Exception(s"Model ${run.models} does not contains a parameter $name, available parameters are ${params.map{case(name, dtype, default, doc) => s"$name[$dtype]($default)"}.mkString(", ")}")
      }
    })
    //Creating the one vs Rest classifier
    new OneVsRest()
      .setClassifier(classifier)
      .setFeaturesCol("features")
      .setLabelCol("category_vector")
      .setPredictionCol("predicted_vector")
  }

  def classificationPath()(implicit conf:Settings) = s"${conf.epiHome}/alert-ml"

  // A model set is one directory holding a label indexer and the classifier fitted against it. The
  // two only mean something together: classifyAlerts turns a prediction into a category through
  // the indexer's label array, so an old classifier read with a new indexer returns wrong
  // categories and no error at all. They also used to be written minutes apart - the indexer when
  // the training set was vectorized, the classifier once the best run had been chosen - and the
  // search loop classifies through this same actor while a retraining runs (receive answers from a
  // Future, so both are in flight at once), which is exactly when that mix happened.
  //
  // A retraining now fits both models inside a directory nobody reads, and publishes the set by
  // writing the marker file last. A classification resolves the newest marked directory, so it
  // reads either the whole previous set or the whole new one.
  def modelSetPrefix = "model-"
  def labelIndexerPath(modelSet:String) = s"$modelSet/label-indexer.ml"
  def alertClassifierPath(modelSet:String) = s"$modelSet/classifier.ml"
  def modelSetMarkerPath(modelSet:String) = s"$modelSet/complete"

  def modelSets()(implicit conf:Settings) = {
    val root = conf.getSparkStorage.getNode(AlertActor.classificationPath())
    if(!root.exists)
      Seq[String]()
    else
      root.list(recursive = false)
        .map(_.path)
        .filter(p => p.split("/").last.startsWith(AlertActor.modelSetPrefix))
        .sorted
  }
  def completeModelSets()(implicit conf:Settings) = {
    val st = conf.getSparkStorage
    AlertActor.modelSets().filter(p => st.getNode(AlertActor.modelSetMarkerPath(p)).exists)
  }
  // No published set means either a fresh installation or models written by a version that kept
  // them directly under alert-ml. That flat layout is read as it stands, so an existing model goes
  // on classifying until the next retraining publishes its first set
  def activeModelSetPath()(implicit conf:Settings) =
    AlertActor.completeModelSets().lastOption.getOrElse(AlertActor.classificationPath())
  def newModelSetPath()(implicit conf:Settings) = {
    val stamp = new java.text.SimpleDateFormat("yyyyMMdd-HHmmss-SSS").format(new java.util.Date())
    val path = s"${AlertActor.classificationPath()}/${AlertActor.modelSetPrefix}$stamp"
    conf.getSparkStorage.ensurePathExists(path)
    path
  }
  def publishModelSet(modelSet:String)(implicit conf:Settings) = {
    conf.getSparkStorage.getNode(AlertActor.modelSetMarkerPath(modelSet))
      .setContent(new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date()), WriteMode.overwrite)
    l.msg(s"Alert model set $modelSet published. Every classification from now on, the search loop's included, uses it")
    AlertActor.pruneModelSets()
    modelSet
  }
  // The set published before this one is kept: a classification that resolved it a moment before
  // the marker appeared is still loading from it. Everything older goes, and so does what a
  // retraining that died halfway left behind, once no running retraining could still be filling it
  // (fsBatchTimeout bounds one, a day is far past that)
  def pruneModelSets(keep:Int = 2)(implicit conf:Settings) = {
    val st = conf.getSparkStorage
    val complete = AlertActor.completeModelSets()
    val abandoned =
      AlertActor.modelSets()
        .filterNot(complete.contains)
        .filter(p =>
          st.getNode(p).getFileModificationTime()
            .map(t => System.currentTimeMillis - t > 24 * 3600 * 1000L)
            .getOrElse(false)
        )
    (complete.dropRight(keep) ++ abandoned).foreach{p =>
      l.msg(s"Removing the obsolete alert model set $p")
      st.getNode(p).delete(recurse = true)
    }
  }

  // Nothing caches these models in memory: every classification loads them from disk as they are
  // at that moment, so the timestamp logged on each load is what tells, from the fs log alone,
  // whether a classification used the model a retraining just wrote
  def modelStamp(path:String)(implicit conf:Settings) =
    conf.getSparkStorage.getNode(path).getFileModificationTime()
      .map(t => new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new java.util.Date(t)))
      .getOrElse("never written")

  def getLabelIndexer(modelSet:String)(implicit conf:Settings) = {
    val storage = conf.getSparkStorage
    if(!storage.getNode(AlertActor.labelIndexerPath(modelSet)).exists)
      throw new Exception(s"Cannot find the alert label indexer at ${AlertActor.labelIndexerPath(modelSet)}. Please train alerts")
    l.msg(s"Loading the alert label indexer from ${AlertActor.labelIndexerPath(modelSet)} (last written ${AlertActor.modelStamp(AlertActor.labelIndexerPath(modelSet))})")
    StringIndexerModel.load(AlertActor.labelIndexerPath(modelSet)) 
  }
  
  def getAlertClassifier(modelSet:String)(implicit conf:Settings) = {
    val storage = conf.getSparkStorage
    if(!storage.getNode(AlertActor.alertClassifierPath(modelSet)).exists)
      throw new Exception(s"Cannot find alert classification model. Please train alerts")
    else {
      l.msg(s"Classifying alerts with the model stored at ${AlertActor.alertClassifierPath(modelSet)} (last written ${AlertActor.modelStamp(AlertActor.alertClassifierPath(modelSet))})")
      OneVsRestModel.load(AlertActor.alertClassifierPath(modelSet)) 
    }
  }
  
  def fitLabelIndexer(alerts:Seq[TaggedAlert], modelSet:String)(implicit conf:Settings)  = {
    val spark = conf.getSparkSession
    val storage = conf.getSparkStorage
    import spark.implicits._
    storage.ensurePathExists(modelSet)
    assert(alerts.map(_.given_category).distinct.size > 1)
    
    l.msg(s"Fitting the alert label indexer on categories ${alerts.flatMap(_.given_category).distinct.sorted.mkString(", ")}, writing ${AlertActor.labelIndexerPath(modelSet)}")
    new StringIndexer()
      .setInputCol("given_category")
      .setOutputCol("category_vector")
      .setHandleInvalid("keep")
      .fit(alerts.toDS)
      .write.overwrite()
      .save(AlertActor.labelIndexerPath(modelSet))

       getLabelIndexer(modelSet)
   }

  def finalTrain(alertsdf:DataFrame, run:AlertRun, modelSet:String)(implicit conf:Settings) = {
    val r = new Random(20121205)
    val model = AlertActor.getModel(run)
    conf.getSparkStorage.ensurePathExists(modelSet)
    val finaltrain = if (run.balance_classes.get) {
      alertsdf.where(!col("deleted"))
    } else {
      alertsdf.where(!col("augmented"))
    }
    model.fit(finaltrain.where(col("given_category")=!=lit("?")))
     .write.overwrite()
     .save(AlertActor.alertClassifierPath(modelSet))
    l.msg(s"Alert classifier ${run.models} written to ${AlertActor.alertClassifierPath(modelSet)} (${AlertActor.modelStamp(AlertActor.alertClassifierPath(modelSet))}) on ${finaltrain.count} alerts")
    
    getAlertClassifier(modelSet)
  }

  def classifyAlerts(alerts:Seq[TaggedAlert], modelSet:String = null)(implicit conf:Settings):Seq[TaggedAlert] = {
    val set = if(modelSet == null) AlertActor.activeModelSetPath() else modelSet
    val df = AlertActor.alerts2modeldf(alerts, reuseLabels = true, modelSet = set) 
    classifyAlerts(df, set)
      .collect
      .toSeq
  }
  def classifyAlerts(alertdf:DataFrame, modelSet:String)(implicit conf:Settings) = {
    val spark = conf.getSparkSession
    import spark.implicits._
    val model = getAlertClassifier(modelSet)
    val predicted = model.transform(alertdf)

    val labelIndexer = AlertActor.getLabelIndexer(modelSet)
    val labelConverter = new IndexToString()
      .setInputCol("predicted_vector")
      .setOutputCol("episomer_category")
      .setLabels(labelIndexer.labelsArray(0))

    labelConverter.transform(predicted)
      .select("id", "date", "topic", "country", "number_of_posts", "topwords", "topposts", "given_category", "episomer_category", "test", "augmented", "deleted")
      .as[TaggedAlert]
  }
  def trainTest(alertsdf:DataFrame, run:AlertRun, trainRatio:Double, modelSet:String)(implicit conf:Settings) = {
    val r = new Random(20121205)
    //If using balanced classes training set if given in R and no multiple runs can be performed
    if(run.balance_classes.get && run.runs > 1)
      l.msg("Current run contains balanced classes and multiple runs, this is not supported. A single run will be used")
    val joinedPredictions = 
      Seq.range(0, if(run.balance_classes.get) 1 else run.runs)
        .map(i => r.nextLong)
        .map{seed =>
           val (training, test) = if (run.balance_classes.get) {
              (alertsdf.where(!col("deleted") && !col("test")), alertsdf.where(col("test")))
           } else {
             val alertsdfNoAugmented = alertsdf.where(!col("augmented"))
             val Array(training, test) = alertsdfNoAugmented.randomSplit(Array(trainRatio, 1-trainRatio), seed)
             (training, test)
           }
           l.msg(s"Training: ${training.groupBy("given_category").count().collect().mkString(", ")}")
           l.msg(s"Test: ${test.groupBy("given_category").count().collect().mkString(", ")}")
           val model = AlertActor.getModel(run)
           val m = model.fit(training.where(col("given_category")=!=lit("?")))
           m.transform(test)
         }
         .reduce(_.union(_))
     
     val evaluator = 
       new MulticlassClassificationEvaluator()
         .setLabelCol("category_vector")
         .setPredictionCol("predicted_vector")
     val categories = AlertActor.getLabelIndexer(modelSet).labelsArray(0)
     
     val ret = AlertRun(
       ranking = 0,
       models = run.models,
       alerts = None,
       runs = run.runs,
       f1score = evaluator.setMetricName("f1").evaluate(joinedPredictions),
       accuracy = evaluator.setMetricName("accuracy").evaluate(joinedPredictions), 
       precision_by_class = AlertActor.getMetricByLabel(evaluator, "precisionByLabel", categories, joinedPredictions),
       sensitivity_by_class = AlertActor.getMetricByLabel(evaluator, "recallByLabel", categories, joinedPredictions),
       fscore_by_class = AlertActor.getMetricByLabel(evaluator, "fMeasureByLabel", categories, joinedPredictions),
       last_run = {
         val sdfDate = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
         val now = new java.util.Date()
         Some(sdfDate.format(now))
       },
       balance_classes  = run.balance_classes,
       force_to_use = run.force_to_use,
       active = run.active,
       documentation = run.documentation,
       custom_parameters = run.custom_parameters 
     )
     ret
  }

  def getMetricByLabel(evaluator:MulticlassClassificationEvaluator, metricName:String, categories:Array[String], predictions:DataFrame) = {
    categories.zipWithIndex.map{case (c, i) => 
      val v = evaluator.set(evaluator.getParam("metricLabel"), i.toDouble).setMetricName(metricName).evaluate(predictions)
      s"${categories(i)}:${(v*1000).toInt/1000.0}"
    }.mkString("\n")
  }

  def alerts2modeldf(alerts:Seq[TaggedAlert], reuseLabels:Boolean, modelSet:String)(implicit conf:Settings) =  {
     val spark = conf.getSparkSession
     implicit val storage = conf.getSparkStorage
     import spark.implicits._
     val indexer = 
       if(reuseLabels)
         AlertActor.getLabelIndexer(modelSet)
       else 
         AlertActor.fitLabelIndexer(alerts, modelSet)

     
     //Preparing Data
     Some(alerts)
       //Finding top words language
       .map{ case(alerts) => (alerts, 
         alerts.map{ta => 
           ta.topposts
             .map{case (lang, posts) => (lang, posts.size)}
             .reduceOption((p1, p2) => (p1, p2) match {case ((lang1, count1), (lang2, count2)) => if(count1 > count2) p1 else p2})
             .map{case(lang, count) => lang}
             .getOrElse("en")
           }
       )}
       //Transforming into data frame
       .map{case (alerts, topwordlangs) => 
          alerts.zip(topwordlangs).map{
            case (TaggedAlert(id, date, topic, country, number_of_posts, topwords, topposts, given_category, _ ,test, augmented, deleted), twlang)
               => (id, date, topic, country, number_of_posts, topwords, twlang, topposts, given_category.orElse(Some("?")), test, augmented, deleted)
          }.toDF("id", "date", "topic", "country", "number_of_posts", "topwords", "twlang", "topposts", "given_category", "test", "augmented", "deleted")
       }
       //Text vectorisation
       .map{df => 
         val langs = conf.languages.get.map(_.code)
         val baseCols = Seq("id", "date", "topic", "country", "number_of_posts", "topwords", "twlang", "given_category", "topposts", "test", "augmented", "deleted")
         //Splitting posts in different columns by language
         df.select((
           baseCols.map(c => col(c)) ++  
           Seq(lit("en").as("topic_lang"), lit("en").as("country_lang")) ++
           langs.map(lang => array_join(col("topposts").getItem(lang), "\n").as(s"top_$lang")) ++ 
           langs.map(lang => lit(lang).as(s"toplang_$lang"))) 
           :_*
         )
	       //Vectorizing all columns by language
         .vectorize(
           languages = conf.languages.get, 
           reuseIndex = true, 
           indexPath = conf.langIndexPath, 
           textLangCols = Map("topic" -> Some("topic_lang"), "country"-> Some("country_lang"), "topwords"-> Some("twlang")) ++ Map(langs.map(lang => (s"top_$lang", Some(s"toplang_$lang"))):_*), 
           tokenizerRegex = conf.splitter
         )
	       //Adding all vectors on a single vector (translation to english of all vectors could be interesting for future)
         .select((
           baseCols.map(c => col(c)) ++
           Seq(
	       udf((row:Any) => {
                 row.asInstanceOf[Seq[Row]].map(vectors => vectors.getAs[MLVector](0)).reduceOption(_.sum(_)).getOrElse(null)
               })
               .apply(
       	         concat(
       	           (Seq("topic_vec", "country_vec", "topwords_vec") ++ 
       	             langs.map(lang => s"top_${lang}_vec")
                   ).map(c => col(c))
       	           :_*
       	         )
               ).as("word_vec"),
               udf((count:Int)=> 1 - Math.pow(Math.E, -1.0*count/1000.0)).apply(col("number_of_posts")).as("adjusted_count")
	   )):_*
         )
	       //Removing potentially empty alerts
         //.where(col("word_vec").isNotNull)
       }
       //Indexing text categories as multiclass vector
       .map{df => 
         indexer.transform(df)}
       //Joining vectors with the numver of posts
       .map{df => 
           (new VectorAssembler())
             .setInputCols(Array("word_vec", "adjusted_count"))
             .setOutputCol("features")
             .transform(df)
        }
	.get
	.select("id", "date", "topic", "country", "number_of_posts", "topwords", "topposts", "given_category", "features", "category_vector", "test", "augmented", "deleted")
	.as[(String, String, String, String, Int, String, Map[String, Seq[String]], String, MLVector, Double, Boolean, Boolean, Boolean)]
	.toDF
  }
}



