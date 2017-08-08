import java.io.{File, IOException, PrintWriter}

import org.json4s.DefaultFormats
import org.json4s.native.Json
import org.json4s.native.JsonMethods._

import scala.io.Source

/**
  * Created by fadhilurr izki on 08/08/17.
  */
object NaiveBayes {
  def fit(tokens : List[List[String]], label : List[Int]): Unit = {
    val alpha = 0.001
    val vocab = tokens.flatten.groupBy(l => l).map(t => (t._1, t._2.length)).filter(_._2 > 5).keys.toList
    val list_label = label.groupBy(l => l).map(t => (t._1, t._2.length))
    var resultt = List[Tuple4[Int, Double, List[String], List[Double]]]()
    list_label.map { elem =>
      val class_ = elem._1
      val token_class_ = tokens.filter(sent => label(tokens.indexOf(sent)) == elem._1).flatten
      val token_class_map = token_class_.groupBy(l => l).map(t => (t._1, t._2.length))
      var result = Map[String, Double]()
      vocab.map { word =>
        var counts = 0
        if (token_class_map.keys.toList.contains(word)) {
          counts = token_class_map.values.toList(token_class_map.keys.toList.indexOf(word))
        }
        result = result + (word -> ((counts + alpha) / (token_class_.size + alpha * vocab.size)))
      }
      resultt :+= Tuple4(elem._1, (elem._2/list_label.values.sum.toDouble),result.keys.toList, result.values.toList)
    }
    val a = Json(DefaultFormats).write(resultt)
    val pthcfidf = "./model/prob-naive-bayes"
    val pw = new PrintWriter(new File(pthcfidf))
    pw.write(a)
    pw.close
  }

  def predict(token: List[String], word_prob: List[Tuple4[Int, Double, List[String], List[Double]]]): Int = {
    val nb_score = word_prob.map { t =>
      val filtered_ = t._3.filter(token.contains(_))
      (t._1.toInt -> (filtered_.map (word => t._4.toList(t._3.indexOf(word))).product * t._2))
    }.toMap
    nb_score.keys.toList(nb_score.values.toList.indexOf(nb_score.values.max))
  }

  def predictAll(tokens: List[List[String]]): List[Int] = {
    tokens.map(token => predict(token, loadModel()))
  }

  def loadModel() : List[Tuple4[Int, Double, List[String], List[Double]]] = {
    var lines = ""
    for (line <- Source.fromFile("./model/prob-naive-bayes").getLines) {
      lines = line
    }
    implicit val formats = org.json4s.DefaultFormats
    parse(lines).extract[List[Tuple4[Int, Double, List[String], List[Double]]]]
  }

}
