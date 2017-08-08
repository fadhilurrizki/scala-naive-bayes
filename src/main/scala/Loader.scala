import java.io.File

import scala.io.Source

/**
  * Created by fadhilurrizki on 08/08/17.
  */
object Loader {

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }

  def loadTxt(filepath: String): (List[String], List[Int]) = {
    //dataset review polarity, 1000 positive 1000 negative
    var content = List[String]()
    var label = List[Int]()
    //load positive
    val pospath = filepath + "/pos"
    val posfilenames = getListOfFiles(pospath)
    for(i<-0 to posfilenames.size-1) {
      var text = ""
      for (line <- Source.fromFile(posfilenames(i)).getLines) {
        text += " " + line
      }
      content :+= text
      label :+= 1
    }
    val negpath = filepath + "/neg"
    val negfilenames = getListOfFiles(negpath)
    for(i<-0 to negfilenames.size-1) {
      var text = ""
      for (line <- Source.fromFile(negfilenames(i)).getLines) {
        text += " " + line
      }
      content :+= text
      label :+= 0
    }
    (content,label)
  }
}
