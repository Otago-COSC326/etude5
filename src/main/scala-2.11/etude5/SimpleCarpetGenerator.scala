package etude5

import scala.collection.mutable.ListBuffer

/**
 * Created by tinhtooaung on 8/05/15.
 */


case class Carpet(strips: ListBuffer[(String, Int)], var weight: Int = 0){

  for(item <- strips){
    addStrip(item)
  }

  def addStrip(strip: (String, Int)): Unit ={
    strips += strip
    weight += strip._2
  }

  def removeStrip(strip: (String, Int)): Unit = {
    strips -= strip
    weight -= strip._2
  }

}

class SimpleCarpetGenerator(strips: List[String]) {

  def findMatchableStrip(strip: String) : List[(String, Int)] = {
    val result = ListBuffer.empty[(String, Int)]
    for(s <- strips){
      var matchesCount = 0

      for (c <- s.zipWithIndex){
        if(strip(c._2) == c._1){
          matchesCount += 1
        }
      }
      if(matchesCount != 0){
        result += (s->matchesCount)
      }
    }
    result.toList
  }

  def findBestMatches(length: Int) = {
    val possibleCarpets = ListBuffer.empty[Carpet]

    def findPossibleCarpets() = {
      val mutableStrips = ListBuffer[String](strips: _*)
      for(s <- mutableStrips){
        val matchables = findMatchableStrip(s)
        val carpetStrips = matchables.sortBy(_._2).grouped(length).toList.head
        possibleCarpets += Carpet(ListBuffer(carpetStrips: _*))
      }
    }
    findPossibleCarpets()

    for(carpet <- possibleCarpets){
      println(carpet)
    }

  }

}
