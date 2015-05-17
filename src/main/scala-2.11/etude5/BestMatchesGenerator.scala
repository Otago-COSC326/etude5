package etude5

import org.neo4j.graphdb.{Node, Path}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

/**
 * Created by tinhtooaung on 14/05/15.
 */
class BestMatchesGenerator(strips: List[Strip]) extends GraphCarpetGenerator(strips, "b"){

  override def neo4jStoreDir = "/tmp/temp-neo-best"

  constructGraph { strip =>
    findMatchableStrip(strip)
  }

  def findBestMatches(length: Int): Unit = {
    if(strips.isEmpty || (strips.size < length)){
      return
    }
    if(length == 1){
      println(strips.head.value)
      return
    }
    val result = getBestMatches(length)
    result match {
      case Left(s) => printNodesWithSort(s)
      case Right(s) => println(s.map(_.value).mkString(" "))
    }
  }

  def getBestMatches(length: Int): Either[List[Node], List[Strip]] = {
    val possiblePaths = ListBuffer[Path]()
    for(s <- strips){
      possiblePaths ++= findPossiblePaths(s, length)
    }
    var chosenNodes = mutable.LinkedHashSet[Node]()
    var nextWeight = 0
    var count = 0
    do{
      val chosen = if(chosenNodes.nonEmpty){
        val newPaths = possiblePaths.toList.filter { path =>
          val nodeValues = path.nodes().map(_("value").getOrElse(""))
          val matchableValues = findMatchableStrip(Strip(chosenNodes.last("value").getOrElse(""),
            chosenNodes.last("uniqueId").getOrElse(""))).map(_._1.value)
          nodeValues.toList.intersect(matchableValues).nonEmpty
        }
        chooseBestPath(newPaths.toList, nextWeight, chosenNodes.last("uniqueId").getOrElse(""))
      }else{
        chooseBestPath(possiblePaths.toList)
      }
      if(chosen != null && chosen.isDefined){
        possiblePaths -= chosen.get
        chosen.get.nodes().toList.foreach{ node =>
          chosenNodes += node
        }
        nextWeight += getCurrentWeight(chosen.get)
      }
      count += 1
    }while(chosenNodes.size < length && count < 1000)
    val finalResult = chosenNodes.toList.grouped(length).toList.head
    if(finalResult.size != length){
      Right(strips.take(length).sortBy(_.value))
    }else{
      Left(finalResult)
    }
  }

  def findPossiblePaths(startStrip: Strip, length: Int, eligibleStrip: List[Strip]= null) = {
    val possiblePaths = ListBuffer.empty[Path]
    var currentLength = length
    do {
      val matchableStrips = findMatchableStrip(startStrip, eligibleStrip)
      for(matchable <- matchableStrips if matchableStrips.nonEmpty){
        val startNode = getNodeByUniqeId(id = startStrip.id)
        val endNode = getNodeByUniqeId(id = matchable._1.id)
        possiblePaths ++= findMaxPathBetweenNodes(startNode, endNode, currentLength)
      }
      if(possiblePaths.isEmpty) {
        currentLength -= 1
      }
    }while(currentLength > 0 && possiblePaths.isEmpty)
    possiblePaths
  }

  def chooseBestPath(paths: List[Path], currentWeight: Int = 0, uniqueId:String = ""): Option[Path]= {
    if(paths.nonEmpty){
      val result = paths.minBy { path =>
        var sum = currentWeight
        path.relationships().foreach { rel =>
          sum += rel("count").getOrElse(0)

          if(uniqueId.nonEmpty) {
            try{
              val extraRel = getNodeByUniqeId(id=uniqueId).getRelationships.filter { extraRel =>
                extraRel.getEndNode()("uniqueId") == path.startNode()("uniqueId")
              }.toList.maxBy(_("count").getOrElse(0))
              sum += extraRel("count").getOrElse(0)
            }catch {
              case _: UnsupportedOperationException =>
            }
          }
        }
        sum
      }
      return Some(result)
    }
    None
  }
}
