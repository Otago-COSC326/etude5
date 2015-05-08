package etude5

import eu.fakod.neo4jscala.{Cypher, EmbeddedGraphDatabaseServiceProvider, Neo4jWrapper}
import org.neo4j.graphalgo.{WeightedPath, GraphAlgoFactory}
import org.neo4j.graphdb.{Path, Node, Direction}
import org.neo4j.kernel.Traversal

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

/**
 * Created by tinhtooaung on 7/05/15.
 */
class CarpetGenerator(strips: List[String], option: String)
  extends Neo4jWrapper with EmbeddedGraphDatabaseServiceProvider with Cypher{

  def neo4jStoreDir = "/tmp/temp-neo-test"

  constructGraph()

  def getBestMatches(length: Int) = {
    val possiblePaths = ListBuffer.empty[Path]
    for{
      s <- strips
      matchableStrips= findMatchableStrip(s)
      matchable <- matchableStrips if matchableStrips.nonEmpty && matchableStrips.size >= length
    }{
//      println(s"$s => $matchableStrips")
      val startNode = getNodeByValue(value = s)
      val endNode = getNodeByValue(value = matchable._1)
      possiblePaths ++= findMaxPathBetweenNodes(startNode, endNode, length)
    }

    if(possiblePaths.isEmpty) {
      val startNode = getNodeByValue(value = strips.head)
      val endNode = getNodeByValue(value = strips.last)
      possiblePaths ++= findMaxPathBetweenNodes(startNode, endNode, length)
    }

    val chosen = possiblePaths.minBy { path =>
      var sum = 0
      path.relationships().foreach { rel =>
        sum += rel("count").getOrElse(0)
      }
      sum
    }
    printPaths(List(chosen))
  }

  def findMaxPathBetweenNodes(startNode: Node, endNode: Node, length: Int): List[Path] = {
    val finder = GraphAlgoFactory.pathsWithLength(
      Traversal.pathExpanderForTypes("matches", Direction.BOTH), length - 1
    )
    finder.findAllPaths(startNode, endNode).filter { path =>
      path.length() == length - 1
    }.toList
  }

  def printPaths(paths: List[Path]) = {
    for(path <- paths){
      println(path.nodes().toList.map(_("value").getOrElse("")).mkString(" "))
    }
  }

  def constructGraph() = {
    withTx {
      implicit neo =>
        val wordIndex = indexManager.forNodes("strips")
        for {
          strip <- strips
          matchableWord <- findMatchableStrip(strip)
        }{
          var start = getNodeByValue(value = strip)
          if(start == null){
            start = createNode
            start("value") = strip
            wordIndex.add(start, "value", strip)
          }

          var end = getNodeByValue(value = matchableWord._1)
          if(end == null && matchableWord != null){
            end = createNode
            end("value") = matchableWord._1
            wordIndex.add(end, "value", matchableWord._1)
          }
          start.createRelationshipTo(end, "matches").setProperty("count", matchableWord._2)
        }
//        println("Node count: " + getAllNodes.size)
    }
  }

  def findMatchableStrip(strip: String) : List[(String, Int)] = {
    val result = ListBuffer.empty[(String, Int)]
    for(s <- strips){
      var matchesCount = strip.length
      for (c <- s.zipWithIndex){
        if(strip(c._2) == c._1){
          matchesCount -= 1
        }
      }
      if(matchesCount != 3){
        result += (s->matchesCount)
      }
    }
    result.toList
  }

  def getNodeByValue(index: String = "strips", value: String) = {
    val wordIndex  = indexManager(ds).forNodes("strips")
    val startNode = wordIndex.get("value", value).getSingle
    startNode
  }
}
