package etude5

import java.net.URI

import eu.fakod.neo4jscala._
import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb.{Direction, Node, Path}
import org.neo4j.kernel.Traversal

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

/**
 * Created by tinhtooaung on 7/05/15.
 */
class GraphCarpetGenerator(strips: List[Strip], option: String)
  extends Neo4jWrapper with EmbeddedGraphDatabaseServiceProvider with Cypher{

  def neo4jStoreDir = ""
//  override def uri: URI = URI.create("http://localhost:7474")
//  override def userPw: Option[(String, String)] = Some(("neo4j", "test"))

  def getCurrentWeight(path: Path): Int ={
    var sum: Int = 0
    path.relationships().foreach { rel =>
      sum += rel("count").getOrElse(0)
    }
    sum
  }

  def findMaxPathBetweenNodes(startNode: Node, endNode: Node, length: Int): List[Path] = {
    val finder = GraphAlgoFactory.pathsWithLength(
      Traversal.pathExpanderForTypes("matches", Direction.BOTH), length - 1
    )
    finder.findAllPaths(startNode, endNode).toList
  }

  def printPaths(paths: List[Path]) = {
    for(path <- paths){
      printNodesWithSort(path.nodes().toList)
    }
  }

  def printNodesWithSort(nodes: List[Node]) = {
    println(nodes.toList.sortBy(_("value").getOrElse("")).map{ node =>
      node("value").getOrElse("")
    }.mkString("\n"))
  }

  def printNodes(nodes: List[Node]) = {
    println(nodes.toList.map{ node =>
      node("value").getOrElse("")
    }.mkString("\n"))
  }

  def constructGraph(f: Strip => List[(Strip, Int)])(d: (Node, Node) => Boolean) = {
    withTx {
      implicit neo =>
        val wordIndex = indexManager.forNodes("strips")

        for {
          strip <- strips
          matchableStrip = f(strip)
          matchableWord <- matchableStrip if matchableStrip.nonEmpty
        }{
          var start = getNodeByUniqeId(id = strip.id)

          if(start == null){
            start = createNode
            start("value") = strip.value
            start("uniqueId") = strip.id
            wordIndex.add(start, "value", strip.value)
            wordIndex.add(start, "uniqueId", strip.id)
            wordIndex.add(start, "combo", (strip.id, strip.value))
          }

          var end = getNodeByUniqeId(id = matchableWord._1.id)
          if(end == null && matchableWord != null){
            end = createNode
            end("value") = matchableWord._1.value
            end("uniqueId") = matchableWord._1.id
            wordIndex.add(end, "value", matchableWord._1.value)
            wordIndex.add(end, "uniqueId", matchableWord._1.id)
            wordIndex.add(start, "combo", (matchableWord._1.id, matchableWord._1.value))
          }
          val noRelation = d(start, end)
          if(noRelation){
            start.createRelationshipTo(end, "matches").setProperty("count", matchableWord._2)
          }
        }
//        println("Node count: " + getAllNodes.size)
    }
  }

  def getNodeByValue(index: String = "strips", value: String) = {
    val wordIndex  = indexManager(ds).forNodes("strips")
    val startNode = wordIndex.get("value", value).getSingle
    startNode
  }

  def getNodeByUniqeId(index: String = "strips", id: String) = {
    val wordIndex  = indexManager(ds).forNodes("strips")
    val startNode = wordIndex.get("uniqueId", id).getSingle
    startNode
  }


}
