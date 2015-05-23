package etude5

import java.util.UUID

import com.sun.java.swing.plaf.gtk.GTKConstants.ExpanderStyle
import eu.fakod.neo4jscala.{Cypher, EmbeddedGraphDatabaseServiceProvider, Neo4jWrapper, Neo4jIndexProvider}
import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb.factory.GraphDatabaseSettings
import org.neo4j.graphdb.traversal.{Evaluation, Evaluator, Evaluators}
import org.neo4j.graphdb.{Expander, Direction, Node, Path}
import org.neo4j.kernel.Traversal
import org.neo4j.kernel.impl.cache.NoCacheProvider

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Try}


/**
 * Created by tinhtooaung on 14/05/15.
 */
class NonMatchesFlipGenerator(var strips: Set[Strip]) extends Neo4jWrapper with EmbeddedGraphDatabaseServiceProvider with Cypher{

  override def neo4jStoreDir: String = s"/tmp/neo/non"

  override def configParams = {
    Map[String, String](GraphDatabaseSettings.cache_type.name() -> NoCacheProvider.NAME)
  }

  constructGraph

  def findNonMatches(length: Int) = {
    val result = getNonMatches(length)
    if(result.nonEmpty){
      println(result.mkString("\n"))
      println(s"${result.size}")
    }else{
      println("not possible")
    }
  }

  def getNonMatches(length: Int): List[String] ={
    if(strips.isEmpty || (strips.size < length)){
      return List.empty[String]
    }
    if(length == 1){
      return List[String](strips.head.value)
    }
    var possiblePaths = ListBuffer[Path]()
    for{
      startStrip <- strips
    } {
      val startNode = getNodeByTuple(id = startStrip.id, value = startStrip.value)
      if (startNode != null) {
        val newPaths = Traversal.description()
          .relationships(string2RelationshipType("matches"), Direction.BOTH)
          .evaluator(Evaluators.atDepth(length - 1))
          .traverse(startNode).iterator().toList
        possiblePaths ++= ListBuffer(newPaths: _*)
      }else{
        throw new Exception("Start node is null")
      }
    }

    var result = List.empty[String]
    for(path <- possiblePaths if result.isEmpty){
      val strips = path.nodes().map(_("value").getOrElse("")).toList
      if(isNonMatches(strips)){
        result = strips
      }else{
        result = flip(strips)
      }
    }
    result
  }

  def findNotMatchableStrips(strip: Strip): List[(Strip, Int)] ={
    val result = ListBuffer.empty[(Strip, Int)]
    for{
      s <- strips
    }{
      if(isNonMatches(List(s.value, strip.value))){
        result += (s -> 1)
      }
    }
    result.toList
  }

  def flip(bestResult: List[String]) : List[String] = {
    val size = bestResult.size
    var candidates = ListBuffer.empty[List[String]]
    for(i <- 1 to size){
      val firstPartValues = bestResult.slice(0, i)
      val secondPartValues = bestResult.slice(i, size).map(_.reverse)

      val result = firstPartValues ++ secondPartValues
      //      println(firstPartValues.mkString(" ") + " ---- " + secondPartValues.mkString(" "))
      //      println("===================")
      candidates += result
    }
    candidates = candidates.filter { candidate =>
      isNonMatches(candidate)
    }
    if(candidates.nonEmpty){
      Random.shuffle(candidates).head
    }else {
      List.empty[String]
    }
  }

  def isNonMatches(list: List[String]): Boolean = {
    list.sliding(2).foldLeft(0){ (matches, nodes) =>
      val node1: String = nodes.head
      val node2: String = nodes.last
      var currentMatches: Int = matches
      for((char, index) <- node1.zipWithIndex){
        if(node2(index) == char){
          currentMatches += 1
        }
      }
      currentMatches
    } == 0
  }

  def constructGraph = {
    withTx {
      implicit neo =>
        val wordIndex = indexManager.forNodes("strips")

        for {
          strip <- strips
          matchableStrip = findNotMatchableStrips(strip)
          matchableWord <- matchableStrip if matchableStrip.nonEmpty
        }{
          var start = getNodeByTuple(id = strip.id, value=strip.value)

          if(start == null){
            start = createNode
            start("value") = strip.value
            start("uniqueId") = strip.id
            wordIndex.add(start, "combo", (strip.id, strip.value))
          }

          var end = getNodeByTuple(id = matchableWord._1.id, value = matchableWord._1.value)
          if(end == null && matchableWord != null){
            end = createNode
            end("value") = matchableWord._1.value
            end("uniqueId") = matchableWord._1.id
            wordIndex.add(end, "combo", (matchableWord._1.id, matchableWord._1.value))
          }
          start --> "matches" --> end
        }
//        println("Node count: " + getAllNodes.size)
    }
  }

  def getNodeByTuple(index: String = "strips", value: String, id: String): Node = {
    var startNode: Option[Node] = None
    val wordIndex = indexManager(ds).forNodes("strips")
    val result = wordIndex.get("combo", (id, value)).iterator().toList
    if(result.nonEmpty){
      startNode = Some(result.head)
      startNode.get
    }else{
      null
    }
  }
}
