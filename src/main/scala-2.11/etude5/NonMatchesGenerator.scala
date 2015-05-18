package etude5

import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb.{Node, Direction, Path}
import org.neo4j.kernel.Traversal

import scala.collection.JavaConversions._
import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer


/**
 * Created by tinhtooaung on 14/05/15.
 */
class NonMatchesGenerator(var strips: Set[Strip]) extends GraphCarpetGenerator(strips.toList, "-n"){

  override def neo4jStoreDir = "/tmp/temp-neo-non"

  constructGraph { strip =>
    findNotMatchableStrips(strip)
  }

  def findNonMatches(length: Int): Unit = {
    if(strips.isEmpty || (strips.size < length)){
      return
    }
    if(length == 1){
      println(strips.head.value)
      return
    }
    val result = getNonMatches(length)
    if(result.nonEmpty){
      printNodes(result)
    }else{
      println("not possible")
    }
  }

  def getNonMatches(length: Int): List[Node] ={
    var possiblePaths = HashSet.empty[Path]
    val finder = GraphAlgoFactory.pathsWithLength(
      Traversal.pathExpanderForTypes("matches", Direction.BOTH), length - 1
    )
    for{
      startStrip <- strips
    }{
      for(endStrip <- strips if endStrip.id != startStrip.id) {
        val startNode = getNodeByUniqeId(id = startStrip.id)
        val endNode = getNodeByUniqeId(id = endStrip.id)
        if(startNode != null && endNode != null){
          possiblePaths ++= finder.findAllPaths(startNode, endNode).toList
        }
      }
    }

    var paths = possiblePaths.filter {path =>
      path.length() == length - 1
    }
    paths = paths.filter { path =>
      var valid = true
      path.nodes().toList.sliding(2, 1).foreach { list =>
        if(valid){
          valid = isNoMatches(list.head("value").get, list.last("value").get)
        }
      }
      valid
    }

    if(paths.nonEmpty){
      paths.head.nodes().toList
    }else{
      List[Node]()
    }
  }

  def findNotMatchableStrips(strip: Strip): List[(Strip, Int)] ={
    val result = ListBuffer.empty[(Strip, Int)]
    for{
      s <- strips if s.value != strip.value
    }{
      var matchesCount = 0
      for(c <- s.value.zipWithIndex){
        if(strip.value(c._2) == c._1){
          matchesCount += 1
        }
      }
      if(matchesCount == 0){
        result += (s -> 1)
      }
    }
    //    println(s"${strip.value} => ${result.map(_._1.value).mkString("-")}")
    result.toList
  }

  override def constructGraph(f: Strip => List[(Strip, Int)]) = {
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
          }

          var end = getNodeByUniqeId(id = matchableWord._1.id)
          if(end == null && matchableWord != null){
            end = createNode
            end("value") = matchableWord._1.value
            end("uniqueId") = matchableWord._1.id
            wordIndex.add(end, "value", matchableWord._1.value)
            wordIndex.add(end, "uniqueId", matchableWord._1.id)
          }
          val noRelation = !start.getRelationships.exists { rel =>
            rel.getEndNode()("uniqueId") == end("uniqueId")
          }
          if(noRelation){
            start.createRelationshipTo(end, "matches").setProperty("count", matchableWord._2)
          }
        }
        println("Node count: " + getAllNodes.size)
    }
  }

  def isNoMatches(s1: String, s2: String): Boolean = {
    var matchesCount = 0
    for(c <- s1.zipWithIndex){
      if(s2(c._2) == c._1){
        matchesCount += 1
      }
    }
//    println(s"${s1}, ${s2}, ${matchesCount == 0}")
    matchesCount == 0
  }
}
