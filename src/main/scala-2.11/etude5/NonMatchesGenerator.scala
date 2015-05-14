package etude5

import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb.{Direction, Path, Node}
import org.neo4j.kernel.Traversal
import scala.collection.JavaConversions._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by tinhtooaung on 14/05/15.
 */
class NonMatchesGenerator(strips: List[Strip]) extends GraphCarpetGenerator(strips, "-n"){

  constructGraph { strip =>
    findNotMatchableStrips(strip)
  }

  def findNonMatches(length: Int): Unit ={
    if(strips.isEmpty || (strips.size < length)){
      return
    }
    if(length == 1){
      println(strips.head.value)
      return
    }
    val possiblePaths = ListBuffer.empty[Path]
    val finder = GraphAlgoFactory.pathsWithLength(
      Traversal.pathExpanderForTypes("matches", Direction.BOTH), length - 1
    )
    for{
      startStrip <- strips
      endStrip <- strips
    }{
      possiblePaths ++= finder.findAllPaths(getNodeByUniqeId(id = startStrip.id), getNodeByUniqeId(id = endStrip.id)).toList
    }

    val paths = possiblePaths.filter(_.length() == length - 1)
    if(paths.nonEmpty){
      printPaths(List[Path](paths.head))
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
}
