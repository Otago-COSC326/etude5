package etude5

import java.util.UUID

import org.neo4j.graphdb.traversal.{Evaluation, Evaluator, Evaluators}
import org.neo4j.graphdb.{Node, Path}
import org.neo4j.kernel.Traversal

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by tinhtooaung on 14/05/15.
 */
class BestMatchesFlipGenerator(var strips: List[Strip]) extends GraphCarpetGenerator(strips, "b"){

  override def neo4jStoreDir: String = s"/tmp/neo/${UUID.randomUUID().toString}"

//  strips ++= strips.map { strip =>
//    Strip(strip.value.reverse, strip.id)
//  }
  //  strips = strips.toList.sortBy { s =>
  //    strips.count(_.value == s.value)
  //  }.reverse

  constructGraph { strip =>
    findMatchableStrip(strip)
  }{(start, end) =>
    !start.getRelationships.exists { rel =>
      rel.getEndNode()("uniqueId") == end("uniqueId")
    }
  }

  def findBestMatches(length: Int): Unit = {
    val result = getBestMatches(length)
    println(result.mkString("\n"))
    println(countMatchesWithString(result))
  }

  def getBestMatches(length: Int): List[String] = {
    if(strips.isEmpty || (strips.size < length)){
      return List.empty[String]
    }
    if(length == 1){
      return List(strips.head.value)
    }
    var possiblePaths = ListBuffer[Path]()
    for(s <- strips){
      possiblePaths ++= findPossiblePaths(s, length)
    }

    val chosenOne = possiblePaths.maxBy{ path =>
      countMatchesWithString(findBestResult(path))
    }

    findBestResult(chosenOne)
  }

  def findBestResult(path: Path): List[String] = {
    val chosenString = path.nodes().map(_("value").getOrElse("")).toList
    var bestResult = (chosenString.size -> chosenString)
    for(i <-1 to 10){
      bestResult = flip(chosenString.size, bestResult)
    }
    bestResult._2
  }

  def flip(size: Int, bestResult: (Int, List[String])) = {
    val size: Int = bestResult._2.size
    var myBestResult = bestResult
    for(i <- 1 to size){
      val firstPartValues = myBestResult._2.slice(0, i)
      val secondPartValues = myBestResult._2.slice(i, size).map(_.reverse)

      val result = (firstPartValues ++ secondPartValues).sorted
      val count = countMatchesWithString(result.toList)

//      println(s"Count => ${count}/ ${bestResult._1}")
//      println(firstPartValues.mkString(" ") + " ---- " + secondPartValues.mkString(" "))
//      println("===================")
      if(count > bestResult._1){
        myBestResult = (count -> result.toList.sorted)
      }
    }
    if(myBestResult._1 > bestResult._1){
      myBestResult
    }else{
      bestResult
    }
  }

  def countMatches(list: List[Node]): Int = {
    list.sliding(2).foldLeft(0){ (matches, nodes) =>
      val node1: Node = nodes.head
      val node2: Node = nodes.last
      var currentMatches: Int = matches
      for((char, index) <- node1("value").getOrElse("").zipWithIndex){
        if((node2("value")getOrElse(""))(index) == char){
          currentMatches += 1
        }
      }
      currentMatches
    }
  }

  def countMatchesWithString(list: List[String]): Int = {
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
    }
  }

  def removeDuplicates(list: List[Node]): List[Node] = {
    list.sliding(2).filter { slider =>
      val node1Value = slider.head("value").getOrElse("")
      val node1Id = slider.head("uniqueId").getOrElse("")
      val node2Value = slider.last("value").getOrElse("")
      val node2Id = slider.last("uniqueId").getOrElse("")
      (node1Value != node2Value.reverse) && (node1Id != node2Id)
    }.flatten.toList
  }

  def flip(list: List[Node]): List[Node] = {
    ???
  }

  def findPossiblePaths(startStrip: Strip, length: Int) = {
    Traversal.description().depthFirst()
      .evaluator(Evaluators.atDepth(length - 1))
//      .evaluator(new Evaluator {
//      override def evaluate(path: Path): Evaluation = {
//        val ids = path.nodes().map(_("uniqueId").getOrElse("")).filter(_ != "").toList
//        if(ids.distinct.size == ids.size){
//          Evaluation.INCLUDE_AND_CONTINUE
//        }else{
//          Evaluation.EXCLUDE_AND_CONTINUE
//        }
//      }
//    })
      .traverse(getNodeByTuple(id=startStrip.id, value=startStrip.value))
      .iterator().toList
  }

  def getNodeByTuple(index: String = "strips", value: String, id: String): Node = {
    var startNode: Option[Node] = None
    val wordIndex = indexManager(ds).forNodes("strips")
    //        val wordIndex = getNodeIndex("strips").get
    val result = wordIndex.get("combo", (id, value)).iterator().toList
    if(result.nonEmpty){
      startNode = Some(result.head)
      startNode.get
    }else{
      null
    }
  }

  def findMatchableStrip(strip: Strip, eligibleStrips: List[Strip] = null) : List[(Strip, Int)] = {
    val result = ListBuffer.empty[(Strip, Int)]
    val stripsPool = if(eligibleStrips != null){
      eligibleStrips
    }else{
      strips
    }
    for(s <- stripsPool if s.id != strip.id){
      var matchesCount = strip.value.length
      for (c <- s.value.zipWithIndex){
        if(strip.value(c._2) == c._1){
          matchesCount -= 1
        }
      }
      if(matchesCount != strip.value.length){
        result += (s->matchesCount)
      }
    }
    result.toList
  }

  def findMatchCount(s1: String, s2: String): Int = {
    var matchesCount = 0
    for(c <- s1.zipWithIndex){
      if(s2(c._2) == c._1){
        matchesCount += 1
      }
    }
    //    println(s"${s1} - ${s2} => ${matchesCount}")
    matchesCount
  }

//  def findMatchCount(nodes: List[String]): Int = {
//    var totalCount = 0
//    nodes.sortBy(_("value").getOrElse("")).sliding(2).foreach { list: List[Node] =>
//      totalCount += findMatchCount(list.head("value").getOrElse(""), list.last("value").getOrElse(""))
//    }
//    totalCount
//  }

  override def constructGraph(f: Strip => List[(Strip, Int)])(d: (Node, Node) => Boolean) = {
    withTx {
      implicit neo =>
        val wordIndex = indexManager.forNodes("strips")

        for {
          strip <- strips
          matchableStrip = f(strip)
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
        println("Node count: " + getAllNodes.size)
    }
  }
}
