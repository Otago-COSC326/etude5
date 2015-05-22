package etude5

import java.util.UUID

import org.neo4j.graphalgo.GraphAlgoFactory
import org.neo4j.graphdb.{Direction, Node, Path}
import org.neo4j.kernel.Traversal

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/**
 * Created by tinhtooaung on 14/05/15.
 */
class NonMatchesGreedyGenerator(var strips: Set[Strip]){

  strips ++= strips.map { strip =>
    Strip(strip.value.reverse, strip.id)
  }

  def findNonMatches(length: Int): Unit = {
    val result = getNonMatches(length)
    if(result.nonEmpty){
      println(result.mkString("\n"))
      println(result.size)
    }else{
      println("not possible")
    }

  }

  def getNonMatches(length: Int): mutable.LinkedHashSet[Strip] ={
    if(strips.isEmpty || (strips.size < length)){
      return mutable.LinkedHashSet.empty[Strip]
    }
    if(length == 1){
      return mutable.LinkedHashSet(strips.head)
    }

    var stock = mutable.ListBuffer[Strip](strips.toList: _*)
    var result = mutable.LinkedHashSet.empty[Strip]
    do {
      result ++= processItems(length, mutable.Queue[Strip](stock.toList: _*), result)
      stock = stock.filter { stockItem =>
        !result.map(_.id).contains(stockItem.id)
      }
      println(s"${result.size} - ${stock.size}")
    }while(result.size < length && stock.nonEmpty)
    println(result.toList.distinct.size == result.toList.size)
    result
  }

  def processItems(length:Int, startQueue: mutable.Queue[Strip],
                   currentResult: mutable.LinkedHashSet[Strip]): mutable.LinkedHashSet[Strip] = {
    val backQueue = mutable.Queue.empty[Strip]
    val cyclic = mutable.HashSet.empty[Strip]
    val startQueue = mutable.Queue[Strip](strips.toList: _*)
    val result = mutable.LinkedHashSet.empty[Strip]

    do{
      var candidates = List.empty[Strip]
      if(backQueue.isEmpty){
        cyclic.clear()
        result.clear()
        candidates = findNotMatchableStrips(startQueue.dequeue())
      }else{
        val bq = backQueue.dequeue()
        cyclic += bq
//        var valid = !result.map(_.id).contains(bq.id)
//        if(valid){
//          valid = !currentResult.map(_.id).contains(bq.id)
//        }
//        if(valid){
//          candidates = findNotMatchableStrips(bq)
//        }
        candidates = findNotMatchableStrips(bq)
      }
      if(candidates.nonEmpty){
        var valid = !result.map(_.id).contains(candidates.head.id)
        if(valid){
          valid = !currentResult.map(_.id).contains(candidates.head.id)
        }
        if(valid){
          println(result.size)
          result += candidates.head
          candidates.tail.foreach { can =>
            if(!backQueue.contains(can))backQueue.enqueue(can)
          }
        }
      }else if(backQueue.nonEmpty){
        val lastItemInQueue = backQueue.last
        backQueue.dequeueFirst(_ == lastItemInQueue)
        backQueue.enqueue(result.last)
        result -= result.last
        result += lastItemInQueue
      }
    }while(result.size < length && startQueue.nonEmpty)
    result
  }

  def findNotMatchableStrips(strip: Strip): List[Strip] ={
    val result = ListBuffer.empty[Strip]
    for{
      s <- strips if (s.value != strip.value) && (s.id != strip.id)
    }{
      var matchesCount = 0
      for(c <- s.value.zipWithIndex){
        if(strip.value(c._2) == c._1){
          matchesCount += 1
        }
      }
      if(matchesCount == 0){
        result += s
      }
    }
    result.toList
  }

  def isNoMatches(s1: String, s2: String): Boolean = {
    var matchesCount = 0
    for(c <- s1.zipWithIndex){
      if(s2(c._2) == c._1){
        matchesCount += 1
      }
    }
    matchesCount == 0
  }
}
