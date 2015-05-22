package etude5

import eu.fakod.neo4jscala.{EmbeddedGraphDatabaseServiceProvider, Neo4jWrapper}

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.Random

/**No
 * Created by tinhtooaung on 14/05/15.
 */
class BalanceMatchesRandomGenerator(strips: List[Strip]){

  def findBestMatches(length: Int) = {

    var bestResult: Option[(Int, List[Strip])] = None
    var bufferList = ListBuffer.empty[Strip]
    var matchCount = 0
    var nonMatchCount = 0
    var currentBalanceCount = 0
    for(i <- 1 to 1000){
      bufferList = ListBuffer[Strip](Random.shuffle(strips).take(length): _*)
      matchCount = countMatches(bufferList.toList)
      nonMatchCount = countNonMatches(bufferList.toList)
      currentBalanceCount = Math.abs(matchCount - nonMatchCount)
      if(bestResult.isDefined){
        if(bestResult.get._1 > currentBalanceCount){
          bestResult = Some(currentBalanceCount -> bufferList.toList)
        }
      }else{
        bestResult = Some(currentBalanceCount -> bufferList.toList)
      }
    }
    println(bestResult.get._2.map(_.value).mkString("\n"))
    println(bestResult.get._1)
  }

  def countMatches(list: List[Strip]): Int = {
    list.sliding(2).foldLeft(0){ (matches, strips) =>
      val strip1: Strip = strips.head
      val strip2: Strip = strips.last
      var currentMatches: Int = matches
      for((char, index) <- strip1.value.zipWithIndex){
        if(strip2.value(index) == char){
          currentMatches += 1
        }
      }
      currentMatches
    }
  }

  def countNonMatches(list: List[Strip]): Int = {
    list.sliding(2).foldLeft(0){ (nonmatches, strips) =>
      val strip1: Strip = strips.head
      val strip2: Strip = strips.last
      var currentNonMatches: Int = nonmatches
      for((char, index) <- strip1.value.zipWithIndex){
        if(strip2.value(index) != char){
          currentNonMatches += 1
        }
      }
      currentNonMatches
    }
  }
}
