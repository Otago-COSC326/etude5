package etude5

import eu.fakod.neo4jscala.{EmbeddedGraphDatabaseServiceProvider, Neo4jWrapper}

import scala.collection.immutable.HashSet
import scala.collection.mutable.ListBuffer

/**No
 * Created by tinhtooaung on 14/05/15.
 */
class BalanceMatchesRandomGenerator(strips: List[Strip]) extends Neo4jWrapper with EmbeddedGraphDatabaseServiceProvider{

  def findBalanceMatches(length: Int): Unit ={
    val bestLength = length/2
    val nonLength = length - bestLength
    val result = ListBuffer.empty[String]
    var matchCount = 0
    val nonMatchCount = (nonLength - 1) * 3

    var lastMatchValue: String = ""
    val bestMatchesGenerator: BestMatchesGenerator = new BestMatchesGenerator(strips)


    bestMatchesGenerator.getBestMatches(bestLength) match {
      case Left(s) => {
        result ++= s.map(_("value").getOrElse(""))
        matchCount = bestMatchesGenerator.findMatchCount(s)
        lastMatchValue = s.last("value").getOrElse("")
      }
      case Right(s) => {
        result ++= s.map(_.value)
        lastMatchValue = s.last.value
      }
    }

    var firstNoMatches: String = ""
    new NonMatchesGenerator(HashSet(strips : _*)).getNonMatches(nonLength) match {
      case Left(s) => {
        result ++= s.map(_("value").getOrElse(""))
        firstNoMatches = s.head("value").getOrElse("")
      }
      case Right(s) => {
        result ++= s.map(_.value)
        firstNoMatches = s.head.value
      }
    }

    println(result.mkString("\n"))
    val concatMatchesCount = bestMatchesGenerator.findMatchCount(lastMatchValue, firstNoMatches)
    val concatNonMatchesCount = strips.head.value.length - concatMatchesCount
    val totalMatches = matchCount + concatMatchesCount
    val totalNonMatches = nonMatchCount + concatNonMatchesCount
    val absolute = totalMatches - totalNonMatches
    println(Math.abs(absolute))
  }

  override def neo4jStoreDir: String = "/tmp/temp-neo-balance"
}
