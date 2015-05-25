package etude5.nonmatches

import etude5.Strip
import etude5.nonmatches.Relationship.Relationship

import scala.collection.mutable.ListBuffer

/**
 * Created by tinhtooaung on 25/05/15.
 */

object Relationship extends Enumeration {
  type Relationship = Value
  val INVALID, ORIGINAL, REVERSED = Value
}

class NonMatchesGraph(strips: List[Strip], length: Int) {

  val max: Int = 200
  val reversedStrips = ListBuffer(strips.map(_.value.reverse): _*)
  val orginalStrips = ListBuffer(strips: _*)
  var finalResult = ListBuffer.empty[String]
  val adjacencyMatrix = Array.ofDim[Relationship](strips.size, strips.size)
  var currentTries = 0
  val used = Array.fill[Boolean](strips.size){false}

  def possible(): List[String] = {
    resetUsed()
    buildRelationships()
    val dfsResult = tryAllStartingPoints
    dfsResult
  }

  def resetUsed() {
    for(i <- used.indices){
      used(i) = false
    }
  }

  def tryAllStartingPoints: List[String] = {
    var partialCarpet = ListBuffer.empty[String]
    for((s, i) <- strips.map(_.value).zipWithIndex){
      currentTries = 0
      partialCarpet += s
      used(i) = true
      partialCarpet = dfs(i, partialCarpet, previousReversed = false)
      if(partialCarpet != null){
        partialCarpet = evaluateResult(partialCarpet.toList)
        finalResult ++= ListBuffer(partialCarpet: _*)
        return partialCarpet.toList
      }else{
        partialCarpet = ListBuffer.empty[String]
      }
      resetUsed()
    }
    null
  }


  def dfs(sourcePos: Int, partialCarpet: ListBuffer[String], previousReversed: Boolean): ListBuffer[String] ={
    var nextResult = ListBuffer.empty[String]
    val available = adjacencyMatrix(sourcePos)
    var nextReversed = false
    var nextString = ""
    var nextPos = 0

    if(partialCarpet.size == length){
      return partialCarpet
    }
    for(i <- available.indices){
      if(!used(i) && available(i) != Relationship.INVALID){
        nextPos = i
        if(available(nextPos) == Relationship.ORIGINAL){
          nextString = orginalStrips(nextPos).value
        }else{
          nextString = reversedStrips(nextPos)
        }
        if(previousReversed){
          nextString = nextString.reverse
        }

        used(nextPos) = true
        partialCarpet += nextString
        nextResult = dfs(nextPos, partialCarpet, nextReversed)

        if(nextResult != null){
          return nextResult
        }

        currentTries += 1
        if(currentTries > max){
          return null
        }
        used(nextPos) = false
        nextReversed = false
        partialCarpet -= partialCarpet.last
      }
    }
    return null
  }


  def evaluateResult(partialCarpet: List[String]): ListBuffer[String] ={
    val result = ListBuffer(partialCarpet: _*)
    for(i <- 1 until result.size){
      if(!hasNoMatch(result(i), result(i - 1))){
        result(i) = result(i).reverse
      }
    }
    result
  }

  def buildRelationships() = {
    for{
      row <- strips.indices
      col <- strips.indices
    }{
      if(row == col){
        adjacencyMatrix(row)(col) = Relationship.INVALID
      }else{
        adjacencyMatrix(row)(col) = determineRelationship(row, col)
      }
    }
  }

  def determineRelationship(rowPos: Int, comparisonPos: Int): Relationship = {
    if(hasNoMatch(strips(rowPos).value, orginalStrips(comparisonPos).value)){
      return Relationship.ORIGINAL
    }
    if(hasNoMatch(strips(rowPos).value, reversedStrips(comparisonPos))){
      return Relationship.REVERSED
    }
    Relationship.INVALID
  }

  def hasNoMatch(a: String, b: String): Boolean = {
    for((c, index) <- a.zipWithIndex){
      if(b(index) == c){
        return false
      }
    }
    true
  }
}
