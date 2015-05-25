package etude5.nonmatches

import etude5.Strip

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by tinhtooaung on 25/05/15.
 */
class Graph {
  val vertexList = ListBuffer[Vertex]()
  val adjMat = Array.ofDim[Int](200, 200)
  for{
    i <- 0 until 200
    j <- 0 until 200
  }{
    adjMat(i)(j) = 0
  }
  var nVerts: Int = 0
  val stack = mutable.Stack.empty[Int]

  def addVertex(lab: String): Int ={
    vertexList += new Vertex(lab)
    nVerts += 1
    vertexList.size - 1
  }

  def addEdge(start: Int, end: Int) = {
    adjMat(start)(end) = 1
    adjMat(end)(start) = 1
  }

  def displayVertex(v: Int) = {
    println(vertexList(v).label)
  }

  def dfs(startIndex: Int) = {
    val vistedNodes = ListBuffer.empty[Vertex]
    vistedNodes += vertexList(startIndex)
    vertexList(startIndex).wasVisted = true
//    displayVertex(startIndex)
    stack.push(startIndex)

    while(stack.nonEmpty){
      val v = getAdjUnvisitedVertex(stack.head)
      if(v == -1){
        stack.pop
      }else{
        vertexList(v).wasVisted = true
        vistedNodes += vertexList(v)
//        displayVertex(v)
        stack.push(v)
      }
    }

    for(i <- 0 until nVerts){
      vertexList(i).wasVisted = false
    }

    vistedNodes
  }

  def getAdjUnvisitedVertex(v: Int): Int ={
    for(j <- 0 to nVerts){
      if(adjMat(v)(j) == 1 && vertexList(j).wasVisted == false){
        return j
      }
    }
    return -1
  }
}

object Graph {
  def run(strips: List[Strip]) = {
    val graph = new Graph
    strips.foreach { strip =>
      val index = graph.addVertex(strip.value)
      findNonMatchableStrip(strip, strips).foreach { matchable =>
        graph.addEdge(index, graph.addVertex(matchable.value))
      }
    }
    for(i <- 0 until graph.vertexList.size){
      println(graph.dfs(i).map(_.label).mkString(" "))
    }
  }

  def findNonMatchableStrip(strip: Strip, strips: List[Strip]) = {
    val result = ListBuffer.empty[Strip]
    for{
      s <- strips.filter(_ != strip)
    }{
      if(isNonMatches(List(s.value, strip.value)) ||
        isNonMatches(List(s.value.reverse, strip.value))){
        result += s
      }
    }
    result.toList
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
}