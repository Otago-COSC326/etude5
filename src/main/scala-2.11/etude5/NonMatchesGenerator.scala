package etude5

import java.util.UUID

import eu.fakod.neo4jscala.{Cypher, EmbeddedGraphDatabaseServiceProvider, Neo4jWrapper, Neo4jIndexProvider}
import org.neo4j.graphdb.factory.GraphDatabaseSettings
import org.neo4j.graphdb.traversal.{Evaluation, Evaluator, Evaluators}
import org.neo4j.graphdb.{Direction, Node, Path}
import org.neo4j.kernel.Traversal
import org.neo4j.kernel.impl.cache.NoCacheProvider

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import scala.util.{Random, Try}


/**
 * Created by tinhtooaung on 14/05/15.
 */
class NonMatchesGenerator(var strips: Set[Strip]) extends Neo4jWrapper with EmbeddedGraphDatabaseServiceProvider with Cypher{

//    override def neo4jStoreDir: String = s"data/${UUID.randomUUID().toString}"
  override def neo4jStoreDir: String = s"/tmp/neo/non"

  override def configParams = {
    Map[String, String](GraphDatabaseSettings.cache_type.name() -> NoCacheProvider.NAME)
  }

  //  override def NodeIndexConfig = ("strips", Some(Map("provider" -> "lucene", "type" -> "fulltext"))) :: Nil

  strips ++= strips.map { strip =>
    Strip(strip.value.reverse, strip.id)
  }

  init

  def init = {
    constructGraph { strip =>
      findNotMatchableStrips(strip)
    }{ (start, end) =>
      !start.getRelationships.exists { rel =>
        val endNode: Node = rel.getEndNode()
        (endNode("uniqueId").getOrElse(""), endNode("value").getOrElse("")) ==
          (end("uniqueId").getOrElse(""), end("value").getOrElse(""))
      }
    }
  }


  def findNonMatches(length: Int): Boolean = {
    var result = getNonMatches(length)
    result match {
      case Left(s) => {
        if(s.nonEmpty){
          s.foreach { node =>
//            println(s"${node("value").getOrElse("")} - ${node("uniqueId").getOrElse("")}")
            println(s"${node("value").getOrElse("")}")
          }
          println(s"${s.size}")
          true
        }else{
          println("not possible")
          false
        }
      }
      case Right(s) =>{
        if(s.nonEmpty){
          println(s.head.value)
          true
        }else{
          println("not possible")
          false
        }
      }
    }
  }

  def getNonMatches(length: Int): Either[List[Node], List[Strip]] ={
    if(strips.isEmpty || (strips.size < length)){
      return Right(List.empty[Strip])
    }
    if(length == 1){
      return Right(List(strips.head))
    }
    var possiblePaths = ListBuffer[Path]()
    for{
      startStrip <- strips
    } {
      val startNode = getNodeByTuple(id = startStrip.id, value = startStrip.value)
      if (startNode != null) {
        val newPaths = Traversal.description().depthFirst()
          .relationships(string2RelationshipType("matches"), Direction.BOTH)
          .evaluator(Evaluators.atDepth(length - 1))
          .evaluator(new Evaluator {
              override def evaluate(path: Path): Evaluation = {
                val ids = path.nodes().map(_("uniqueId").getOrElse("")).filter(_ != "").toList
                if(ids.distinct.size == ids.size){
                  Evaluation.INCLUDE_AND_CONTINUE
                }else{
                  Evaluation.EXCLUDE_AND_CONTINUE
                }
              }
            })
          //          .uniqueness(Uniqueness.NODE_GLOBAL)
          .traverse(startNode).iterator().toList
        possiblePaths ++= ListBuffer(newPaths: _*)
      }else{
        throw new Exception("Start node is null")
      }
    }

    val paths = possiblePaths.filter {path =>
      path.length() >= length - 1
    }.filter { path =>
      val ids = path.nodes.map(_("uniqueId").getOrElse("")).toList
      ids.distinct.size == ids.size
    }.filter { path =>
      var valid = true
      path.nodes().toList.sliding(2, 1).foreach { list =>
        if(valid){
          valid = isNoMatches(list.head("value").getOrElse(""), list.last("value").getOrElse(""))
        }
      }
      valid
    }

    if(paths.nonEmpty){
      Left(List[Node](paths.head.nodes().toList: _*))
    }else{
      Right(List.empty[Strip])
    }
  }

  def findNotMatchableStrips(strip: Strip): List[(Strip, Int)] ={
    val result = ListBuffer.empty[(Strip, Int)]
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
        result += (s -> 1)
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

  def constructGraph(f: Strip => List[(Strip, Int)])(d: (Node, Node) => Boolean) = {
    withTx {
      implicit neo =>
        val wordIndex = indexManager.forNodes("strips")
        //        val wordIndex = getNodeIndex("strips").get

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
            //            wordIndex += (start, "combo", MD5.hash((strip.id, strip.value).toString))
            //            wordIndex.putIfAbsent(start, "uniqueId", strip.id)
          }

          var end = getNodeByTuple(id = matchableWord._1.id, value = matchableWord._1.value)
          if(end == null && matchableWord != null){
            end = createNode
            end("value") = matchableWord._1.value
            end("uniqueId") = matchableWord._1.id
            wordIndex.add(end, "combo", (matchableWord._1.id, matchableWord._1.value))
            //            wordIndex += (end, "combo", MD5.hash((matchableWord._1.id, matchableWord._1.value).toString))
            //            wordIndex.putIfAbsent(end, "uniqueId", matchableWord._1.id)
          }
          //          val noRelation = d(start, end)
          //          if(noRelation){
          start --> "matches" --> end
          //            start.createRelationshipTo(end, "matches").setProperty("count", matchableWord._2)
          //          }
        }
//        println("Node count: " + getAllNodes.size)
    }
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

  object MD5 {
    def hash(s: String) = {
      val m = java.security.MessageDigest.getInstance("MD5")
      val b = s.getBytes("UTF-8")
      m.update(b, 0, b.length)
      new java.math.BigInteger(1, m.digest()).toString(16)
    }
  }
}
