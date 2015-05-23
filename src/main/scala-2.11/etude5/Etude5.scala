package etude5

import java.util.{Scanner, UUID}

import scala.collection.mutable.ListBuffer
import scala.util.{Random, Try}

import scalax.file.Path

/**
 * Created by tinhtooaung on 7/05/15.
 */
object Etude5 {

  def main(args: Array[String]): Unit = {

    if(args.isEmpty || args.length != 2){
      println("Usage: [-n] [size] :No Matches")
      println("Usage: [-m] [size] :Maximum matches")
      println("Usage: [-b] [size] :Balance")
      return
    }

//    var path: Path = Path.fromString("/tmp/temp-neo-best")
//    Try(path.deleteRecursively(continueOnFailure = false))
//
    cleanup

    val data: List[Strip] = getInputData
    args(0) match {
      case "-m" => {
        new BestMatchesFlipGenerator(data).findBestMatches(args(1).toInt)
      }
      case "-n" => {
        new NonMatchesGenerator(data.toSet).findNonMatches(args(1).toInt)
      }
      case "-b" => {
//        new BalanceMatchesGenerator(data).findBalanceMatches(args(1).toInt)
        new BalanceMatchesRandomGenerator(data).findBestMatches(args(1).toInt)
      }
      case _ => println("Not implemented yet")
    }
  }

  def getInputData : List[Strip]= {
    val scanner = new Scanner(System.in)
    var id = 1
    try{
      val inputs = ListBuffer.empty[Strip]
      while(scanner.hasNextLine){
        val line: String = scanner.nextLine()
        if(line != ""){
          inputs += Strip(line, id.toString)
          id += 1
        }
      }
      val result = inputs.toList.sortBy { s =>
        inputs.count(_.value == s.value)
      }
      result.reverse
    }finally {
      scanner.close()
    }
  }

  def cleanup = {
    val path = Path.fromString("/tmp/neo/non")
    Try(path.deleteRecursively(continueOnFailure = false))
  }
}
