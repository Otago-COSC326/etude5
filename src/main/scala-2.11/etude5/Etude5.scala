package etude5

import java.util.{Scanner, UUID}

import scala.collection.mutable.ListBuffer
import scala.util.Try

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

    var path: Path = Path.fromString("/tmp/temp-neo-best")
    Try(path.deleteRecursively(continueOnFailure = false))

    path = Path.fromString("/tmp/temp-neo-non")
    Try(path.deleteRecursively(continueOnFailure = false))


    args(0) match {
      case "-m" => {
        val data: List[Strip] = getInputData()
        new BestMatchesGenerator(data).findBestMatches(args(1).toInt)
      }
      case "-n" => {
        val data: List[Strip] = getInputData(false)
        new NonMatchesGenerator(data).findNonMatches(args(1).toInt)
      }
      case "-b" => {
        val data: List[Strip] = getInputData()
        new BalanceMatchesGenerator(data).findBalanceMatches(args(1).toInt)
      }
      case _ => println("Not implemented yet")
    }
  }

  def getInputData(allowDuplicated: Boolean = true) : List[Strip]= {
    val scanner = new Scanner(System.in)
    try{
      val inputs = ListBuffer.empty[Strip]
      while(scanner.hasNextLine){
        val line: String = scanner.nextLine()
        if(line != ""){
          if(allowDuplicated){
            inputs += Strip(line, UUID.randomUUID().toString)
          }else{
            if(!inputs.exists(_.value == line)){
              inputs += Strip(line, UUID.randomUUID().toString)
            }
          }
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

}
