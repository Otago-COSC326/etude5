package etude5

import java.util.{UUID, Scanner}
import sun.security.provider.MD5

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

    val path: Path = Path.fromString("/tmp/temp-neo-test")
    Try(path.deleteRecursively(continueOnFailure = false))


    args(0) match {
      case "-b" => {
        val data: List[Strip] = getInputData()
        if(data.isEmpty || (data.size < args(1).toInt)){
          return
        }
        new BestMatchesGenerator(data, args(0)).findBestMatches(args(1).toInt)
      }
      case "-n" => {
        val data: List[Strip] = getInputData(false)
        new NonMatchesGenerator(data).findNonMatches(args(1).toInt)
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
