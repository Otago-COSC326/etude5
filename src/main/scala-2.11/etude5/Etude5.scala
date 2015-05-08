package etude5

import java.util.Scanner
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

    val path: Path = Path.fromString("/tmp/temp-neo-test")
    Try(path.deleteRecursively(continueOnFailure = false))

    val generator = new CarpetGenerator(getInputData, args(0))
    generator.getBestMatches(args(1).toInt)
  }

  def getInputData : List[String]= {
    val scanner = new Scanner(System.in)
    try{
      val inputs = ListBuffer.empty[String]
      while(scanner.hasNextLine){
        inputs += scanner.nextLine()
      }
      inputs.toList
    }finally {
      scanner.close()
    }
  }

}
