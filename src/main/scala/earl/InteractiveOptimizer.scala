package earl

import java.util.StringTokenizer

import earl.webio.VeeRouteService

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object InteractiveOptimizer {
  def rangeValidator(range: Range)(arg: Int): Option[String] = {
    if (range.contains(arg)) None else Some(s"Number $arg is not in $range")
  }

  def toInts(str: String): Seq[Int] = {
    val st = new StringTokenizer(str, ", ")
    IndexedSeq.fill(st.countTokens())(st.nextToken().toInt)
  }

  def rangeDistinctValidator(range: Range)(seq: Seq[Int]): Option[String] = {
    seq.find(i => !range.contains(i)) match {
      case Some(j) => Some(s"Number $j is not in $range")
      case None => if (seq.distinct.size != seq.size) {
        Some("Duplicate values found")
      } else None
    }
  }

  def readOrStop[T](stopWord: String, parse: String => T, validate: T => Option[String]): Option[T] = {
    Try {
      print(">")
      StdIn.readLine()
    } match {
      case Success(line) if line == stopWord => None
      case Success(line) =>
        Try {
          parse(line)
        } match {
          case Success(parsed) =>
            validate(parsed) match {
              case None => Some(parsed)
              case Some(error) =>
                println(error)
                readOrStop(stopWord, parse, validate)
            }
          case Failure(th) =>
            println(s"Error: could not parse input")
            th.printStackTrace()
            readOrStop(stopWord, parse, validate)
        }
      case Failure(th) =>
        println(s"Error: could not read a line")
        th.printStackTrace()
        readOrStop(stopWord, parse, validate)
    }
  }

  def workWithDataset(d: VeeRouteService.Dataset): Unit = {
    println(s"Dataset #${d.reference.number} (${d.reference.name}) is loaded")
    println("Functions:")
    d.functions.foreach(f => println(s"    ${f.number} => ${f.name} (isPublic = ${f.isPublic})"))

    def composeOne(printLast: Boolean): Unit = {
      println(s"Individual #${d.individuals.size - 1}:")
      d.individuals.last.fitness.foreach(p => println(s"    $p"))
      println(s"Choose the index of the next individual, or type 'stop'. [0; ${d.individuals.size - 1}] available.")
      readOrStop("stop", _.toInt, rangeValidator(d.individuals.indices)) match {
        case Some(id) =>
          println(s"Chosen individual $id:")
          val ind = d.individuals(id)
          ind.fitness.foreach(p => println(s"    $p"))
          println(s"Choose the optimizer to be used or type 'break'. [0; ${VeeRouteService.optimizers.size - 1}] available.")
          readOrStop("break", _.toInt, rangeValidator(VeeRouteService.optimizers.indices)) match {
            case Some(optimizerIndex) =>
              println(s"Choose the function(s) to optimize or type 'break'. [0; ${d.functions.size - 1}] available.")
              readOrStop("break", toInts, rangeDistinctValidator(d.functions.indices)) match {
                case Some(functionIndices) =>
                  ind.optimize(VeeRouteService.optimizers(optimizerIndex), functionIndices.map(d.functions) :_*)
                  composeOne(true)
                case None =>
                  composeOne(false)
              }
            case None =>
              composeOne(false)
          }
        case None =>
      }
    }

    composeOne(true)
  }

  def main(args: Array[String]): Unit = {
    val srv = VeeRouteService
    val datasets = srv.datasets
    println(s"${datasets.size} datasets found:")
    datasets.foreach(d => println(s"    ${d.number} => ${d.name}"))

    def readOne(): Unit = {
      println(s"Choose the number of a dataset, or type 'exit'. [0; ${datasets.size - 1}] available.")
      readOrStop("exit", _.toInt, rangeValidator(datasets.indices)) match {
        case Some(number) =>
          srv.withDataset(datasets(number))(workWithDataset)
          readOne()
        case None =>
      }
    }
    readOne()
  }
}
