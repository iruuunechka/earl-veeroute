package earl

import java.io.File
import java.nio.file._
import java.util.stream.Collectors

import scala.collection.JavaConverters._

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

case class RunDatabase(
  problemName: String,
  objectives: Seq[String],
  optimizers: Seq[String],
  individuals: Seq[Seq[Double]],
  acts: Seq[RunDatabase.OptimizationAct]
) {
  def saveTo(filename: String): Unit = {
    Files.write(Paths.get(filename), Seq(this.asJson.toString()).asJava)
  }
  def saveTo(file: File): Unit = {
    Files.write(file.toPath, Seq(this.asJson.toString()).asJava)
  }
}

object RunDatabase {
  case class OptimizationAct(source: Int, target: Int, optimizer: Int, firstObjective: Int, time: Long)

  def load(filename: String): RunDatabase = {
    val fileContents = Files.lines(Paths.get(filename)).collect(Collectors.joining("\n"))
    decode[RunDatabase](fileContents) match {
      case Left(th) => throw th
      case Right(db) => db
    }
  }
}
