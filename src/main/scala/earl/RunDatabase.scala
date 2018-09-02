package earl

import java.io.{File, IOException}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
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

  def load(filename: String): RunDatabase = load(Paths.get(filename))
  def load(path: Path): RunDatabase = {
    val fileContents = Files.lines(path).collect(Collectors.joining("\n"))
    decode[RunDatabase](fileContents) match {
      case Left(th) => throw th
      case Right(db) => db
    }
  }

  def loadAll(path: Path): Seq[RunDatabase] = {
    val visitor = new DatabaseFileVisitor
    Files.walkFileTree(path, visitor)
    visitor.result()
  }

  def loadAll(root: String): Seq[RunDatabase] = loadAll(Paths.get(root))

  private[this] class DatabaseFileVisitor extends FileVisitor[Path] {
    private[this] val databases = IndexedSeq.newBuilder[RunDatabase]

    def result(): Seq[RunDatabase] = databases.result()

    override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = FileVisitResult.CONTINUE
    override def visitFileFailed(file: Path, exc: IOException): FileVisitResult = FileVisitResult.CONTINUE
    override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = FileVisitResult.CONTINUE

    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      if (file.toString.endsWith(".json")) {
        try {
          databases += load(file)
        } catch {
          case _: Throwable =>
        }
      }
      FileVisitResult.CONTINUE
    }
  }
}
