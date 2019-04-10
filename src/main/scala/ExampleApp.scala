
import java.time.LocalDateTime

import scalaz.zio._
import scalaz.zio.console.{Console, putStrLn}
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend
import TimedResult._
import Helpers._
import ZIOHelpers._

object ExampleApp extends App {

  def run(args: List[String]): ZIO[Console, Nothing, StatusCode] =
    httpClientExample.fold[Int](_ => 1, _ => 0)

  def httpClientExample: ZIO[Console, Throwable, Unit] = {

    implicit val backend  = AsyncHttpClientZioBackend()

    // Declare now, run later
    val timedTasks: List[Task[TimedResult[Response[String]]]] = reqs.map(_.send()).map(timeTask)
    val timedResults: Task[List[TimedResult[Response[String]]]] = Task.collectAllPar(timedTasks)

    for {
      _ <- log("BEGIN: Basic Sequential")
      _ <- timePrintWrite("Seq1")(request1)
      _ <- timePrintWrite("Seq2")(request2)
      _ <- log("END:   Basic Sequential\n")

      _ <- log("BEGIN: Fork Join with separate output effect")
      fiber1 <- timeTask(request1.send()).fork
      fiber2 <- timeTask(request2.send()).fork
      fiber3 <- timeTask(request3.send()).fork
      fiber = (fiber1 zip fiber2) zip fiber3
      tuple <- fiber.join
      _ <- printWrite("Fibre1", extractDate)(tuple._1._1)
      _ <- printWrite("Fibre2", extractDate)(tuple._1._2)
      _ <- printWrite("Fibre3", extractDate)(tuple._2)
      _ <- log("END:   Fork Join with separate output effect\n")

      _ <- log("BEGIN: Fork Join with combined output effect")
      tfib1 <- timePrintWrite("Combined1")(request1).fork
      tfib2 <- timePrintWrite("Combined2")(request2).fork
      tfib3 <- timePrintWrite("Combined3")(request3).fork
      _ <- (tfib1 zip tfib2 zip tfib3).join
      _ <- log("END:   Fork Join with combined output effect\n")

      _ <- log("BEGIN: Parallel collect with separate foreach output")
      all <- timedResults
      _ <- ZIO.foreach(all.zipWithIndex)(printWriteI("collectAllPar", extractDate))
      _ <- log("END:   Parallel collect with separate foreach output\n")

      _ <- log("BEGIN: Parallel foreach with combined output effect")
      _ <- ZIO.foreachPar(reqs.zipWithIndex)(timePrintWriteI("foreachPar"))
      _ <- log("END:   Parallel foreach with combined output effect\n")
    } yield backend.close()

  }

}

object ZIOHelpers {

  def log(msg: String): ZIO[Console, Throwable, Unit] = for {
    now <- Task(LocalDateTime.now)
    _ <- putStrLn(s"$now\t$msg")
  } yield ()

}
