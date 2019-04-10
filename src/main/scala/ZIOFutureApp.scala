
import scalaz.zio._
import scalaz.zio.console.Console
import com.softwaremill.sttp._
import Helpers._
import ZIOHelpers._
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend

import scala.concurrent.{ExecutionContext, Future}

object ZIOFutureApp extends App {
  implicit val ec = ExecutionContext.global

  def run(args: List[String]): ZIO[Console, Nothing, StatusCode] =
    httpClientExample.fold[Int](_ => 1, _ => 0)

  def httpClientExample: ZIO[Console, Throwable, Unit] = {

    implicit val backend: SttpBackend[Future, Nothing] = AsyncHttpClientFutureBackend()

    val zfut1: Task[TimedResult[Response[String]]] = FutureHelpers.fromFuture(request1)
    val zfut2: Task[TimedResult[Response[String]]] = FutureHelpers.fromFuture(request2)
    val zfut3: Task[TimedResult[Response[String]]] = FutureHelpers.fromFuture(request3)

    val timedTasks: List[Task[TimedResult[Response[String]]]] = List(zfut1, zfut2, zfut3)
    val timedResults: Task[List[TimedResult[Response[String]]]] = Task.collectAllPar(timedTasks)

      for {
      _ <- log("BEGIN: ZIO Sequential Future 1")
      seq1 <- FutureHelpers.fromFuture(request1)
      seq2 <- FutureHelpers.fromFuture(request2)
      _ <- printWrite("ZIOFuture1", extractDate)(seq1)
      _ <- printWrite("ZIOFuture2", extractDate)(seq2)
      _ <- log("END:   ZIO Sequential Future 1\n")

      _ <- log("BEGIN: ZIO Sequential Future 2 - outside val")
      seq3 <- zfut1
      seq4 <- zfut2
      _ <- printWrite("ZIOFuture3", extractDate)(seq3)
      _ <- printWrite("ZIOFuture4", extractDate)(seq4)
      _ <- log("END:   ZIO Sequential Future 2 - outside val\n")

      _ <- log("BEGIN: ZIO Parallel Future 1")
      all1 <- timedResults
      _ <- ZIO.foreach(all1.zipWithIndex)(printWriteI("collectAllPar-fromFuture", extractDate))
      _ <- log("END:   ZIO Parallel Future 1\n")

      _ <- log("BEGIN: ZIO Parallel Future 2")
      all2 <- Task.collectAllPar(List(FutureHelpers.fromFuture(request1), FutureHelpers.fromFuture(request2), FutureHelpers.fromFuture(request3)))
      _ <- ZIO.foreach(all2.zipWithIndex)(printWriteI("collectAllPar-fromFuture", extractDate))
      _ <- log("END:   ZIO Parallel Future 2\n")

    } yield backend.close()
  }

}
