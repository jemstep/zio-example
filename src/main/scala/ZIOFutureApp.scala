
import scalaz.zio._
import com.softwaremill.sttp._
import com.jemstep.helpers.Helpers._
import ZIOHelpers._
import com.jemstep.time.OurTime
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend
import org.slf4j.{Logger, LoggerFactory}
import scalaz.zio.clock.Clock
import scalaz.zio.console.Console

import scala.concurrent.{ExecutionContext, Future}
import com.jemstep.helpers.FutureHelpers

object ZIOFutureApp extends App {
  def logger: Logger = LoggerFactory.getLogger(this.getClass)
  implicit val ec = ExecutionContext.global
  implicit val backend: SttpBackend[Future, Nothing] = AsyncHttpClientFutureBackend()
  val environment = new Clock.Live with Console.Live with OurTime.Live

  def run(args: List[String]) =
    httpClientExample.provide(environment).foldM(err => handleError(err, logger), _ => UIO.succeed(0))

  def httpClientExample: ZIO[OurAppEnv, Throwable, Unit] = {

    val zfut1: Task[TimedResponse] = FutureHelpers.fromFuture(request1)
    val zfut2: Task[TimedResponse] = FutureHelpers.fromFuture(request2)
    val zfut3: Task[TimedResponse] = FutureHelpers.fromFuture(request3)

    val timedTasks: List[Task[TimedResponse]] = List(zfut1, zfut2, zfut3)
    val timedResults: Task[List[TimedResponse]] = Task.collectAllPar(timedTasks)

    for {
      _ <- log("BEGIN: ZIO Sequential Future 1")
      seq1 <- FutureHelpers.fromFuture(request1)
      seq2 <- FutureHelpers.fromFuture(request2)
      _ <- printWrite("ZIOFuture1")(seq1)
      _ <- printWrite("ZIOFuture2")(seq2)
      _ <- log("END:   ZIO Sequential Future 1\n")

      _ <- log("BEGIN: ZIO Sequential Future 2 - outside val")
      seq3 <- zfut1
      seq4 <- zfut2
      _ <- printWrite("ZIOFuture3")(seq3)
      _ <- printWrite("ZIOFuture4")(seq4)
      _ <- log("END:   ZIO Sequential Future 2 - outside val\n")

      _ <- log("BEGIN: ZIO Parallel Future 1")
      all1 <- timedResults
      _ <- ZIO.foreach(all1.zipWithIndex)(printWriteI("collectAllPar-fromFuture"))
      _ <- log("END:   ZIO Parallel Future 1\n")

      _ <- log("BEGIN: ZIO Parallel Future 2")
      all2 <- Task.collectAllPar(List(FutureHelpers.fromFuture(request1), zfut2.flatMap(_ => Task.fail(new RuntimeException("Oops!"))), FutureHelpers.fromFuture(request3)))
      _ <- ZIO.foreach(all2.zipWithIndex)(printWriteI("collectAllPar-fromFuture"))
      _ <- log("END:   ZIO Parallel Future 2\n")

    } yield backend.close()
  }

}
