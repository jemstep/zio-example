import java.time.LocalDateTime

import Helpers.TimedResponse
import com.softwaremill.sttp.{Request, sttp}
import com.softwaremill.sttp._
import org.slf4j.Logger
import scalaz.zio.console.{Console, putStrLn}
import scalaz.zio.{Task, ZIO}

import scala.concurrent.{ExecutionContext, Future}

object Helpers {
  val githubQuery = "http language:scala"
  val uri1 = uri"https://api.github.com/search/repositories?q=$githubQuery"

  val searchQuery = "scala +zio +cats-effect"
  val uri2 = uri"https://www.google.com/search?q=$searchQuery"
  val uri3 = uri"https://duckduckgo.com/search?q=$searchQuery"
  val request1: Request[String, Nothing] = sttp.get(uri1)
  val request2: Request[String, Nothing] = sttp.get(uri2)
  val request3: Request[String, Nothing] = sttp.get(uri3)

  def reqs[F[_]](implicit b: SttpBackend[F, Nothing]) = List(request1, request2, request3).map(_.send())

  val errorResponse: Task[Response[String]] = Task.fail(new RuntimeException("Oh no! I'm a failure!"))

  type TimedResponse = TimedResult[Response[String]]

  def handleError[R[_], S](err: Throwable, logger: Logger)(implicit b: SttpBackend[R, S]): ZIO[Console, Nothing, StatusCode] = for {
    statusCode <- Task.succeed(err.getMessage.length)
    errorMsg = s"App failure. Exiting with status '$statusCode'. Error message: '${err.getMessage}'"
    _ <- Task.succeed(logger.error(errorMsg))
    _ <- putStrLn(errorMsg)
    _ <- Task.succeed(b.close())
  } yield statusCode

}


object FutureHelpers {

  def log[A](msg: A)(implicit ec: ExecutionContext): Future[Unit] = for {
    now <- Future.successful(LocalDateTime.now)
    _ <- Future.successful(println(s"$now\t$msg"))
  } yield ()

  def futureReq(req: Request[String, Nothing])(implicit ec: ExecutionContext, b: SttpBackend[Future, Nothing]): Future[TimedResponse] = {
    for {
      t1 <- Future.successful(LocalDateTime.now())
      r  <- req.send()
      t2 <- Future.successful(LocalDateTime.now())
    } yield TimedResult(t1, t2, r)
  }

  def fromFuture(request: Request[String, Nothing])(implicit b: SttpBackend[Future, Nothing]): Task[TimedResponse] =
    ZIO.fromFuture(implicit ec => futureReq(request))

}