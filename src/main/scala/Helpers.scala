import java.time.LocalDateTime

import Helpers.TimedResponse
import com.softwaremill.sttp.sttp
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
  def request1[F[_]](implicit b: SttpBackend[F, Nothing]): F[Response[String]] = sttp.get(uri1).send()
  def request2[F[_]](implicit b: SttpBackend[F, Nothing]): F[Response[String]] = sttp.get(uri2).send()
  def request3[F[_]](implicit b: SttpBackend[F, Nothing]): F[Response[String]] = sttp.get(uri3).send()

  def reqs[F[_]](implicit b: SttpBackend[F, Nothing]) = List(request1, request2, request3)

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

  def futureReq(response: Future[Response[String]])(implicit ec: ExecutionContext): Future[TimedResponse] = {
    for {
      t1 <- Future.successful(LocalDateTime.now())
      r  <- response
      t2 <- Future.successful(LocalDateTime.now())
    } yield TimedResult(t1, t2, r)
  }

  def fromFuture(response: Future[Response[String]]): Task[TimedResponse] =
    ZIO.fromFuture(implicit ec => futureReq(response))

}