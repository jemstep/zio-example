import java.time.LocalDateTime

import com.softwaremill.sttp.{Request, sttp}
import com.softwaremill.sttp._
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
  val reqs = List(request1, request2, request3)

  val extractDate: Response[String] => Option[String] = r => r.header("Date")

}


object FutureHelpers {

  def log[A](msg: A)(implicit ec: ExecutionContext): Future[Unit] = for {
    now <- Future.successful(LocalDateTime.now)
    _ <- Future.successful(println(s"$now\t$msg"))
  } yield ()

  def futureReq(req: Request[String, Nothing])(implicit ec: ExecutionContext, b: SttpBackend[Future, Nothing]): Future[TimedResult[Response[String]]] = {
    for {
      t1 <- Future.successful(LocalDateTime.now())
      r  <- req.send()
      t2 <- Future.successful(LocalDateTime.now())
    } yield TimedResult(t1, t2, r)
  }

  def fromFuture(request: Request[String, Nothing])(implicit b: SttpBackend[Future, Nothing]): Task[TimedResult[Response[String]]] =
    ZIO.fromFuture(implicit ec => futureReq(request))

}