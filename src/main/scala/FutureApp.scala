import java.time.LocalDateTime

import com.softwaremill.sttp.{Request, Response, SttpBackend}
import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._

import Helpers._
import FutureHelpers._

object FutureApp extends App {
  implicit val ec = ExecutionContext.global

  def httpClientExample: Future[Unit] = {

    implicit val backend  = AsyncHttpClientFutureBackend()

    val outside1 = futureReq(request1)
    val outside2 = futureReq(request2)
    val outside3 = futureReq(request3)

    for {
      _ <- log("BEGIN: Basic Sequential Future")
      seq1 <- futureReq(request1)
      seq2 <- futureReq(request2)
      seq3 <- futureReq(request3)
      _ <- log(seq1)
      _ <- log(seq2)
      _ <- log(seq3)
      _ <- log("END:   Basic Sequential Future\n")

      _ <- log("BEGIN: Basic Concurrent Future")
      c1 <- outside1
      c2 <- outside2
      c3 <- outside3
      _ <- log(c1)
      _ <- log(c2)
      _ <- log(c3)
      _ <- log("END:   Basic Concurrent Future\n")

    } yield backend.close()

  }

  val result: Future[Int] = httpClientExample.map(_ => 0).recover {
    case _ => 1
  }
  val r = Await.result(result, 20.seconds)
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

}