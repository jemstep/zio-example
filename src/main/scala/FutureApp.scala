import com.softwaremill.sttp.asynchttpclient.future.AsyncHttpClientFutureBackend

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import com.jemstep.helpers.Helpers._
import com.jemstep.helpers.FutureHelpers._
import com.softwaremill.sttp.SttpBackend

object FutureApp extends App {
  implicit val ec = ExecutionContext.global

  def httpClientExample: Future[Unit] = {

    implicit val backend: SttpBackend[Future, Nothing] = AsyncHttpClientFutureBackend()

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

      _ <- log("BEGIN: Basic Parallel Future")
      c1 <- outside1
      c2 <- outside2
      c3 <- outside3
      _ <- log(c1)
      _ <- log(c2)
      _ <- log(c3)
      _ <- log("END:   Basic Parallel Future\n")

    } yield backend.close()

  }

  val result: Future[Int] = httpClientExample.map(_ => 0).recover {
    case _ => 1
  }
  val r = Await.result(result, 20.seconds)
}
