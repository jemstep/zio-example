package com.jemstep.randomInternetRequests

import scalaz.zio.ZIO
import com.jemstep.helpers.Helpers
import scalaz.zio._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend

trait RandomInternetRequests {
  def randomInternetRequests: RandomInternetRequests.Service[RandomInternetRequests]
}

object RandomInternetRequests extends Serializable {
  trait Service[R] {
    def request1: ZIO[RandomInternetRequests, Throwable, String]
    def request2: ZIO[RandomInternetRequests, Throwable, String]
    def request3: ZIO[RandomInternetRequests, Throwable, String]
  }

  trait Live extends RandomInternetRequests {
    implicit val backend: SttpBackend[Task, Nothing]  = AsyncHttpClientZioBackend()

    type Request = ZIO[RandomInternetRequests, Throwable, String]

    def interpretResponse(request: ZIO[RandomInternetRequests, Throwable, Response[String]]): Request =
      for {
        response <- request
        result   <- response.body match {
          case Left(failureMessage) => ZIO.fail(new Exception(failureMessage))
          case Right(successBody)   => ZIO.succeed(successBody)
        }
      } yield result

    val randomInternetRequests: Service[RandomInternetRequests] = new Service[RandomInternetRequests] {
      override def request1: Request = interpretResponse(Helpers.request1)
      override def request2: Request = interpretResponse(Helpers.request2)
      override def request3: Request = interpretResponse(Helpers.request3)
    }
  }
  object Live extends Live
}

package object randomInternetRequests extends RandomInternetRequests.Service[RandomInternetRequests] {
  final val request1: ZIO[RandomInternetRequests, Throwable, String] =
    ZIO.accessM(_.randomInternetRequests.request1)
  final val request2: ZIO[RandomInternetRequests, Throwable, String] =
    ZIO.accessM(_.randomInternetRequests.request2)
  final val request3: ZIO[RandomInternetRequests, Throwable, String] =
    ZIO.accessM(_.randomInternetRequests.request3)
}
