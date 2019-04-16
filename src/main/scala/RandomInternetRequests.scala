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
    def request1: ZIO[RandomInternetRequests, Throwable, Either[String, String]]
    def request2: ZIO[RandomInternetRequests, Throwable, Either[String, String]]
    def request3: ZIO[RandomInternetRequests, Throwable, Either[String, String]]
  }

  trait Live extends RandomInternetRequests {
    implicit val backend: SttpBackend[Task, Nothing]  = AsyncHttpClientZioBackend()

    val randomInternetRequests: Service[RandomInternetRequests] = new Service[RandomInternetRequests] {
      override def request1: ZIO[RandomInternetRequests, Throwable, Either[String, String]] =
        Helpers.request1 map (_.body)
      override def request2: ZIO[RandomInternetRequests, Throwable, Either[String, String]] =
        Helpers.request2 map (_.body)
      override def request3: ZIO[RandomInternetRequests, Throwable, Either[String, String]] =
        Helpers.request3 map (_.body)
    }
  }
  object Live extends Live
}

package object randomInternetRequests extends RandomInternetRequests.Service[RandomInternetRequests] {
  final val request1: ZIO[RandomInternetRequests, Throwable, Either[String, String]] =
    ZIO.accessM(_.randomInternetRequests.request1)
  final val request2: ZIO[RandomInternetRequests, Throwable, Either[String, String]] =
    ZIO.accessM(_.randomInternetRequests.request2)
  final val request3: ZIO[RandomInternetRequests, Throwable, Either[String, String]] =
    ZIO.accessM(_.randomInternetRequests.request3)
}
