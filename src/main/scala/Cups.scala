package com.jemstep.cups

import scalaz.zio.ZIO
import com.jemstep.helpers.Helpers
import scalaz.zio._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend

trait Cups {
  def cups: Cups.Service[Cups]
}

object Cups extends Serializable {
  trait Service[R] {
    def request1: ZIO[Cups, Throwable, Either[String, String]]
    def request2: ZIO[Cups, Throwable, Either[String, String]]
    def request3: ZIO[Cups, Throwable, Either[String, String]]
  }

  trait Live extends Cups {
    implicit val backend: SttpBackend[Task, Nothing]  = AsyncHttpClientZioBackend()

    val cups: Service[Cups] = new Service[Cups] {
      override def request1: ZIO[Cups, Throwable, Either[String, String]] =
        Helpers.request1 map (_.body)
      override def request2: ZIO[Cups, Throwable, Either[String, String]] =
        Helpers.request2 map (_.body)
      override def request3: ZIO[Cups, Throwable, Either[String, String]] =
        Helpers.request3 map (_.body)
    }
  }
  object Live extends Live
}

package object cups extends Cups.Service[Cups] {
  final val request1: ZIO[Cups, Throwable, Either[String, String]] =
    ZIO.accessM(_.cups.request1)
  final val request2: ZIO[Cups, Throwable, Either[String, String]] =
    ZIO.accessM(_.cups.request2)
  final val request3: ZIO[Cups, Throwable, Either[String, String]] =
    ZIO.accessM(_.cups.request3)
}
