package com.jemstep.time

import scalaz.zio.{IO, UIO, ZIO}

import java.time.LocalTime

trait OurTime extends Serializable {
  def time: OurTime.Service[Any]
}

object OurTime extends Serializable {
  trait Service[R] extends Serializable {
    def now: ZIO[R, Nothing, LocalTime]
  }

  trait Live extends OurTime {
    val time: Service[Any] = new Service[Any] {
      def now: UIO[LocalTime] = IO.effectTotal(LocalTime.now())
    }
  }
  object Live extends Live
}

package object time extends OurTime.Service[OurTime] {
  final val now: ZIO[OurTime, Nothing, LocalTime] =
    ZIO.accessM(_.time.now)
}
