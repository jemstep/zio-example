package com.jemstep.time

import scalaz.zio.{IO, UIO, ZIO}

import java.time.LocalTime

trait OurTime extends Serializable {
  val time: OurTime.Service[Any]
}

object OurTime extends Serializable {
  trait Service[R] extends Serializable {
    val now: ZIO[R, Nothing, LocalTime]
  }

  trait Live extends OurTime {
    val time: Service[Any] = new Service[Any] {
      val now: UIO[LocalTime] = IO.effectTotal(LocalTime.now())
    }
  }
  object Live extends Live
}

package object time extends OurTime.Service[OurTime] {
  final val timeService: ZIO[OurTime, Nothing, OurTime.Service[Any]] =
    ZIO.access(_.time)

  final val now: ZIO[OurTime, Nothing, LocalTime] =
    ZIO.accessM(_.time.now)

}
