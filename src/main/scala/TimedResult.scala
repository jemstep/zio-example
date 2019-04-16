import java.time.LocalTime

import com.jemstep.time.{OurTime, time}
import scalaz.zio._

case class TimedResult[Result](start: LocalTime, end: LocalTime, result: Result) {
  override def toString: String = s"start: '$start' end: '$end'"
}

object TimedResult {

  def timeTask[Result](task: ZIO[OurTime, Throwable, Result]): ZIO[OurTime, Throwable, TimedResult[Result]] = for {
    t1 <- time.now
    r  <- task
    t2 <- time.now
  } yield TimedResult(t1, t2, r)

}