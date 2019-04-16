import java.time.LocalDateTime

import scalaz.zio._

case class TimedResult[Result](start: LocalDateTime, end: LocalDateTime, result: Result) {
  override def toString: String = s"start: '$start' end: '$end'"
}

object TimedResult {

  def timeTask[Result](task: Task[Result]): Task[TimedResult[Result]] = for {
    t1 <- Task(LocalDateTime.now())
    r  <- task
    t2 <- Task(LocalDateTime.now())
  } yield TimedResult(t1, t2, r)

}