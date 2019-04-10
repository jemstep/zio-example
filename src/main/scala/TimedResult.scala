import java.time.LocalDateTime
import java.io._

import scalaz.zio._
import scalaz.zio.console.{Console, _}
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend

case class TimedResult[Result](start: LocalDateTime, end: LocalDateTime, result: Result) {
  override def toString: String = s"start: '$start' end: '$end'"
}

object TimedResult {

  val extractDate: Response[String] => Option[String] = r => r.header("Date")

  def timePrintWriteI(prefix: String)(req: (Request[String, Nothing], Int)): ZIO[Console, Throwable, Unit] = {
    timePrintWrite(s"$prefix${req._2 + 1}")(req._1)
  }

  def timePrintWrite(prefix: String)(req: Request[String, Nothing]): ZIO[Console, Throwable, Unit] = {
    implicit val backend  = AsyncHttpClientZioBackend()

    for {
      t <- timeTask(req.send())
      _ <- printWrite(prefix, extractDate)(t)
    } yield backend.close()
  }

  def printWriteI[Result, E](prefix: String, extract: Result => E)(tr: (TimedResult[Result], Int)): ZIO[Console, Throwable, Unit] = {
    printWrite(s"$prefix${tr._2 + 1}", extract)(tr._1)
  }

  def printWrite[Result](prefix: String)(tr: TimedResult[Result]): ZIO[Console, Throwable, Unit] = {
    val ext: Result => Result = r => r
    printWrite(prefix, ext)(tr)
  }

  def printWrite[Result, E](prefix: String, extract: Result => E)(tr: TimedResult[Result]): ZIO[Console, Throwable, Unit] = for {
    now <- Task(LocalDateTime.now)
    header = s"$now $prefix Start: '${tr.start}' End: '${tr.end}' Extract: '${extract(tr.result)}'"
    _ <- putStrLn(s"$header")
    filename = s"/tmp/zio-${prefix}-${now}.out"
    pw <- Task(new PrintWriter(new File(filename)))
    txt = s"$header\n\n${tr.result}"
    _ <- Task(pw.write(txt))
    _ <- Task(pw.close())
  } yield ()

  def timeTask[Result](task: Task[Result]): Task[TimedResult[Result]] = for {
    t1 <- Task(LocalDateTime.now())
    r  <- task
    t2 <- Task(LocalDateTime.now())
  } yield TimedResult(t1, t2, r)

  def timeFun[Result](fun: () => Result): TimedResult[Result] = {
    val t1 = LocalDateTime.now()
    val r = fun()
    val t2 = LocalDateTime.now()
    TimedResult(t1, t2, r)
  }

}