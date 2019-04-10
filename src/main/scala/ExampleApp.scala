import java.time.LocalDateTime
import java.io._

import scalaz.zio._
import scalaz.zio.console.{Console, _}
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend

object ExampleApp extends App {

  def run(args: List[String]): ZIO[Console, Nothing, StatusCode] =
    httpClientExample.fold[Int](_ => 1, _ => 0)

  def httpClientExample: ZIO[Console, Throwable, Unit] = {

    implicit val backend  = AsyncHttpClientZioBackend()
    val githubQuery = "http language:scala"
    val uri1 = uri"https://api.github.com/search/repositories?q=$githubQuery"

    val googleQuery = "scala +zio +cats-effect"
    val uri2 = uri"https://www.google.com/search?q=$googleQuery"
    val request1: Request[String, Nothing] = sttp.get(uri1)
    val request2: Request[String, Nothing] = sttp.get(uri2)
    val reqs = List(request1, request2)

    for {
      _ <- timePrintWrite("Seq")((request1, 0))
      _ <- timePrintWrite("Seq")((request2, 1))
      fiber1 <- taskTime(request1.send()).fork
      fiber2 <- taskTime(request2.send()).fork
      fiber = fiber1 zip fiber2
      tuple <- fiber.join
      _ <- printWrite("Par1", extractDate)(tuple._1)
      _ <- printWrite("Par2", extractDate)(tuple._2)
      _ <- ZIO.foreachPar(reqs.zipWithIndex)(timePrintWrite("All"))
    } yield {
      backend.close()
    }

  }

  val extractDate: Response[String] => Option[String] = r => r.header("Date")

  def timePrintWrite(prefix: String)(req: (Request[String, Nothing], Int)): ZIO[Console, Throwable, Unit] = {
    implicit val backend  = AsyncHttpClientZioBackend()

    for {
      t <- taskTime(req._1.send())
      _ <- printWrite(s"${prefix}${req._2 + 1}", extractDate)(t)
    } yield backend.close()
  }

  def printWrite[Result, E](prefix: String, extract: Result => E)(tr: TimedResult[Result]): ZIO[Console, Throwable, Unit] = for {
    now <- Task(LocalDateTime.now)
    header = s"$now $prefix Start: '${tr.start}' End: '${tr.end}' Diff: '${tr.diff}' Extract: '${extract(tr.result)}'"
    _ <- putStrLn(s"\n\n>>>\n$header\n<<<\n")
    filename = s"/tmp/zio-${prefix}-${now}.out"
    pw <- Task(new PrintWriter(new File(filename)))
    txt = s"$header\n\n${tr.result}"
    _ <- Task(pw.write(txt))
    _ <- Task(pw.close())
  } yield ()


  def taskTime[Result](action: Task[Result]): Task[TimedResult[Result]] = for {
    t1 <- Task(LocalDateTime.now())
    r  <- action
    t2 <- Task(LocalDateTime.now())
  } yield TimedResult(t1, t2, r)

}

case class TimedResult[Result](start: LocalDateTime, end: LocalDateTime, result: Result) {
  def diff = end.getSecond - start.getSecond
}
