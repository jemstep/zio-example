import java.time.LocalDateTime

import java.io._

import scalaz.zio._
import scalaz.zio.console._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend

object ExampleApp extends App {

  def run(args: List[String]) =
    httpClientExample.fold[Int](_ => 1, _ => 0)

  def httpClientExample = {
    implicit val backend  = AsyncHttpClientZioBackend()
    val githubQuery = "http language:scala"
    val uri1 = uri"https://api.github.com/search/repositories?q=$githubQuery"

    val googleQuery = "scala +zio +cats-effect"
    val uri2 = uri"https://www.google.com/search?q=$googleQuery"
    val request1: Request[String, Nothing] = sttp.get(uri1)
    val request2: Request[String, Nothing] = sttp.get(uri2)

    val extractDate: Response[String] => Option[String] = r => r.header("Date")

    for {
      seq1 <- taskTime(request1.send())
      seq2 <- taskTime(request2.send())
      _ <- printWrite("Seq1", extractDate)(seq1)
      _ <- printWrite("Seq2", extractDate)(seq2)
    } yield {
      backend.close()
    }

  }

  def printWrite[Result, E](prefix: String, extract: Result => E)(tr: TimedResult[Result]) = for {
    now <- Task(LocalDateTime.now)
    header = s"$now $prefix Start: '${tr.start}' End: '${tr.end}' Diff: '${tr.diff}' Extract: '${extract(tr.result)}'\n"
    _ <- putStrLn(s"\n\n>>>\n$header\n<<<\n")
    filename = s"/tmp/zio-${prefix}-${now.getNano}.out"
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

object TimedResult {


}