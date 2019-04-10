import java.time.LocalDateTime

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
      r1 <- taskTime(request1.send())
      r2 <- taskTime(request2.send())
      _ <- printResult("Seq 1", extractDate)(r1)
      _ <- printResult("Seq 2", extractDate)(r2)
      all <- Task.collectAllPar(List(taskTime(request1.send()), taskTime(request2.send())))
      combined = all.foldLeft("") { case (acc, r) => acc ++ r.result.code.toString}
      _  <- Task(all.foreach(printResult("ParA", extractDate)))
      b = all.foreach(printResult("ParB", extractDate))
      _ <- putStrLn(s"\n !!! Done $combined ${LocalDateTime.now}")
    } yield {
      all.foreach(printResult("Par", extractDate))
      backend.close()
    }

  }

  def printResult[Result, E](prefix: String, extract: Result => E)(tr: TimedResult[Result]) = {
    putStrLn(s"\n !!! $prefix ${tr.start} ${tr.diff} ${extract(tr.result)}\n")
  }

  def taskTime[Result](action: Task[Result]): Task[TimedResult[Result]] = for {
    t1 <- Task(LocalDateTime.now())
    r  <- action
    t2 <- Task(LocalDateTime.now())
  } yield TimedResult(t1, t2, r)

  case class TimedResult[Result](start: LocalDateTime, end: LocalDateTime, result: Result) {
    def diff = end.getSecond - start.getSecond
  }
}
