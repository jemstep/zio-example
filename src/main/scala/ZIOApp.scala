import java.io.{File, PrintWriter}
import com.jemstep.randomInternetRequests.{RandomInternetRequests, randomInternetRequests}
import java.time.LocalDateTime

import com.jemstep.time.{OurTime, time}

import scalaz.zio._
import scalaz.zio.console.{Console, putStrLn}
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend
import com.jemstep.helpers.TimedResult._
import com.jemstep.helpers.Helpers._
import com.jemstep.helpers.TimedResult
import ZIOHelpers._
import org.slf4j.{Logger, LoggerFactory}
import scalaz.zio.clock.Clock

object ZIOApp extends App {
  val environment = new Clock.Live with Console.Live with OurTime.Live
  def logger: Logger = LoggerFactory.getLogger(this.getClass)
  implicit val backend: SttpBackend[Task, Nothing]  = AsyncHttpClientZioBackend()

  def run(args: List[String]) =
    httpClientExample.provide(environment).foldM(err => handleError(err, logger), _ => UIO.succeed(0))

  // A much simpler example involving only services
  def randomInternetRequestsExample: ZIO[RandomInternetRequests with Console, Throwable, (String, String, String)] = for {
    result1 <- randomInternetRequests.request1
    _       <- putStrLn(s"Result1: $result1")
    result2 <- randomInternetRequests.request2
    _       <- putStrLn(s"Result2: $result2")
    result3 <- randomInternetRequests.request3
    _       <- putStrLn(s"Result3: $result3")
  } yield (result1, result2, result3)

  // A flashy demo of control over effects
  def httpClientExample: ZIO[OurAppEnv, Throwable, Unit] = {

    // Declare now, run later
    val timedTasks: List[OurTask[TimedResponse]] = reqs.map(timeTask)
    val timedResults: OurTask[List[TimedResponse]] = ZIO.collectAllPar(timedTasks)

    for {
      _ <- log("BEGIN: Basic Sequential")
      _ <- timePrintWrite("Seq1")(request1)
      _ <- timePrintWrite("Seq2")(request2)
      _ <- log("END:   Basic Sequential\n")

      _ <- log("BEGIN: Fork Join with separate output effect")
      fiber1 <- timeTask(request1).fork
      fiber2 <- timeTask(request2).fork
      fiber3 <- timeTask(request3).fork
      fiber = (fiber1 zip fiber2) zip fiber3
      tuple <- fiber.join
      _ <- printWrite("Fibre1")(tuple._1._1)
      _ <- printWrite("Fibre2")(tuple._1._2)
      _ <- printWrite("Fibre3")(tuple._2)
      _ <- log("END:   Fork Join with separate output effect\n")

      _ <- log("BEGIN: Fork Join with combined output effect")
      tfib1 <- timePrintWrite("Combined1")(request1).fork
      tfib2 <- timePrintWrite("Combined2")(request2).fork
      tfib3 <- timePrintWrite("Combined3")(request3).fork
      _ <- (tfib1 zip tfib2 zip tfib3).join
      _ <- log("END:   Fork Join with combined output effect\n")

      _ <- log("BEGIN: Parallel collect with separate foreach output")
      all <- timedResults
      _ <- ZIO.foreach(all.zipWithIndex)(printWriteI("collectAllPar"))
      _ <- log("END:   Parallel collect with separate foreach output\n")

//      _ <- log("BEGIN: Parallel collect with an error")
//      tasksWithErr = List(request1, errorResponse, request3).map(timeTask)
//      all <- Task.collectAllPar(tasksWithErr)
//      _ <- ZIO.foreach(all.zipWithIndex)(printWriteI("collectAllParWithErr"))
//      _ <- log("END:   Parallel collect with an error\n")

      _ <- log("BEGIN: Parallel foreach with combined output effect")
      _ <- ZIO.foreachPar(reqs.zipWithIndex)(timePrintWriteI("foreachPar"))
      _ <- log("END:   Parallel foreach with combined output effect\n")
    } yield backend.close()

  }

}

object ZIOHelpers {

  type OurAppEnv = Console with Clock with OurTime
  type OurZIOApp = ZIO[OurAppEnv, Nothing, Int]
  type OurTask[A] = ZIO[OurAppEnv, Throwable, A]

  def log(msg: String): ZIO[Console with OurTime, Throwable, Unit] = for {
    now <- time.now
    _ <- putStrLn(s"$now\t$msg")
  } yield ()

  def timePrintWriteI[A](prefix: String)(task: (Task[A], Int)): ZIO[Console with OurTime, Throwable, Unit] = {
    timePrintWrite(s"$prefix${task._2 + 1}")(task._1)
  }

  def timePrintWrite[A](prefix: String)(task: Task[A]): ZIO[Console with OurTime, Throwable, Unit] = {
    for {
      t <- timeTask(task)
      _ <- printWrite(prefix)(t)
    } yield ()
  }

  def printWriteI[Result, E](prefix: String)(tr: (TimedResult[Result], Int)): ZIO[Console, Throwable, Unit] = {
    printWrite(s"$prefix${tr._2 + 1}")(tr._1)
  }

  def printWrite[Result, E](prefix: String)(tr: TimedResult[Result]): ZIO[Console, Throwable, Unit] = for {
    now <- Task(LocalDateTime.now)
    header = s"$now $prefix Start: '${tr.start}' End: '${tr.end}'"
    _ <- putStrLn(s"$header")
    filename = s"/tmp/zio-${prefix}-${now}.out"
    pw <- Task(new PrintWriter(new File(filename)))
    txt = s"$header\n\n${tr.result}"
    _ <- Task(pw.write(txt))
    _ <- Task(pw.close())
  } yield ()

}
