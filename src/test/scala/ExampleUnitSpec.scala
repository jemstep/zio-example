import org.specs2.mutable.Specification
import com.jemstep.randomInternetRequests.RandomInternetRequests
import java.io.Reader
import java.io.IOException
import java.io.PrintStream
import scalaz.zio.Exit.Cause.Fail
import scalaz.zio.Exit.Failure

import scalaz.zio._
import scalaz.zio.console.Console

class ExampleUnitSpec extends Specification {

  trait TestRandomInternetRequestsWithNoFailures extends RandomInternetRequests {
    override val randomInternetRequests: RandomInternetRequests.Service[RandomInternetRequests] =
      new RandomInternetRequests.Service[RandomInternetRequests] {
        override def request1: ZIO[RandomInternetRequests, Throwable, String] =
          ZIO.succeed("A successfuly parsed body")
        override def request2: ZIO[RandomInternetRequests, Throwable, String] =
          ZIO.succeed("One more successfully parsed body")
        override def request3: ZIO[RandomInternetRequests, Throwable, String] =
          ZIO.succeed("Another successfuly parsed body")
      }
  }

  val failureException = new Exception("An unsuccessfully parsed body!")

  trait TestRandomInternetRequestsWithAFailure extends RandomInternetRequests {
    override val randomInternetRequests: RandomInternetRequests.Service[RandomInternetRequests] =
      new RandomInternetRequests.Service[RandomInternetRequests] {
        override def request1: ZIO[RandomInternetRequests, Throwable, String] =
          ZIO.succeed("A successfuly parsed body")
        override def request2: ZIO[RandomInternetRequests, Throwable, String] =
          ZIO.fail(failureException)
        override def request3: ZIO[RandomInternetRequests, Throwable, String] =
          ZIO.succeed("Another successfuly parsed body")
      }
  }

  trait TestConsole {
    val console: Console.Service[Any] = new Console.Service[Any] {
      override def putStr(line: String): ZIO[Any, Nothing, Unit] =
        ZIO.succeed(())

      override def putStr(stream: PrintStream)(line: String): ZIO[Any, Nothing, Unit] =
        ZIO.succeed(())

      override def putStrLn(line: String): ZIO[Any, Nothing, Unit] =
        ZIO.succeed(())

      override def putStrLn(stream: PrintStream)(line: String): ZIO[Any, Nothing, Unit] =
        ZIO.succeed(())

      override val getStrLn: ZIO[Any, IOException, String] =
        ZIO.succeed("Some legitimate input")

      override def getStrLn(reader: Reader): ZIO[Any, IOException, String] =
        ZIO.succeed("A line which I totally read from your reader")
    }
  }

  object TestEnvironmentWithDummyLogging extends Console with RandomInternetRequests
      with TestRandomInternetRequestsWithNoFailures with TestConsole

  object TestEnvironmentWithRealLogging extends Console with RandomInternetRequests
      with TestRandomInternetRequestsWithNoFailures with Console.Live

  object TestEnvironmentWithDummyLoggingAndAFailingRequest extends Console with RandomInternetRequests
      with TestRandomInternetRequestsWithAFailure with TestConsole

  "randomInternetRequests example" should {

    val runtime = new DefaultRuntime {}

    "produce the expected results without actual logging using the test environment with a dummy console" in {
      runtime.unsafeRun(ZIOApp.randomInternetRequestsExample.provide(TestEnvironmentWithDummyLogging)) must_==
      (("A successfuly parsed body",
        "One more successfully parsed body",
        "Another successfuly parsed body"))
    }

    "produce the expected results with actual logging using the test environment with real logging" in {
      runtime.unsafeRun(ZIOApp.randomInternetRequestsExample.provide(TestEnvironmentWithRealLogging)) must_==
      (("A successfuly parsed body",
        "One more successfully parsed body",
        "Another successfuly parsed body"))
    }

    "produce an error and no log results when using a dummy console and a requests service with errors" in {
      runtime.unsafeRunSync(
        ZIOApp
          .randomInternetRequestsExample
          .provide(TestEnvironmentWithDummyLoggingAndAFailingRequest)) must_==
      (Failure(Fail(failureException)))
    }
  }
}
