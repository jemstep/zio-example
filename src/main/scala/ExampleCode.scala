import scalaz.zio.App
import scalaz.zio.console._

object ExampleApp extends App {

  def run(args: List[String]) =
    myAppLogic.fold(_ => 1, _ => 0)

  val myAppLogic =
    for {
      _ <- putStrLn("Hello! What is your name?")
      n <- getStrLn
      _ <- putStrLn(s"Hello, ${n}, welcome to ZIO!")
    } yield ()
}

object ExampleCode {

  def emptyListOfNumbers = List.empty[Int]
  def doubler(x: Int) = x + x
  def multiplier(x: Int, y: Int) = x * y

}
