import scalaz.zio.App
import scalaz.zio.console._
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.zio.AsyncHttpClientZioBackend

object ExampleApp extends App {

  def run(args: List[String]) =
    myAppLogic.fold[Int](_ => 1, _ => 0)

  def myAppLogic = {
    val query = "http language:scala"
    val uri = uri"https://api.github.com/search/repositories?q=$query"
    val response = myGet(uri)

    for {
      r <- response
      _ <- putStrLn(s"Response Code: ${r.code} Date: ${r.header("Date")} Content length: ${r.header("Content-Length")}")
      _ <- putStrLn(s"Headers: ${r.headers}")
    } yield ()

  }

  def myGet(uri: Uri)= {
    val request: Request[String, Nothing] = sttp.get(uri)
    implicit val backend  = AsyncHttpClientZioBackend()
    request.send()
  }
}
