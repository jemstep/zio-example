import com.softwaremill.sttp.{Request, sttp}
import com.softwaremill.sttp._

object Helpers {
  val githubQuery = "http language:scala"
  val uri1 = uri"https://api.github.com/search/repositories?q=$githubQuery"

  val searchQuery = "scala +zio +cats-effect"
  val uri2 = uri"https://www.google.com/search?q=$searchQuery"
  val uri3 = uri"https://duckduckgo.com/search?q=$searchQuery"
  val request1: Request[String, Nothing] = sttp.get(uri1)
  val request2: Request[String, Nothing] = sttp.get(uri2)
  val request3: Request[String, Nothing] = sttp.get(uri3)
  val reqs = List(request1, request2, request3)

}
