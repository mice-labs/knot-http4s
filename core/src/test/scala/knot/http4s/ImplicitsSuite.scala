package knot.http4s

import implicits.{*, given}
import cats.implicits.*
import cats.effect.IO
import fs2.*
import knot.text.Read
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.uri
import org.http4s.{Headers, Media, MediaType, ParseFailure, Uri}
import weaver.SimpleIOSuite

object ImplicitsSuite extends SimpleIOSuite {
  test("Media[F]: unmarshal") {
    val media = Media[IO](
      Stream.emits("hello-world".getBytes),
      Headers(`Content-Type`(MediaType("text", "text")))
    )
    for {
      result <- media.unmarshall[String]
    } yield expect.same(result, "hello-world")
  }
  pureTest("Read: apply[ParseFailure, Uri]") {
    val fa = Read[ParseFailure, Uri]

    expect.same(fa.run("http://www.google.com"), uri"http://www.google.com".asRight)
  }
}
