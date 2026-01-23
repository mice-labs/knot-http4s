package knot.http4s.circe

import implicits.*
import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import weaver.SimpleIOSuite

object ImplicitsSuite extends SimpleIOSuite {
  test("Media[F]: unmarshallJson") {
    given JsonMediaUnmarshaller[IO] =
      JsonMediaUnmarshaller.json

    for {
      result <- TestMedia.json.unmarshallJson
      expected = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(result, expected)
  }
}
