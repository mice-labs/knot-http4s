package knot.http4s.circe

import cats.effect.IO
import io.circe.Json
import io.circe.syntax.*
import weaver.SimpleIOSuite

object JsonMediaUnmarshallerSuite extends SimpleIOSuite {
  test("JsonMediaUnmarshaller: json") {
    for {
      r1 <- JsonMediaUnmarshaller
        .json[IO]
        .run(TestMedia.json)
      r2 <- JsonMediaUnmarshaller
        .json[IO]
        .run(TestMedia.yaml)
        .attempt
      e1 = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(r1, e1) and expect(r2.isLeft)
  }
  test("JsonMediaUnmarshaller: yaml") {
    for {
      r1 <- JsonMediaUnmarshaller
        .yaml[IO]
        .run(TestMedia.yaml)
      r2 <- JsonMediaUnmarshaller
        .yaml[IO]
        .run(TestMedia.json)
        .attempt
      r3 <- JsonMediaUnmarshaller
        .yaml[IO]
        .run(TestMedia.brokenYaml)
        .attempt
      e1 = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(r1, e1) and expect(r2.isLeft) and expect(r3.isLeft)
  }
  test("JsonMediaUnmarshaller: superset") {
    for {
      r1 <- JsonMediaUnmarshaller
        .superset[IO]
        .run(TestMedia.json)
      r2 <- JsonMediaUnmarshaller
        .superset[IO]
        .run(TestMedia.yaml)
      e = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(r1, e) and expect.same(r2, e)
  }
}
