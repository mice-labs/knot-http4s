package knot.http4s.circe

import cats.{Eq, Id}
import cats.effect.IO
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import io.circe.Json
import io.circe.syntax.*
import org.http4s.{Headers, Media, MediaType}
import fs2.*
import knot.fs2.circe.JsonUnpickle
import org.scalacheck.Arbitrary
import knot.http4s.circe.util.Http4sInstances.given
import knot.http4s.circe.util.CatsEffectInstances.given
import org.http4s.headers.`Content-Type`
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object JsonMediaUnmarshallerSuite extends SimpleIOSuite with Discipline {
  given [F[_], A](using Eq[Media[F] => F[A]]): Eq[JsonMediaUnmarshaller[F, A]] =
    Eq.by[JsonMediaUnmarshaller[F, A], Media[F] => F[A]](_.run)

  given [F[_], A](using Arbitrary[Media[F] => F[A]]): Arbitrary[JsonMediaUnmarshaller[F, A]] =
    Arbitrary(Arbitrary.arbitrary[Media[F] => F[A]].map(JsonMediaUnmarshaller.instance))

  checkAll("JsonMediaUnmarshaller[IO, *]", MonadErrorTests[JsonMediaUnmarshaller[IO, *], Throwable].monadError[Int, Int, Int])
  checkAll("JsonMediaUnmarshaller[Id, *]", MonadTests[JsonMediaUnmarshaller[Id, *]].monad[Int, Int, Int])
  checkAll(
    "JsonMediaUnmarshaller[Id, *]",
    FlatMapTests[JsonMediaUnmarshaller[Id, *]](JsonMediaUnmarshaller.flatMapForJsonMediaUnmarshaller).flatMap[Int, Int, Int]
  )
  checkAll(
    "JsonMediaUnmarshaller[IO, *]",
    ApplicativeErrorTests[JsonMediaUnmarshaller[IO, *], Throwable](JsonMediaUnmarshaller.applicativeThrowForJsonMediaUnmarshaller).applicativeError[Int, Int, Int]
  )
  checkAll(
    "JsonMediaUnmarshaller[Id, *]",
    ApplicativeTests[JsonMediaUnmarshaller[Id, *]](JsonMediaUnmarshaller.applicativeForJsonMediaUnmarshaller).applicative[Int, Int, Int]
  )
  checkAll("JsonMediaUnmarshaller[Id, *]", ApplyTests[JsonMediaUnmarshaller[Id, *]](JsonMediaUnmarshaller.applyForJsonMediaUnmarshaller).apply[Int, Int, Int])
  checkAll(
    "JsonMediaUnmarshaller[Id, *]",
    FunctorTests[JsonMediaUnmarshaller[Id, *]](JsonMediaUnmarshaller.functorForJsonMediaUnmarshaller).functor[Int, Int, Int]
  )
  pureTest("JsonMediaUnmarshaller: shift using FlatMap") {
    val fa    = JsonMediaUnmarshaller.shift[Map[String, *], Int](_ => Map("two" -> 2))
    val media = Media[Map[String, *]](Stream.empty, Headers.empty)
    expect.same(fa.run(media), Map("two" -> 2))
  }
  test("JsonMediaUnmarshaller: mediaRange mediatype") {
    val fa = JsonMediaUnmarshaller.mediaRange[IO](
      MediaType.application.json,
      MediaType.application.xml
    )
    for {
      r1 <- fa
        .run(
          Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.application.json)))
        )
        .attempt
      r2 <- fa
        .run(
          Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.application.xml)))
        )
        .attempt
      r3 <- fa
        .run(
          Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.image.bmp)))
        )
        .attempt
      r4 <- fa
        .run(
          Media[IO](Stream.empty, Headers.empty)
        )
        .attempt
    } yield expect.all(r1.isRight, r2.isRight, r3.isLeft, r4.isLeft)
  }
  test("JsonMediaUnmarshaller: mediaRange unmarshaller") {
    val fa = JsonMediaUnmarshaller.mediaRange(
      JsonMediaUnmarshaller.unpickleMediaRange(MediaType.application.json)(JsonUnpickle.pure[IO, Int](6)),
      JsonMediaUnmarshaller.unpickleMediaRange(MediaType.application.xml)(JsonUnpickle.pure[IO, Int](7))
    )
    for {
      r1 <- fa.run(
        Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.application.json)))
      )
      r2 <- fa.run(
        Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.application.xml)))
      )
      r3 <- fa
        .run(
          Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.image.bmp)))
        )
        .attempt
    } yield expect.eql(r1, 6) and
      expect.eql(r2, 7) and
      expect(r3.isLeft)
  }
  test("JsonMediaUnmarshaller: unpickle") {
    val fa = JsonMediaUnmarshaller.unpickle(JsonUnpickle.pure[IO, Int](5))
    for {
      r1 <- fa.run(Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.image.bmp))))
    } yield expect.eql(r1, 5)
  }
  test("JsonMediaUnmarshaller: unpickleMediaRange") {
    val fa = JsonMediaUnmarshaller.unpickleMediaRange(
      MediaType.application.json,
      MediaType.application.xml
    )(JsonUnpickle.pure[IO, Int](5))
    for {
      r1 <- fa.run(
        Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.application.json)))
      )
      r2 <- fa.run(
        Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.application.xml)))
      )
      r3 <- fa
        .run(
          Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.image.bmp)))
        )
        .attempt
    } yield expect.eql(r1, 5) and
      expect.eql(r2, 5) and
      expect(r3.isLeft)
  }
  test("JsonMediaUnmarshaller: json") {
    for {
      r1 <- JsonMediaUnmarshaller
        .json[IO, Json]
        .run(TestMedia.json)
      r2 <- JsonMediaUnmarshaller
        .json[IO, Json]
        .run(TestMedia.yaml)
        .attempt
      e1 = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(r1, e1) and expect(r2.isLeft)
  }
  test("JsonMediaUnmarshaller: yaml") {
    for {
      r1 <- JsonMediaUnmarshaller
        .yaml[IO, Json]
        .run(TestMedia.yaml)
      r2 <- JsonMediaUnmarshaller
        .yaml[IO, Json]
        .run(TestMedia.json)
        .attempt
      r3 <- JsonMediaUnmarshaller
        .yaml[IO, Json]
        .run(TestMedia.brokenYaml)
        .attempt
      e1 = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(r1, e1) and expect(r2.isLeft) and expect(r3.isLeft)
  }
  test("JsonMediaUnmarshaller: superset") {
    for {
      r1 <- JsonMediaUnmarshaller
        .superset[IO, Json]
        .run(TestMedia.json)
      r2 <- JsonMediaUnmarshaller
        .superset[IO, Json]
        .run(TestMedia.yaml)
      e = Json.obj("tako" -> "neko".asJson)
    } yield expect.same(r1, e) and expect.same(r2, e)
  }
}
