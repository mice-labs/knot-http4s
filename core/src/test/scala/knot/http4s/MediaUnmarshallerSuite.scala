package knot.http4s

import cats.effect.IO
import cats.{Eq, Id}
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.http4s.{Headers, Media, MediaRange, MediaType}
import fs2.*
import knot.fs2.Unpickle
import org.scalacheck.Arbitrary
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import knot.http4s.util.Http4sInstances.given
import knot.http4s.util.CatsEffectInstances.given
import org.http4s.headers.`Content-Type`

object MediaUnmarshallerSuite extends SimpleIOSuite with Discipline {
  given [F[_], A](using Eq[Media[F] => F[A]]): Eq[MediaUnmarshaller[F, A]] =
    Eq.by[MediaUnmarshaller[F, A], Media[F] => F[A]](_.run)

  given [F[_], A](using Arbitrary[Media[F] => F[A]]): Arbitrary[MediaUnmarshaller[F, A]] =
    Arbitrary(Arbitrary.arbitrary[Media[F] => F[A]].map(MediaUnmarshaller.instance))

  checkAll("MediaUnmarshaller[IO, *]", MonadErrorTests[MediaUnmarshaller[IO, *], Throwable].monadError[Int, Int, Int])
  checkAll("MediaUnmarshaller[Id, *]", MonadTests[MediaUnmarshaller[Id, *]].monad[Int, Int, Int])
  checkAll("MediaUnmarshaller[Id, *]", FlatMapTests[MediaUnmarshaller[Id, *]](MediaUnmarshaller.flatMapForMediaUnmarshaller).flatMap[Int, Int, Int])
  checkAll(
    "MediaUnmarshaller[IO, *]",
    ApplicativeErrorTests[MediaUnmarshaller[IO, *], Throwable](MediaUnmarshaller.applicativeThrowForMediaUnmarshaller).applicativeError[Int, Int, Int]
  )
  checkAll("MediaUnmarshaller[Id, *]", ApplicativeTests[MediaUnmarshaller[Id, *]](MediaUnmarshaller.applicativeForMediaUnmarshaller).applicative[Int, Int, Int])
  checkAll("MediaUnmarshaller[Id, *]", ApplyTests[MediaUnmarshaller[Id, *]](MediaUnmarshaller.applyForMediaUnmarshaller).apply[Int, Int, Int])
  checkAll("MediaUnmarshaller[Id, *]", FunctorTests[MediaUnmarshaller[Id, *]](MediaUnmarshaller.functorForMediaUnmarshaller).functor[Int, Int, Int])

  test("MediaUnmarshaller: apply using EntityDecoder") {
    val media = Media[IO](
      Stream.emits("hello-world".getBytes),
      Headers(`Content-Type`(MediaType("text", "text")))
    )
    val fa = MediaUnmarshaller[IO, String]
    for {
      result <- fa.run(media)
    } yield expect.same(result, "hello-world")
  }
  pureTest("MediaUnmarshaller: shift using FlatMap") {
    val fa    = MediaUnmarshaller.shift[Map[String, *], Int](_ => Map("two" -> 2))
    val media = Media[Map[String, *]](Stream.empty, Headers.empty)
    expect.same(fa.run(media), Map("two" -> 2))
  }
  test("MediaUnmarshaller: mediaRange mediatype") {
    val fa = MediaUnmarshaller.mediaRange[IO](
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
  test("MediaUnmarshaller: mediaRange unmarshaller") {
    val fa = MediaUnmarshaller.mediaRange(
      MediaUnmarshaller.unpickleMediaRange(MediaType.application.json)(Unpickle.pure[IO, Int](6)),
      MediaUnmarshaller.unpickleMediaRange(MediaType.application.xml)(Unpickle.pure[IO, Int](7))
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
  test("MediaUnmarshaller: unpickle") {
    val fa = MediaUnmarshaller.unpickle(Unpickle.pure[IO, Int](5))
    for {
      r1 <- fa.run(Media[IO](Stream.empty, Headers(`Content-Type`(MediaType.image.bmp))))
    } yield expect.eql(r1, 5)
  }
  test("MediaUnmarshaller: unpickleMediaRange") {
    val fa = MediaUnmarshaller.unpickleMediaRange(
      MediaType.application.json,
      MediaType.application.xml
    )(Unpickle.pure[IO, Int](5))
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
}
