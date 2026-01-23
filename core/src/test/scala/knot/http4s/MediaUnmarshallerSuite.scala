package knot.http4s

import cats.effect.IO
import cats.{Eq, Id}
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.http4s.{Headers, Media, MediaType}
import fs2.*
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
}
