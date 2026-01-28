package knot.http4s

import cats.Eq
import cats.implicits.*
import cats.laws.discipline.*
import cats.laws.discipline.eq.*
import cats.laws.discipline.arbitrary.*
import org.http4s.Uri
import org.http4s.implicits.uri
import org.scalacheck.{Arbitrary, Cogen}
import weaver.SimpleIOSuite
import weaver.discipline.Discipline

object UriDecoderSuite extends SimpleIOSuite with Discipline {

  given ExhaustiveCheck[Uri] =
    ExhaustiveCheck.instance(
      List(
        uri"https://google.com",
        uri"file://users/home/test.json"
      )
    )
  given [E, A](using Eq[Uri => Either[E, A]]): Eq[UriDecoder[E, A]] =
    Eq.by[UriDecoder[E, A], Uri => Either[E, A]](_.run)

  given Cogen[Uri] =
    Cogen[String].contramap(_.renderString)

  given [E, A](using Arbitrary[A], Arbitrary[E]): Arbitrary[UriDecoder[E, A]] =
    Arbitrary(Arbitrary.arbitrary[Uri => Either[E, A]].map(UriDecoder.instance))

  checkAll("UriDecoder[MiniInt, *]", MonadErrorTests[UriDecoder[MiniInt, *], MiniInt].monadError[Int, Int, Int])
  pureTest("UriDecoder[String, Scheme]: map") {
    val fa = UriDecoder.instance(uri => Either.fromOption(uri.scheme, "DNE")).map(_.value)
    expect.eql(
      fa.run(uri"file://user/home/test.yaml"),
      "file".asRight
    )
  }
  object ImplicitResolution:
    given UriDecoder[MiniInt, Int] =
      UriDecoder.pure(1)
    UriDecoder[MiniInt, Int]
}
