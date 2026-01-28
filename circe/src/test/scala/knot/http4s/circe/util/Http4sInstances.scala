package knot.http4s.circe.util

import cats.effect.IO
import cats.implicits.*
import cats.laws.discipline.ExhaustiveCheck
import cats.{Eq, Id}
import fs2.*
import knot.http4s.circe.util.Fs2Instances.given
import org.http4s.headers.`Content-Type`
import org.http4s.{EntityBody, Headers, Media, MediaRange, MediaType}
import org.scalacheck.Cogen

object Http4sInstances {

  given [F[x] >: Pure[x]]: ExhaustiveCheck[EntityBody[F]] =
    ExhaustiveCheck.instance(
      List(
        Stream.emits("67".getBytes),
        Stream.empty
      )
    )

  given ExhaustiveCheck[MediaType] =
    ExhaustiveCheck.instance(
      List(
        MediaType("text", "*"),
        MediaType("image", "*")
      )
    )

  given [F[x] >: Pure[x]]: ExhaustiveCheck[Media[F]] =
    ExhaustiveCheck.instance(
      for {
        body      <- ExhaustiveCheck[EntityBody[F]].allValues
        mediaType <- ExhaustiveCheck[MediaType].allValues
      } yield Media(body, Headers(`Content-Type`(mediaType)))
    )

  given Cogen[Headers] =
    Cogen[List[(String, String)]].contramap(_.headers.map(h => h.name.toString -> h.value))

  given cogenId: Cogen[Media[Id]] =
    Cogen[(Stream[Id, Byte], Headers)].contramap(m => m.body -> m.headers)

  given cogenIO: Cogen[Media[IO]] =
    Cogen[(Stream[IO, Byte], Headers)].contramap(m => m.body -> m.headers)
}
