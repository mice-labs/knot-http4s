package knot.http4s.circe

import cats.effect.IO
import org.http4s.{Headers, Media, MediaType}
import fs2.Stream
import io.circe.Json
import io.circe.syntax.*
import io.circe.yaml.syntax.*
import org.http4s.headers.`Content-Type`

object TestMedia {
  val json = Media[IO](
    Stream.emits(
      Json.obj("tako" -> "neko".asJson).noSpaces.getBytes()
    ),
    Headers(`Content-Type`(MediaType("application", "json")))
  )
  val yaml = Media[IO](
    Stream.emits(
      Json.obj("tako" -> "neko".asJson).asYaml.spaces2.getBytes()
    ),
    Headers(`Content-Type`(MediaType("application", "yaml")))
  )
  val brokenYaml = Media[IO](
    Stream.emits("{/".getBytes()),
    Headers(`Content-Type`(MediaType("application", "yaml")))
  )
}
