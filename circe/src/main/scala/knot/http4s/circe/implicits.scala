package knot.http4s.circe

import cats.effect.Concurrent
import io.circe.Decoder
import org.http4s.Media

object implicits:
  extension [F[_]: Concurrent](media: Media[F])
    def unmarshallJson[A: Decoder]: F[A] =
      JsonMediaUnmarshaller.json.run(media)

    def unmarshallYaml[A: Decoder]: F[A] =
      JsonMediaUnmarshaller.yaml.run(media)

    def unmarshallJsonSuperset[A: Decoder]: F[A] =
      JsonMediaUnmarshaller.superset.run(media)
