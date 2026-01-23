package knot.http4s.circe

import io.circe.Json
import org.http4s.{Media, ParseFailure, Uri}

object implicits:
  extension [F[_]](media: Media[F])
    def unmarshallJson(using JsonMediaUnmarshaller[F]): F[Json] =
      JsonMediaUnmarshaller[F].run(media)
