package knot.http4s

import knot.text.Read
import org.http4s.{Media, ParseFailure, Uri}

object implicits:
  extension [F[_]](media: Media[F])
    def unmarshall[A: MediaUnmarshaller[F, *]]: F[A] =
      MediaUnmarshaller[F, A].run(media)
  given Read[ParseFailure, Uri] =
    Read.instance(Uri.fromString)
