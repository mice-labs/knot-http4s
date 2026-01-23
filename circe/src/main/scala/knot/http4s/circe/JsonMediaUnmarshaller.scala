package knot.http4s.circe

import cats.ApplicativeThrow
import cats.implicits.*
import cats.effect.Concurrent
import io.circe.Json
import knot.http4s.MediaUnmarshaller
import org.http4s.*
import org.http4s.circe.CirceEntityDecoder

trait JsonMediaUnmarshaller[F[_]] extends MediaUnmarshaller[F, Json]:
  def handleErrorWith(f: Throwable => JsonMediaUnmarshaller[F])(using ApplicativeThrow[F]): JsonMediaUnmarshaller[F] =
    JsonMediaUnmarshaller.instance(m => run(m).handleErrorWith(f(_).run(m)))

object JsonMediaUnmarshaller:
  private val yamlMediaRange = Seq(
    MediaType("application", "yaml"),
    MediaType("text", "yaml"),
    MediaType("application", "x-yaml")
  )

  def apply[F[_]: JsonMediaUnmarshaller]: JsonMediaUnmarshaller[F] =
    summon[JsonMediaUnmarshaller[F]]

  def instance[F[_]](f: Media[F] => F[Json]): JsonMediaUnmarshaller[F] =
    m => f(m)

  def json[F[_]: Concurrent]: JsonMediaUnmarshaller[F] = {
    val backend = CirceEntityDecoder
      .circeEntityDecoder[F, Json]
    instance(backend.decode(_, true).rethrowT)
  }

  def yaml[F[_]: Concurrent]: JsonMediaUnmarshaller[F] = {
    val backend = EntityDecoder.decodeBy[F, Json](yamlMediaRange.head, yamlMediaRange.tail: _*) { msg =>
      EntityDecoder.text[F].decode(msg, false).subflatMap { text =>
        io.circe.yaml.parser.parse(text).leftMap(e => InvalidMessageBodyFailure("Invalid YAML", Some(e)))
      }
    }
    instance(backend.decode(_, true).rethrowT)
  }

  def mediaRange[F[_]: Concurrent](
      f1: JsonMediaUnmarshaller[F],
      fn: JsonMediaUnmarshaller[F]*
  ): JsonMediaUnmarshaller[F] =
    fn.foldLeft(f1) { case (result, next) =>
      result.handleErrorWith { case _: MediaTypeMismatch =>
        next
      }
    }

  def superset[F[_]: Concurrent]: JsonMediaUnmarshaller[F] =
    mediaRange(json, yaml)
