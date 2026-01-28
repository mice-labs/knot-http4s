package knot.http4s

import cats.{Monad, MonadError}
import cats.implicits.*
import cats.evidence.As
import knot.Decoder
import org.http4s.Uri

trait UriDecoder[E, A] extends Decoder[E, Uri, A]:
  override def map[B](f: A => B): UriDecoder[E, B] =
    UriDecoder.instance(run(_).map(f))

  def flatMap[B](f: A => UriDecoder[E, B]): UriDecoder[E, B] =
    UriDecoder.instance(uri => run(uri).flatMap(f(_).run(uri)))

  def handleErrorWith(f: E => UriDecoder[E, A]): UriDecoder[E, A] =
    UriDecoder.instance(uri => run(uri).handleErrorWith(e => f(e).run(uri)))

object UriDecoder:
  def apply[E, A](using UriDecoder[E, A]): UriDecoder[E, A] =
    summon[UriDecoder[E, A]]

  def instance[E, A](f: Uri => Either[E, A]): UriDecoder[E, A] =
    uri => f(uri)

  def pure[E, A](a: A): UriDecoder[E, A] =
    instance(_ => a.asRight)

  def raiseError[E, A](e: E): UriDecoder[E, A] =
    instance(_ => e.asLeft)

  given [E]: MonadError[UriDecoder[E, *], E] =
    new MonadError[UriDecoder[E, *], E]:
      def pure[A](a: A): UriDecoder[E, A] =
        UriDecoder.pure(a)

      def raiseError[A](e: E): UriDecoder[E, A] =
        UriDecoder.raiseError(e)

      def handleErrorWith[A](fa: UriDecoder[E, A])(f: E => UriDecoder[E, A]): UriDecoder[E, A] =
        fa.handleErrorWith(f)

      def flatMap[A, B](fa: UriDecoder[E, A])(f: A => UriDecoder[E, B]): UriDecoder[E, B] =
        fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => UriDecoder[E, Either[A, B]]): UriDecoder[E, B] =
        UriDecoder.instance { json =>
          Monad[Either[E, *]].tailRecM(a)(a => f(a).run(json))
        }
