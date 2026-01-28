package knot.http4s.circe

import cats.*
import cats.implicits.*
import cats.effect.Concurrent
import cats.evidence.As
import io.circe.Decoder
import knot.fs2.circe.JsonUnpickle
import knot.http4s.MediaUnmarshaller
import org.http4s.*

trait JsonMediaUnmarshaller[F[_], A] extends MediaUnmarshaller[F, A]:
  override def map[B](f: A => B)(using Functor[F]): JsonMediaUnmarshaller[F, B] =
    JsonMediaUnmarshaller.instance(run(_).map(f))

  override def ap[B, C](f: MediaUnmarshaller[F, B])(using F: Apply[F], ev: A As (B => C)): JsonMediaUnmarshaller[F, C] = {
    JsonMediaUnmarshaller.instance { a =>
      val fb: F[B => C] = F.map(run(a))(ev.coerce)
      val fc: F[B]      = f.run(a)
      F.ap(fb)(fc)
    }
  }

  def flatMap[B](f: A => JsonMediaUnmarshaller[F, B])(using FlatMap[F]): JsonMediaUnmarshaller[F, B] =
    JsonMediaUnmarshaller.shift(a => run(a).flatMap(f(_).run(a)))

  override def handleErrorWith(f: Throwable => MediaUnmarshaller[F, A])(using ApplicativeError[F, Throwable]): JsonMediaUnmarshaller[F, A] =
    JsonMediaUnmarshaller.instance(a => run(a).handleErrorWith(e => f(e).run(a)))

object JsonMediaUnmarshaller extends JsonMediaUnmarshallerInstances:
  private val yamlMediaRange = Seq(
  )

  def apply[F[_], A](using JsonMediaUnmarshaller[F, A]): JsonMediaUnmarshaller[F, A] =
    summon[JsonMediaUnmarshaller[F, A]]

  def instance[F[_], A](f: Media[F] => F[A]): JsonMediaUnmarshaller[F, A] =
    m => f(m)

  def shift[F[_], A](f: Media[F] => F[A])(using F: FlatMap[F]): JsonMediaUnmarshaller[F, A] =
    F match {
      case ap: Applicative[F] @unchecked =>
        instance(r => F.flatMap(ap.pure(r))(f))
      case _ =>
        instance(f)
    }

  def pure[F[_]: Applicative, A](a: A): JsonMediaUnmarshaller[F, A] =
    instance(_ => a.pure[F])

  def raiseError[F[_]: ApplicativeThrow, A](e: Throwable): JsonMediaUnmarshaller[F, A] =
    instance(_ => e.raiseError)

  def mediaRange[F[_]: ApplicativeThrow](m1: MediaRange, mn: MediaRange*): JsonMediaUnmarshaller[F, Unit] = {
    val consumes = (m1 +: mn).toSet
    instance { m =>
      m.contentType match {
        case Some(c) =>
          ApplicativeThrow[F].raiseUnless(
            consumes.exists(_.satisfiedBy(c.mediaType))
          )(MediaTypeMismatch(c.mediaType, consumes))
        case None =>
          MediaTypeMissing(consumes).raiseError
      }
    }
  }

  private def mediaRangeP[F[_]: ApplicativeThrow, A](consumes: Set[MediaRange])(
      f1: JsonMediaUnmarshaller[F, A],
      fn: JsonMediaUnmarshaller[F, A]*
  ): JsonMediaUnmarshaller[F, A] =
    f1.handleErrorWith { case e: MediaTypeMismatch =>
      if (fn.isEmpty) MediaTypeMismatch(e.messageType, e.expected ++ consumes).raiseError
      else mediaRangeP(e.expected ++ consumes)(fn.head, fn.tail: _*)
    }

  def mediaRange[F[_]: ApplicativeThrow, A](
      f1: JsonMediaUnmarshaller[F, A],
      fn: JsonMediaUnmarshaller[F, A]*
  ): JsonMediaUnmarshaller[F, A] =
    mediaRangeP(Set.empty)(f1, fn: _*)

  def unpickle[F[_], A](fa: JsonUnpickle[F, A]): JsonMediaUnmarshaller[F, A] =
    instance(m => fa.run(m.body))

  def unpickleMediaRange[F[_]: ApplicativeThrow, A](m1: MediaRange, mn: MediaRange*)(
      fa: JsonUnpickle[F, A]
  ): JsonMediaUnmarshaller[F, A] =
    mediaRange(m1, mn: _*).productR(unpickle(fa))

  def json[F[_]: Concurrent, A: Decoder]: JsonMediaUnmarshaller[F, A] =
    unpickleMediaRange(MediaType.application.json)(JsonUnpickle.json)

  def yaml[F[_]: Concurrent, A: Decoder]: JsonMediaUnmarshaller[F, A] =
    unpickleMediaRange(MediaType("application", "yaml"), MediaType("text", "yaml"), MediaType("application", "x-yaml"))(
      JsonUnpickle.yaml
    )

  def superset[F[_]: Concurrent, A: Decoder]: JsonMediaUnmarshaller[F, A] =
    mediaRange(json, yaml)

sealed abstract class JsonMediaUnmarshallerInstances extends JsonMediaUnmarshallerInstances0:
  given [F[_]: MonadThrow]: MonadThrow[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerMonadThrow[F]:
      def F: MonadThrow[F] = MonadThrow[F]

sealed abstract class JsonMediaUnmarshallerInstances0 extends JsonMediaUnmarshallerInstances1:
  given [F[_]: Monad]: Monad[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerMonad[F]:
      def F: Monad[F] = Monad[F]

sealed abstract class JsonMediaUnmarshallerInstances1 extends JsonMediaUnmarshallerInstances2:
  given flatMapForJsonMediaUnmarshaller[F[_]: FlatMap]: FlatMap[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerFlatMap[F]:
      def F: FlatMap[F] = FlatMap[F]

sealed abstract class JsonMediaUnmarshallerInstances2 extends JsonMediaUnmarshallerInstances3:
  given applicativeThrowForJsonMediaUnmarshaller[F[_]: ApplicativeThrow]: ApplicativeThrow[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerApplicativeThrow[F]:
      def F: ApplicativeThrow[F] = ApplicativeThrow[F]

sealed abstract class JsonMediaUnmarshallerInstances3 extends JsonMediaUnmarshallerInstances4:
  given applicativeForJsonMediaUnmarshaller[F[_]: Applicative]: Applicative[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerApplicative[F]:
      def F: Applicative[F] = Applicative[F]

sealed abstract class JsonMediaUnmarshallerInstances4 extends JsonMediaUnmarshallerInstances5:
  given applyForJsonMediaUnmarshaller[F[_]: Apply]: Apply[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerApply[F]:
      def F: Apply[F] = Apply[F]

sealed abstract class JsonMediaUnmarshallerInstances5:
  given functorForJsonMediaUnmarshaller[F[_]: Functor]: Functor[JsonMediaUnmarshaller[F, *]] =
    new JsonMediaUnmarshallerFunctor[F]:
      def F: Functor[F] = Functor[F]

sealed trait JsonMediaUnmarshallerMonadThrow[F[_]] extends MonadThrow[JsonMediaUnmarshaller[F, *]] with JsonMediaUnmarshallerApplicativeThrow[F] with JsonMediaUnmarshallerMonad[F]:
  implicit def F: MonadThrow[F]

sealed trait JsonMediaUnmarshallerMonad[F[_]] extends Monad[JsonMediaUnmarshaller[F, *]] with JsonMediaUnmarshallerFlatMap[F] with JsonMediaUnmarshallerApplicative[F]:
  implicit def F: Monad[F]

sealed trait JsonMediaUnmarshallerFlatMap[F[_]] extends FlatMap[JsonMediaUnmarshaller[F, *]] with JsonMediaUnmarshallerApply[F]:
  implicit def F: FlatMap[F]
  def flatMap[A, B](fa: JsonMediaUnmarshaller[F, A])(f: A => JsonMediaUnmarshaller[F, B]): JsonMediaUnmarshaller[F, B] =
    fa.flatMap(f)
  def tailRecM[A, B](a: A)(f: A => JsonMediaUnmarshaller[F, Either[A, B]]): JsonMediaUnmarshaller[F, B] =
    JsonMediaUnmarshaller.instance { m =>
      F.tailRecM(a)(f(_).run(m))
    }

sealed trait JsonMediaUnmarshallerApplicativeThrow[F[_]] extends ApplicativeThrow[JsonMediaUnmarshaller[F, *]] with JsonMediaUnmarshallerApplicative[F]:
  implicit def F: ApplicativeThrow[F]
  def raiseError[A](e: Throwable): JsonMediaUnmarshaller[F, A] =
    JsonMediaUnmarshaller.raiseError(e)

  def handleErrorWith[A](fa: JsonMediaUnmarshaller[F, A])(f: Throwable => JsonMediaUnmarshaller[F, A]): JsonMediaUnmarshaller[F, A] =
    fa.handleErrorWith(f)

sealed trait JsonMediaUnmarshallerApplicative[F[_]] extends Applicative[JsonMediaUnmarshaller[F, *]] with JsonMediaUnmarshallerApply[F]:
  implicit def F: Applicative[F]
  def pure[A](a: A): JsonMediaUnmarshaller[F, A] =
    JsonMediaUnmarshaller.pure(a)

sealed trait JsonMediaUnmarshallerApply[F[_]] extends Apply[JsonMediaUnmarshaller[F, *]] with JsonMediaUnmarshallerFunctor[F]:
  implicit def F: Apply[F]

  override def ap[A, B](ff: JsonMediaUnmarshaller[F, A => B])(fa: JsonMediaUnmarshaller[F, A]): JsonMediaUnmarshaller[F, B] =
    ff.ap(fa)

  override def map2Eval[A, B, C](fa: JsonMediaUnmarshaller[F, A], fb: Eval[JsonMediaUnmarshaller[F, B]])(
      f: (A, B) => C
  ): Eval[JsonMediaUnmarshaller[F, C]] = {
    // We should only evaluate fb once
    val memoFb = fb.memoize

    Eval.now(JsonMediaUnmarshaller.instance { a =>
      val fb              = fa.run(a)
      val efc             = memoFb.map(_.run(a))
      val efz: Eval[F[C]] = F.map2Eval(fb, efc)(f)
      // This is not safe and results in stack overflows:
      // see: https://github.com/typelevel/cats/issues/3947
      efz.value
    })
  }

sealed trait JsonMediaUnmarshallerFunctor[F[_]] extends Functor[JsonMediaUnmarshaller[F, *]]:
  implicit def F: Functor[F]

  override def map[A, B](fa: JsonMediaUnmarshaller[F, A])(f: A => B): JsonMediaUnmarshaller[F, B] =
    fa.map(f)

  override def void[A](fa: JsonMediaUnmarshaller[F, A]): JsonMediaUnmarshaller[F, Unit] =
    JsonMediaUnmarshaller.instance(fa.run.andThen(F.void))
