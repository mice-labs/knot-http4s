package knot.http4s

import cats.*
import cats.evidence.As
import cats.implicits.*
import knot.Kleisli
import org.http4s.{EntityDecoder, Media}

trait MediaUnmarshaller[F[_], A] extends Kleisli[F, Media[F], A]:
  override def map[B](f: A => B)(using Functor[F]): MediaUnmarshaller[F, B] =
    MediaUnmarshaller.instance(run(_).map(f))

  def ap[B, C](f: MediaUnmarshaller[F, B])(using F: Apply[F], ev: A As (B => C)): MediaUnmarshaller[F, C] = {
    MediaUnmarshaller.instance { a =>
      val fb: F[B => C] = F.map(run(a))(ev.coerce)
      val fc: F[B]      = f.run(a)
      F.ap(fb)(fc)
    }
  }

  def flatMap[B](f: A => MediaUnmarshaller[F, B])(using FlatMap[F]): MediaUnmarshaller[F, B] =
    MediaUnmarshaller.shift(a => run(a).flatMap(f(_).run(a)))

  def handleErrorWith(f: Throwable => MediaUnmarshaller[F, A])(using ApplicativeError[F, Throwable]): MediaUnmarshaller[F, A] =
    MediaUnmarshaller.instance(a => run(a).handleErrorWith(e => f(e).run(a)))

object MediaUnmarshaller extends MediaUnmarshallerInstances:
  def apply[F[_], A](using MediaUnmarshaller[F, A]): MediaUnmarshaller[F, A] =
    summon[MediaUnmarshaller[F, A]]

  def instance[F[_], A](f: Media[F] => F[A]): MediaUnmarshaller[F, A] =
    m => f(m)

  def shift[F[_], A](f: Media[F] => F[A])(using F: FlatMap[F]): MediaUnmarshaller[F, A] =
    F match {
      case ap: Applicative[F] @unchecked =>
        instance(r => F.flatMap(ap.pure(r))(f))
      case _ =>
        instance(f)
    }

  def pure[F[_]: Applicative, A](a: A): MediaUnmarshaller[F, A] =
    instance(_ => a.pure[F])

  def raiseError[F[_]: ApplicativeThrow, A](e: Throwable): MediaUnmarshaller[F, A] =
    instance(_ => e.raiseError)

  given [F[_]: MonadThrow, A](using EntityDecoder[F, A]): MediaUnmarshaller[F, A] =
    m => EntityDecoder[F, A].decode(m, true).rethrowT

sealed abstract class MediaUnmarshallerInstances extends MediaUnmarshallerInstances0:
  given [F[_]: MonadThrow]: MonadThrow[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerMonadThrow[F]:
      def F: MonadThrow[F] = MonadThrow[F]

sealed abstract class MediaUnmarshallerInstances0 extends MediaUnmarshallerInstances1:
  given [F[_]: Monad]: Monad[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerMonad[F]:
      def F: Monad[F] = Monad[F]

sealed abstract class MediaUnmarshallerInstances1 extends MediaUnmarshallerInstances2:
  given flatMapForMediaUnmarshaller[F[_]: FlatMap]: FlatMap[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerFlatMap[F]:
      def F: FlatMap[F] = FlatMap[F]

sealed abstract class MediaUnmarshallerInstances2 extends MediaUnmarshallerInstances3:
  given applicativeThrowForMediaUnmarshaller[F[_]: ApplicativeThrow]: ApplicativeThrow[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerApplicativeThrow[F]:
      def F: ApplicativeThrow[F] = ApplicativeThrow[F]

sealed abstract class MediaUnmarshallerInstances3 extends MediaUnmarshallerInstances4:
  given applicativeForMediaUnmarshaller[F[_]: Applicative]: Applicative[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerApplicative[F]:
      def F: Applicative[F] = Applicative[F]

sealed abstract class MediaUnmarshallerInstances4 extends MediaUnmarshallerInstances5:
  given applyForMediaUnmarshaller[F[_]: Apply]: Apply[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerApply[F]:
      def F: Apply[F] = Apply[F]

sealed abstract class MediaUnmarshallerInstances5:
  given functorForMediaUnmarshaller[F[_]: Functor]: Functor[MediaUnmarshaller[F, *]] =
    new MediaUnmarshallerFunctor[F]:
      def F: Functor[F] = Functor[F]

sealed trait MediaUnmarshallerMonadThrow[F[_]] extends MonadThrow[MediaUnmarshaller[F, *]] with MediaUnmarshallerApplicativeThrow[F] with MediaUnmarshallerMonad[F]:
  implicit def F: MonadThrow[F]

sealed trait MediaUnmarshallerMonad[F[_]] extends Monad[MediaUnmarshaller[F, *]] with MediaUnmarshallerFlatMap[F] with MediaUnmarshallerApplicative[F]:
  implicit def F: Monad[F]

sealed trait MediaUnmarshallerFlatMap[F[_]] extends FlatMap[MediaUnmarshaller[F, *]] with MediaUnmarshallerApply[F]:
  implicit def F: FlatMap[F]
  def flatMap[A, B](fa: MediaUnmarshaller[F, A])(f: A => MediaUnmarshaller[F, B]): MediaUnmarshaller[F, B] =
    fa.flatMap(f)
  def tailRecM[A, B](a: A)(f: A => MediaUnmarshaller[F, Either[A, B]]): MediaUnmarshaller[F, B] =
    MediaUnmarshaller.instance { m =>
      F.tailRecM(a)(f(_).run(m))
    }

sealed trait MediaUnmarshallerApplicativeThrow[F[_]] extends ApplicativeThrow[MediaUnmarshaller[F, *]] with MediaUnmarshallerApplicative[F]:
  implicit def F: ApplicativeThrow[F]
  def raiseError[A](e: Throwable): MediaUnmarshaller[F, A] =
    MediaUnmarshaller.raiseError(e)

  def handleErrorWith[A](fa: MediaUnmarshaller[F, A])(f: Throwable => MediaUnmarshaller[F, A]): MediaUnmarshaller[F, A] =
    fa.handleErrorWith(f)

sealed trait MediaUnmarshallerApplicative[F[_]] extends Applicative[MediaUnmarshaller[F, *]] with MediaUnmarshallerApply[F]:
  implicit def F: Applicative[F]
  def pure[A](a: A): MediaUnmarshaller[F, A] =
    MediaUnmarshaller.pure(a)

sealed trait MediaUnmarshallerApply[F[_]] extends Apply[MediaUnmarshaller[F, *]] with MediaUnmarshallerFunctor[F]:
  implicit def F: Apply[F]

  override def ap[A, B](ff: MediaUnmarshaller[F, A => B])(fa: MediaUnmarshaller[F, A]): MediaUnmarshaller[F, B] =
    ff.ap(fa)

  override def map2Eval[A, B, C](fa: MediaUnmarshaller[F, A], fb: Eval[MediaUnmarshaller[F, B]])(
      f: (A, B) => C
  ): Eval[MediaUnmarshaller[F, C]] = {
    // We should only evaluate fb once
    val memoFb = fb.memoize

    Eval.now(MediaUnmarshaller.instance { a =>
      val fb              = fa.run(a)
      val efc             = memoFb.map(_.run(a))
      val efz: Eval[F[C]] = F.map2Eval(fb, efc)(f)
      // This is not safe and results in stack overflows:
      // see: https://github.com/typelevel/cats/issues/3947
      efz.value
    })
  }

sealed trait MediaUnmarshallerFunctor[F[_]] extends Functor[MediaUnmarshaller[F, *]]:
  implicit def F: Functor[F]

  override def map[A, B](fa: MediaUnmarshaller[F, A])(f: A => B): MediaUnmarshaller[F, B] =
    fa.map(f)

  override def void[A](fa: MediaUnmarshaller[F, A]): MediaUnmarshaller[F, Unit] =
    MediaUnmarshaller.instance(fa.run.andThen(F.void))
