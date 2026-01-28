package knot.http4s.circe.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import cats.laws.discipline.*
import cats.{Eq, Id}
import fs2.{Compiler, Stream}
import org.scalacheck.{Arbitrary, Cogen, Gen}

object Fs2Instances {
  val STREAM_LENGTH = 3

  given [F[_], G[_], A](using Compiler[F, G], Eq[G[List[A]]]): Eq[Stream[F, A]] =
    Eq.by(_.take(STREAM_LENGTH).compile.toList)

  given cogenIO[A: Cogen]: Cogen[Stream[IO, A]] =
    Cogen[List[A]].contramap { s =>
      s.take(STREAM_LENGTH).compile.toList.unsafeRunSync()
    }

  given cogenId[A: Cogen]: Cogen[Stream[Id, A]] =
    Cogen[List[A]].contramap { s =>
      s.take(STREAM_LENGTH).compile.toList
    }
}
