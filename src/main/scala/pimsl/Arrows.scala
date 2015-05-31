/*
 * Copyright 2015 David Gregory
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package pimsl

import pimsl.Monad._
import scala.language.higherKinds

/**
 * An arrow F[A, B] represents a computation which requires A as
 * input and returns B.
 *
 * Arrow provides operations for applying arrows over an input
 * and composing these arrows sequentially.
 *
 * An arrow may simply be a Scala function but could also be a
 * functor object (see [[pimsl.Arrow.Kleisli]]).
 *
 * A minimal implementation requires `arr`, `first` and `sequence`.
 */
trait Arrow[F[_, _]] {
  /**
   * Lifts a function A => B into an arrow F[A, B]
   */
  def arr[A, B](f: A => B): F[A, B]

  /**
   * Sequences an arrow F[A, B] with an arrow F[B, C] to
   * produce a new arrow F[A, C].
   */
  def sequence[A, B, C](f: F[A, B], g: F[B, C]): F[A, C]

  /**
   * Applies an arrow F[A, B] over the first element of a
   * Tuple2 (A, C) to produce a new arrow F[(A, C), (B, C)].
   */
  def first[A, B, C](f: F[A, B]): F[(A, C), (B, C)]

  /**
   * Applies an arrow F[A, B] over the second element of a
   * Tuple2 (C, A) to produce a new arrow F[(C, A), (C, B)].
   */
  def second[C, A, B](f: F[A, B]): F[(C, A), (C, B)] = {
    sequence(
      sequence(arr((t: (C, A)) => t.swap), first[A, B, C](f)),
      arr((t: (B, C)) => t.swap)
    )
  }

  /**
   * Applies two arrows F[A, B] and F[C, D] over corresponding
   * halves of a Tuple2 (A, C) to produce a new arrow
   * F[(A, C), (B, D)].
   */
  def product[A, B, C, D](f: F[A, B], g: F[C, D]): F[(A, C), (B, D)] = {
    sequence(
      first[A, B, C](f),
      second[B, C, D](g)
    )
  }

  /**
   * Applies two arrows F[A, B] and F[A, C] over corresponding
   * halves of a Tuple2 (A, A) copied from a single input A
   * to produce a new arrow F[A, (B, C)].
   */
  def fanOut[A, B, C](f: F[A, B], g: F[A, C]): F[A, (B, C)] = {
    sequence(
      sequence(
        arr((a: A) => (a, a)),
        first[A, B, A](f)
      ),
      second[B, A, C](g)
    )
  }
}

/**
 * A trait representing polymorphic functions on an arrow F[A, B]
 * which takes A as input and returns a TC[B].
 */
trait >->[F[_, _], TC[_]] {
  /**
   * Run the arrow F[A, B] using input A to return a TC[B].
   */
  def apply[A, B](f: F[A, B])(a: A): TC[B]
  /**
   * Lifts the function A => TC[B] into an arrow F[A, B].
   */
  def kleisli[A, B](f: A => TC[B]): F[A, B]
}

object Arrow {
  /**
   * Identity type to support polymorphic return values in the
   * `flatMap` operation.
   */
  type Id[A] = A

  /**
   * Provides the operations required to use sequence comprehensions
   * after feeding input into an arrow, e.g.
   *
   * {{{
   * val addOne: Int => Int = a => a + 1
   * val addTwo: Int => Int = a => a + 2
   *
   * val proc: Int => Int = x => for {
   *   y <- addOne -< x
   *   z <- addTwo -< x
   * } yield y + z
   * }}}
   *
   */
  class ArrowSyntax[A, B, F[_, _], TC[_]](f: F[A, B], a: A)(implicit arrow: Arrow[F] with (F >-> TC)) {
    final def map[C](g: B => C): TC[C] = arrow(f >>> arrow.arr(g))(a)
    final def flatMap[C](g: B => TC[C]): TC[C] = arrow(f >>> arrow.kleisli(g))(a)
  }

  /**
   *  Provides infix versions of [[pimsl.Arrow]]'s operations over
   *  arrow instances.
   *
   *  Also provides an operator `-<` to feed input into an arrow
   *  within sequence comprehensions.
   */
  implicit class ArrowOps[A, B, F[_, _], TC[_]](f: F[A, B])(implicit arrow: Arrow[F] with (F >-> TC)) {
    /**
     * Sequences `this` with an arrow F[B, C] to
     * produce a new arrow F[A, C].
     */
    final def >>>[C](g: F[B, C]): F[A, C] = arrow.sequence(f, g)
    /**
     * Sequences an arrow F[C, A] with `this` to
     * produce a new arrow F[C, B].
     */
    final def <<<[C](g: F[C, A]): F[C, B] = arrow.sequence(g, f)
    /**
     * Applies `this` over the first element of a
     * Tuple2 (A, C) to produce a new
     * arrow F[(A, C), (B, C)].
     */
    final def first[C]: F[(A, C), (B, C)] = arrow.first[A, B, C](f)
    /**
     * Applies `this` over the second element of a
     * Tuple2 (C, A) to produce a new
     * arrow F[(C, A), (C, B)].
     */
    final def second[C]: F[(C, A), (C, B)] = arrow.second[C, A, B](f)
    /**
     * Applies `this` and an arrow F[C, D] over corresponding
     * halves of a Tuple2 (A, C) to produce a new arrow
     * F[(A, C), (B, D)].
     */
    final def ***[C, D](g: F[C, D]): F[(A, C), (B, D)] = arrow.product(f, g)
    /**
     * Applies `this` and an arrow F[A, C] over corresponding
     * halves of a Tuple2 (A, A) copied from a single input A
     * to produce a new arrow F[A, (B, C)].
     */
    final def &&&[C](g: F[A, C]): F[A, (B, C)] = arrow.fanOut(f, g)
    /**
     * Used to feed an input A into an arrow within sequence
     * comprehensions, e.g.
     *
     * {{{
     * val addOne: Int => Int = a => a + 1
     *
     * val proc: Int => Int = x => for {
     *   y <- addOne -< x
     * } yield y
     * }}}
     */
    final def -<(a: A): ArrowSyntax[A, B, F, TC] = new ArrowSyntax(f, a)
  }

  /**
   * Type class instance witnessing that Function1 is an Arrow.
   */
  implicit val function1Arrow = new Arrow[Function1] with (Function1 >-> Id) {
    final def arr[A, B](f: A => B) = f

    final def sequence[A, B, C](f: A => B, g: B => C) = g compose f

    final def first[A, B, C](f: A => B) = product(f, identity[C])

    final override def product[A, B, C, D](f: A => B, g: C => D) =
      (t: (A, C)) => (f(t._1), g(t._2))

    final def apply[A, B](f: A => B)(a: A): Id[B] = f(a)

    final def kleisli[A, B](f: A => Id[B]): A => B = f
  }

  /**
   * Case class representing a Kleisli arrow A => TC[B] for any
   * [[pimsl.Monad]][TC].
   */
  case class Kleisli[A, B, TC[_]: Monad](f: A => TC[B]) {
    def apply(a: A) = f(a)
  }

  /**
   * Type class instance witnessing that any [[pimsl.Monad]][TC]
   * is an arrow via the operation A => TC[B].
   */
  implicit def kleisliArrow[TC[_]: Monad]: Arrow[({ type f[x, y] = Kleisli[x, y, TC] })#f] with (({ type f[x, y] = Kleisli[x, y, TC] })#f >-> TC) = {
    type KleisliTC[A, B] = Kleisli[A, B, TC]

    new Arrow[KleisliTC] with (KleisliTC >-> TC) {
      private val monad = implicitly[Monad[TC]]

      final def arr[A, B](f: A => B): KleisliTC[A, B] =
        Kleisli { (a: A) => monad.map(monad.unit(a))(f) }

      final def sequence[A, B, C](f: KleisliTC[A, B], g: KleisliTC[B, C]): KleisliTC[A, C] =
        Kleisli { (a: A) => monad.flatMap(f(a))(g(_)) }

      final def first[A, B, C](f: KleisliTC[A, B]): KleisliTC[(A, C), (B, C)] =
        Kleisli { (t: (A, C)) =>
          monad.flatMap(f(t._1)) { (b: B) => monad.unit((b, t._2)) }
        }

      final def apply[A, B](f: KleisliTC[A, B])(a: A): TC[B] = f(a)

      final def kleisli[A, B](f: A => TC[B]): KleisliTC[A, B] = Kleisli(f)
    }
  }

}

