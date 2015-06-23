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
 */
trait Arrow[F[_, _], TC[_]] {
  /**
   * Lifts a function A => B into an arrow F[A, B]
   */
  def arr[A, B](f: A => B): F[A, B]

  /**
   * Lifts a function A => TC[B] into an arrow F[A, B].
   */
  def kleisli[A, B](f: A => TC[B]): F[A, B]

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

  /**
   * Run the arrow F[A, B] using input A to return a TC[B].
   */
  def apply[A, B](f: F[A, B])(a: A): TC[B]
}

trait ArrowPlus[F[_, _]] {
  /**
   * An identity value for arrows F[A, B]
   */
  def zero[A, B]: F[A, B]

  /**
   * An associative operation on arrows F[A, B]
   */
  def plus[A, B](l: F[A, B], r: F[A, B]): F[A, B]
}

trait ArrowChoice[F[_, _], TC[_]] {
  protected val arrow: Arrow[F, TC]

  def left[A, B, C](f: F[A, B]): F[Either[A, C], Either[B, C]]

  def right[A, B, C](f: F[A, B]): F[Either[C, A], Either[C, B]] = {
    def mirror[A, B]: Either[A, B] => Either[B, A] = {
      case Left(a) => Right(a)
      case Right(a) => Left(a)
    }
    arrow.sequence(
      arrow.sequence(
        arrow.arr(mirror[C, A]), left[A, B, C](f)
      ),
      arrow.arr(mirror[B, C])
    )
  }

  def split[A, B, C, D](f: F[A, B], g: F[C, D]): F[Either[A, C], Either[B, D]] =
    arrow.sequence(left(f), right(g))

  def fanIn[A, B, C](f: F[A, C], g: F[B, C]): F[Either[A, B], C] = {
    val untag: Either[C, C] => C = {
      case Left(c) => c
      case Right(c) => c
    }
    arrow.sequence(split(f, g), arrow.arr(untag))
  }
}

object Arrow {
  /**
   * Identity type to support polymorphic return values as output
   * from an arrow.
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
  class ArrowSyntax[A, B, F[_, _], TC[_]](f: F[A, B], a: A)(implicit arrow: Arrow[F, TC]) {
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
  implicit class ArrowOps[A, B, F[_, _], TC[_]](f: F[A, B])(implicit arrow: Arrow[F, TC]) {
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
   *  Provides an infix version of [[pimsl.ArrowPlus]].plus over
   *  arrow instances.
   */
  implicit class ArrowPlusOps[A, B, F[_, _]](f: F[A, B])(implicit arrow: ArrowPlus[F]) {
    /**
     * Applies an associative binary operation onto `this` and an arrow F[A, B]
     */
    def <+>(g: F[A, B]): F[A, B] = arrow.plus(f, g)
  }

  implicit class ArrowChoiceOps[A, B, F[_, _], TC[_]](f: F[A, B])(implicit arrow: Arrow[F, TC], arrowChoice: ArrowChoice[F, TC]) {
    def left[C]: F[Either[A, C], Either[B, C]] = arrowChoice.left(f)
    def right[C]: F[Either[C, A], Either[C, B]] = arrowChoice.right(f)
    def +++[C, D](g: F[C, D]): F[Either[A, C], Either[B, D]] = arrowChoice.split(f, g)
    def |||[C](g: F[C, B]): F[Either[A, C], B] = arrowChoice.fanIn(f, g)
  }

  /**
   * Type class instance witnessing that Function1 is an Arrow.
   */
  implicit val function1Arrow = new Arrow[Function1, Id] {
    final def arr[A, B](f: A => B) = f

    final def kleisli[A, B](f: A => Id[B]): A => B = f

    final def sequence[A, B, C](f: A => B, g: B => C) = g compose f

    final def first[A, B, C](f: A => B) = product(f, identity[C])

    final override def product[A, B, C, D](f: A => B, g: C => D) =
      (t: (A, C)) => (f(t._1), g(t._2))

    final def apply[A, B](f: A => B)(a: A): Id[B] = f(a)
  }

  implicit val function1ArrowChoice = new ArrowChoice[Function1, Id] {
    protected val arrow = implicitly[Arrow[Function1, Id]]

    def left[A, B, C](f: A => B) =
      split(f, identity[C])

    override def right[A, B, C](f: A => B) =
      split(identity[C], f)

    override def split[A, B, C, D](f: A => B, g: C => D) = {
      val leftComposeF: A => Either[B, D] = a => Left(f(a))
      val rightComposeG: C => Either[B, D] = c => Right(g(c))
      fanIn(leftComposeF, rightComposeG)
    }

    override def fanIn[A, B, C](f: A => C, g: B => C) =
      (either: Either[A, B]) => either.fold(f, g)
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
  implicit def kleisliArrow[TC[_]: Monad]: Arrow[Kleisli[?, ?, TC], TC] = {
    type KleisliTC[A, B] = Kleisli[A, B, TC]

    new Arrow[KleisliTC, TC] {
      private val monad = implicitly[Monad[TC]]

      final def arr[A, B](f: A => B): KleisliTC[A, B] =
        Kleisli { (a: A) => monad.unit(f(a)) }

      final def kleisli[A, B](f: A => TC[B]): KleisliTC[A, B] = Kleisli(f)

      final def sequence[A, B, C](f: KleisliTC[A, B], g: KleisliTC[B, C]): KleisliTC[A, C] =
        Kleisli { (a: A) => monad.flatMap(f(a))(g(_)) }

      final def first[A, B, C](f: KleisliTC[A, B]): KleisliTC[(A, C), (B, C)] =
        Kleisli { (t: (A, C)) =>
          monad.map(f(t._1)) { (b: B) => (b, t._2) }
        }

      final def apply[A, B](f: KleisliTC[A, B])(a: A): TC[B] = f(a)
    }
  }

  /**
   * Type class instance witnessing that any [[pimsl.MonadPlus]][TC]
   * is an arrow with an associative binary operation.
   */
  implicit def kleisliArrowPlus[TC[_]: MonadPlus]: ArrowPlus[Kleisli[?, ?, TC]] = {
    type KleisliTC[A, B] = Kleisli[A, B, TC]

    new ArrowPlus[KleisliTC] {
      private val monad = implicitly[MonadPlus[TC]]

      def zero[A, B]: KleisliTC[A, B] = Kleisli { _ => monad.zero }

      def plus[A, B](l: KleisliTC[A, B], r: KleisliTC[A, B]): KleisliTC[A, B] =
        Kleisli { (a: A) => monad.plus(l(a), r(a)) }
    }
  }

  implicit def kleisliArrowChoice[TC[_]: Monad] = new ArrowChoice[Kleisli[?, ?, TC], TC] {
    type KleisliTC[A, B] = Kleisli[A, B, TC]

    protected val arrow = implicitly[Arrow[KleisliTC, TC]]

    def left[A, B, C](f: KleisliTC[A, B]) =
      split(f, arrow.arr(identity[C]))

    override def right[A, B, C](f: KleisliTC[A, B]) =
      split(arrow.arr(identity[C]), f)

    override def split[A, B, C, D](f: KleisliTC[A, B], g: KleisliTC[C, D]) = {
      val fIntoLeft: KleisliTC[A, Either[B, D]] = f >>> arrow.arr(Left(_))
      val gIntoRight: KleisliTC[C, Either[B, D]] = g >>> arrow.arr(Right(_))
      fanIn(fIntoLeft, gIntoRight)
    }

    override def fanIn[A, B, C](f: KleisliTC[A, C], g: KleisliTC[B, C]) = (f, g) match {
      case (Kleisli(f), Kleisli(g)) => Kleisli((either: Either[A, B]) => either.fold(f, g))
    }
  }
}

