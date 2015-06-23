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

import scala.language.higherKinds
import scala.util.{ Either, Try, Success, Failure }

trait Monad[TC[_]] {
  def unit[A](a: A): TC[A]

  def map[A, B](tc: TC[A])(f: A => B): TC[B] =
    flatMap(tc)(a => unit(f(a)))

  def map2[A, B, C](l: TC[A], r: TC[B])(f: (A, B) => C) =
    flatMap(l)(a => map(r)(b => f(a, b)))

  def flatMap[A, B](tc: TC[A])(f: A => TC[B]): TC[B]

  def traverse[A, B](ltc: List[TC[A]])(f: A => TC[B]): TC[List[B]] =
    ltc match {
      case Nil => unit(Nil)
      case h :: t => map2(flatMap(h)(f), traverse(t)(f))(_ :: _)
    }

  def sequence[A](ltc: List[TC[A]]): TC[List[A]] = traverse(ltc)(unit(_))
}

trait MonadPlus[TC[_]] extends Monad[TC] {
  def zero[A]: TC[A]

  def plus[A](l: TC[A], r: TC[A]): TC[A]

  def sum[A](ltc: List[TC[A]]): TC[A] =
    ltc.foldLeft(zero[A])(plus(_, _))

  def filter[A](tc: TC[A])(f: A => Boolean): TC[A] =
    flatMap(tc)(a => if (f(a)) unit(a) else zero)
}

object Monad {

  implicit class MonadSyntax[A, TC[_]](tc: TC[A])(implicit m: Monad[TC]) {
    def map2[B, C](otc: TC[B])(f: (A, B) => C) = m.map2(tc, otc)(f)
  }

  implicit class MonadListSyntax[A, TC[_]](ltc: List[TC[A]])(implicit m: Monad[TC]) {
    def traverse[B](f: A => TC[B]): TC[List[B]] = m.traverse(ltc)(f)
    def sequence: TC[List[A]] = m.sequence(ltc)
  }

  implicit val listMonad = new MonadPlus[List] {
    def zero[A] = List[A]()

    def plus[A](l: List[A], r: List[A]) = l ++ r

    def unit[A](a: A): List[A] = List(a)

    def flatMap[A, B](list: List[A])(f: A => List[B]) = list flatMap f
  }

  implicit val optionMonad = new MonadPlus[Option] {
    def zero[A] = None

    def plus[A](l: Option[A], r: Option[A]) = l orElse r

    def unit[A](a: A): Some[A] = Some(a)

    def flatMap[A, B](option: Option[A])(f: A => Option[B]) = option flatMap f
  }

  implicit def eitherLeftMonad[A] = new Monad[Either[?, A]] {
    def unit[B](b: B) = Left(b)

    def flatMap[B, C](either: Either[B, A])(f: B => Either[C, A]) =
      either.left flatMap f
  }

  implicit def eitherRightMonad[A] = new Monad[Either[A, ?]] {
    def unit[B](b: B) = Right(b)

    def flatMap[B, C](either: Either[A, B])(f: B => Either[A, C]) =
      either.right flatMap f
  }

  implicit def tryMonad = new MonadPlus[Try] {
    def zero[A]: Try[A] = Failure(new Exception("Try.zero"))

    def plus[A](l: Try[A], r: Try[A]) = if (l.isSuccess) l else r

    def unit[A](a: A) = Success(a)

    def flatMap[A, B](t: Try[A])(f: A => Try[B]) = t flatMap f
  }

  case class State[S, +A](func: S => (S, A)) {
    def apply(s: S) = func(s)._2
    def map[B](f: A => B)(implicit monad: StateMonad[S]) = monad.map(this)(f)
    def flatMap[B](f: A => State[S, B])(implicit monad: StateMonad[S]) = monad.flatMap(this)(f)
  }
  object State {
    implicit def monad[S] = new StateMonad[S]
    def put[S](s: S): State[S, Unit] = monad.put(s)
    def get[S]: State[S, S] = monad.get
  }

  class StateMonad[S] extends Monad[State[S, ?]] {
    def unit[A](a: A): State[S, A] = State(s => (s, a))

    def flatMap[A, B](state: State[S, A])(mapFunc: A => State[S, B]) = State {
      (initialState: S) =>
        {
          val (newState, returnValue) = state.func(initialState)
          mapFunc(returnValue) match { case State(newFunc) => newFunc(newState) }
        }
    }

    def get: State[S, S] = State(s => (s, s))

    def put[A](s: A): State[A, Unit] = State(_ => (s, ()))
  }

}