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

import pimsl.Arrow._
import scala.language.higherKinds

trait Applyable[F[_, _], TC[_]] {
  def apply[A, B](f: F[A, B])(a: A): TC[B]
  def flatten[A](a: TC[TC[A]]): TC[A]
}

object Applyable {
  type Id[A] = A

  implicit val function1Applyable: Applyable[Function1, Id] = new Applyable[Function1, Id] {
    def apply[A, B](f: A => B)(a: A): B = f(a)
    def flatten[A](a: A): A = a
  }

  implicit def kleisliApplyable[TC[_]: Monad]: Applyable[Kleisli[?, ?, TC], TC] = new Applyable[Kleisli[?, ?, TC], TC] {
    type KleisliTC[A, B] = Kleisli[A, B, TC]
    protected val monad = implicitly[Monad[TC]]

    def apply[A, B](f: KleisliTC[A, B])(a: A): TC[B] = f(a)
    def flatten[A](a: TC[TC[A]]): TC[A] = monad.flatMap(a)(identity)
  }
}