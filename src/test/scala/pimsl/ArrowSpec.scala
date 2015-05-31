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

import org.scalacheck._
import org.scalatest._
import org.scalatest.matchers._
import pimsl.Arrow._
import pimsl.Monad._
import prop._
import scala.language.higherKinds

class ArrowSpec extends FlatSpec with Matchers with PropertyChecks {
  val addOne: Int => Int = a => a + 1
  val addTwo: Int => Int = a => a + 2
  val divTwo: Int => Int = a => a / 2

  "The Function1 arrow" should "sequence functions using >>>" in {
    (addTwo >>> addTwo >>> divTwo)(2) shouldBe 3
  }

  it should "compose functions using <<<" in {
    (divTwo <<< addTwo <<< addTwo)(2) shouldBe 3
  }

  it should "map over the first element of Tuple2 using first" in {
    (addOne.first)((0, 0)) shouldBe (1, 0)
    (addTwo.first)((0, 0)) shouldBe (2, 0)
  }

  it should "map over the second element of Tuple2 using second" in {
    (addOne.second)((0, 0)) shouldBe (0, 1)
    (addTwo.second)((0, 0)) shouldBe (0, 2)
  }

  it should "apply functions over both sides of Tuple2 using ***" in {
    (addOne *** addTwo)((1, 2)) shouldBe (2, 4)
    (addTwo *** addOne)((1, 2)) shouldBe (3, 3)
  }

  it should "clone a single input into different functions using &&&" in {
    (addOne &&& addTwo)(0) shouldBe (1, 2)
    (addTwo &&& addOne)(0) shouldBe (2, 1)
  }

  it should "be usable with for comprehensions using -<" in {
    val proc: Int => Int = x => for {
      y <- addOne -< x
    } yield y

    proc(0) shouldBe 1
  }

  it should "allow multiple arrow generators" in {
    val proc: Int => Int = x => for {
      y <- addOne -< x
      z <- addTwo -< x
    } yield y + z

    proc(0) shouldBe 3
  }

  it should "allow complex arrows within a generator" in {
    val proc: ((Int, Int)) => ((Int, Int), (Int, Int), Int) = (t) => for {
      x <- (addOne *** addTwo) -< t
      y <- addTwo.second -< t
      z <- addTwo -< x._1
    } yield (x, y, z)

    proc((0, 0)) shouldBe ((1, 2), (0, 2), 3)
  }

  type KleisliList[A, B] = Kleisli[A, B, List]
  val listInit: KleisliList[Int, Int] = Kleisli((i: Int) => List(i, i, i))
  val listTupleInit: KleisliList[(Int, Int), (Int, Int)] = Kleisli((t: (Int, Int)) => List(t))
  val listAddOne: KleisliList[Int, Int] = Kleisli((i: Int) => List(i + 1))
  val listAddTwo: KleisliList[Int, Int] = Kleisli((i: Int) => List(i + 2))
  val listDivTwo: KleisliList[Int, Int] = Kleisli((i: Int) => List(i / 2))

  "The Kleisli List arrow" should "sequence monads using >>>" in {
    (listInit >>> listAddTwo >>> listAddTwo >>> listDivTwo)(2) shouldBe List(3, 3, 3)
  }

  it should "compose monads using <<<" in {
    (listDivTwo <<< listAddTwo <<< listInit)(2) shouldBe List(2, 2, 2)
  }

  it should "map over the first element of Tuple2 using first" in {
    (listTupleInit >>> listAddOne.first)((0, 0)) shouldBe List((1, 0))
    (listTupleInit >>> listAddTwo.first)((0, 0)) shouldBe List((2, 0))
  }

  it should "map over the second element of Tuple2 using second" in {
    (listTupleInit >>> listAddOne.second)((0, 0)) shouldBe List((0, 1))
    (listTupleInit >>> listAddTwo.second)((0, 0)) shouldBe List((0, 2))
  }

  it should "apply functions over both sides of Tuple2 using ***" in {
    (listTupleInit >>> listAddOne *** listAddTwo)((1, 2)) shouldBe List((2, 4))
    (listTupleInit >>> listAddTwo *** listAddOne)((1, 2)) shouldBe List((3, 3))
  }

  it should "clone a single input into different functions using &&&" in {
    (listInit >>> listAddOne &&& listAddTwo)(0) shouldBe List((1, 2), (1, 2), (1, 2))
    (listInit >>> listAddTwo &&& listAddOne)(0) shouldBe List((2, 1), (2, 1), (2, 1))
  }

  it should "be usable with for comprehensions using -<" in {
    val proc: Int => List[Int] = x => for {
      y <- listInit -< x
    } yield y

    proc(0) shouldBe List(0, 0, 0)
  }

  it should "allow multiple arrow generators" in {
    val proc: Int => List[Int] = x => for {
      y <- (listAddOne <<< listInit) -< x
      z <- (listAddTwo <<< listInit) -< x
    } yield y + z

    proc(0) shouldBe List(3, 3, 3, 3, 3, 3, 3, 3, 3)
  }

  it should "return the product of yield operations over List" in {
    val proc1: Int => List[Int] = x => for {
      i <- listInit -< x
    } yield i

    proc1(0).length shouldBe 3

    val proc2: Int => List[Int] = x => for {
      i <- listInit -< x
      j <- listInit -< x
    } yield i + j

    proc2(0).length shouldBe 9

    val proc3: Int => List[Int] = x => for {
      i <- listInit -< x
      j <- listInit -< x
      k <- listInit -< x
    } yield i + j + k

    proc3(0).length shouldBe 27
  }

  it should "allow complex arrows within a generator" in {
    val proc: ((Int, Int)) => List[((Int, Int), (Int, Int))] =
      (t: (Int, Int)) => for {
        x <- (listAddOne *** listAddTwo <<< listTupleInit) -< t
        y <- (listAddTwo.second <<< listTupleInit) -< t
      } yield (x, y)

    proc((0, 0)) shouldBe List(((1, 2), (0, 2)))
  }
}