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

  it should "follow the arrow laws" in {
    val a = implicitly[Arrow[Function1, Id]]
    val f: Int => Int = a => a + 10
    val g: Int => Int = a => a * 2
    def assoc[A, B, C](t: ((A, B), C)): (A, (B, C)) =
      (t._1._1, (t._1._2, t._2))

    forAll { (i: Int, t: (Int, Int), nt: Tuple2[(Int, Int), Int]) =>
      a.arr(identity[Int])(i) shouldBe identity[Int](i)

      a.arr(f >>> g)(i) shouldBe (a.arr(f) >>> a.arr(g))(i)

      a.arr(f).first(t) shouldBe a.arr(f.first[Int])(t)

      (f >>> g).first(t) shouldBe (f.first[Int] >>> g.first)(t)

      (f.first >>> a.arr((tp: (Int, Int)) => tp._1))(t) shouldBe {
        (a.arr((tp: (Int, Int)) => tp._1) >>> f)(t)
      }

      (f.first >>> a.arr(identity[Int] _ *** g))(t) shouldBe {
        (a.arr(identity[Int] _ *** g) >>> f.first)(t)
      }

      (f.first.first >>> a.arr((tp: Tuple2[(Int, Int), Int]) => assoc(tp)))(nt) shouldBe {
        (a.arr((tp: Tuple2[(Int, Int), Int]) => assoc(tp)) >>> f.first)(nt)
      }
    }
  }

  type KleisliList[A, B] = Kleisli[A, B, List]
  val listInit: KleisliList[Int, Int] = Kleisli((i: Int) => List(i, i, i))
  val listTupleInit: KleisliList[(Int, Int), (Int, Int)] = Kleisli((t: (Int, Int)) => List(t))
  val listAddOne: KleisliList[Int, Int] = Kleisli((i: Int) => List(i + 1))
  val listAddTwo: KleisliList[Int, Int] = Kleisli((i: Int) => List(i + 2))
  val listDivTwo: KleisliList[Int, Int] = Kleisli((i: Int) => List(i / 2))

  "The Kleisli arrow" should "sequence monads using >>>" in {
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

  it should "apply an associative monadic operation using <+>" in {
    val listOfTwos = (listInit >>> listAddTwo >>> listDivTwo)
    val listOfThrees = (listInit >>> listAddTwo >>> listAddTwo >>> listDivTwo)

    (listOfTwos <+> listOfThrees)(2) shouldBe List(2, 2, 2, 3, 3, 3)
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
    val proc: ((Int, Int)) => List[((Int, Int), (Int, Int), Int)] =
      (t: (Int, Int)) => for {
        x <- (listAddOne *** listAddTwo <<< listTupleInit) -< t
        y <- (listAddTwo.second <<< listTupleInit) -< t
        z <- (listAddTwo <<< listInit) -< x._1
      } yield (x, y, z)

    proc((0, 0)) shouldBe List(((1, 2), (0, 2), 3), ((1, 2), (0, 2), 3), ((1, 2), (0, 2), 3))
  }

  type KleisliOpt[A, B] = Kleisli[A, B, Option]
  it should "follow the arrow laws" in {
    val a = implicitly[Arrow[KleisliOpt, Option]]
    val f: KleisliOpt[Int, Int] = Kleisli((i: Int) => Some(i + 10))
    val g: KleisliOpt[Int, Int] = Kleisli((i: Int) => Some(i * 2))
    val id: Int => Option[Int] = (i: Int) => Some(identity[Int](i))
    def assoc[A, B, C](t: ((A, B), C)): (A, (B, C)) =
      (t._1._1, (t._1._2, t._2))

    forAll { (i: Int, t: (Int, Int), nt: Tuple2[(Int, Int), Int]) =>
      a.kleisli(id)(i) shouldBe id(i)

      a.kleisli((j: Int) => (f >>> g)(j))(i) shouldBe {
        (a.kleisli((k: Int) => f(k)) >>> a.kleisli(l => g(l)))(i)
      }

      a.kleisli((j: Int) => f(j)).first(t) shouldBe {
        a.kleisli((u: (Int, Int)) => f.first[Int](u))(t)
      }

      (f >>> g).first(t) shouldBe (f.first[Int] >>> g.first)(t)

      (f.first >>> a.arr((tp: (Int, Int)) => tp._1))(t) shouldBe {
        (a.arr((tp: (Int, Int)) => tp._1) >>> f)(t)
      }

      (f.first >>> a.kleisli((tp: (Int, Int)) => (a.kleisli(id) *** g)(tp)))(t) shouldBe {
        (a.kleisli((tp: (Int, Int)) => (a.kleisli(id) *** g)(tp)) >>> f.first)(t)
      }

      (f.first.first >>> a.arr((tp: Tuple2[(Int, Int), Int]) => assoc(tp)))(nt) shouldBe {
        (a.arr((tp: Tuple2[(Int, Int), Int]) => assoc(tp)) >>> f.first)(nt)
      }
    }
  }

  "Function1 ArrowChoice" should "split two input arrows as Either to Either outputs using +++" in {
    val intToString: Int => String = _.toString
    val stringToInt: String => Int = _.toInt
    (intToString +++ stringToInt)(Left(1)) shouldBe Left("1")
    (intToString +++ stringToInt)(Right("2")) shouldBe Right(2)
  }

  it should "join two input arrows as Either with same outputs using |||" in {
    val intToString: Int => String = _.toString
    val stringToString: String => String = _.toUpperCase
    (intToString ||| stringToString)(Left(1)) shouldBe "1"
    (intToString ||| stringToString)(Right("upper")) shouldBe "UPPER"
  }

  it should "apply an arrow to the Left of an Either input arrow using left" in {
    val intMultTen: Int => Int = _ * 10
    val leftInt: Either[Int, String] = Left(1)
    val rightString: Either[Int, String] = Right("unaltered")
    intMultTen.left(leftInt) shouldBe Left(10)
    intMultTen.left(rightString) shouldBe Right("unaltered")
  }

  it should "apply an arrow to the Right of an Either input arrows using right" in {
    val stringToUpper: String => String = _.toUpperCase
    val leftInt: Either[Int, String] = Left(1)
    val rightString: Either[Int, String] = Right("altered")
    stringToUpper.right(leftInt) shouldBe Left(1)
    stringToUpper.right(rightString) shouldBe Right("ALTERED")
  }

  "Kleisli ArrowChoice" should "split two input arrows as Either to Either outputs using +++" in {
    val intToString: KleisliOpt[Int, String] = Kleisli(i => Some(i.toString))
    val stringToInt: KleisliOpt[String, Int] = Kleisli(s => Some(s.toInt))
    (intToString +++ stringToInt)(Left(1)) shouldBe Some(Left("1"))
    (intToString +++ stringToInt)(Right("2")) shouldBe Some(Right(2))
  }

  it should "join two input arrows as Either with same outputs using |||" in {
    val intToString: KleisliOpt[Int, String] = Kleisli(i => Some(i.toString))
    val stringToString: KleisliOpt[String, String] = Kleisli(s => Some(s.toUpperCase))
    (intToString ||| stringToString)(Left(1)) shouldBe Some("1")
    (intToString ||| stringToString)(Right("upper")) shouldBe Some("UPPER")
  }

  it should "apply an arrow to the Left of an Either input arrow using left" in {
    val intMultTen: KleisliOpt[Int, Int] = Kleisli(i => Some(i * 10))
    val leftInt: Either[Int, String] = Left(1)
    val rightString: Either[Int, String] = Right("unaltered")
    intMultTen.left(leftInt) shouldBe Some(Left(10))
    intMultTen.left(rightString) shouldBe Some(Right("unaltered"))
  }

  it should "apply an arrow to the Right of an Either input arrows using right" in {
    val stringToUpper: KleisliOpt[String, String] = Kleisli(s => Some(s.toUpperCase))
    val leftInt: Either[Int, String] = Left(1)
    val rightString: Either[Int, String] = Right("altered")
    stringToUpper.right(leftInt) shouldBe Some(Left(1))
    stringToUpper.right(rightString) shouldBe Some(Right("ALTERED"))
  }
}