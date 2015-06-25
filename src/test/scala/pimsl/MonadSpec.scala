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
import scala.language.higherKinds
import scala.util.{ Either, Try, Success, Failure }
import pimsl.Monad._
import prop._

class MonadSpec extends FlatSpec with Matchers with PropertyChecks {
  /* Use the same exception instance during equality comparisons */
  val e = new Exception

  /* Syntactic sugar for the tests */
  implicit class MapMSyntax[A, TC[_]](tc: TC[A])(implicit monad: Monad[TC]) {
    def mapM[B](f: A => B): TC[B] = monad.map(tc)(f)
  }
  implicit class FilterMSyntax[A, TC[_]](tc: TC[A])(implicit monad: MonadPlus[TC]) {
    def filterM(f: A => Boolean): TC[A] = monad.filter(tc)(f)
  }
  implicit class SumMSyntax[A, TC[_]](ltc: List[TC[A]])(implicit m: MonadPlus[TC]) {
    def sumM: TC[A] = m.sum(ltc)
  }

  "Monad.map" should "map over a List" in {
    List(1, 2, 3).mapM(_ + 1) shouldBe List(2, 3, 4)
    List(1, 2, 3).mapM(_.toString) shouldBe List("1", "2", "3")
    List[Int]().mapM(_ + 1) shouldBe List()
  }

  it should "map over an Option" in {
    (Some(1): Option[Int]).mapM(_ + 1) shouldBe Some(2)
    (Some(1): Option[Int]).mapM(_.toString) shouldBe Some("1")
    (None: Option[Int]).mapM(_ + 1) shouldBe None
  }

  type EitherAOrString[A] = Either[A, String]

  it should "map over the left side of Either" in {
    (Left(1): EitherAOrString[Int]).mapM(_ + 1) shouldBe Left(2)
    (Left(1): EitherAOrString[Int]).mapM(_.toString) shouldBe Left("1")
    (Right("oops"): EitherAOrString[Int]).mapM(_ + 1) shouldBe Right("oops")
  }

  type EitherStringOrA[A] = Either[String, A]

  it should "map over the right side of Either" in {
    (Right(1): EitherStringOrA[Int]).mapM(_ + 1) shouldBe Right(2)
    (Right(1): EitherStringOrA[Int]).mapM(_.toString) shouldBe Right("1")
    (Left("oops"): EitherStringOrA[Int]).mapM(_ + 1) shouldBe Left("oops")
  }

  it should "map over a successful Try" in {
    Try(1).mapM(a => a + 1) shouldBe Success(2)
    Try[Int](throw e).mapM(a => a + 1) should be a 'failure
  }

  "Monad.map2" should "map over the product of two Lists" in {
    List(1, 2, 3).map2(List("4", "5", "6"))((a, b) => a + b.toInt) shouldBe List(5, 6, 7, 6, 7, 8, 7, 8, 9)
  }

  it should "combine Options" in {
    (Some(1): Option[Int]).map2(Some(4))((a, b) => a + b) shouldBe Some(5)
    (None: Option[Int]).map2(Some(4))((a, b) => a + b) shouldBe None
  }

  it should "combine Trys" in {
    (Success(1): Try[Int]).map2(Success(4))((a, b) => a + b) shouldBe Success(5)
    (Failure(e): Try[Int]).map2(Success(4))((a, b) => a + b) should be a 'failure
  }

  "Monad.sum" should "concatenate Lists" in {
    List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)).sumM shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  it should "pick the first defined Option" in {
    List(None, None, Some(1), None, Some(3)).sumM shouldBe Some(1)
  }

  it should "pick the first successful Try" in {
    val e = new Exception
    List(Failure(e), Failure(e), Success(1), Failure(e), Success(3)).sumM shouldBe Success(1)
    List[Try[Int]](Failure(e), Failure(e), Failure(e)).sumM should be a 'failure
  }

  "Monad.sequence" should "turn a List of Option into an Option of List" in {
    val loa: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val hasNone: List[Option[Int]] = List(Some(1), None, Some(3))
    loa.sequence shouldBe Some(List(1, 2, 3))
    hasNone.sequence shouldBe None
  }

  it should "turn a list of Try into an Try of List" in {
    val lta: List[Try[Int]] = List(Success(1), Success(2), Success(3))
    val hasFailure: List[Try[Int]] = List(Success(1), Failure(e), Success(3))
    lta.sequence shouldBe Success(List(1, 2, 3))
    hasFailure.sequence shouldBe Failure(e)
  }

  "Monad.traverse" should "sequence a list, applying a mapping function" in {
    val loa: List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val hasNone: List[Option[Int]] = List(Some(1), None, Some(3))
    loa.traverse(i => Some(i.toString)) shouldBe Some(List("1", "2", "3"))
    hasNone.traverse(i => Some(i.toString)) shouldBe None
  }

  "Monad.filter" should "filter a List" in {
    List(1, 2, 3, 4).filterM(_ % 2 == 0) shouldBe List(2, 4)
    List(1, 3, 5, 7).filterM(_ % 2 == 0) shouldBe List()
    List[Int]().filterM(_ % 2 == 0) shouldBe List()
  }

  it should "filter an Option" in {
    (Some(2): Option[Int]).filterM(_ % 2 == 0) shouldBe Some(2)
    (Some(1): Option[Int]).filterM(_ % 2 == 0) shouldBe None
    (None: Option[Int]).filterM(_ % 2 == 0) shouldBe None
  }

  it should "filter a Try" in {
    (Success(2): Try[Int]).filterM(_ % 2 == 0) shouldBe Success(2)
    (Success(1): Try[Int]).filterM(_ % 2 == 0) should be a 'failure
    (Failure(e): Try[Int]).filterM(_ % 2 == 0) should be a 'failure
  }

  "the State monad" should "follow the monad laws" in {
    val genState: Gen[State[Int, Int]] = for {
      i <- Arbitrary.arbitrary[Int]
      j <- Arbitrary.arbitrary[Int]
    } yield State(s => (i, j))
    implicit lazy val arbState: Arbitrary[State[Int, Int]] = Arbitrary(genState)

    forAll { (i: Int, j: Int, state: State[Int, Int]) =>
      val sm = implicitly[Monad[State[Int, ?]]]

      val inc: Int => State[Int, Unit] = a => State(s => (s + a, ()))
      val get: Int => State[Int, Int] = a => State(s => (s, s))

      // Left identity
      sm.flatMap(sm.unit(i))(inc)(j) shouldBe inc(i)(j)
      sm.flatMap(sm.unit(i))(get)(j) shouldBe get(i)(j)

      // Right identity
      sm.flatMap(state)(sm.unit(_))(i) shouldBe state(i)

      val f: Int => State[Int, Int] = a => sm.unit(a + 1)
      val g: Int => State[Int, Int] = a => sm.unit(a % 2)

      // Associativity
      sm.flatMap(sm.flatMap(state)(f))(g)(i) shouldBe
        sm.flatMap(state)(x => sm.flatMap(f(x))(g))(i)
    }
  }

  it should "be usable with sequence comprehensions" in {
    val addThreeAndGet = for {
      _ <- State[Int, Unit](s => (s + 3, ()))
      a <- State[Int, Int](s => (s, s))
    } yield a

    addThreeAndGet(0) shouldBe 3
  }

  it should "allow get and put operations" in {
    type Stack = List[Int]

    def pop = State[Stack, Int] {
      case Nil => throw new NoSuchElementException
      case x :: xs => (xs, x)
    }
    def push(a: Int) = State[Stack, Unit] {
      case xs => (a :: xs, ())
    }

    val popEmptyStack = for {
      a <- pop
    } yield a

    intercept[NoSuchElementException] {
      popEmptyStack(Nil)
    }

    val getPut = for {
      initValues <- State.get[Stack]
      _ <- if (initValues.isEmpty) State.put(List(5, 1)) else State.get[Stack]
      _ <- push(3)
      finalValues <- State.get
    } yield finalValues

    getPut(Nil) shouldBe List(3, 5, 1)
    getPut(List(1)) shouldBe List(3, 1)
  }

  "the List monad" should "follow the monad laws" in {
    forAll { (i: Int, list: List[Int]) =>
      val lm = implicitly[Monad[List]]

      val f1: Int => List[Int] = a => List(a + 1)
      val f2: Int => List[Int] = a => Nil

      // Left identity
      lm.flatMap(lm.unit(i))(f1) shouldBe f1(i)
      lm.flatMap(lm.unit(i))(f2) shouldBe f2(i)

      // Right identity
      lm.flatMap(list)(lm.unit(_)) shouldBe list

      val f: Int => List[Int] = a => lm.unit(a + 1)
      val g: Int => List[Int] = a => lm.unit(a % 2)

      // Associativity
      lm.flatMap(lm.flatMap(list)(f))(g) shouldBe
        lm.flatMap(list)(x => lm.flatMap(f(x))(g))
    }
  }

  "the Option monad" should "follow the monad laws" in {
    forAll { (i: Int, opt: Option[Int]) =>
      val om = implicitly[Monad[Option]]
      val none = None

      val f1: Int => Option[Int] = a => Some(a + 1)
      val f2: Int => Option[Int] = a => None

      // Left identity
      om.flatMap(om.unit(i))(f1) shouldBe f1(i)
      om.flatMap(om.unit(i))(f2) shouldBe f2(i)

      // Right identity
      om.flatMap(opt)(om.unit(_)) shouldBe opt

      val f: Int => Option[Int] = a => om.unit(a + 1)
      val g: Int => Option[Int] = a => om.unit(a % 2)

      // Associativity
      om.flatMap(om.flatMap(opt)(f))(g) shouldBe
        om.flatMap(opt)(x => om.flatMap(f(x))(g))
    }
  }

  "the Left Either monad" should "follow the monad laws" in {
    forAll { (i: Int, either: Either[Int, String]) =>
      val lem = implicitly[Monad[Either[?, String]]]

      val f1: Int => Either[Int, String] = a => Left(a + 1)
      val f2: Int => Either[Int, String] = a => Right("error")

      // Left identity
      lem.flatMap(lem.unit(i))(f1) shouldBe f1(i)
      lem.flatMap(lem.unit(i))(f2) shouldBe f2(i)

      // Right identity
      lem.flatMap(either)(lem.unit(_)) shouldBe either

      val f: Int => Either[Int, String] = a => lem.unit(a + 1)
      val g: Int => Either[Int, String] = a => lem.unit(a % 2)

      // Associativity
      lem.flatMap(lem.flatMap(either)(f))(g) shouldBe
        lem.flatMap(either)(x => lem.flatMap(f(x))(g))
    }
  }

  "the Right Either monad" should "follow the monad laws" in {
    forAll { (i: Int, either: Either[String, Int]) =>
      val rem = implicitly[Monad[Either[String, ?]]]

      val f1: Int => Either[String, Int] = a => Right(a + 1)
      val f2: Int => Either[String, Int] = a => Left("error")

      // Left identity
      rem.flatMap(rem.unit(i))(f1) shouldBe f1(i)
      rem.flatMap(rem.unit(i))(f2) shouldBe f2(i)

      // Right identity
      rem.flatMap(either)(rem.unit(_)) shouldBe either

      val f: Int => Either[String, Int] = a => rem.unit(a + 1)
      val g: Int => Either[String, Int] = a => rem.unit(a % 2)

      // Associativity
      rem.flatMap(rem.flatMap(either)(f))(g) shouldBe
        rem.flatMap(either)(x => rem.flatMap(f(x))(g))
    }
  }

  "the Try monad" should "follow the monad laws" in {
    val arbSucc: Gen[Success[Int]] = for {
      i <- Arbitrary.arbitrary[Int]
    } yield Success(i)
    val arbFail: Gen[Failure[Int]] = Gen.const(Failure(new Exception))
    implicit lazy val arbTry: Arbitrary[Try[Int]] =
      Arbitrary(Gen.oneOf(arbSucc, arbFail))

    forAll { (i: Int, `try`: Try[Int]) =>
      val tm = implicitly[Monad[Try]]

      val f1: Int => Try[Int] = a => Try(a + 1)
      val f2: Int => Try[Unit] = a => Try(throw e)

      // Left identity
      tm.flatMap(tm.unit(i))(f1) shouldBe f1(i)
      tm.flatMap(tm.unit(i))(f2) shouldBe f2(i)

      // Right identity
      tm.flatMap(`try`)(tm.unit(_)) shouldBe `try`

      val f: Int => Try[Int] = a => tm.unit(a + 1)
      val g: Int => Try[Int] = a => tm.unit(a % 2)

      // Associativity
      tm.flatMap(tm.flatMap(`try`)(f))(g) shouldBe
        tm.flatMap(`try`)(x => tm.flatMap(f(x))(g))
    }
  }
}