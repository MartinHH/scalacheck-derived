package io.github.martinhh

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen.perturb
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

sealed trait SimpleADT

object SimpleADT:
  val expectedGen: Gen[SimpleADT] =
    Gen.oneOf(Gen.const(SimpleCaseObject), SimpleCaseClass.expectedGen)

  val expectedCogen: Cogen[SimpleADT] =
    Cogen { (seed, value) =>
      value match
        case SimpleCaseObject =>
          perturb(
            perturb[SimpleCaseObject.type](
              seed,
              SimpleCaseObject
            )(SimpleCaseObject.expectedCogen),
            0
          )
        case cc: SimpleCaseClass =>
          perturb(
            perturb[SimpleCaseClass](
              seed,
              cc
            )(SimpleCaseClass.expectedCogen),
            1
          )
    }

case object SimpleCaseObject extends SimpleADT:
  val expectedCogen: Cogen[SimpleCaseObject.type] =
    Cogen.cogenUnit.contramap(_ => ())

case class SimpleCaseClass(x: Int, y: String, z: Double) extends SimpleADT

object SimpleCaseClass:
  val expectedGen: Gen[SimpleCaseClass] =
    for {
      x <- arbitrary[Int]
      y <- arbitrary[String]
      z <- arbitrary[Double]
    } yield SimpleCaseClass(x, y, z)

  val expectedCogen: Cogen[SimpleCaseClass] =
    Cogen { (seed, value) =>
      perturb[Unit](
        perturb[Double](
          perturb[String](
            perturb[Int](
              seed,
              value.x
            ),
            value.y
          ),
          value.z
        ),
        ()
      )
    }

case class CaseClassWithContainers(
  set: Set[Int],
  list: List[Boolean],
  option: Option[String],
  either: Either[String, Double]
)

object CaseClassWithContainers:
  val expectedGen: Gen[CaseClassWithContainers] =
    for {
      set <- arbitrary[Set[Int]]
      list <- arbitrary[List[Boolean]]
      option <- arbitrary[Option[String]]
      either <- arbitrary[Either[String, Double]]
    } yield CaseClassWithContainers(set, list, option, either)

  val expectedCogen: Cogen[CaseClassWithContainers] =
    Cogen { (seed, value) =>
      perturb[Unit](
        perturb[Either[String, Double]](
          perturb[Option[String]](
            perturb[List[Boolean]](
              perturb[Set[Int]](
                seed,
                value.set
              ),
              value.list
            ),
            value.option
          ),
          value.either
        ),
        ()
      )
    }

enum ABC(val asChar: Char):
  case A extends ABC('A')
  case B extends ABC('B')
  case C extends ABC('C')

object ABC:
  val expectedGen: Gen[ABC] =
    Gen.oneOf(ABC.A, ABC.B, ABC.C)

  val expectedCogen: Cogen[ABC] =
    Cogen { (seed, value) =>
      val ordinal = value.asChar - ABC.A.asChar
      perturb(perturb(seed, ()), ordinal)
    }

sealed trait ComplexADTWithNestedMembers

case object AnotherCaseObject extends ComplexADTWithNestedMembers

sealed abstract class AbstractSubClass[N <: SimpleADT](abc: ABC)
  extends ComplexADTWithNestedMembers:
  def nestedSimple: N

object AbstractSubClass:
  case class SubclassA(a: Int, b: String, nestedSimple: SimpleCaseClass)
    extends AbstractSubClass[SimpleCaseClass](ABC.A)

  object SubclassA:
    val expectedGen: Gen[SubclassA] =
      for {
        a <- arbitrary[Int]
        b <- arbitrary[String]
        n <- SimpleCaseClass.expectedGen
      } yield SubclassA(a, b, n)

  case class SubclassB(nestedSimple: SimpleADT) extends AbstractSubClass[SimpleADT](ABC.B)

  object SubclassB:
    val expectedGen: Gen[SubclassB] =
      SimpleADT.expectedGen.map(SubclassB.apply)

  case class SubclassC(c: String, d: Double, anotherLetter: ABC)
    extends AbstractSubClass[SimpleCaseObject.type](ABC.C):
    override def nestedSimple: SimpleCaseObject.type = SimpleCaseObject

  object SubclassC:
    val expectedGen: Gen[SubclassC] =
      for {
        c <- arbitrary[String]
        d <- arbitrary[Double]
        l <- ABC.expectedGen
      } yield SubclassC(c, d, l)

object ComplexADTWithNestedMembers:
  val expectedGen: Gen[ComplexADTWithNestedMembers] =
    Gen.oneOf(
      Gen.const(AnotherCaseObject),
      AbstractSubClass.SubclassA.expectedGen,
      AbstractSubClass.SubclassB.expectedGen,
      AbstractSubClass.SubclassC.expectedGen
    )
