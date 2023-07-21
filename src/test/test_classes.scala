package io.github.martinhh

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen.perturb
import org.scalacheck.Gen
import org.scalacheck.Shrink

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

  @annotation.nowarn("msg=Stream .* is deprecated")
  val expectedShrink: Shrink[SimpleADT] =
    Shrink {
      case SimpleCaseObject =>
        Stream.empty
      case scc: SimpleCaseClass =>
        SimpleCaseClass.expectedShrink.shrink(scc)
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

  val expectedShrink: Shrink[SimpleCaseClass] =
    given shrinkTuple: Shrink[(Int, String, Double)] = Shrink.shrinkTuple3
    Shrink.xmap[(Int, String, Double), SimpleCaseClass](
      SimpleCaseClass.apply.tupled(_),
      a => (a.x, a.y, a.z)
    )

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

  @annotation.nowarn("msg=Stream .* is deprecated")
  val expectedShrink: Shrink[ABC] =
    Shrink(_ => Stream.empty)

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

case class HasGivenInstances(x: Int)

object HasGivenInstances:

  // this is a nonsense-implementation, only here to prove that it takes precedence
  // over derivation (and over `Shrink.shrinkAny`):
  @annotation.nowarn("msg=Stream .* is deprecated")
  given specialHasGivenInstancesShrink: Shrink[HasGivenInstances] = Shrink { hgi =>
    (1 to 3).map(i => HasGivenInstances(hgi.x + i)).toStream
  }

case class HasMemberThatHasGivenInstances(member: HasGivenInstances)

object HasMemberThatHasGivenInstances:
  val expectedShrink: Shrink[HasMemberThatHasGivenInstances] =
    Shrink.xmap(HasMemberThatHasGivenInstances.apply, _.member)

// format: off
case class MaxCaseClass(
  a1: Int, b1: Int, c1: Int, d1: Int, e1: Int, f1: Int, g1: Int, h1: Int, i1: Int, j1: Int,
  k1: Int, l1: Int, m1: Int, n1: Int, o1: Int, p1: Int, q1: Int, r1: Int, s1: Int, t1: Int,
  u1: Int, v1: Int, w1: Int, x1: Int, y1: Int, z1: Int,
  a2: Int
)
// format: on

enum MaxEnum:
  case A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1,
    W1, X1, Y1
