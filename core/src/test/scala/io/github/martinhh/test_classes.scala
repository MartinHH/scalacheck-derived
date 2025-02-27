package io.github.martinhh

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen.cogenList
import org.scalacheck.Cogen.perturb
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.rng.Seed

// helper for defining expected Cogens
private def perturbSingletonInSum[T](ordinal: Int, seed: Seed, value: T) =
  perturb(
    perturb[T](
      seed,
      value
    )(Cogen.cogenUnit.contramap(_ => ())),
    ordinal
  )

sealed trait SimpleADT

object SimpleADT:
  val expectedGen: Gen[SimpleADT] =
    Gen.oneOf(Gen.const(SimpleCaseObject), SimpleCaseClass.expectedGen)

  val expectedCogen: Cogen[SimpleADT] =
    Cogen { (seed, value) =>
      value match
        case SimpleCaseObject =>
          perturbSingletonInSum(0, seed, SimpleCaseObject)
        case cc: SimpleCaseClass =>
          perturb(
            perturb[SimpleCaseClass](
              seed,
              cc
            )(SimpleCaseClass.expectedCogen),
            1
          )
    }

  @annotation.nowarn("cat=deprecation")
  val expectedShrink: Shrink[SimpleADT] =
    Shrink {
      case SimpleCaseObject =>
        Stream.empty
      case scc: SimpleCaseClass =>
        SimpleCaseClass.expectedShrink.shrink(scc)
    }

case object SimpleCaseObject extends SimpleADT

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

  @annotation.nowarn("cat=deprecation")
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

    val expectedShrink: Shrink[SubclassA] =
      given Shrink[SimpleCaseClass] = SimpleCaseClass.expectedShrink
      given shrinkTuple: Shrink[(Int, String, SimpleCaseClass)] = Shrink.shrinkTuple3
      Shrink.xmap[(Int, String, SimpleCaseClass), SubclassA](
        SubclassA.apply.tupled(_),
        a => (a.a, a.b, a.nestedSimple)
      )

  case class SubclassB(nestedSimple: SimpleADT) extends AbstractSubClass[SimpleADT](ABC.B)

  object SubclassB:
    val expectedGen: Gen[SubclassB] =
      SimpleADT.expectedGen.map(SubclassB.apply)

    val expectedShrink: Shrink[SubclassB] =
      given Shrink[SimpleADT] = SimpleADT.expectedShrink
      Shrink.xmap[SimpleADT, SubclassB](
        SubclassB(_),
        _.nestedSimple
      )

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

    val expectedShrink: Shrink[SubclassC] =
      given Shrink[ABC] = ABC.expectedShrink
      given shrinkTuple: Shrink[(String, Double, ABC)] = Shrink.shrinkTuple3
      Shrink.xmap[(String, Double, ABC), SubclassC](
        SubclassC.apply.tupled(_),
        a => (a.c, a.d, a.anotherLetter)
      )

object ComplexADTWithNestedMembers:
  val expectedGen: Gen[ComplexADTWithNestedMembers] =
    Gen.oneOf(
      Gen.const(AnotherCaseObject),
      AbstractSubClass.SubclassA.expectedGen,
      AbstractSubClass.SubclassB.expectedGen,
      AbstractSubClass.SubclassC.expectedGen
    )

  val expectedShrink: Shrink[ComplexADTWithNestedMembers] =
    Shrink {
      case AnotherCaseObject =>
        Stream.empty[AnotherCaseObject.type]
      case a: AbstractSubClass.SubclassA =>
        AbstractSubClass.SubclassA.expectedShrink.shrink(a)
      case b: AbstractSubClass.SubclassB =>
        AbstractSubClass.SubclassB.expectedShrink.shrink(b)
      case c: AbstractSubClass.SubclassC =>
        AbstractSubClass.SubclassC.expectedShrink.shrink(c)
    }

case class HasGivenInstances(x: Int)

object HasGivenInstances:

  // these are a nonsense-implementations, only here to prove that they takes precedence
  // over/within derivation:

  given specialHasGivenInstancesArbitrary: Arbitrary[HasGivenInstances] =
    Arbitrary(Gen.const(HasGivenInstances(42)))

  given specialHasGivenInstancesCogen: Cogen[HasGivenInstances] =
    Cogen(_.x + 1)

  @annotation.nowarn("cat=deprecation")
  given specialHasGivenInstancesShrink: Shrink[HasGivenInstances] = Shrink { hgi =>
    (1 to 3).map(i => HasGivenInstances(hgi.x + i)).toStream
  }

case class HasMemberThatHasGivenInstances(member: HasGivenInstances)

object HasMemberThatHasGivenInstances:

  val expectedGen: Gen[HasMemberThatHasGivenInstances] =
    HasGivenInstances.specialHasGivenInstancesArbitrary.arbitrary.map(
      HasMemberThatHasGivenInstances.apply
    )

  val expectedCogen: Cogen[HasMemberThatHasGivenInstances] =
    Cogen { (seed, value) =>
      perturb[Unit](
        perturb[HasGivenInstances](
          seed,
          value.member
        )(using HasGivenInstances.specialHasGivenInstancesCogen),
        ()
      )
    }

  val expectedShrink: Shrink[HasMemberThatHasGivenInstances] =
    Shrink.xmap(HasMemberThatHasGivenInstances.apply, _.member)

// has member that has an existing given (for List[T]) that requires derivation of a dependency
case class CaseClassWithListOfCaseClass(list: List[SimpleCaseClass])

object CaseClassWithListOfCaseClass:
  val expectedGen: Gen[CaseClassWithListOfCaseClass] =
    Gen
      .containerOf[List, SimpleCaseClass](SimpleCaseClass.expectedGen)
      .map(CaseClassWithListOfCaseClass(_))

  val expectedCogen: Cogen[CaseClassWithListOfCaseClass] =
    Cogen { (seed, value) =>
      perturb[Unit](
        perturb[List[SimpleCaseClass]](
          seed,
          value.list
        )(cogenList(using SimpleCaseClass.expectedCogen)),
        ()
      )
    }

  val expectedShrink: Shrink[CaseClassWithListOfCaseClass] =
    given Shrink[SimpleCaseClass] = SimpleCaseClass.expectedShrink
    Shrink.xmap[List[SimpleCaseClass], CaseClassWithListOfCaseClass](
      CaseClassWithListOfCaseClass(_),
      _.list
    )(using Shrink.shrinkContainer)

// A simple list as most basic test for recursive structures
enum RecursiveList[+T]:
  case Cns(t: T, ts: RecursiveList[T])
  case Nl

object RecursiveList:
  def expectedGen[T](using arbT: Arbitrary[T]): Gen[RecursiveList[T]] =
    Gen.oneOf(
      for {
        t <- arbT.arbitrary
        ts <- expectedGen[T]
      } yield Cns(t, ts),
      Gen.const(Nl)
    )
  def expectedCogen[T](using cogenT: Cogen[T]): Cogen[RecursiveList[T]] =
    Cogen { (seed, value) =>
      value match
        case c: Cns[T] =>
          perturb(
            perturb[Cns[T]](
              seed,
              c
            )(Cogen { (seed, value) =>
              perturb[Unit](
                perturb[RecursiveList[T]](
                  perturb[T](
                    seed,
                    value.t
                  ),
                  value.ts
                )(expectedCogen),
                ()
              )
            }),
            0
          )
        case Nl =>
          perturbSingletonInSum(1, seed, Nl)
    }

  @annotation.nowarn("cat=deprecation")
  def expectedShrink[T](using shrinkT: Shrink[T]): Shrink[RecursiveList[T]] =
    Shrink {
      case c: Cns[T] =>
        Shrink
          .xmap[(T, RecursiveList[T]), Cns[T]](
            { case (t, ts) => Cns(t, ts) },
            cns => (cns.t, cns.ts)
          )(Shrink.shrinkTuple2(using shrinkT, expectedShrink))
          .shrink(c)
      case Nl => Stream.empty
    }

// A list with an intermediate "nested" sealed trait (to test that nested sealed traits do not
// break support for recursion in sum types)
sealed trait NestedSumsRecursiveList[+T]
object NestedSumsRecursiveList:

  sealed trait NotNl[+T] extends NestedSumsRecursiveList[T]
  case class Cns[+T](t: T, ts: NestedSumsRecursiveList[T]) extends NotNl[T]
  case object Nl extends NestedSumsRecursiveList[Nothing]

  def expectedGen[T](using arbT: Arbitrary[T]): Gen[NestedSumsRecursiveList[T]] =
    Gen.oneOf(
      for {
        t <- arbT.arbitrary
        ts <- expectedGen[T]
      } yield Cns(t, ts),
      Gen.const(Nl)
    )

  def expectedCogen[T](using cogenT: Cogen[T]): Cogen[NestedSumsRecursiveList[T]] =
    Cogen { (seed, value) =>
      value match
        case c: Cns[T] =>
          perturb(
            perturb(
              perturb[Cns[T]](
                seed,
                c
              )(Cogen { (seed, value) =>
                perturb[Unit](
                  perturb[NestedSumsRecursiveList[T]](
                    perturb[T](
                      seed,
                      value.t
                    ),
                    value.ts
                  )(expectedCogen),
                  ()
                )
              }),
              0 // ordinal of Cns within NotNl
            ),
            0 // ordinal of NotNl within NestedSumsRecursiveList
          )
        case Nl =>
          perturbSingletonInSum(1, seed, Nl)
    }

  @annotation.nowarn("cat=deprecation")
  def expectedShrink[T](using shrinkT: Shrink[T]): Shrink[NestedSumsRecursiveList[T]] =
    Shrink {
      case c: Cns[T] =>
        Shrink
          .xmap[(T, NestedSumsRecursiveList[T]), Cns[T]](
            { case (t, ts) => Cns(t, ts) },
            cns => (cns.t, cns.ts)
          )(Shrink.shrinkTuple2(using shrinkT, expectedShrink))
          .shrink(c)
      case Nl => Stream.empty
    }

// an option-like type
enum Maybe[+T]:
  case Defined(t: T)
  case Undefined

object Maybe:
  def expectedGen[T](using arbT: Arbitrary[T]): Gen[Maybe[T]] =
    Gen.oneOf(
      arbT.arbitrary.map(Defined.apply),
      Gen.const(Undefined)
    )

  def expectedCogen[T](using cogenT: Cogen[T]): Cogen[Maybe[T]] =
    Cogen { (seed, value) =>
      value match
        case d: Defined[T] =>
          perturb(
            perturb[Defined[T]](
              seed,
              d
            )(Cogen { (seed, value) =>
              perturb[Unit](
                perturb[T](
                  seed,
                  value.t
                ),
                ()
              )
            }),
            0
          )
        case Undefined =>
          perturbSingletonInSum(1, seed, Undefined)
    }

  @annotation.nowarn("cat=deprecation")
  def expectedShrink[T](shrinkT: => Shrink[T]): Shrink[Maybe[T]] =
    Shrink {
      case d: Defined[T] =>
        Shrink
          .xmap[T, Defined[T]](
            Defined.apply,
            _.t
          )(shrinkT)
          .shrink(d)
      case Undefined => Stream.empty
    }

// another option-like type (for the sake of having a more complex example)
sealed trait MaybeMaybe[+T]

object MaybeMaybe:
  case class IsMaybe[+T](maybe: Maybe[T]) extends MaybeMaybe[T]
  case object IsNotMaybe extends MaybeMaybe[Nothing]

  def expectedGen[T](using arbT: Arbitrary[T]): Gen[MaybeMaybe[T]] =
    Gen.oneOf(
      Maybe.expectedGen[T].map(IsMaybe.apply),
      Gen.const(IsNotMaybe)
    )

  def expectedCogen[T](using cogenT: Cogen[T]): Cogen[MaybeMaybe[T]] =
    Cogen { (seed, value) =>
      value match
        case d: IsMaybe[T] =>
          perturb(
            perturb[IsMaybe[T]](
              seed,
              d
            )(Cogen { (seed, value) =>
              perturb[Unit](
                perturb[Maybe[T]](
                  seed,
                  value.maybe
                )(Maybe.expectedCogen[T]),
                ()
              )
            }),
            0
          )
        case IsNotMaybe =>
          perturbSingletonInSum(1, seed, IsNotMaybe)
    }

  @annotation.nowarn("cat=deprecation")
  def expectedShrink[T](shrinkT: => Shrink[T]): Shrink[MaybeMaybe[T]] =
    Shrink {
      case d: IsMaybe[T] =>
        Shrink
          .xmap[Maybe[T], IsMaybe[T]](
            IsMaybe.apply,
            _.maybe
          )(Maybe.expectedShrink(shrinkT))
          .shrink(d)
      case IsNotMaybe => Stream.empty
    }

// an example of a recursive structure where the actual recursion happens multiple somewhere
// deeper down the hierarchy of nested sums and products
case class MaybeMaybeList[+T](head: T, tail: MaybeMaybe[MaybeMaybeList[T]])

object MaybeMaybeList:
  def expectedGen[T](using arbT: Arbitrary[T]): Gen[MaybeMaybeList[T]] =
    given arbTail: Arbitrary[MaybeMaybeList[T]] = Arbitrary(expectedGen)
    for {
      head <- arbT.arbitrary
      tail <- MaybeMaybe.expectedGen[MaybeMaybeList[T]]
    } yield MaybeMaybeList(head, tail)

  def expectedCogen[T](using cogenT: Cogen[T]): Cogen[MaybeMaybeList[T]] =
    Cogen { (seed, value) =>
      perturb[Unit](
        perturb[MaybeMaybe[MaybeMaybeList[T]]](
          perturb[T](
            seed,
            value.head
          ),
          value.tail
        )(MaybeMaybe.expectedCogen[MaybeMaybeList[T]](using expectedCogen)),
        ()
      )
    }

  def expectedShrink[T](using shrinkT: Shrink[T]): Shrink[MaybeMaybeList[T]] =
    lazy val shrinkTuple: Shrink[(T, MaybeMaybe[MaybeMaybeList[T]])] =
      Shrink.shrinkTuple2(using shrinkT, MaybeMaybe.expectedShrink(expectedShrink))
    Shrink.xmap[(T, MaybeMaybe[MaybeMaybeList[T]]), MaybeMaybeList[T]](
      { case (head, tail) => MaybeMaybeList(head, tail) },
      mml => (mml.head, mml.tail)
    )(shrinkTuple)

enum DirectRecursion:
  case Continue(next: DirectRecursion)
  case Stop

object DirectRecursion:

  def expectedGen: Gen[DirectRecursion] =
    Gen.oneOf(Gen.lzy(expectedGen.map(Continue(_))), Gen.const(Stop))

  def expectedCogen: Cogen[DirectRecursion] =
    Cogen { (seed, value) =>
      value match
        case Continue(dr) =>
          perturb(
            perturb[Unit](
              perturb[DirectRecursion](
                seed,
                dr
              )(expectedCogen),
              ()
            ),
            0
          )
        case Stop =>
          perturbSingletonInSum(1, seed, Stop)
    }

  @annotation.nowarn("cat=deprecation")
  def expectedShrink: Shrink[DirectRecursion] =
    Shrink {
      case c: Continue =>
        Shrink
          .xmap[DirectRecursion, Continue](
            Continue.apply,
            _.next
          )(expectedShrink)
          .shrink(c)
      case Stop => Stream.empty
    }

sealed trait SealedDiamond

object SealedDiamond:
  sealed trait SubtraitA extends SealedDiamond
  sealed trait SubtraitB extends SealedDiamond
  case object Foo extends SealedDiamond
  case object Bar extends SubtraitA with SubtraitB

  def expectedGen: Gen[SealedDiamond] =
    Gen.oneOf(Gen.const(Bar), Gen.const(Foo))

  @annotation.nowarn("cat=deprecation")
  val expectedShrink: Shrink[SealedDiamond] =
    Shrink { _ =>
      Stream.empty[SealedDiamond]
    }

// format: off
case class MaxCaseClass(
  a1: Int, b1: Int, c1: Int, d1: Int, e1: Int, f1: Int, g1: Int, h1: Int, i1: Int, j1: Int,
  k1: Int, l1: Int, m1: Int, n1: Int, o1: Int, p1: Int, q1: Int, r1: Int, s1: Int, t1: Int,
  u1: Int, v1: Int, w1: Int, x1: Int, y1: Int, z1: Int
)
// format: on

enum MaxEnum:
  case A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1,
    W1, X1

// format: off
case class MaxShrinkableCaseClass(
  a1: Int, b1: Int, c1: Int, d1: Int, e1: Int, f1: Int, g1: Int, h1: Int, i1: Int, j1: Int,
  k1: Int, l1: Int, m1: Int, n1: Int, o1: Int, p1: Int, q1: Int, r1: Int, s1: Int, t1: Int,
  u1: Int, v1: Int, w1: Int, x1: Int, y1: Int
)
// format: on

enum MaxShrinkableEnum:
  case A1, B1, C1, D1, E1, F1, G1, H1, I1, J1, K1, L1, M1, N1, O1, P1, Q1, R1, S1, T1, U1, V1,
    W1
