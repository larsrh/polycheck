package polycheck

import org.scalacheck._

import shapeless._

sealed trait TCList
sealed trait TCCons[TCH[_], TCT <: TCList] extends TCList
sealed trait TCNil extends TCList

object TCList {

  sealed trait Member[TC[_], TCs <: TCList] {
    def at[T](pack: Pack[TCs, T]): TC[T]
  }

  sealed trait LowPriorityMember {
    implicit def memberThere[TC[_], TCH[_], TCT <: TCList](implicit ev: Member[TC, TCT]): Member[TC, TCCons[TCH, TCT]] = new Member[TC, TCCons[TCH, TCT]] {
      def at[T](pack: Pack[TCCons[TCH, TCT], T]) = pack match {
        case PackCons(_, tct) => ev.at(tct)
      }
    }
  }

  object Member extends LowPriorityMember {
    implicit def memberHere[TC[_], TCs <: TCList]: Member[TC, TCCons[TC, TCs]] = new Member[TC, TCCons[TC, TCs]] {
      def at[T](pack: Pack[TCCons[TC, TCs], T]) = pack match {
        case PackCons(tch, _) => tch
      }
    }
  }

  sealed trait Pack[TCs <: TCList, T]
  case class PackCons[TCH[_], T, TCT <: TCList](tch: TCH[T], tct: Pack[TCT, T]) extends Pack[TCCons[TCH, TCT], T]
  case class PackNil[T]() extends Pack[TCNil, T]

  object Pack {
    implicit def packNil[T]: Pack[TCNil, T] = PackNil[T]()
    implicit def packCons[TCH[_], T, TCT <: TCList](implicit tch: TCH[T], tct: Pack[TCT, T]): Pack[TCCons[TCH, TCT], T] = PackCons(tch, tct)
  }

}

sealed abstract class Type[S, TCs <: TCList] {
  type T
  val instances: TCList.Pack[TCs, T]
  val name: String

  implicit def instance[TC[_]](implicit ev: TCList.Member[TC, TCs]): TC[T] = ev.at(instances)

  override def toString: String =
    s"Type[$name]"

  def forAll[P](f: T => P)(implicit prop: P => Prop, arb: TCList.Member[Arbitrary, TCs]): Prop =
    Prop.forAllNoShrink(instance(arb).arbitrary)(f)

  def forAll[P](f: (T, T) => P)(implicit prop: P => Prop, arb: TCList.Member[Arbitrary, TCs]): Prop =
    forAll { t1 => forAll { t2 => f(t1, t2) } }

  def forAll[P](f: (T, T, T) => P)(implicit prop: P => Prop, arb: TCList.Member[Arbitrary, TCs]): Prop =
    forAll { t1 => forAll { t2 => forAll { t3 => f(t1, t2, t3) } } }

  def forAllShrink[P](f: T => P)(implicit prop: P => Prop, arb: TCList.Member[Arbitrary, TCs], shrink: TCList.Member[Shrink, TCs]): Prop = {
    implicit val s = instance(shrink)
    Prop.forAll(instance(arb).arbitrary)(f)
  }

  def forAllShrink[P](f: (T, T) => P)(implicit prop: P => Prop, arb: TCList.Member[Arbitrary, TCs], shrink: TCList.Member[Shrink, TCs]): Prop =
    forAllShrink { t1 => forAllShrink { t2 => f(t1, t2) } }

  def forAllShrink[P](f: (T, T, T) => P)(implicit prop: P => Prop, arb: TCList.Member[Arbitrary, TCs], shrink: TCList.Member[Shrink, TCs]): Prop =
    forAllShrink { t1 => forAllShrink { t2 => forAllShrink { t3 => f(t1, t2, t3) } } }

}

object Type {

  def make[T0, S, TCs <: TCList](name0: String)(implicit ev: TCList.Pack[TCs, T0]): Type[S, TCs] = new Type[S, TCs] {
    type T = T0
    val instances = ev
    val name = name0
  }

  implicit def typeArbitrary[S, TCs <: TCList, Ts <: HList](implicit s: Shape.Aux[S, Ts], ev: Shape.Types[Ts, TCs]): Arbitrary[Type[S, TCs]] =
    Arbitrary(Gen.oneOf(s.types(ev)))

}
