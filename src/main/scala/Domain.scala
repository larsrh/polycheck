package polycheck

import scala.reflect.runtime.universe._

import org.scalacheck._

import shapeless._

trait Domain[S] {
  type Instantiations <: HList

  def types[TCs <: TCList](implicit ev: Domain.Types[Instantiations, TCs]): List[Type[S, TCs]] = ev.types

  def exhaust[TCs <: TCList](name: String)(f: Type[S, TCs] => Prop)(implicit ev: Domain.Types[Instantiations, TCs]): Properties =
    new Properties(name) {
      for (t <- types)
        property(s"[${t.name}]") = f(t)
    }

  def exhaustAll[TCs <: TCList](name: String)(f: Type[S, TCs] => Properties)(implicit ev: Domain.Types[Instantiations, TCs]): Properties =
    new Properties(name) {
      for (t <- types)
        include(f(t), prefix =s"[${t.name}].")
    }
}

object Domain {
  trait DomainCompanion[S, Ts <: HList] {
    implicit def sDomain: Aux[S, Ts] = new Domain[S] {
      type Instantiations = Ts
    }
  }

  type Aux[S, Ts <: HList] = Domain[S] { type Instantiations = Ts }

  def apply[S](implicit ev: Domain[S]): ev.type = ev

  sealed trait Types[Ts <: HList, TCs <: TCList] {
    def types[S]: List[Type[S, TCs]]
  }

  object Types {
    implicit def nilTypes[TCs <: TCList]: Types[HNil, TCs] = new Types[HNil, TCs] {
      def types[S] = Nil
    }
    implicit def consTypes[H, T <: HList, TCs <: TCList](implicit h: TCList.Pack[TCs, H], t: Types[T, TCs], tag: WeakTypeTag[H]): Types[H :: T, TCs] = new Types[H :: T, TCs] {
      def types[S] =
        Type.make[H, S, TCs](tag.tpe.toString)(h) :: t.types[S]
    }
  }
}

object domains {

  sealed trait Small
  object Small extends Domain.DomainCompanion[Small, types.small.One :: types.small.Two :: HNil]

  sealed trait Default
  object Default extends Domain.DomainCompanion[Default, Int :: String :: Float :: HNil]

}
