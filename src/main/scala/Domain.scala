package polycheck

import scala.reflect.runtime.universe._

import org.scalacheck._

import shapeless._

trait Domain {
  type Instantiations <: HList

  def types[TCs <: TCList](implicit ev: Domain.Types[Instantiations, TCs]): List[Type[this.type, TCs]] = ev.types

  def exhaust[TCs <: TCList](name: String)(f: Type[this.type, TCs] => Prop)(implicit ev: Domain.Types[Instantiations, TCs]): Properties =
    new Properties(name) {
      for (t <- types)
        property(s"[${t.name}]") = f(t)
    }

  def exhaustAll[TCs <: TCList](name: String)(f: Type[this.type, TCs] => Properties)(implicit ev: Domain.Types[Instantiations, TCs]): Properties =
    new Properties(name) {
      for (t <- types)
        include(f(t), prefix =s"[${t.name}].")
    }
}

object Domain {

  trait Aux[Ts <: HList] extends Domain {
    type Instantiations = Ts
  }

  sealed trait Types[Ts <: HList, TCs <: TCList] {
    def types[S <: Domain]: List[Type[S, TCs]]
  }

  object Types {
    implicit def nilTypes[TCs <: TCList]: Types[HNil, TCs] = new Types[HNil, TCs] {
      def types[S <: Domain] = Nil
    }
    implicit def consTypes[H, T <: HList, TCs <: TCList](implicit h: TCList.Pack[TCs, H], t: Types[T, TCs], tag: WeakTypeTag[H]): Types[H :: T, TCs] = new Types[H :: T, TCs] {
      def types[S <: Domain] =
        Type.make[H, S, TCs](tag.tpe.toString)(h) :: t.types[S]
    }
  }

  def apply[S <: Domain]: Domain.Aux[S#Instantiations] = new Aux[S#Instantiations] {}

}

object domains {

  sealed trait Small extends Domain.Aux[types.small.One :: types.small.Two :: HNil]
  object Small extends Small

  sealed trait Default extends Domain.Aux[Int :: String :: Float :: HNil]
  object Default extends Default

}
