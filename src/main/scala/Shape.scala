package polycheck

import scala.reflect.runtime.universe._

import org.scalacheck._

import shapeless._

trait Shape[S] {
  type Instantiations <: HList

  def types[TCs <: TCList](implicit ev: Shape.Types[Instantiations, TCs]): List[Type[S, TCs]] = ev.types

  def exhaust[TCs <: TCList](name: String)(f: Type[S, TCs] => Prop)(implicit ev: Shape.Types[Instantiations, TCs]): Properties =
    new Properties(name) {
      for (t <- types)
        property(s"[${t.name}]") = f(t)
    }

  def exhaustAll[TCs <: TCList](name: String)(f: Type[S, TCs] => Properties)(implicit ev: Shape.Types[Instantiations, TCs]): Properties =
    new Properties(name) {
      for (t <- types)
        include(f(t), prefix =s"[${t.name}].")
    }
}

object Shape {
  trait ShapeCompanion[S, Ts <: HList] {
    implicit def sShape: Aux[S, Ts] = new Shape[S] {
      type Instantiations = Ts
    }
  }

  type Aux[S, Ts <: HList] = Shape[S] { type Instantiations = Ts }

  def apply[S](implicit ev: Shape[S]): ev.type = ev

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

object shapes {

  sealed trait Small
  object Small extends Shape.ShapeCompanion[Small, types.small.One :: types.small.Two :: HNil]

  sealed trait Default
  object Default extends Shape.ShapeCompanion[Default, Int :: String :: Float :: HNil]

}
