package polycheck

import spire.algebra.{AdditiveSemigroup, Eq}
import spire.implicits._

import shapeless._

import org.scalacheck._

object Test extends Properties("Test") {

  sealed trait Numeric
  object Numeric extends Shape.ShapeCompanion[Numeric, Int :: Float :: Double :: HNil]

  type NumericClasses = Arbitrary :&: Shrink :&: Eq :&: AdditiveSemigroup :&: TCNil

  property("commutativity") =
    Prop.forAll { typ: Type[Numeric, NumericClasses] =>
      typ.forAllShrink { (t1, t2) =>
        implicit val SG = typ.instance[AdditiveSemigroup]
        implicit val EQ = typ.instance[Eq]

        t1 + t2 === t2 + t1
      }
    }

  include(Shape[Numeric].exhaust[NumericClasses]("commutativity_all") { typ =>
    typ.forAll { (t1, t2) =>
      implicit val SG = typ.instance[AdditiveSemigroup]
      implicit val EQ = typ.instance[Eq]

      t1 + t2 === t2 + t1
    }
  })

}
