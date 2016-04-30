import org.scalacheck._

package object polycheck {

  type :&:[TC[_], TCs <: TCList] = TCCons[TC, TCs]

  type DefaultType[TCs <: TCList] = Type[domains.Default, TCs]
  type SmallType[TCs <: TCList] = Type[domains.Small, TCs]
  type SimpleType = DefaultType[Arbitrary :&: TCNil]

}
