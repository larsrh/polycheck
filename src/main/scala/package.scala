import org.scalacheck._

package object polycheck {

  type :&:[TC[_], TCs <: TCList] = TCCons[TC, TCs]

  type DefaultType[TCs <: TCList] = Type[shapes.Default, TCs]
  type SmallType[TCs <: TCList] = Type[shapes.Small, TCs]
  type SimpleType = DefaultType[Arbitrary :&: TCNil]

}
