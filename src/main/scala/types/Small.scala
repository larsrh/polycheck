package polycheck.types

import org.scalacheck._

object small {

  sealed trait One
  sealed trait Two

  case object A extends One with Two
  case object B extends Two

  object One {
    implicit def oneArbitrary: Arbitrary[One] = Arbitrary(Gen.oneOf(List(A)))
  }

  object Two {
    implicit def twoArbitrary: Arbitrary[Two] = Arbitrary(Gen.oneOf(List(A, B)))
  }

}
