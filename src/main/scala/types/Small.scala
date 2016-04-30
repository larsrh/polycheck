package polycheck.types

import org.scalacheck._

object small {

  sealed trait One { def perturb: Long }
  sealed trait Two { def perturb: Long }

  case object A extends One with Two { val perturb = 1L }
  case object B extends Two          { val perturb = 2L }

  object One {
    implicit def oneArbitrary: Arbitrary[One] = Arbitrary(Gen.oneOf(List(A)))
    implicit def oneCogen: Cogen[One] = Cogen(_.perturb)
  }

  object Two {
    implicit def twoArbitrary: Arbitrary[Two] = Arbitrary(Gen.oneOf(List(A, B)))
    implicit def twoCogen: Cogen[Two] = Cogen(_.perturb)
  }

}
