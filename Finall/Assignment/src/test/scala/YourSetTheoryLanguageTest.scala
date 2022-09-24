import main.*
import main.BooleanExpression.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YourSetTheoryLanguageTest extends AnyFlatSpec with Matchers {
  behavior of "my first language for set theory operations"

  it should "create a gate to test the De Morgan's law" in {
    NOT(OR(Value(true), Value(false))).eval shouldBe AND(NOT(Value(true)), NOT(Value(false))).eval
  }
}