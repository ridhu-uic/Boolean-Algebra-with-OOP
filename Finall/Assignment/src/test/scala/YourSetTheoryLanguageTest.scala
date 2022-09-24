import main.*
import main.BooleanExpression.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YourSetTheoryLanguageTest extends AnyFlatSpec with Matchers {
  behavior of "my first language for set theory operations"

  it should "create a gate to test the De Morgan's law" in {
    NOT(OR(Value(true), Value(false))).eval shouldBe AND(NOT(Value(true)), NOT(Value(false))).eval
  }

  it should "Assign XOR gate expression to logicGate2 " in {
    assign(LogicGate("logicGate2"), XOR(Value(true),Value(true)))
    TestGate(LogicGate("logicGate2"), false)
  }

  it should "abide by the De Morgan's law" in {
    assign(LogicGate("logicGate1"), NOT(OR(Value(true), Value(false))))
    assign(LogicGate("logicGate2"), AND(NOT(Value(true)), NOT(Value(false))))
    assert(gate_Value(LogicGate("logicGate1")).eval == gate_Value(LogicGate("logicGate2")).eval)
  }

  it should "throw an error for an Undeclared LogicGate being tested" in {
    assertThrows[Exception](TestGate(LogicGate("logicGate6"),true))
  }

  it should "Should evaluate LogicGate 2" in {
    assign(LogicGate("logicGate2"), AND(input_Value(LogicGate("logicGate2"),"A"), input_Value(LogicGate("logicGate2"),"D")))
    scope(LogicGate("logicGate2"),Input("A"),Value(true))
    scope(LogicGate("logicGate2"),Input("D"),Value(true))
    assert(gate_Value(LogicGate("logicGate2")).eval==false)
  }

}