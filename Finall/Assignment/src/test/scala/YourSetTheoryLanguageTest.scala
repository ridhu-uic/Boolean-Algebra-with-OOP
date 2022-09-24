import main.*
import main.BooleanExpression.{Value, *}
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

  it should "abide by the De Morgan's law through inputs" in {
    assign(LogicGate("logicGate1"), NOT(OR(input_Value(LogicGate("logicGate1"),"A"), Value(false))))
    scope(LogicGate("logicGate1"),Input("A"),Value(true))
    scope(LogicGate("logicGate2"),Input("A"),Value(true))
    assign(LogicGate("logicGate2"), AND(NOT(input_Value(LogicGate("logicGate2"),"A")), NOT(Value(false))))
    assert(gate_Value(LogicGate("logicGate1")).eval == gate_Value(LogicGate("logicGate2")).eval)
  }

  it should "checking gate_Value(logicGate).eval" in {
    assign(LogicGate("logicGate1"), NOT(OR(input_Value(LogicGate("logicGate1"), "A"), Value(false))))
    scope(LogicGate("logicGate1"), Input("A"), Value(true))
    assert(!gate_Value(LogicGate("logicGate1")).eval)
  }


}