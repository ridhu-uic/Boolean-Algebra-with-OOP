import scala.collection.mutable.Map
object Main:

  trait state:
    var value : Boolean = false
    var expression : BooleanExpression = BooleanExpression.Value(false)
    var input : Map[String,Boolean]=scala.collection.mutable.Map()
  class LogicGate(name: String) extends state

  enum BooleanExpression:

    case Value(v: Boolean)
    case input(o1 : String)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression)
    case AND(o1: BooleanExpression, o2: BooleanExpression)
    case NAND(o1: BooleanExpression, o2: BooleanExpression)
    case NOR(o1: BooleanExpression, o2: BooleanExpression)
    case XOR(o1: BooleanExpression, o2: BooleanExpression)
    case XNOR(o1: BooleanExpression, o2: BooleanExpression)


    def eval: Boolean = this match
      case Value(x: Boolean) => x

      case NOT(o1) => !o1.eval
      case OR(o1, o2) => o1.eval | o2.eval
      case AND(o1, o2) => o1.eval & o2.eval
      case NAND(o1, o2) => !(o1.eval & o2.eval)
      case NOR(o1, o2) => !(o1.eval | o2.eval)
      case XOR(o1, o2) => o1.eval ^ o2.eval
      case XNOR(o1, o2) => !(o1.eval ^ o2.eval)

  def assign(gate: LogicGate, expression: BooleanExpression): Boolean =
    gate.expression = expression
    var summa : Boolean = gate.expression.eval


  def scope(gate : LogicGate,str: String, value: Boolean): Unit =
    gate.input(str) = value

  def testGate(gate : LogicGate, expected : Boolean) : Boolean =
    true

  @main def main(): Unit = {
    //println(NOT(Value(true)).eval)
    import BooleanExpression.*
    var logicGate1 : LogicGate = new LogicGate("logicGate1")
    assign(logicGate1,NOT(XOR(Value(true),Value(true))))
    println(logicGate1.expression)
    scope(logicGate1,"A",true)
    scope(logicGate1,"B",false)
  }