import scala.collection.mutable.Map
object Main:

  // Implementing Logic Gate as a class as it is real world object which holds a value
  // Implementing through trait because later the same trait properties can be used to make a different class
  trait state:
    var value : Boolean = false   // holds the stateValue of the gate
  class LogicGate(val name: String) extends state: // The class LogicGate inherits from state and has a name on its own
    var expression : BooleanExpression = BooleanExpression.Value(false) // holds the expression used to evaluate

  var inputMap : Map[String,Map[String,Boolean]]=scala.collection.mutable.Map()

  // I have used enumerations and functions to explore functional programming features of scala along with
  // Object Oriented Programming
  enum BooleanExpression:
    //Declaring the Boolean Functions
    case Value(v: Boolean)
    case input(l : LogicGate,key : String)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression)
    case AND(o1: BooleanExpression, o2: BooleanExpression)
    case NAND(o1: BooleanExpression, o2: BooleanExpression)
    case NOR(o1: BooleanExpression, o2: BooleanExpression)
    case XOR(o1: BooleanExpression, o2: BooleanExpression)
    case XNOR(o1: BooleanExpression, o2: BooleanExpression)

    //Using eval to evaluate the Boolean Functions
    def eval: Boolean = this match
      case Value(x: Boolean) => x
      case input(l : LogicGate,key : String) => {
        val temp_map : Map[String,Boolean]=inputMap.getOrElse(l.name, throw new Exception(l.name))
        temp_map(key)
      }
      case NOT(o1) => !o1.eval
      case OR(o1, o2) => o1.eval | o2.eval
      case AND(o1, o2) => o1.eval & o2.eval
      case NAND(o1, o2) => !(o1.eval & o2.eval)
      case NOR(o1, o2) => !(o1.eval | o2.eval)
      case XOR(o1, o2) => o1.eval ^ o2.eval
      case XNOR(o1, o2) => !(o1.eval ^ o2.eval)


  //Using assign function to assign the input expression to the variable inside LogicGate object
  def assign(gate: LogicGate, expression: BooleanExpression): Unit =
    gate.expression = expression


  //Using scope to define input variables of the given gate
  def scope(gate : LogicGate,str: String, value: Boolean): Unit =
    val temp_map : Map[String,Boolean]=Map()
    temp_map.update(str,value)
    //inputMap.update(gate.name,temp_map)
    if inputMap.isDefinedAt(gate.name) then inputMap(gate.name)(str)=value
    else inputMap.update(gate.name,temp_map)


  //Using testGate to evaluate the expression and compare the value with the expected value
  def testGate(gate : LogicGate, expected : Boolean) : Boolean =
    gate.value=gate.expression.eval
    if gate.value==expected then true
    else false

  @main def main(): Unit = {
    //println(NOT(Value(true)).eval)
    import BooleanExpression.*
    val logicGate1 : LogicGate = new LogicGate("logicGate1")
    assign(logicGate1,NOT(XOR(input(logicGate1,"A"),Value(true))))
    println(logicGate1.expression)
    scope(logicGate1,"A",true)
    scope(logicGate1,"B",false)
    println("inputMap")
    println(inputMap)
    println("TestGate")
    println(testGate(logicGate1,false))
  }