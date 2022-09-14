package scala
import scala.collection.mutable.Map

object Assignment:
  var logicalGateMap : Map[String,Boolean] = scala.collection.mutable.Map()

  var inputMap : Map[String,Boolean] = scala.collection.mutable.Map()

  enum BooleanExpression:
    case Value(v: Boolean)
    case Variable(x: String)
    case Input(x: String)
    case logicGate(x:String)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression)
    case AND(o1: BooleanExpression, o2: BooleanExpression)
    case NAND(o1: BooleanExpression, o2: BooleanExpression) extends BooleanExpression
    case NOR(o1: BooleanExpression, o2: BooleanExpression)
    case XOR(o1: BooleanExpression, o2: BooleanExpression)
    case XNOR(o1: BooleanExpression, o2: BooleanExpression)

    def eval: Boolean = this match
      case Value(x: Boolean) => x
      case Variable(x: String) => logicalGateMap.getOrElse(x.toString, throw new Exception(x)) // why it doesnt work without toString
      case Input(x: String) => inputMap.getOrElse(x.toString, throw new Exception(x))
      case logicGate(x: String) => {print(x.toString)
        logicalGateMap(x.toString)=false
        println(logicalGateMap)
        true}
      case NOT(o1) => !o1.eval
      case OR(o1, o2) => o1.eval | o2.eval
      case AND(o1, o2) => o1.eval & o2.eval
      case NAND(o1, o2) => !(o1.eval & o2.eval)
      case NOR(o1, o2) => !(o1.eval | o2.eval)
      case XOR(o1, o2) => o1.eval ^ o2.eval
      case XNOR(o1, o2) => !(o1.eval ^ o2.eval)

  def Assign(lg : scala.Assignment.BooleanExpression, valueExp : scala.Assignment.BooleanExpression) =
    val value = valueExp.eval
    print("logic gate expression ")
    if lg.toString.matches("logicGate.*") == true then {
      val Str1 = lg.toString
      //println(Str)
      //println(Str.length)
      val gate = Str1.substring(10,Str1.length-1)
      //print("gate = ")
      //println(gate)
      logicalGateMap(gate)=value
      print("LogicGateMap is ")
      println(logicalGateMap)
    }
    else if lg.toString.matches("Input.*") == true then {
      val Str2 = lg.toString
      println(Str2)
      println(Str2.length)
      val gate = Str2.substring(6, Str2.length - 1)
      print("gate = ")
      println(gate)
      inputMap(gate) = value
      print("inputMap is ")
      println(inputMap)
    }
    else
      print("else loop")

  @main def runIT =
    import BooleanExpression.*
    Assign(Input("A"), Value(true))
    Assign(Input("B"), Value(false))
    println(Assign(logicGate("LogicGate1"), NOT(OR(Input("A"), Input("B")))))
