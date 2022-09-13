package scala

import scala.BoolExp.BooleanExpression.{NOT, Value}


/*
//The operator Assign takes the specification of the logic gate and assigns it to a variable named logicGate1.
Assign(LogicGate("logicGate1"), NOT(OR(Input("A"), Input("B"))))
//test the created logic gate by specifying inputs for this logic gate
//in this case the scope of the input variables is defined by the logic gate module/variable
Scope(LogicGate("logicGate1"), Assign(Input("A"), true))
Scope(LogicGate("logicGate1"), Assign(Input("B"), false))

*/



 object BoolExp:


   var logicGate1 : Boolean = false //(The variable is declared because only classes can have undeclared variable error)

   def Assign(gate : String, expression: BooleanExpression) =
     if gate=="logicGate1" then logicGate1=expression.eval
     else print("The value of gate is not valid")

   enum BooleanExpression:
    case Value(v:Boolean)
    case Input(x : String)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression)
    case AND(o1: BooleanExpression, o2: BooleanExpression)
    case NAND(o1: BooleanExpression, o2: BooleanExpression) extends BooleanExpression
    case NOR(o1: BooleanExpression, o2: BooleanExpression)
    case XOR(o1: BooleanExpression, o2: BooleanExpression)
    case XNOR(o1: BooleanExpression, o2: BooleanExpression)

    val EnvironmentTable: Map[String, Boolean] = Map("A" -> true, "B"->false)

    def eval: Boolean = this match
      case Value(x:Boolean) => x
      case Input(x : String) => EnvironmentTable.getOrElse(x.toString,throw new Exception(x))   // why it doesnt work without toString
      case NOT(o1) => !o1.eval
      case OR(o1, o2) => o1.eval | o2.eval
      case AND(o1, o2) => o1.eval & o2.eval
      case NAND(o1, o2) => !(o1.eval & o2.eval)
      case NOR(o1, o2) => !(o1.eval | o2.eval)
      case XOR(o1, o2) => o1.eval ^ o2.eval
      case XNOR(o1,o2) => !(o1.eval ^ o2.eval)





   @main def runBoolExp =
    import BooleanExpression.*
    //    Add(Add(Add(...)))
    print("The value is : ")
    //println( eval(XOR(Input("A"), NOT(Value(false)))))
    println(NOT(Value(false)).eval)


    print("The value of logicGate1 is ")
    println(logicGate1)
    Assign("logicGate1",XOR(Input("A"), NOT(Value(false))))
    print("The value of logicGate1 after assign is ")
    println(logicGate1)