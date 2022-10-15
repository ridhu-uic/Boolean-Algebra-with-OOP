import BooleanOperations.{BooleanExpression, LogicGateMap, classMap, inputGateMap, methodMap, fieldMap, accessSpecifier  }
import BooleanOperations.BooleanExpression.*
import BooleanOperations.accessSpecifier.*
import scala.collection.mutable.ListBuffer
object main:



  @main def runIT() : Unit =
    NewObject("A",ClassDef("example",List(Field("A",public_access,Value(false)),Field("B",public_access,Value(true))),List(Method("One",public_access,List(XOR(NOT(Value(false)),Value(true)),NOT(get_Field_Object("A","B")))),Method("two",public_access,List(XOR(NOT(Value(true)),Value(true)))),Method("three",public_access,List(invokeMethod("One"),get_Field("B")))),"None")).classOperation
    //println(classMap)
    println(methodMap)
    println(Object("A",get_Field("A")).classOperation)
    println("method invoking")
    println("The result of method One in object A is :" + Object("A", invokeMethod("One")).classOperation)
    println("The result of method One in object A is :" + Object("A",invokeMethod("three")).classOperation)
    /*
    //assign is used to assign the BooleanExpression to the logicGate1
    assign(LogicGate("logicGate1"),XOR(Value(true),input_Value(LogicGate("logicGate1"),"A"))).eval
    scope(LogicGate("logicGate1"),Input("A"),Value(true)).eval
    println(LogicGateMap)
    println(inputGateMap)
    gate_Value(LogicGate("logicGate1")).eval

    //The println statement is used to test the BooleanOperation
    println(OR(NOT(Value(true)),Value(true)))
    //scope is used to assign the value true to input A in logicGate1
    scope(LogicGate("logicGate1"),Input("A"),Value(true)).eval
    //The inputGateMap is printed for sample
    println(inputGateMap)
    //The logicGateMap is printed for sample
    println(LogicGateMap)
    //TestGate evaluated the expression in logicGate and compares it with the expected Value
    println(TestGate(LogicGate("logicGate1"),true).eval)


    //In next few statement the minimal axiom for Boolean Expession is calculated
    //The expression is made using 6 NAND Gates:
    // NAND(NAND(NAND(A,B),C),NAND(A,NAND(NAND(A,C),A)))=C
    assign(LogicGate("logicGate2"),NAND(NAND(input_Value(LogicGate("logicGate2"),"A"),input_Value(LogicGate("logicGate2"),"B")),input_Value(LogicGate("logicGate2"),"C"))).eval
    //assign(LogicGate("logicGate2"),NAND(Value(true),Value(false)))
    assign(LogicGate("logicGate3"),NAND(input_Value(LogicGate("logicGate2"),"A"),NAND(NAND(input_Value(LogicGate("logicGate2"),"A"),input_Value(LogicGate("logicGate2"),"C")),input_Value(LogicGate("logicGate2"),"A")))).eval
    assign(LogicGate("logicGate4"),NAND(gate_Value(LogicGate("logicGate2")),gate_Value(LogicGate("logicGate3")))).eval
    //Scope is used to assign values to the input
    scope(LogicGate("logicGate2"),Input("A"),Value(true)).eval
    scope(LogicGate("logicGate2"),Input("B"),Value(true)).eval
    scope(LogicGate("logicGate2"),Input("C"),Value(false)).eval
    println("LogicGate4")

    //The logicGate 4 has the expression.The output is C irrespective of A and B
    // False must be returned as the value of C is false but the expected mentioned in TestGate is true
    println(TestGate(LogicGate("logicGate4"),true).eval)
    println(gate_Value(LogicGate("logicGate4")).eval)
    */