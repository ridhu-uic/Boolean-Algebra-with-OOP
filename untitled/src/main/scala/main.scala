import BooleanOperations.{BooleanExpression, LogicGateMap, accessSpecifier, classMap, classTypeMap, fieldAccessMap, fieldMap, inputGateMap, methodAccessMap, methodMap, objectFieldMap, objectMethodMap}
import BooleanOperations.BooleanExpression.{AND, Method, Value, XOR, *}
import BooleanOperations.accessSpecifier.*

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
object main:

  @main def runIT() : Unit =


    /*

    */
    println(main)








    /*

    println()
    println()
    println(classMap)
    println()
    println("New Object Creation")
    NewObject("new Object","example 1").classOperation
    println()
    println()
    println("New Object Creation Done")
    println()
    println()
    println("Abstract Object Creation")
    println(classTypeMap)
    //NewObject("abstract object", "abstract class 1").classOperation
    println()
    println()
    println("New Object Creation Done")
    println(objectMethodMap)
    println()
    println()

    println()
    println("New Object Creation")
    NewObject("new Object", "example 2").classOperation
    println()
    println()
    println("New Object Creation Done")
    println()
    println()
    */
    /*
    Object("new Object", invokeMethod("two1", parameters = collection.mutable.Map("A" -> Value(false), "B" -> Value(true)))).classOperation

    println()
    println()
    println("two2 calling")
    Object("new Object", invokeMethod("two2", parameters = collection.mutable.Map("A" -> Value(false), "B" -> Value(true)))).classOperation

    println("three3 calling")
    Object("new Object", invokeMethod("three3", parameters = collection.mutable.Map("A" -> Value(false), "B" -> Value(true)))).classOperation

    println()
    println()
    println("One calling")
    Object("new Object", invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(false), "B" -> Value(true)))).classOperation


    */



    /*
    NewObject("A","example").classOperation

    println()
    println()
    println("Object Field Map")
    println(objectFieldMap)
    println()
    println()
    println("Object Method Map")
    println(objectMethodMap)
    //println(Object("A",get_Field("A")).classOperation)
    println("method invoking")
    println()
    println()
    Object("new Object",invokeMethod("One",parameters = collection.mutable.Map("A"->Value(false),"B"->Value(true)))).classOperation
    println()
    println()
    println("Method Access Map")
    println(methodAccessMap)
    println()
    println()
    println("Field Access Map")
    println(fieldAccessMap)

    println("The result of method One in object A is :" + Object("new Object", invokeMethod("One",parameters = collection.mutable.Map("A"->Value(true),"B"->Value(true)))).classOperation)
    println("The result of method One in object A is :" + Object("new Object",invokeMethod("three",parameters = collection.mutable.Map("A"->Value(true),"B"->Value(true)))).classOperation)
    */


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