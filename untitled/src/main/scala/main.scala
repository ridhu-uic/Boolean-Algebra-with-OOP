import BooleanOperations.{BooleanExpression, LogicGateMap, accessSpecifier, classMap, classTypeMap, fieldAccessMap, fieldMap, inputGateMap, methodAccessMap, methodMap, objectFieldMap, objectMethodMap}
import BooleanOperations.BooleanExpression.{AND, Method, Value, XOR, *}
import BooleanOperations.accessSpecifier.*

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
object main:

  @main def runIT() : Unit =


    /*
    ClassDef("example 2", false,
      List(set_Field("A1", Value(true))),
      List(Field("A1", protected_access, Value(false)),
        Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(false)), NOT(get_Field("B")), NOT(getParameter("A")), NOT(setParameter("A", Value(true))))),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(AND(NOT(Value(true)), Value(true)), AND(get_Field("B"), getParameter("A")))),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true))), XOR(NOT(Value(true)), Value(true)), AND(AND(Value(true), NOT(NAND(Value(false), OR(Value(true), Value(false))))), NOT(Value(false))), get_Field("B1")))),
      "abstract class 1").classOperation
    */
    println(main)

    interface("hello", List(Field("A1", protected_access, Value(false)),
      Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),List("None")
    ).classOperation

    interface("hello 2", List(Field("A1", protected_access, Value(false)),
      Field("B1", private_access, Value(true))),
      List(Method("hello21", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello212", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello213", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello214", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), List("hello")
    ).classOperation

    ClassDef("example", false, List(set_Field("A", Value(true))),
      List(Field("A", private_access, Value(false)),
        Field("B", public_access, Value(true))),
      List(Method("One", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      "None",implements = List("None")).classOperation
    ClassDef("example 1", false,
      List(set_Field("A1", Value(true))),
      List(Field("A1", protected_access, Value(false)),
        Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(false)), NOT(get_Field("B")), NOT(getParameter("A")), NOT(setParameter("A", Value(true))))),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(AND(NOT(Value(true)), Value(true)), AND(get_Field("B"), getParameter("A")))),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(AND(NOT(Value(true)), Value(true)))),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true))), XOR(NOT(Value(true)), Value(true)), AND(AND(Value(true), NOT(NAND(Value(false), OR(Value(true), Value(false))))), NOT(Value(false))), get_Field("B1"))),
        Method("hello21", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello212", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(Value(true),Value(false)))),
        Method("hello213", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(Value(true),Value(false)))),
        Method("hello214", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(Value(true),Value(false))))
      ),
      "example",implements=List("hello 2")).classOperation

    ClassDef("abstract class 1", true,
      List(set_Field("A1", Value(true))),
      List(Field("A1", protected_access, Value(false)),
        Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
      "example",implements = List("None")).classOperation

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