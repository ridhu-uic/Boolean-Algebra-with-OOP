import BooleanOperations.{BooleanExpression, LogicGateMap, accessSpecifier, classMap, classTypeMap, fieldAccessMap, fieldMap, inputGateMap, methodAccessMap, methodMap, objectFieldMap, objectMethodMap}
import BooleanOperations.BooleanExpression.{AND, Method, Value, XOR, *}
import BooleanOperations.accessSpecifier.*

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer



import scala.collection.mutable.ListBuffer
import BooleanOperations.accessSpecifier.{private_access, public_access}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.List

class YourSetTheoryLanguageTest extends AnyFlatSpec with Matchers {
  behavior of "my first language for set theory operations"

  it should "Creates an interface" in {
    assert(interface("hello", List(Field("A1", protected_access, Value(false)),
      Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), List("None")
    ).classOperation==Value(true))
  }

  it should "Creates two interfaces, hello 2 interface inherits hello interface " in {
    assert(interface("hello", List(Field("A1", protected_access, Value(false)),
      Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), List("None")
    ).classOperation==Value(true))

 assert(interface("hello 2", List(Field("A1", protected_access, Value(false)),
   Field("B1", private_access, Value(true))),
   List(Method("hello21", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
     Method("hello212", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
     Method("hello213", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
     Method("hello214", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), List("hello")
 ).classOperation==Value(true))
  }


  it should "Class Defined without being abstract and implents no interface" in {
  assert(ClassDef("example", false, List(set_Field("A", Value(true))),
    List(Field("A", private_access, Value(false)),
      Field("B", public_access, Value(true))),
    List(Method("One", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
      Method("two", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
      Method("three", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
    "None", implements = List("None")).classOperation==Value(true))
  }


  it should "Class created extends another class and implents an interface" in {

    assert(interface("hello", List(Field("A1", protected_access, Value(false)),
      Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), List("None")
    ).classOperation==Value(true))

    assert(interface("hello 2", List(Field("A1", protected_access, Value(false)),
      Field("B1", private_access, Value(true))),
      List(Method("hello21", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello212", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello213", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("hello214", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), List("hello")
    ).classOperation==Value(true))

    assert(ClassDef("example", false, List(set_Field("A", Value(true))),
      List(Field("A", private_access, Value(false)),
        Field("B", public_access, Value(true))),
      List(Method("One", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      "None", implements = List("None")).classOperation==Value(true))
    assert(
      ClassDef("example 1", false,
        List(set_Field("A1", Value(true))),
        List(Field("A1", protected_access, Value(false)),
          Field("B1", private_access, Value(true))),
        List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(false)), NOT(get_Field("B")), NOT(getParameter("A")), NOT(setParameter("A", Value(true))))),
          Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(AND(NOT(Value(true)), Value(true)), AND(get_Field("B"), getParameter("A")))),
          Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(AND(NOT(Value(true)), Value(true)))),
          Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true))), XOR(NOT(Value(true)), Value(true)), AND(AND(Value(true), NOT(NAND(Value(false), OR(Value(true), Value(false))))), NOT(Value(false))), get_Field("B1"))),
          Method("hello21", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
          Method("hello212", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(Value(true), Value(false)))),
          Method("hello213", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(Value(true), Value(false)))),
          Method("hello214", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(Value(true), Value(false))))
        ),
        "example", implements = List("hello 2")).classOperation==Value(true))
  }

  it should "Creation of an abstract class which extends" in {
    assert(ClassDef("example", false, List(set_Field("A", Value(true))),
      List(Field("A", private_access, Value(false)),
        Field("B", public_access, Value(true))),
      List(Method("One", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      "None", implements = List("None")).classOperation==Value(true))
    assert(
      ClassDef("abstract class 1", true,
        List(set_Field("A1", Value(true))),
        List(Field("A1", protected_access, Value(false)),
          Field("B1", private_access, Value(true))),
        List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
          Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
          Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
          Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
        "example", implements = List("None")).classOperation==Value(true))
  }

  it should "Creation of a class which extends an abstract class" in {
    ClassDef("abstract class 1", true,
      List(set_Field("A1", Value(true))),
      List(Field("A1", protected_access, Value(false)),
        Field("B1", private_access, Value(true))),
      List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
      "example", implements = List("None")).classOperation
  assert(ClassDef("example 2", false,
  List(set_Field("A1", Value(true))),
  List(Field("A1", protected_access, Value(false)),
    Field("B1", private_access, Value(true))),
  List(Method("two1", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(false)), NOT(get_Field("B")), NOT(getParameter("A")), NOT(setParameter("A", Value(true))))),
    Method("two2", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(AND(NOT(Value(true)), Value(true)), AND(get_Field("B"), getParameter("A")))),
    Method("three3", public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true))), XOR(NOT(Value(true)), Value(true)), AND(AND(Value(true), NOT(NAND(Value(false), OR(Value(true), Value(false))))), NOT(Value(false))), get_Field("B1")))),
  "abstract class 1", implements = List("None")).classOperation==Value(true))
  }




}