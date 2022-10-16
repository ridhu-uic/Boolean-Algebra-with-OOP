import BooleanOperations.{BooleanExpression, LogicGateMap, accessSpecifier, classMap, fieldAccessMap, fieldMap, inputGateMap, methodAccessMap, methodMap, objectFieldMap, objectMethodMap}
import BooleanOperations.BooleanExpression.*
import BooleanOperations.accessSpecifier.*

import scala.collection.mutable.ListBuffer
import BooleanOperations.accessSpecifier.{private_access, public_access}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class YourSetTheoryLanguageTest extends AnyFlatSpec with Matchers {
  behavior of "my first language for set theory operations"

  it should "create a gate to test the De Morgan's law" in {
    NOT(OR(Value(true), Value(false))).eval shouldBe AND(NOT(Value(true)), NOT(Value(false))).eval
  }

  it should "create a new Class with the fields and methods" in {
    ClassDef("class A", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 1", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("Object 1", "B")))), "None").classOperation
  }


  it should "create two classes in which one extends another" in {
    ClassDef("class A", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 1", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("Object 1", "B")))), "None").classOperation
    ClassDef("class B", List(set_Field("C", Value(true))), List(Field("C", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("Ram", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 1", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("Raghu")))), "None").classOperation
  }


  it should "create an object of the base class" in {
    ClassDef("class A", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 1", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("Object 1", "B")))), "None").classOperation
    ClassDef("class B", List(set_Field("C", Value(true))), List(Field("C", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("Ram", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 2", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("Raghu")))), "class A").classOperation
    NewObject("Object 1", "class A")
  }

  it should "create an object of the derived class" in {
    ClassDef("class A", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 1", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("Object 1", "B")))), "None").classOperation
    ClassDef("class B", List(set_Field("C", Value(true))), List(Field("C", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("Ram", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 2", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("Raghu")))), "class A").classOperation
    NewObject("Object 2", "class B")
  }

  it should "Call the method of the child class" in {
    ClassDef("class A", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 1", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("Object 1", "B")))), "None").classOperation
    ClassDef("class B", List(set_Field("C", Value(true))), List(Field("C", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("Ram", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("Object 2", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("Raghu")))), "class A").classOperation
    NewObject("Object 2", "class B").classOperation
    Object("Object 2", invokeMethod("Ram")).classOperation
  }

  it should "Call the method of the Base class" in {
    ClassDef("example", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("new Object", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("new Object", "B")))), "None").classOperation
    ClassDef("example 1", List(set_Field("A1", Value(true))), List(Field("A1", protected_access, Value(false)), Field("B1", private_access, Value(true))), List(Method("two1", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("new Object", "B")))), Method("two2", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three3", public_access, List(invokeMethod("One"), get_Field_Object("new Object", "B")))), "example").classOperation
    NewObject("new Object", "example 1").classOperation
    Object("new Object", invokeMethod("One")).classOperation

  }

  it should "throw an exception for access private method" in {
    ClassDef("example", List(set_Field("A", Value(true))), List(Field("A", private_access, Value(false)), Field("B", public_access, Value(true))), List(Method("One", private_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("new Object", "B")))), Method("two", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three", public_access, List(invokeMethod("One"), get_Field_Object("new Object", "B")))), "None").classOperation
    ClassDef("example 1", List(set_Field("A1", Value(true))), List(Field("A1", protected_access, Value(false)), Field("B1", private_access, Value(true))), List(Method("two1", public_access, List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field_Object("new Object", "B")))), Method("two2", public_access, List(XOR(NOT(Value(true)), Value(true)))), Method("three3", public_access, List(invokeMethod("One"), get_Field_Object("new Object", "B")))), "example").classOperation
    NewObject("new Object", "example 1").classOperation
    assertThrows[Exception]( Object("new Object", invokeMethod("One")).classOperation)

  }


}