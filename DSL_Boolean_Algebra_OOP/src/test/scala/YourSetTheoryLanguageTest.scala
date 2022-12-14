import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import Sets_Classes.*
import Sets_Classes.BooleanExpression.*
import scala.collection.immutable.List

class YourSetTheoryLanguageTest extends AnyFlatSpec with Matchers {
  behavior of "my first language for set theory operations"
  
  it should "compute much more compound expressions" in {
    println(NAND(NOT(Variable("A")), OR(Value(true), AND(Value(false), Variable("B")))).map(optimizer))
    println(XOR(Variable("B"),Value(true)).map(optimizerXOR))
    assert(XOR(NOT(NOT(Variable("B"))),NAND(NOT(Variable("A")), OR(Value(true), AND(Value(false), Variable("B"))))).map(optimizer) ==  NOT(Variable("B")))
  }

  it should "compute compound expressions" in {
    assert(NAND(NOT(Variable("A")),OR(Value(true),AND(Value(false),Variable("B")))).map(optimizer)==Value(true))
  }

  it should "compute and assert the truth table of NAND GATE" in {
    assert(NAND(Variable("A"), Variable("B")).map(optimizerNAND) == NOT(Variable("B"))) //"A" -> true
    assert(XNOR(Value(false), Variable("B")).map(optimizerXNOR) == NOT(Variable("B")))
    assert(XNOR(Value(false), Value(false)).map(optimizerXNOR) == Value(true))
  }

  it should "compute and assert the truth table of XNOR GATE" in {
    assert(XNOR(Variable("A"), Variable("B")).map(optimizerXNOR) == Variable("B")) //"A" -> true
    assert(XNOR(Value(false), Variable("B")).map(optimizerXNOR) == NOT(Variable("B")))
    assert(XNOR(Value(false), Value(false)).map(optimizerXNOR) == Value(true))
  }

  it should "compute and assert the truth table of OR GATE" in {
    assert(OR(Variable("A"),Variable("B")).map(optimizerOR)==Value(true))  //"A" -> true
    assert(OR(Value(false),Variable("B")).map(optimizerOR)==Variable("B"))
    assert(OR(Value(false),Value(false)).map(optimizerOR)==Value(false))
  }

  it should "compute and assert the truth table of NOT GATE" in {
    assert(NOT(NOT(Variable("B"))).map(optimizerNOT)==Variable("B"))
    assert(NOT(Variable("A")).map(optimizerNOT) == Value(false)) //"A" -> true

  }

  it should "Using map for compound expressions" in {
    //NAND(NOT(Variable("A")), OR(Value(true), AND(Value(false), Variable("B"))))

    assert(AND(OR(Value(true), NOT(Variable("C"))), Value(false)).map(
      optimizer
    ) == Value(false))


    assert(AND(Value(true), Value(false)).map(
      optimizer
    ) == Value(false))
  }

  it should "Using map with own function" in {
    assert(AND(Value(true), Value(false)).map({
      (or: BooleanExpression) =>
        or match
          case AND(o1: BooleanExpression, o2: BooleanExpression) =>
            if o1 == Value(false) then
              Value(false)
            else if o1 == Value(true) then
              if o2 == Value(true) then
                Value(true)
              else
                o2
            else if o2 == Value(false) then
              Value(false)
            else if o2 == Value(true) then
              o1
            else
              AND(o1, o2)
          case _ => throw new Exception("incorrect expression")
    }) == Value(false))
  }



  it should "throw an exception because user did not define the exception" in  {
    assert(interface("Shape", List(Field("x", accessSpecifier.protected_access, Value(false)),
      Field("y", accessSpecifier.private_access, Value(true))),
      List(Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), null
    ).operate.eval)

    assert(interface("Car1", List(Field("Oil", accessSpecifier.protected_access, Value(false)),
      Field("Gear", accessSpecifier.private_access, Value(true))),
      List(Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
      ), List("Shape")
    ).operate.eval)

    assert(ClassDef("Car_Animation1", false, List(set_Field("Car", Value(true))),
      List(Field("Car", accessSpecifier.private_access, Value(false)),
        Field("Shape", accessSpecifier.public_access, Value(true))),
      List(Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(getParameter("B")))),
        Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), setParameter("B", Value(true))))),

      null, implements = List("Car1")).operate.eval)

    assert(NewObject("Mcqueen", "Car_Animation1").operate.eval)

    assertThrows[Exception](scope2("Testing", {
      CatchException(
        name = "No Oil",
        block = {
          PRINT(NOT(Value(false))).operate

          IF(Object("Mcqueen", invokeMethod("GetOil", collection.mutable.Map("A" -> Value(true), "B" -> Value(true)))).operate.eval)
          THEN(throwException("No Oil").operate)
          PRINT(NOT(Value(false))).operate
          PRINT(NOT(Value(false))).operate
          println(exceptionStack.top)
          PRINT(NOT(Value(false))).operate


        },

        catchBlock = Catch("No Oil", PRINT(Value(true)).operate, reason = Reason("Add petrol"))
      )
      Value(true)
    }))

  }

  it should "Testing scope2" in {
    assert(interface("ShaperC", List(Field("x", accessSpecifier.protected_access, Value(false)),
      Field("y", accessSpecifier.private_access, Value(true))),
      List(Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), null
    ).operate.eval)

    assert(interface("Carnival", List(Field("Oil", accessSpecifier.protected_access, Value(false)),
      Field("Gear", accessSpecifier.private_access, Value(true))),
      List(Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
      ), List("ShaperC")
    ).operate.eval)

    assert(ClassDef("Car_Animation1O", false, List(set_Field("Car", Value(true))),
      List(Field("Car", accessSpecifier.private_access, Value(false)),
        Field("Shape", accessSpecifier.public_access, Value(true))),
      List(Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(getParameter("B")))),
        Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), setParameter("B", Value(true))))),

      null, implements = List("Carnival")).operate.eval)

    assert(NewObject("McqueenGirl", "Car_Animation1O").operate.eval)
    exceptionClassDef(name ="No Oil", reason = Reason("Add Oil")).operate

    assert(scope2("Testing", {
      CatchException(
        name = "No Oil",
        block = {
          PRINT(NOT(Value(false))).operate

          IF(Object("McqueenGirl",invokeMethod("GetOil",collection.mutable.Map("A" -> Value(true), "B" -> Value(true)))).operate.eval)
          THEN(throwException("No Oil").operate)
          PRINT(NOT(Value(false))).operate
          PRINT(NOT(Value(false))).operate
          println(exceptionStack.top)
          PRINT(NOT(Value(false))).operate


        },

        catchBlock = Catch("No Oil",PRINT(Value(true)).operate,reason=Reason("Add petrol"))
      )
      Value(true)
    }))
  }


  it should "Testing While" in {


    assert(ClassDef("Car_Animation1D", false, List(set_Field("Car", Value(true))),
      List(Field("Car", accessSpecifier.private_access, Value(false)),
        Field("Shape", accessSpecifier.public_access, Value(true))),
      List(Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true)), List(Value(true))),
        Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true))))),
      null, implements = null).operate.eval)

    assert(NewObject("Mcqueen New", "Car_Animation1D").operate.eval)

    logicGateStack.push("H")
    logicGateStack.push("A")
    logicGateStack.push("L")
    logicGateStack.push("L")

    assert {
      WHILE(logicGateStack.nonEmpty ,{
      logicGateStack.pop
      Object("Mcqueen New",invokeMethod("PutOil",collection.mutable.Map("A" -> Value(true)))).operate
      }
    )
    }
  }



  it should "Throw an error of misplaced if while Using IF ELSE IF and ELSE" in {
    assert {
      IF(NOT(Value(false)).eval)
    }
    println(ifExeStack.top)
    println(ifStack.top)
    assert(THEN({
      PRINT(Value(true)).operate
      PRINT(Value(true)).operate
      PRINT(Value(true)).operate
    }))

    println(ifExeStack.isEmpty)
    println(ifStack.top)

    assert(ELSEIF(Value(true).eval))
    assertThrows[Exception](ELSE(Value(true)))

  }


  it should "Using IF ELSE IF and ELSE" in {
    assert{IF(NOT(Value(false)).eval)}
    println(ifExeStack.top)
    println(ifStack.top)
    assert(THEN({
      PRINT(Value(true)).operate
      PRINT(Value(true)).operate
      PRINT(Value(true)).operate
    }))

  }



  it should "Throw an exception because class does not override a method in the interface inherited by the implemented interface" in {
    assert(interface("Shaper", List(Field("x", accessSpecifier.protected_access, Value(false)),
      Field("y", accessSpecifier.private_access, Value(true))),
      List(Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))), null
    ).operate.eval)

    assert(interface("Car", List(Field("Oil", accessSpecifier.protected_access, Value(false)),
      Field("Gear", accessSpecifier.private_access, Value(true))),
      List(Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
      ), List("Shaper")
    ).operate.eval)

    assertThrows[Exception](ClassDef("Car_Animation", false, List(set_Field("Car", Value(true))),
      List(Field("Car", accessSpecifier.private_access, Value(false)),
        Field("Shape", accessSpecifier.public_access, Value(true))),
      List(Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(getParameter("B")))),
        Method("GetOil2", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("Circle2", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), setParameter("B", Value(true))))),

      null, implements = List("Car")).operate)


  }

  it should "Create two interfaces and Implement it in a class" in {
    assert(interface("ShaperN", List(Field("x", accessSpecifier.protected_access, Value(false)),
      Field("y", accessSpecifier.private_access, Value(true))),
      List(Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),null
    ).operate.eval)

    assert(interface("Cargo", List(Field("Oil", accessSpecifier.protected_access, Value(false)),
      Field("Gear", accessSpecifier.private_access, Value(true))),
      List(Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
          ), List("ShaperN")
    ).operate.eval)

    assert(ClassDef("Car_Animation1T", false, List(set_Field("Car", Value(true))),
      List(Field("Car", accessSpecifier.private_access, Value(false)),
        Field("Shape", accessSpecifier.public_access, Value(true))),
      List(Method("PutOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(getParameter("B")))),
        Method("GetOil", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("Rectangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
      Method("Square", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
      Method("Circle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
      Method("Triangle", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("PutOil", parameters = collection.mutable.Map("A" -> Value(true))), setParameter("B",Value(true))))),

      null, implements = List("Cargo")).operate.eval)

    assert(NewObject("McqueenMan","Car_Animation1T").operate.eval)

  }

  it should "Creates an interface" in {
    assert(interface("hello", List(Field("A1", accessSpecifier.protected_access, Value(false)),
      Field("B1", accessSpecifier.private_access, Value(true))),
      List(Method("two1", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two2", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("And test", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three3", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
      inherits=null
    ).operate.eval)

  }

  it should "Using the object created for the class which inherits a abstract class to call the override method" in {
    assert(ClassDef("Parent", true, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
      null, null).operate.eval)
    assert(
      ClassDef("Child", false, List(set_Field("A", Value(true))),
        List(Field("A", accessSpecifier.private_access, Value(false)),
          Field("B", accessSpecifier.public_access, Value(true))),
        List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(true)))),
          Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
          Method("Seven", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("Eight", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
          Method("Nine", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
        "Parent", implements = null).operate.eval)

    assert(NewObject("obj c", "Child").operate.eval)

    assert(Object("obj c",invokeMethod("three",collection.mutable.Map("A" -> Value(true), "B" -> Value(true)))).operate.eval)

  }


  it should "Creating an object for the class which inherits a abstract class" in {
    assert(ClassDef("Parent1", true, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
      null, null).operate.eval)
    assert(
      ClassDef("Child1", false, List(set_Field("A", Value(true))),
        List(Field("A", accessSpecifier.private_access, Value(false)),
          Field("B", accessSpecifier.public_access, Value(true))),
        List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(true)))),
          Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
          Method("Seven", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("Eight", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
          Method("Nine", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
        "Parent1", implements = null).operate.eval)

    assert(NewObject("obj d","Child1").operate.eval)

  }


  it should "throw an Exception because object cannot be created for an abstract class" in {
    assert(ClassDef("Parent5", true, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
      null, null).operate.eval)
    assertThrows[Exception](NewObject("obj B","Parent5").operate.eval)

  }

  it should "Create an object for the class and call a method" in {
    assert(ClassDef("example", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      null, implements = null).operate.eval)

    assert(NewObject("obj A", "example").operate.eval)

    assert(Object("obj A",invokeMethod("three",collection.mutable.Map("A" -> Value(true), "B" -> Value(true)))).operate.eval)

  }



  it should "Create an object for the class" in {
    assert(ClassDef("exampleClass1", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      null, implements = null).operate.eval)

    assert(NewObject("obj b","exampleClass1").operate.eval)

  }

  it should "inheriting a abstract class with abstract methods in Child Class" in {
    assert(ClassDef("Parent2", true, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod()))),
        null, null).operate.eval)
    assert(
      ClassDef("Child2", false, List(set_Field("A", Value(true))),
        List(Field("A", accessSpecifier.private_access, Value(false)),
          Field("B", accessSpecifier.public_access, Value(true))),
        List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(NOT(Value(true)))),
          Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B"))),
          Method("Seven", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("Eight", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
          Method("Nine", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
        "Parent2", implements = null).operate.eval)

  }

  it should "inheriting a class with abstract methods, should throw an exception because the method is not defined in the derived class" in {
    assert(ClassDef("Parent3", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      null, implements = null).operate.eval)

    assertThrows[Exception] {
      ClassDef("Child3", false, List(set_Field("A", Value(true))),
        List(Field("A", accessSpecifier.private_access, Value(false)),
          Field("B", accessSpecifier.public_access, Value(true))),
        List(Method("Seven", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
          Method("Eight", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
          Method("Nine", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
        "Parent3", implements = null).operate
    }
  }


  it should "inheriting a class with abstract methods " in {
    assert(ClassDef("Parent4", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(abstractMethod())),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      null, implements = null).operate.eval)

    assert(ClassDef("Child4", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("Seven", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      "Parent4", implements = null).operate.eval)
  }

  it should "Create a class " in {
    assert(ClassDef("exampleClass", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      null, implements = null).operate.eval)

  }

  it should "Create a new object for the class " in {
    assert(ClassDef("class1", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true),"B"->Value(false))), get_Field("B")))),
      null, implements = null).operate.eval)
    assert(NewObject("object A","class1").operate.eval)

  }


  it should "Using the object to setField " in {
    assert(ClassDef("class 2", false, List(set_Field("A", Value(true))),
      List(Field("A", accessSpecifier.private_access, Value(false)),
        Field("B", accessSpecifier.public_access, Value(true))),
      List(Method("One", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(false)), Value(true)), NOT(get_Field("B")))),
        Method("two", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)), List(XOR(NOT(Value(true)), Value(true)))),
        Method("three", accessSpecifier.public_access, parameters = collection.mutable.Map("A" -> Value(true), "B" -> Value(true)),
          List(invokeMethod("One", parameters = collection.mutable.Map("A" -> Value(true))), get_Field("B")))),
      null, implements = null).operate.eval)
    assert(NewObject("Object1", "class 2").operate.eval)

    assert(!Object("Object1",set_Field("A", NOT(Value(true)))).operate.eval)


  }
  it should "create a gate to test the De Morgan's law" in {
    NOT(OR(Value(true), Value(false))).eval shouldBe AND(NOT(Value(true)), NOT(Value(false))).eval
  }

  it should "Assign XOR gate expression to logicGate2 " in {
    assign(LogicGate("logicGate2"), XOR(Value(true),Value(true))).operate
    TestGate(LogicGate("logicGate2"), false)
  }

  it should "abide by the De Morgan's law" in {
    assign(LogicGate("logicGate1"), NOT(OR(Value(true), Value(false)))).operate
    assign(LogicGate("logicGate2"), AND(NOT(Value(true)), NOT(Value(false)))).operate
    assert(TestGate(LogicGate("logicGate2"), false)==TestGate(LogicGate("logicGate1"), false))
  }

  it should "Testing Scope" in {

    assign(LogicGate("logicGate1"), NOT(OR(input("A"), input("B")))).operate
    scope(LogicGate("logicGate1"), assign(Input("A"), Value(true))).operate
    scope(LogicGate("logicGate1"), assign(Input("B"), Value(true))).operate

    assert(!TestGate(LogicGate("logicGate1"),true))
  }

  it should "Testing Scope with XNOR, NOR and NAND" in {


    assign(LogicGate("logicGate3"), NOR(NOT(XNOR(input("A"), input("B"))),NAND(input("C"),NAND(input("A"),input("B"))))).operate
    scope(LogicGate("logicGate3"), assign(Input("A"), Value(true))).operate
    scope(LogicGate("logicGate3"), assign(Input("B"), Value(true))).operate
    scope(LogicGate("logicGate3"), assign(Input("C"), Value(true))).operate
    assert(!gate_Value(LogicGate("logicGate3")).eval)
    assert(!TestGate(LogicGate("logicGate3"), true))
  }




}