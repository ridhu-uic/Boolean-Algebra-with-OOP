package scala
import scala.collection.mutable.Map

object logicGate:
  var input : Map[String,Boolean] = scala.collection.mutable.Map()
  private var gateStates : Map[String,Boolean] = scala.collection.mutable.Map()


  def NOT(x1: Boolean): Boolean =
    !x1

  def AND (x1 : Boolean,x2 : Boolean): Boolean =
    x1&x2

  def OR(x1: Boolean, x2: Boolean): Boolean =
    x1 | x2

  def XOR(x1: Boolean, x2: Boolean): Boolean =
    x1 ^ x2

  def NAND(x1: Boolean, x2: Boolean): Boolean =
    !(x1 & x2)

  def NOR(x1: Boolean, x2: Boolean): Boolean =
    !(x1 | x2)

  def XNOR(x1: Boolean, x2: Boolean): Boolean =
    x1==x2

  def assign(gate : String, state : Boolean): Boolean =
    if gate.matches("logicGate.*") == true then {
      gateStates(gate)=state
      gateStates(gate)
    }

    else {
      input(gate) = state
      gateStates(gate)
    }

  def value(gate : String) : Boolean =
    if gate.matches("logicGate.*") == true then gateStates(gate)
    else input(gate)

  def scope(gate : String, x1 : Boolean) : Boolean =
    gateStates(gate) = x1
    gateStates(gate)

  def testGate(gate : String, state : Boolean ) : Boolean =
    if gateStates(gate) == state then true
    else false




  @main  def main(): Unit = {
    val x : Boolean = true
    val y : Boolean = false
    assign("logicGate1",NOT(true))
    println(value("logicGate1"))
    println(XOR(x,y))
    println("Scope")
    println(scope("logicGate2",assign("logicGate3",XOR(value("logicGate1"),true))))

    println("Hello world!")
    println(value("logicGate2"))
    println(testGate("logicGate2",true))
  }