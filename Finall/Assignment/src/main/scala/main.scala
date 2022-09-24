object main:
  /*
  * The object is to implement a domain-specific language (DSL) using Scala
  *  for writing and evaluating set operation expressions for designers
  *  of the digital logic to implement logic gates.
  * */

  //LogicGateMap and Input Map store the values of the LogicGate expressions and input values
  //The LogicGateMap is a mutable map which holds the BooleanExpression that has to evaluated for finding the state of the
  //corresponding LogicGate
  val LogicGateMap : collection.mutable.Map[ String,BooleanExpression] =collection.mutable.Map()

  /*
  The inputGateMap is a mutable map which is of form [String,Map[String,Boolean]], it holds the input values
  of the corresponding logic gates.

  example of the value it holds :: "logicGate1"->(A->true,B->false)

  it means in logicGate1, two inputs are defined A and B with values true and False respectively.


  */

  val inputGateMap: collection.mutable.Map[String,Map[String, Boolean]] = collection.mutable.Map()

  //A case class has be defined to hold the value of Input names and be a datatype for further usage in the model
  case class Input(name : String)

  //A case class has be defined to hold the value of Logicgate names and be a datatype for further usage in the model
  case class LogicGate(name : String)

  // assign is a function used to assign value to LogicGates and their inputs.By default it assigns value to the LogicGate, only
  //when the 3rd parameter is changed to some other value (name of the logicGate) it assign input values to the correponding
  //logicGate. Note* BooleanExpression is an enumeration declared and discussed below in the file
  def assign(inputClass : Input | LogicGate, value : BooleanExpression,gate : String = "default") : Boolean = inputClass match
    case LogicGate(name)  =>
      LogicGateMap.put(name, value)
      true
    case Input(name) =>
      if !gate.matches("default") then inputGateMap.put(gate,Map(name->value.eval))
      true


  //scope is function used to give abstraction to the user.It helps in assigning the input values.
  //The inputs to scope are LogicGate, Input and value of the input (datatype : BooleanExpression, eg: true is given as Value(true))
  //The scopefunction will be improved in the future.
  def scope(gate : LogicGate,inputC: Input,value : BooleanExpression) : Unit=
    assign(inputC,value,gate.name)

  /*TestGate is function that evaluates the value of the gate and compares it with the expected value.It returns true if it is equal else
  returns false. Its inputs are LogicGate and Expected Boolean Value
  * */
  def TestGate(gate : LogicGate,expected : Boolean) : Boolean =
    val temp :BooleanExpression = LogicGateMap.getOrElse(gate.name,throw new Exception(gate.name))
    if temp.eval == expected then true
    else false

  /*
  * Boolean Expression is an enumeration used to define values.It has a function named eval to evaluate the corresponding cases
  * */
  enum BooleanExpression:
    //Declaring the Boolean Functions
    //The input Value case is used to return the particular input value in the gate
    case input_Value(l : LogicGate,c : String)
    //The Value case is used to return the Boolean values.Its declared for the simplicity of code and will be removed in future
    case Value(v: Boolean)
    //The gate_Value is used to return the value of the LogicGate after evaluation
    case gate_Value(l: LogicGate)
    //NOT performs negation
    case NOT(o1: BooleanExpression)
    //OR performs Boolean OR operation
    case OR(o1: BooleanExpression, o2: BooleanExpression )
    //AND performs Boolean AND operation
    case AND(o1: BooleanExpression, o2: BooleanExpression )
    //NAND performs Boolean NAND operation
    case NAND(o1: BooleanExpression, o2: BooleanExpression )
    //NOR performs Boolean NOR operation
    case NOR(o1: BooleanExpression , o2: BooleanExpression )
    //XOR performs Boolean XOR operation
    case XOR(o1: BooleanExpression , o2: BooleanExpression )
    //XNOR performs Boolean XNOR operation
    case XNOR(o1: BooleanExpression, o2: BooleanExpression )

    //Using eval to evaluate the Boolean Functions
    def eval: Boolean = this match
      case Value(x: Boolean) => x
      case gate_Value(l : LogicGate) => LogicGateMap(l.name).eval
      case input_Value(l : LogicGate,c : String) => inputGateMap(l.name).getOrElse(c,false)
      case NOT(o1) => !o1.eval
      case OR(o1,o2) => o1.eval | o2.eval
      case AND(o1, o2) => o1.eval & o2.eval
      case NAND(o1, o2) => !(o1.eval & o2.eval)
      case NOR(o1, o2) => !(o1.eval | o2.eval)
      case XOR(o1, o2) => o1.eval ^ o2.eval
      case XNOR(o1, o2) => !(o1.eval ^ o2.eval)
  @main def runIT : Unit =
    import BooleanExpression.*
    //assign is used to assign the BooleanExpression to the logicGate1
    assign(LogicGate("logicGate1"),XOR(Value(true),input_Value(LogicGate("logicGate1"),"A")))
    //The println statement is used to test the BooleanOperation
    println(OR(NOT(Value(true)),Value(true)))
    //scope is used to assign the value true to input A in logicGate1
    scope(LogicGate("logicGate1"),Input("A"),Value(true))
    //The inputGateMap is printed for sample
    println(inputGateMap)
    //The logicGateMap is printed for sample
    println(LogicGateMap)
    //TestGate evaluated the expression in logicGate and compares it with the expected Value
    TestGate(LogicGate("logicGate1"),true)


    //In next few statement the minimal axiom for Boolean Expession is calculated
    //The expression is made using 6 NAND Gates:
    // NAND(NAND(NAND(A,B),C),NAND(A,NAND(NAND(A,C),A)))=C
    assign(LogicGate("logicGate2"),NAND(NAND(input_Value(LogicGate("logicGate2"),"A"),input_Value(LogicGate("logicGate2"),"B")),input_Value(LogicGate("logicGate2"),"C")))
    //assign(LogicGate("logicGate2"),NAND(Value(true),Value(false)))
    assign(LogicGate("logicGate3"),NAND(input_Value(LogicGate("logicGate2"),"A"),NAND(NAND(input_Value(LogicGate("logicGate2"),"A"),input_Value(LogicGate("logicGate2"),"C")),input_Value(LogicGate("logicGate2"),"A"))))
    assign(LogicGate("logicGate4"),NAND(gate_Value(LogicGate("logicGate2")),gate_Value(LogicGate("logicGate3"))))
    //Scope is used to assign values to the input
    scope(LogicGate("logicGate2"),Input("A"),Value(true))
    scope(LogicGate("logicGate2"),Input("B"),Value(true))
    scope(LogicGate("logicGate2"),Input("C"),Value(false))
    println("LogicGate4")
    
    //The logicGate 4 has the expression.The output is C irrespective of A and B
    // False must be returned as the value of C is false but the expected mentioned in TestGate is true
    println(TestGate(LogicGate("logicGate4"),true))
    println(gate_Value(LogicGate("logicGate4")).eval)