object main:
  val LogicGateMap : collection.mutable.Map[ String,BooleanExpression] =collection.mutable.Map()
  val inputGateMap: collection.mutable.Map[String,Map[String, Boolean]] = collection.mutable.Map()
  case class Input(name : String)
  case class LogicGate(name : String)
  def assign(inputClass : Input | LogicGate, value : BooleanExpression,gate : String = "default") : Boolean = inputClass match
    case LogicGate(name)  =>
      LogicGateMap.put(name, value)
      println(LogicGateMap)
      true
    case Input(name) =>
      if !gate.matches("default") then inputGateMap.put(gate,Map(name->value.eval))
      true

  def scope(gate : LogicGate,inputC: Input,value : BooleanExpression) : Unit=
    assign(inputC,value,gate.name)

  def TestGate(gate : LogicGate,expected : Boolean) : Boolean =
    val temp :BooleanExpression = LogicGateMap.getOrElse(gate.name,throw new Exception(gate.name))
    if temp.eval == expected then true
    else false
  enum BooleanExpression:
    //Declaring the Boolean Functions
    case input_Value(l : LogicGate,c : String)
    case Value(v: Boolean)
    case gate_Value(l: LogicGate)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression )
    case AND(o1: BooleanExpression, o2: BooleanExpression )
    case NAND(o1: BooleanExpression, o2: BooleanExpression )
    case NOR(o1: BooleanExpression , o2: BooleanExpression )
    case XOR(o1: BooleanExpression , o2: BooleanExpression )
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
    assign(LogicGate("logicGate1"),XOR(Value(true),input_Value(LogicGate("logicGate1"),"A")))
    println(OR(NOT(Value(true)),Value(true)))

    assign(Input("A"),Value(true),"logicGate1")

    scope(LogicGate("logicGate1"),Input("A"),Value(true))
    println(inputGateMap)

    TestGate(LogicGate("logicGate1"),true)