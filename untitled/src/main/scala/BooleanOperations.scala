import scala.collection.mutable.Stack

object BooleanOperations:

  val LogicGateMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
  val inputGateMap: collection.mutable.Map[String, Map[String, Boolean]] = collection.mutable.Map()
  val classMap: collection.mutable.Map[String, BooleanExpression.ClassDef] = collection.mutable.Map()
  val fieldMap : collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
  val methodMap : collection.mutable.Map[String,collection.mutable.Map[String,List[BooleanExpression]]] = collection.mutable.Map()
  val fieldAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  val methodAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()

  enum accessSpecifier:
    case private_access
    case public_access
    case protected_access
    
  enum BooleanExpression:
      //Declaring the Boolean Functions
      case Input(name: String)
      case get_Field_Object(name_object : String,name_field: String)
      case get_Method_Object(name_object : String,name_field: String)
      case get_Field(name_field: String)
      case set_Field(name : String, value : BooleanExpression)
      case invokeMethod(name_field: String)
      case Field(name : String, access : accessSpecifier, value : BooleanExpression)
      case Method(name : String, access : accessSpecifier, value : List[BooleanExpression])
      case ClassDef(name : String, fields : List[Field],method : List[Method], inherits : String)
      case NewObject(name : String, classType : ClassDef)
      case printClassMap()
      case Object(name : String, action : BooleanExpression)


      case LogicGate(name: String)
      //The input Value case is used to return the particular input value in the gate
      case input_Value(l: LogicGate, c: String)
      //The Value case is used to return the Boolean values.Its declared for the simplicity of code and will be removed in future
      case Value(v: Boolean)
      //The gate_Value is used to return the value of the LogicGate after evaluation
      case gate_Value(l: BooleanExpression)
      //NOT performs negation
      case NOT(o1: BooleanExpression)
      //OR performs Boolean OR operation
      case OR(o1: BooleanExpression, o2: BooleanExpression)
      //AND performs Boolean AND operation
      case AND(o1: BooleanExpression, o2: BooleanExpression)
      //NAND performs Boolean NAND operation
      case NAND(o1: BooleanExpression, o2: BooleanExpression)
      //NOR performs Boolean NOR operation
      case NOR(o1: BooleanExpression, o2: BooleanExpression)
      //XOR performs Boolean XOR operation
      case XOR(o1: BooleanExpression, o2: BooleanExpression)
      //XNOR performs Boolean XNOR operation
      case XNOR(o1: BooleanExpression, o2: BooleanExpression)

      case assign(input: BooleanExpression, value: BooleanExpression, gate: String = "default")


      //scope is function used to give abstraction to the user.It helps in assigning the input values.
      //The inputs to scope are LogicGate, Input and value of the input (datatype : BooleanExpression, eg: true is given as Value(true))
      //The scopefunction will be improved in the future.
      case scope(gate: LogicGate, inputC: Input, value: BooleanExpression)

      case TestGate(gate: LogicGate, value: Boolean)

      def classOperation : BooleanExpression = this match
        case get_Method_Object(name_object : String,name_method: String) =>
          Object(name_object,invokeMethod(name_method)).classOperation

        case Object(name: String, action: BooleanExpression) => action match
          case get_Field(name_field: String) =>
            val x = fieldMap.get(name)
            val y =x.get(name_field)
            println("The value of "+name_field+"in Object "+name+"is :"+y)
            y

          case set_Field(name_field: String,value : BooleanExpression) =>
            val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map(name_field->value)
            fieldMap.update(name,temp_fieldMap)
            value


          case invokeMethod(name_method : String) =>
            println(name_method+" method invoked.")
            val x = methodMap.get(name)
            val operations = x.get(name_method)
            val result = Stack[BooleanExpression]()
            println("operation")
            println(operations)
            for operation <-  operations do
              operation match
                case get_Field(name_field: String) =>
                  println(operation)
                  result.push(Object(name,get_Field(name_field)).classOperation)
                case invokeMethod(name_method : String) =>
                  result.push(get_Method_Object(name,name_method).classOperation)
                case _ =>
                  result.push(Value(operation.eval))
            result.top

        //case Method(name: String, value: BooleanExpression)
        case NewObject(name: String, classType: ClassDef) =>
          classType match
            case ClassDef(name_class, fields, methods,inherits) =>
              println("classdef matched")
              println(fields)
              val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
              val temp_fieldAccessMap : collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
              for Field(name_field: String, access : accessSpecifier, value: BooleanExpression) <- fields do
                temp_fieldMap.update(name_field, value)
                temp_fieldAccessMap.update(name_field,access)

              fieldMap.update(name, temp_fieldMap)
              fieldAccessMap.update(name,temp_fieldAccessMap)
              println(fieldMap)
              val temp_methodMap: collection.mutable.Map[String, List[BooleanExpression]] = collection.mutable.Map()
              val temp_methodAccessMap : collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
              for Method(name_method: String,access : accessSpecifier, value: List[BooleanExpression]) <- methods do
                temp_methodMap.update(name_method, value)
                temp_methodAccessMap.update(name_method,access)
              methodMap.update(name, temp_methodMap)
              methodAccessMap.update(name,temp_methodAccessMap)
              println(methodMap)
              classType

      def dataType: String = this match
        case Input(name: String) => name

        case LogicGate(name: String) => name
        case _ =>
          "Match not found for given datatype"

      //Using eval to evaluate the Boolean Functions
      def eval : Boolean = this match
        case get_Field_Object(name_object: String, name_field: String) =>
          val x = fieldMap.get(name_object)
          val y=x.get(name_field)
          y.eval
        case printClassMap() =>
          println(classMap)
          true
        case Value(x: Boolean) => x
        case input_Value(l: LogicGate, c: String) => inputGateMap(l.name).getOrElse(c, false)
        case NOT(o1) => !o1.eval
        case OR(o1, o2) => o1.eval | o2.eval
        case AND(o1, o2) => o1.eval & o2.eval
        case NAND(o1, o2) => !(o1.eval & o2.eval)
        case NOR(o1, o2) => !(o1.eval | o2.eval)
        case XOR(o1, o2) => o1.eval ^ o2.eval
        case XNOR(o1, o2) => !(o1.eval ^ o2.eval)
        case gate_Value(gate: BooleanExpression) =>
          gate match
            case LogicGate(name: String) =>
              val x = LogicGateMap.getOrElse(name, Value(false))
              x.eval
            case _ =>
              println("Match not found for LogicGate : "+gate+" in gate_Value.")
              false
        case TestGate(gate: LogicGate, value: Boolean) =>
          gate match
            case LogicGate(name: String) =>
              if (LogicGateMap.getOrElse(name, Value(false)).eval == value)
                true
              else
                false


        case assign(input: BooleanExpression, value: BooleanExpression, gate: String) =>
          input match
            case LogicGate(name: String) =>
              LogicGateMap.put(name, value)
              true
            case Input(name) =>
              print("Gate in scope->assign   :")
              println(gate)
              if !gate.matches("default") then
                println("inside if")
                inputGateMap.put(gate , Map(name -> value.eval))
              else
                println("else")
              true
            case _ =>
              println("Match not found for input : "+input+" in assign.")
              false

        case scope(gate: BooleanExpression, inputC: BooleanExpression, value: BooleanExpression) =>
          println("case scope")
          assign(inputC, value, gate.dataType).eval
          true


