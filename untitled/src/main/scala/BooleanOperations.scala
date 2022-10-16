import BooleanOperations.BooleanExpression.get_Field_Object

import scala.collection.mutable.{ListBuffer, Stack}

object BooleanOperations:

  val LogicGateMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
  val inputGateMap: collection.mutable.Map[String, Map[String, Boolean]] = collection.mutable.Map()
  val classMap: collection.mutable.Map[String, BooleanExpression.ClassDef] = collection.mutable.Map()
  val fieldMap : collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
  val methodMap : collection.mutable.Map[String,collection.mutable.Map[String,List[BooleanExpression]]] = collection.mutable.Map()
  val fieldAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  val methodAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  val inheritanceMap :  collection.mutable.Map[String,  String] = collection.mutable.Map()
  val objectTypeMap :  collection.mutable.Map[String,  String] = collection.mutable.Map()
  val objectFieldMap : collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
  val objectMethodMap : collection.mutable.Map[String,collection.mutable.Map[String,List[String]]] = collection.mutable.Map()
  enum accessSpecifier:
    case private_access
    case public_access
    case protected_access

  enum BooleanExpression:
      //Declaring the Boolean Functions
      case Input(name: String)
      case get_Field_Object(name_object : String,name_field: String)
      case get_Method_Object(name_object : String,name_field: String)
      case set_Field_Object(name_object : String,name_field: String, value : BooleanExpression)
      case get_Field(name_field: String)
      case set_Field(name : String, value : BooleanExpression)
      case invokeMethod(name_field: String)
      case Field(name : String, access : accessSpecifier, value : BooleanExpression)
      case Method(name : String, access : accessSpecifier, value : List[BooleanExpression])
      case ClassDef(name : String, constructor : List[BooleanExpression],fields : List[Field],method : List[Method], inherits : String)
      case NewObject(name : String, classType : String)
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
        case ClassDef(name_class, constructor, fields, methods,inherits) =>
          classMap.put(name_class,ClassDef(name_class, constructor, fields, methods,inherits))
          val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
          val temp_fieldAccessMap: collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
          for Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fields do
            temp_fieldMap.update(name_field, value)
            temp_fieldAccessMap.update(name_field, access)

          fieldMap.update(name_class, temp_fieldMap)
          fieldAccessMap.update(name_class, temp_fieldAccessMap)
          println(fieldMap)
          val temp_methodMap: collection.mutable.Map[String, List[BooleanExpression]] = collection.mutable.Map()
          val temp_methodAccessMap: collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
          for Method(name_method: String, access: accessSpecifier, value: List[BooleanExpression]) <- methods do
            temp_methodMap.update(name_method, value)
            temp_methodAccessMap.update(name_method, access)
          methodMap.update(name_class, temp_methodMap)
          methodAccessMap.update(name_class, temp_methodAccessMap)
          println(methodMap)
          if !inherits.matches("None") then
            inheritanceMap.put(name_class,inherits)
          ClassDef(name_class,constructor, fields, methods, inherits)

        case get_Field_Object(name_object : String,name_field: String) =>
          //val objectType = objectTypeMap.getOrElse(name_object,throw new Exception("Object name not found in "+this))
          val contains = objectFieldMap.getOrElse(name_object,throw new Exception("Object not found in "+this)).getOrElse(name_field,throw new Exception("Field not found in "+name_object))
          contains

        case set_Field_Object(name_object : String,name_field: String, value : BooleanExpression) =>
          val fields = objectFieldMap.getOrElse(name_object,throw new Exception("Object not found in "+this))
          println(fields)
          fields.getOrElse(name_field,throw new Exception("Field not found in "+name_object))
          value


        case get_Method_Object(name_object : String,name_method: String) =>
          Object(name_object,invokeMethod(name_method)).classOperation

        case Object(name_object: String, action: BooleanExpression) => action match
          //val className = objectTypeMap.getOrElse(name_object,throw new Exception("Object Type MisMatch"))
          case get_Field(name_field: String) =>
            val x = objectFieldMap.getOrElse(name_object,throw new Exception("Object Not Found in "+action)).getOrElse(name_field,throw new Exception("Field not found"))
            x

          case set_Field(name_field: String,value : BooleanExpression) =>
            val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map(name_field->value)
            objectFieldMap.update(name_object,temp_fieldMap)
            value

          case invokeMethod(name_method : String) =>
            println(name_method+" method invoked.")
            val objectType : String = objectTypeMap.getOrElse(name_object,throw new Exception("Object Type MisMatch"))
            println("object type :"+objectType)
            val inheritedType = inheritanceMap.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
            println(objectMethodMap)
            val vptr = objectMethodMap.getOrElse(name_object,throw new Exception("Object Type MisMatch"))
            println("objectMethods")
            println(vptr)
            val vptr_own_methods=vptr.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
            val vptr_inherited_methods=vptr.getOrElse(inheritedType,throw new Exception("Object Type MisMatch"))
            val vptr_own_methods_access=methodAccessMap.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
            val vptr_own_inherited_access=methodAccessMap.getOrElse(inheritedType,throw new Exception("Inherited Type Mismatch"))
            println("vptr_own_methods")
            println(vptr_own_methods)
            println("vptr_inherited_methods")
            println(vptr_inherited_methods)
            if vptr_own_methods.contains(name_method) then
              println("In vptr own methods")
              val method = methodMap.getOrElse(objectType,throw new Exception("Object Type MisMatch")).getOrElse(name_method,throw new Exception("Method not Found"))
              //val methodsAccess = methodAccessMap.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
              val methodAccess = vptr_own_methods_access.getOrElse(name_method,throw new Exception("Method not Found"))
              if !(methodAccess==accessSpecifier.public_access) then throw new Exception("Methods which are not public cannot be accessed from main")
              val result = Stack[BooleanExpression]()
              for m <- method do
                m match
                  case get_Field(name_field: String) =>
                    result.push(Object(name_object, get_Field(name_field)).classOperation)
                  case invokeMethod(name_method: String) =>
                    result.push(get_Method_Object(name_object, name_method).classOperation)
                  case _ =>
                    result.push(Value(m.eval))
                result.top
              println("method")
              println(method)
            else if vptr_inherited_methods.contains(name_method) then
              val inherited_methods = methodMap.getOrElse(inheritedType, throw new Exception("Object Type MisMatch"))
              val inherited_method = inherited_methods.getOrElse(name_method,throw new Exception("Method not found"))
              val inherited_access = vptr_own_inherited_access.getOrElse(name_method,throw new Exception("Access not found"))
              if inherited_access==accessSpecifier.private_access then throw new Exception("Private method cannot be accessed")
              val result_inherited = Stack[BooleanExpression]()
              for operation <- inherited_method do
                println(operation)
                operation match
                  case get_Field(name_field: String) =>
                    result_inherited.push(Object(name_object, get_Field(name_field)).classOperation)
                  case invokeMethod(name_method: String) =>
                    result_inherited.push(get_Method_Object(name_object, name_method).classOperation)
                  case _ =>
                    result_inherited.push(Value(operation.eval))
                result_inherited.top



            Value(false)
            /*
            if objectMethods.find(name_method) then
              val methods_found = methodMap.getOrElse(objectType,Map())
              println("methods found")
              println(methods_found)
            Value(false)
            */


        //case Method(name: String, value: BooleanExpression)
        case NewObject(name: String, classTypeName: String) =>
          objectTypeMap.update(name,classTypeName)
          val classType = classMap.getOrElse(classTypeName,throw new Exception("Object Type MisMatch"))
          println(classType)
          classType match
            case ClassDef(name_class,constructor, fields, methods,inherits) =>
              val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
              for Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fields do
                temp_fieldMap.put(name_field, value)
              val methodListBuffer = new ListBuffer[String]
              for Method(name_method: String, access: accessSpecifier, value: List[BooleanExpression]) <- methods do
                methodListBuffer+=name_method
              val methodsList : collection.mutable.Map[String,List[String]] = collection.mutable.Map(name_class->methodListBuffer.toList)
              methodListBuffer.clear()
              val inheritedClass = classMap.getOrElse(inherits,"None")
              inheritedClass match
                case ClassDef(name_inherited_class,constructor, fields, inherited_methods,inherits) =>
                  val temp_inheritedFieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
                  for Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fields do
                    if !(accessSpecifier.private_access==access) then
                      if !temp_fieldMap.contains(name_field) then
                        temp_fieldMap.update(name_field, value)
                  val list_Methods = new ListBuffer[String]
                  println()
                  for Method(name_method : String, access_method : accessSpecifier, value_method : List[BooleanExpression]) <- inherited_methods do
                    Value(true)
                    list_Methods+=name_method
                  methodsList.put(name_inherited_class,list_Methods.toList)
                  objectFieldMap.put(name,temp_fieldMap)
                  objectMethodMap.put(name,methodsList)
                  for constructor_operation <- constructor do
                    constructor_operation match
                      case set_Field(name_field: String, value: BooleanExpression) =>
                        val fieldsMap = objectFieldMap.getOrElse(name, throw new Exception("Object not created" + constructor_operation))
                        println("fieldsMap")
                        println(fieldsMap)
                  

              for constructor_operation <- constructor do
                constructor_operation match
                  case set_Field(name_field: String, value: BooleanExpression) =>
                    val fieldsMap =objectFieldMap.getOrElse(name,throw new Exception("Object not created"+constructor_operation))
                    println("fieldsMap")
                    println(fieldsMap)
                    
                    value
              Value(true)
          Value(true)

      def dataType: String = this match
        case Input(name: String) => name

        case LogicGate(name: String) => name
        case _ =>
          "Match not found for given datatype"

      //Using eval to evaluate the Boolean Functions
      def eval : Boolean = this match
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

        case get_Field_Object(name_object : String, name_field : String) =>
          get_Field_Object(name_object, name_field).classOperation.eval


