import BooleanOperations.BooleanExpression.get_Field_Object

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Stack, ArrayBuffer}

object BooleanOperations:
  //The LogicGateMap Stores the LogicGates and their expressions
  val LogicGateMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
  //The inputGateMap Stores the inputs of the logicGates and their value. LogicGate -> Inputs -> Value
  val inputGateMap: collection.mutable.Map[String, Map[String, Boolean]] = collection.mutable.Map()
  //The classMap contains the class datatype name and its format
  val classMap: collection.mutable.Map[String, BooleanExpression.ClassDef] = collection.mutable.Map()

  //Class type  : checks if the class is abstract or not
  val classTypeMap : collection.mutable.Map[String, Boolean] = collection.mutable.Map()
  val interfaceFieldMap: collection.mutable.Map[String, collection.mutable.Map[String, BooleanExpression]] = collection.mutable.Map()
  val interfaceFieldAccessMap : collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  //interfaceMethods: interfaceName -> MethodName.
  val interfaceMethodMap: collection.mutable.Map[String, ListBuffer[String]] = collection.mutable.Map()
  //intereface -> methodAccess -> accessSpecifier
  val interfaceMethodAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  //parametermap interface->function name->parametername->parameter value
  val interfaceMethodParametersMap: collection.mutable.Map[String, collection.mutable.Map[String, collection.mutable.Map[String, BooleanExpression]]] = collection.mutable.Map()
  //The fieldMap stores the fields and their values in each class. ClassName -> Fields -> Values
  val fieldMap : collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
  //The methodMap stores the fields and their values in each class. ClassName -> methods -> List[Statements]
  val methodMap : collection.mutable.Map[String,collection.mutable.Map[String,List[BooleanExpression]]] = collection.mutable.Map()
  //The fieldAccessMap stores the fields and their accessSpecifier in each class. ClassName -> Fields -> AccessSpecifier8
  val fieldAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  //The methodAccessMap stores the methods and their accessSpecifier in each class. ClassName -> methods -> AccessSpecifier8
  val methodAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  //The inheritanceMap stores the class inherited by another class. Base Class-> Derived Class
  val inheritanceMap :  collection.mutable.Map[String,  String] = collection.mutable.Map()
  //The objectTypeMap stored the dataType of the Object. Object name -> Datatype
  val objectTypeMap :  collection.mutable.Map[String,  String] = collection.mutable.Map()
  //The objectFieldMap stores the objectName -> FieldName -> Value
  val objectFieldMap : collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
  //The objectMethodMap stores the objectName -> ClassName (Base or Derived) -> List[Methods].This Map acts like a Virtual Dispatch Table.
  val objectMethodMap : collection.mutable.Map[String,collection.mutable.Map[String,ListBuffer[String]]] = collection.mutable.Map()
  //parametermap classname->function name->parametername->parameter value
  val methodParametersMap : collection.mutable.Map[String,collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]]] = collection.mutable.Map()
  //The accessSpecifiers are declared using enum.
  enum accessSpecifier:
    case private_access
    case public_access
    case protected_access


  enum BooleanExpression:
    //Declaring the Boolean Functions

    //The following DataTypes are explained in the readMe file.
    case get_Field_Object(name_object : String,name_field: String)
    case get_Method_Object(name_object : String,name_field: String)
    case set_Field_Object(name_object : String,name_field: String, value : BooleanExpression)
    case get_Field(name_field: String)
    case set_Field(name : String, value : BooleanExpression)
    case invokeMethod(name_field: String, parameters : collection.mutable.Map[String,BooleanExpression])
    case setParameter(name : String, value : BooleanExpression)
    case getParameter(name : String)
    case Field(name : String, access : accessSpecifier, value : BooleanExpression)
    case Method(name : String, access : accessSpecifier, parameters : collection.mutable.Map[String, BooleanExpression], value : List[BooleanExpression])
    case ClassDef(name : String, isAbstract: Boolean , constructor : List[BooleanExpression],fields : List[Field],method : List[Method], inherits : String, implements : List[String])
    case NewObject(name : String, classType : String)
    case Object(name : String, action : BooleanExpression)
    case abstractMethod()
    case interface(name : String,fields : List[Field],method : List[Method], inherits : List[String])

    //Used to call input
    case Input(name: String)
    //Used to Call LogicGate
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

    //TestGate is used to compare the value of LogicGate and Expected Value
    case TestGate(gate: LogicGate, value: Boolean)

    //The Function Class Operation is defined to do the operations on class datatype.
    def classOperation : BooleanExpression = this match
      /*
      The ClassDef case declared the new class dataType.It stores the fields and Methods of the class.
      It also stores the dataType which the given class inherits.
      These functions are done using the corresponding maps.
      */
      case interface(name_interface : String,fields : List[Field],method : List[Method], inherits : List[String]) =>
        val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
        val temp_fieldAccessMap: collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
        for Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fields do
          temp_fieldMap.update(name_field, value)
          temp_fieldAccessMap.update(name_field, access)

        if inherits!=List("None") then
          for interfaceInherited <- inherits do
            temp_fieldMap.++(interfaceFieldMap.getOrElse(interfaceInherited,throw new Exception("Interface not found")))
            temp_fieldAccessMap.++(interfaceFieldAccessMap.getOrElse(interfaceInherited,throw new Exception("Interface not found")))
        interfaceFieldMap.update(name_interface, temp_fieldMap)
        interfaceFieldAccessMap.update(name_interface, temp_fieldAccessMap)

        val temp_methodMap = new ListBuffer[String]
        val temp_methodAccessMap: collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
        val temp_methodParameterMap: collection.mutable.Map[String, collection.mutable.Map[String, BooleanExpression]] = collection.mutable.Map()
        for Method(name_method: String, access: accessSpecifier, parameters: collection.mutable.Map[String, BooleanExpression], value: List[BooleanExpression]) <- method do
          temp_methodMap+=name_method
          temp_methodAccessMap.update(name_method, access)
          temp_methodParameterMap.update(name_method, parameters)

        if inherits != List("None") then
          for interfaceInherited <- inherits do
            val methodList =interfaceMethodMap.getOrElse(interfaceInherited, throw new Exception("Interface not found"))
            val accessMap =interfaceMethodAccessMap.getOrElse(interfaceInherited, throw new Exception("Interface not found"))
            val parameterMap =interfaceMethodParametersMap.getOrElse(interfaceInherited, throw new Exception("Interface not found"))

            for methodsinherited <- methodList do
              if temp_methodMap.contains(methodsinherited) then throw new Exception("Ambiguity due to same signature of methods")
              else
                temp_methodMap+=methodsinherited
                val methodAccess=accessMap.getOrElse(methodsinherited,throw new Exception("Method Access not found for "+methodsinherited))
                temp_methodAccessMap.update(methodsinherited,methodAccess)
                val methodParameter=parameterMap.getOrElse(methodsinherited,throw new Exception("Method Parameter not found for"+methodsinherited))
                temp_methodParameterMap.update(methodsinherited,methodParameter)

        interfaceMethodMap.update(name_interface,temp_methodMap)
        interfaceMethodAccessMap.update(name_interface,temp_methodAccessMap)
        interfaceMethodParametersMap.update(name_interface,temp_methodParameterMap)




        interfaceMethodMap.update(name_interface,temp_methodMap)
        interfaceMethodAccessMap.update(name_interface,temp_methodAccessMap)
        interfaceMethodParametersMap.update(name_interface,temp_methodParameterMap)

        println("interface Method Parameters")
        println(interfaceMethodParametersMap)
        println()
        println("interface Method Map")
        println(interfaceMethodMap)
        println()
        println("interface Method Access")
        println(interfaceMethodAccessMap)
        Value(true)



      case ClassDef(name_class,isAbstract, constructor, fields, methods,inherits, implements ) =>
        classMap.put(name_class,ClassDef(name_class, isAbstract,constructor, fields, methods,inherits,implements))
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
        val temp_methodParameterMap :  collection.mutable.Map[String, collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
        for Method(name_method: String, access: accessSpecifier,parameters : collection.mutable.Map[String, BooleanExpression], value: List[BooleanExpression]) <- methods do
          temp_methodMap.update(name_method, value)
          temp_methodAccessMap.update(name_method, access)
          temp_methodParameterMap.update(name_method,parameters)
        methodMap.update(name_class, temp_methodMap)
        methodAccessMap.update(name_class, temp_methodAccessMap)
        methodParametersMap.update(name_class,temp_methodParameterMap)
        println(methodMap)

        inheritanceMap.put(name_class,inherits)
        classTypeMap.put(name_class,isAbstract)
        if implements != List("None") then
          for implement <- implements do
            val list_methods = interfaceMethodMap.getOrElse(implement,throw new Exception("Interface not found"))
            for method_interface <- list_methods do
              if !temp_methodMap.contains(method_interface) then throw new Exception("Method not overriden in the class which is present in interface"+method_interface)




        ClassDef(name_class,isAbstract,constructor, fields, methods, inherits, implements)

      //Used to get Method belonging to an object.Its a temporary method.
      /*
      The Object is the datatype designed to do the operations on an object.
      It contains invoking a method, getting a field and setting a field. 
      The Child class methods can be invoked if they are public or protected directly from main.The private methods
      are invoked from the other methods.The base class public and protected methods can be called from the derived class.
      */
      case Object(name_object: String, action: BooleanExpression) =>

        //val className = objectTypeMap.getOrElse(name_object,throw new Exception("Object Type MisMatch"))
        val objectType: String = objectTypeMap.getOrElse(name_object, throw new Exception("Object Type MisMatch"))
        val inheritedType = inheritanceMap.getOrElse(objectType, throw new Exception("Object Type MisMatch"))


        println()
        println(name_object+" doing action : "+ action)
        println()
        action match
          case get_Field(name_field: String) =>
            //val objectType = objectTypeMap.getOrElse(name_object,throw new Exception("Object name not found in "+this))
            val own_fields = fieldMap.getOrElse(objectType, throw new Exception("Own fields not found for" + objectType))
            val extended_fields = fieldMap.getOrElse(inheritedType, throw new Exception("Own fields not found for" + objectType))
            val inheritedAccess = fieldAccessMap.getOrElse(inheritedType, throw new Exception("Own fields not found for" + objectType))

            if own_fields.contains(name_field) then
              println("Own Fields")
              println(own_fields)
              val x = own_fields.getOrElse(name_field, throw new Exception("Found and Missed"))
              x
            else if extended_fields.contains(name_field) then
              println("Extended Fields")
              val field = extended_fields.getOrElse(name_field, throw new Exception("Found and Missed"))
              val access = inheritedAccess.getOrElse(name_field, throw new Exception("Found and Missed"))
              if access == accessSpecifier.public_access | access == accessSpecifier.protected_access then
                println("Field found" + field)
                field
              else
                throw new Exception("Private Field Cannaot be accessed from parent class.")
            else

              throw new Exception("Not Found")

          case set_Field(name_field: String,value : BooleanExpression) =>
            val fieldsMap = objectFieldMap.getOrElse(name_object, throw new Exception("Object not created" + set_Field_Object(name_object: String, name_field: String, value: BooleanExpression)))
            println("fieldsMap")
            fieldsMap.update(name_field, value)
            objectFieldMap.update(name_object, fieldsMap)
            println(objectFieldMap)
            value

          case invokeMethod(name_method : String, parameters : collection.mutable.Map[String,BooleanExpression]) =>
            println(name_method+" method invoked.")

            println("object   :")
            println()
            println(objectMethodMap)
            val vptr = objectMethodMap.getOrElse(name_object,throw new Exception("Object Type MisMatch"))
            val vptr_own_methods=vptr.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
            val vptr_inherited_methods=vptr.getOrElse(inheritedType,throw new Exception("Object Type MisMatch"))
            val vptr_own_methods_access=methodAccessMap.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
            val vptr_own_inherited_access=methodAccessMap.getOrElse(inheritedType,throw new Exception("Inherited Type Mismatch"))

            if vptr_own_methods.contains(name_method) then
              val method = methodMap.getOrElse(objectType,throw new Exception("Object Type MisMatch")).getOrElse(name_method,throw new Exception("Method not Found"))
              //val methodsAccess = methodAccessMap.getOrElse(objectType,throw new Exception("Object Type MisMatch"))
              val methodAccess = vptr_own_methods_access.getOrElse(name_method,throw new Exception("Method not Found"))
              if !(methodAccess==accessSpecifier.public_access) then throw new Exception("Methods which are not public cannot be accessed from main")
              println()
              println("parameter Set")
              println(parameters.keySet)
              println()
              println("real parameters")
              println(methodParametersMap.getOrElse(objectType,throw new Exception("Object Type MisMatch")).getOrElse(name_method,throw new Exception("Method not found")).keySet)
              if !(parameters.keySet == methodParametersMap.getOrElse(objectType,throw new Exception("Object Type MisMatch")).getOrElse(name_method,throw new Exception("Method not found")).keySet)
                then throw new Exception("Method parameters are not matching")

              val result : mutable.Stack[BooleanExpression] = mutable.Stack()
              val oBuffer = new ArrayBuffer[BooleanExpression]()
              for m <- method do
                println("action     :"+m)
                m match
                  case get_Field(name_field: String) =>
                    result.push(Object(name_object, get_Field(name_field)).classOperation)
                  case invokeMethod (name_method2: String, parameters : collection.mutable.Map[String,BooleanExpression]) =>
                    result.push(Object(name_object,invokeMethod (name_method2: String, parameters : collection.mutable.Map[String,BooleanExpression])).classOperation)
                  case getParameter(name_parameter : String) =>
                    result.push(parameters.getOrElse(name_parameter,throw new Exception("Parameter not found.The parameters found"+parameters.keySet)))
                  case NOT(o1) =>
                    println()
                    o1 match
                      case get_Field(name_field: String) =>
                        result.push(Value(NOT(Object(name_object, get_Field(name_field)).classOperation).eval))
                        println("The currest result :   "+result.top)

                      case Value(v) =>
                        result.push(Value(NOT(o1).eval))
                        println("The currest result :   "+result.top)

                      case getParameter(name_parameter : String) =>
                        result.push(Value(NOT(parameters.getOrElse(name_parameter,throw new Exception("Parameter not found.The parameters found"+parameters.keySet))).eval))
                        println("The currest result :   "+result.top)

                      case setParameter(name : String, value : BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are"+parameters.keySet)
                        parameters(name)=value
                        result.push(Value(NOT(value).eval))
                        println("The currest result :   "+result.top)

                      case invokeMethod(name_field: String, parameters : collection.mutable.Map[String,BooleanExpression]) =>
                        result.push(Value(NOT(Object(name_object,o1).classOperation).eval))
                        println("The currest result :   "+result.top)

                      case _ =>
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        result.push(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)


                  case AND(o1,o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing"+Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field"+oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method")=List(o1)
                        methodAccessMap(objectType)("temp_method")=accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method")=parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object,invokeMethod("temp_method",parameters)).classOperation)

                    println("o1 == "+oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == "+oBuffer(1))
                    result.push(Value(AND(oBuffer(0),oBuffer(1)).eval))
                    oBuffer.clear()


                  case OR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>

                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>

                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result.push(Value(OR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case XOR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result.push(Value(XOR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case NOR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result.push(Value(NOR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case NAND(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result.push(Value(NAND(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case XNOR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append((Object(name_object, get_Field(name_field)).classOperation))
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result.push(Value(XNOR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case _ =>
                    throw new Exception("Method match not found")

                println("The final result   :"+result.top)
                result.top
            else if vptr_inherited_methods.contains(name_method) then
              val inherited_methods = methodMap.getOrElse(inheritedType, throw new Exception("Object Type MisMatch"))
              val inherited_method = inherited_methods.getOrElse(name_method,throw new Exception("Method not found"))
              val inherited_access = vptr_own_inherited_access.getOrElse(name_method,throw new Exception("Access not found"))
              if inherited_access==accessSpecifier.private_access then throw new Exception("Private method cannot be accessed")
              if !(parameters.keySet == methodParametersMap.getOrElse(inheritedType, throw new Exception("Object Type MisMatch")).getOrElse(name_method, throw new Exception("Method not found")).keySet) then
                println ("Parameters provided in the invoke method call :"+parameters.keySet)
                println()
                println ("Parameters defined in the class definiton " + methodParametersMap.getOrElse(inheritedType, throw new Exception("Inherited Object Type MisMatch")).getOrElse(name_method, throw new Exception("Method not found")).keySet)
                throw new Exception("Parameters provided and parameters defined are not the same.")

              val oBuffer = new ArrayBuffer[BooleanExpression]()
              val result_inherited :  mutable.Stack[BooleanExpression] = mutable.Stack()
              for m <- inherited_method do
                m match
                  case get_Field(name_field: String) =>
                    result_inherited.push(Object(name_object, get_Field(name_field)).classOperation)
                  case invokeMethod(name_method2: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                    result_inherited.push(Object(name_object, invokeMethod(name_method2: String, parameters: collection.mutable.Map[String, BooleanExpression])).classOperation)
                  case getParameter(name_parameter: String) =>
                    result_inherited.push(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                  case NOT(o1) =>
                    println()
                    o1 match
                      case get_Field(name_field: String) =>
                        result_inherited.push(Value(NOT(Object(name_object, get_Field(name_field)).classOperation).eval))
                        println("The currest result :   " + result_inherited.top)

                      case Value(v) =>
                        result_inherited.push(Value(NOT(o1).eval))
                        println("The currest result :   " + result_inherited.top)

                      case getParameter(name_parameter: String) =>
                        result_inherited.push(Value(NOT(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet))).eval))
                        println("The currest result :   " + result_inherited.top)

                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        result_inherited.push(Value(NOT(value).eval))
                        println("The currest result :   " + result_inherited.top)

                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        result_inherited.push(Value(NOT(Object(name_object, o1).classOperation).eval))
                        println("The currest result :   " + result_inherited.top)

                      case _ =>
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        result_inherited.push(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)


                  case AND(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result_inherited.push(Value(AND(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()


                  case OR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result_inherited.push(Value(OR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case XOR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result_inherited.push(Value(XOR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case NOR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result_inherited.push(Value(NOR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case NAND(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result_inherited.push(Value(NAND(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case XNOR(o1, o2) =>
                    o1 match
                      case get_Field(name_field: String) =>
                        println("printing" + Object(name_object, get_Field(name_field)).classOperation)
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)
                        println("printing field" + oBuffer(0))


                      case Value(v) =>
                        oBuffer.append(o1)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))
                        println()
                        println(oBuffer(0))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o1).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o1)
                        methodAccessMap(objectType)("temp_method") = accessSpecifier.public_access
                        methodParametersMap(objectType)("temp_method") = parameters
                        println(methodMap)
                        println()
                        println()
                        println(objectMethodMap)
                        println()
                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o1 == " + oBuffer(0))

                    o2 match
                      case get_Field(name_field: String) =>
                        oBuffer.append(Object(name_object, get_Field(name_field)).classOperation)


                      case Value(v) =>
                        oBuffer.append(o2)


                      case getParameter(name_parameter: String) =>
                        oBuffer.append(parameters.getOrElse(name_parameter, throw new Exception("Parameter not found.The parameters found" + parameters.keySet)))


                      case setParameter(name: String, value: BooleanExpression) =>
                        if !parameters.contains(name) then throw new Exception("Parameter not found.Parameters found are" + parameters.keySet)
                        parameters(name) = value
                        oBuffer.append(parameters(name))


                      case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
                        oBuffer.append(Object(name_object, o2).classOperation)

                      case _ =>
                        println("Do nothing")
                        Value(true)
                        /*
                        create a temporary method
                        */

                        objectMethodMap(name_object)(objectType).append("temp_method")
                        methodMap(objectType)("temp_method") = List(o2)

                        oBuffer.append(Object(name_object, invokeMethod("temp_method", parameters)).classOperation)

                    println("o2 == " + oBuffer(1))
                    result_inherited.push(Value(XNOR(oBuffer(0), oBuffer(1)).eval))
                    oBuffer.clear()

                  case _ =>
                    throw new Exception("Method match not found")
                result_inherited.top



            Value(false)

          case _ =>
            throw new Exception(name_object+" has no action : "+ action)

      /*
        The NewObject is used to create the new Object.The corresponding maps of the object is defined during the 
      The NewObject Call.
      */
      case NewObject(name: String, classTypeName: String) =>
        objectTypeMap.update(name,classTypeName)
        val classType = classMap.getOrElse(classTypeName,throw new Exception("Object Type MisMatch"))
         println(classType)
        classType match
          case ClassDef(name_class,isAbstract,constructor, fields, methods,inherits, implements ) =>
            if classTypeMap.getOrElse(name_class, "Class type not found") == true then throw new Exception("Object cannot be created for abstract class")
            val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()
            for Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fields do
              temp_fieldMap.put(name_field, value)
            val methodListBuffer = new ListBuffer[String]
            for Method(name_method: String, access: accessSpecifier,parameters : collection.mutable.Map[String,BooleanExpression], value: List[BooleanExpression]) <- methods do
              methodListBuffer+=name_method
            val methodsList : collection.mutable.Map[String,ListBuffer[String]] = collection.mutable.Map(name_class->methodListBuffer)
            println()
            println()
            println("method List")
            println(methodsList)
            println()
            if inherits!="None" then
              val inheritedClass = classMap.getOrElse(inherits,"None")
              val isInheritedAbstract = classTypeMap.getOrElse(inherits, throw new Exception("inherited type not found in abstract classes"))
              inheritedClass match
                case ClassDef(name_inherited_class,isAbstract,constructor, fields, inherited_methods,inherits, implements ) =>
                  for Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fields do
                      if !temp_fieldMap.contains(name_field) then
                        temp_fieldMap.update(name_field, value)
                  val list_Methods = new ListBuffer[String]
                  println()
                  for Method(name_method : String, access_method : accessSpecifier,parameters : collection.mutable.Map[String,BooleanExpression], value_method : List[BooleanExpression]) <- inherited_methods do
                    list_Methods+=name_method

                  if isInheritedAbstract  then
                    println("Is Inherited Abstract")
                    for inheritedAbstractMethod <- list_Methods do
                      if !methodsList.get(name_class).contains(inheritedAbstractMethod) then throw new Exception("Abstract method is not overrided by the derived class")




                  println()
                  println()
                  println("method List")
                  println(methodsList)
                  println()
                  methodsList.put(name_inherited_class,list_Methods)
                  println()
                  println()
                  println("method List")
                  println(methodsList)
                  println()
                  objectFieldMap.put(name,temp_fieldMap)
                  objectMethodMap.put(name,methodsList)
                  for constructor_operation <- constructor do
                    constructor_operation match
                      case set_Field(name_field: String, value: BooleanExpression) =>
                        val fieldsMap = objectFieldMap.getOrElse(name, throw new Exception("Object not created" + constructor_operation))
                        println("fieldsMap")
                        fieldsMap.update(name_field,value)
                        objectFieldMap.update(name,fieldsMap)
                        println(objectFieldMap)
                      case _ =>
                        Value(true)


            else
              objectFieldMap.put(name, temp_fieldMap)
              objectMethodMap.put(name, methodsList)

            for constructor_operation <- constructor do
              constructor_operation match
                case set_Field(name_field: String, value: BooleanExpression) =>
                  val fieldsMap = objectFieldMap.getOrElse(name, throw new Exception("Object not created" + constructor_operation))
                  println("fieldsMap")
                  fieldsMap.update(name_field, value)
                  objectFieldMap.update(name, fieldsMap)
                  println(objectFieldMap)
                case _ =>
                  Value(false)
        Value(true)

      case _ =>
        Value(false)

    //The Datatype function is used to return the LogicGate and Input Datatype
    def dataType: String = this match
      case Input(name: String) => name

      case LogicGate(name: String) => name
      case _ =>
        "Match not found for given datatype"

    //Using eval to evaluate the Boolean Functions
    def eval : Boolean = this match
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

      case _ =>
        false

