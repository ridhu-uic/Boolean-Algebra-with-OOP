import Sets_Classes.BooleanExpression.Reason
import Sets_Classes.BooleanExpression.*

object Sets_Classes:
  /*
  The following datatype are declared to be used for the creation of classes, objects, abstractClass,interfaces and logicGate operation.
  */

  //The objectList, classList, interfaceList are hold the list of objects, classes and interfaces declared.
  val objectList, classList, interfaceList : collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer[String]()
  //isAbstractMap holds the value true, if the class is abstract else false
  val isAbstractMap : collection.mutable.Map[String,Boolean] = collection.mutable.Map()
  //constructorMap holds the constructor of the corresponding class
  val constructorMap: collection.mutable.Map[String, List[BooleanExpression]] = collection.mutable.Map()
  //fieldMap, interfaceFieldMap holds fields of the class and interfaces respectively.
  val fieldMap, interfaceFieldMap : collection.mutable.Map[String,collection.mutable.Map[String,BooleanExpression]] = collection.mutable.Map()
  //interfaceMethodMap,methodMap holds the methods of the class and interface respectively.
  val interfaceMethodMap,methodMap : collection.mutable.Map[String,collection.mutable.Map[String,List[BooleanExpression]]]= collection.mutable.Map()
  //fieldAccessMap, methodAccessMap, interfaceFieldAccessMap, interfaceMethodAccessMap holds the accessTypes of the fields and methods
  val fieldAccessMap, methodAccessMap, interfaceFieldAccessMap, interfaceMethodAccessMap: collection.mutable.Map[String, collection.mutable.Map[String, accessSpecifier]] = collection.mutable.Map()
  //inheritanceMap, objectTypeMap holds the classType of inherited class and object
  val inheritanceMap, objectTypeMap :  collection.mutable.Map[String,  String] = collection.mutable.Map()
  //methodParametersMap, interfaceMethodParameterMap holds parameters of the methods of class and interface
  val methodParametersMap, interfaceMethodParameterMap: collection.mutable.Map[String, collection.mutable.Map[String, collection.mutable.Map[String, BooleanExpression]]] = collection.mutable.Map()
  //interfaceClassBind,interfaceInheritanceMap holds the list of interface implemented in a class and interfaces inherited in interface respectively
  val interfaceClassBind,interfaceInheritanceMap  : collection.mutable.Map[String, List[String]] = collection.mutable.Map()


  //Temporary methods are used to update the class, object and interface maps.
  val temp_methodMap: collection.mutable.Map[String, List[BooleanExpression]] = collection.mutable.Map()
  val temp_methodAccessMap,temp_fieldAccessMap: collection.mutable.Map[String, accessSpecifier] = collection.mutable.Map()
  val temp_methodParameterMap: collection.mutable.Map[String, collection.mutable.Map[String, BooleanExpression]] = collection.mutable.Map()
  val temp_fieldMap: collection.mutable.Map[String, BooleanExpression] = collection.mutable.Map()

  //Object Maps are used to update the values corresponding to the object from the maps of class, interface.
  val objectFieldMap : collection.mutable.Map[String, collection.mutable.Map[objectScope, collection.mutable.Map[String, BooleanExpression]]] = collection.mutable.Map()
  val objectFieldAccessMap, objectMethodAccessMap : collection.mutable.Map[String, collection.mutable.Map[objectScope, collection.mutable.Map[String, accessSpecifier]]] = collection.mutable.Map()
  val objectMethodMap :  collection.mutable.Map[String, collection.mutable.Map[objectScope, collection.mutable.Map[String, List[BooleanExpression]]]] = collection.mutable.Map()
  val objectParameterMap :  collection.mutable.Map[String, collection.mutable.Map[objectScope, collection.mutable.Map[String, collection.mutable.Map[String, BooleanExpression]]]] = collection.mutable.Map()

  //logicGateMap holds the value of logicGates and corresponding values
  val logicGateMap : collection.mutable.Map[ String,BooleanExpression] =collection.mutable.Map()
  //inputGateMap holds the value of logicGates and their inputs
  val inputGateMap: collection.mutable.Map[String,collection.mutable.Map[String, BooleanExpression]] = collection.mutable.Map()
  //Stacks are used to hold the logicGate, object, method or exception in execution.
  val logicGateStack, objectStack, methodStack, exceptionStack : collection.mutable.Stack[String]  = collection.mutable.Stack[String]()
  //The exception Map stores the reasons for the exception.
  val exceptionMap : collection.mutable.Map[String, String] = collection.mutable.Map[String, String]()
  //caughtList stores the caught exceptions
  val caughtList : collection.mutable.ListBuffer[String]  = collection.mutable.ListBuffer[String]()
  //Input is used hold the name of the input to the logicGate.
  val env : collection.mutable.Map[String,Boolean] = collection.mutable.Map("A"->true)
  case class Input(name: String)
  //LogicGate is used hold the name of logicGate.
  case class LogicGate(name: String)
  //accessSpecifiers are used to specify the access and scope of the fields or methods declared in a class.
  enum accessSpecifier:
    case private_access
    case public_access
    case protected_access
  //objectScope is used to separate the own, inherited and implemented fields and methods in a object.
  enum objectScope:
    case own
    case inherited
    case implemented
  //executeMethod is used execute a method
  def executeMethod(statements : List[BooleanExpression]) : BooleanExpression =
    val resultStack : collection.mutable.Stack[BooleanExpression]  = collection.mutable.Stack[BooleanExpression]()
    for statement <- statements do
      resultStack.push(statement.operate)
    resultStack.top

  //interfaceMethodCheck is used to check if the methods in a given interface are implemented in the given class.
  def interfaceMethodCheck(name_class: String, implement: String): Unit =
    println("Method Map :")
    println(methodMap(name_class).keySet)
    println("Interface method Map :")
    println(interfaceMethodMap(implement).keySet)
    for (iMethod, iValue) <- interfaceMethodMap(implement) do
      println(iMethod)
      println(!methodMap(name_class).keySet.contains(iMethod))
      if !methodMap(name_class).contains(iMethod) then
        throw new Exception("Abstract Method" + iMethod + " not implemented in Class." + "Value" + iValue)

  //updating_tempMethodMaps is used update temporary method maps which are later used to declare values for the class, object and interface maps
  def updating_tempMethodMaps(methodList : List[BooleanExpression.Method]) : Any =
    temp_methodMap.clear()
    temp_methodAccessMap.clear()
    temp_methodParameterMap.clear()

    for BooleanExpression.Method(name_method, access, parameters, value) <- methodList do
      if temp_methodMap.contains(name_method) then throw new Exception("Duplicate field name.")
      else
        temp_methodMap.update(name_method, value)
        temp_methodAccessMap.update(name_method, access)
        temp_methodParameterMap.update(name_method, parameters)

  //updating_tempMethodMaps is used update temporary field maps which are later used to declare values for the class, object and interface maps
  def updating_tempFieldMaps(fieldList : List[BooleanExpression.Field]) : Any =
    temp_fieldMap.clear()
    temp_fieldAccessMap.clear()
    for BooleanExpression.Field(name_field: String, access: accessSpecifier, value: BooleanExpression) <- fieldList do
      if temp_fieldMap.contains(name_field) then throw new Exception("Duplicate field name.")
      else
        temp_fieldMap.update(name_field, value)
        temp_fieldAccessMap.update(name_field, access)

  //TestGate is used to test if the value of the logicgate is as expected
  def TestGate(gate : LogicGate,expected : Boolean) : Boolean =
    logicGateStack.push(gate.name)
    val temp :BooleanExpression = logicGateMap.getOrElse(gate.name,throw new Exception(gate.name))

    if temp.eval == expected then
      logicGateStack.pop
      true
    else
      logicGateStack.pop
      false

  //ifStack and ifExecStack are used to define if then else if and else statements in order.
  //resultStack stores the result of the operation.
  val ifStack : collection.mutable.Stack[ifVal] = collection.mutable.Stack[ifVal]()
  val ifExeStack : collection.mutable.Stack[Boolean] = collection.mutable.Stack[Boolean]()

  //ifVal is the enum used to store the latest control condition executed.
  enum ifVal:
    case IF
    case ELSEIF
    case THEN
    case ELSE

  //scope2 is used to define a scope or block of code
  def scope2(name : String, block : => BooleanExpression) : Boolean =
    println(name)
    block.operate
    true
  //CatchException catches the thrown exception
  def CatchException(name: String, block: => BooleanExpression, catchBlock: BooleanExpression): BooleanExpression =
    println("Trying to catch exception :"+name)
    block

    catchBlock.operate

  //IF THEN ELSEIF and ELSE are functions to work in group like the a if else block.
  //It internally uses the stacks to achieve it.
  def IF(condition: => Boolean): Boolean =
    ifStack.push(ifVal.IF)
    if condition then
      ifExeStack.push(true)
    else
      ifExeStack.push(false)
    true
  def THEN(statements: => BooleanExpression) : Boolean =
    if ifExeStack.nonEmpty then
      println(ifStack.top == ifVal.IF)
      if !(ifStack.top == ifVal.IF || ifStack.top == ifVal.ELSEIF) then
        throw new Exception("IF or ELSE IF should be preceded")
      ifStack.push(ifVal.THEN)
      if ifExeStack.top then
        statements
        while ifExeStack.nonEmpty do
          ifExeStack.pop
    true
  def ELSEIF(condition: => Boolean) : Boolean =
    if !(ifStack.top == ifVal.THEN) then
      throw new Exception("Misplaced ELSEIF")
    ifStack.push(ifVal.ELSEIF)
    if ifExeStack.nonEmpty then
      if condition then
        ifExeStack.push(true)
      else
        ifExeStack.push(false)
    true
  def ELSE(statements: => BooleanExpression) : Boolean =
    println(ifVal.THEN)

    if !(ifStack.top == ifVal.THEN) then
      throw new Exception("Misplaced ELSE")
    if ifExeStack.nonEmpty then
      statements
      while ifExeStack.nonEmpty do
        ifExeStack.pop
    true
  //WHILE is the function to implement a loop for the language.
  def WHILE(condition : => Boolean, statements : => BooleanExpression) : Boolean =
    if condition then
      statements
      WHILE(condition,statements)
    true
  def optimizer : BooleanExpression => BooleanExpression =
    (operator:BooleanExpression) => operator match
      case  NOT( o1 : BooleanExpression) => optimizerNOT(NOT(o1))
      case Variable(name : String) => matchCase(Variable(name))
      case AND(o1 : BooleanExpression, o2 : BooleanExpression) => optimizerAND(AND(o1,o2))
      case OR(o1: BooleanExpression, o2: BooleanExpression) => optimizerOR(OR(o1,o2))
      case NAND(o1: BooleanExpression, o2: BooleanExpression) => optimizerNAND(NAND(o1,o2))
      case NOR(o1: BooleanExpression, o2: BooleanExpression) => optimizerNOR(NOR(o1,o2))
      case XOR(o1: BooleanExpression, o2: BooleanExpression) => optimizerXOR(XOR(o1, o2))
      case XNOR(o1: BooleanExpression, o2: BooleanExpression) => optimizerXNOR(XNOR(o1,o2))
      case _ => throw new Exception("case not  optimized in optimizer")
  def optimizerNOT : BooleanExpression => BooleanExpression =
    (or : BooleanExpression) => or match
      case NOT(a1 : BooleanExpression) =>
        val o1 = matchCase(a1)
        o1 match
          case Value(true) => Value(false)
          case Value(false) => Value(true)
          case NOT( o2 : BooleanExpression) =>
            o2
          case o2 : BooleanExpression =>
            NOT(o2)
      case _ => throw new Exception("abnormal behavior")


  def optimizerOR : BooleanExpression => BooleanExpression =
    (or : BooleanExpression) => or match
      case BooleanExpression.OR(a1 : BooleanExpression, a2 : BooleanExpression) =>
        val o1 = matchCase(a1)
        val o2 = matchCase(a2)
        if o1 == BooleanExpression.Value(true) then BooleanExpression.Value(true)
        else if o1 == BooleanExpression.Value(false) then
          if o2 == BooleanExpression.Value(false) then
            BooleanExpression.Value(false)
          else
            o2
        else if o2 == BooleanExpression.Value(true) then
        BooleanExpression.Value(true)
        else if o2 == BooleanExpression.Value(false) then
        o1
        else
          BooleanExpression.OR(o1, o2)
      case _ => throw new Exception("incorrect expression")

  def optimizerAND: BooleanExpression => BooleanExpression =
    (or: BooleanExpression) => or match
      case AND(a1: BooleanExpression, a2: BooleanExpression) =>
        val o1 = matchCase(a1)
        val o2 = matchCase(a2)
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

  def optimizerNAND: BooleanExpression => BooleanExpression =
    (or: BooleanExpression) => or match
      case NAND(a1: BooleanExpression, a2: BooleanExpression) =>
        val o1 = matchCase(a1)
        val o2 = matchCase(a2)
        if o1 == Value(false) then
          Value(true)
        else if o1 == Value(true) then
          if o2 == Value(true) then
            Value(false)
          else
            NOT(o2).map(optimizerNOT)
        else if o2 == Value(false) then
          Value(true)
        else if o2 == Value(true) then
          NOT(o1).map(optimizerNOT)
        else
          NAND(o1, o2)
      case _ => throw new Exception("incorrect expression")


  def optimizerNOR: BooleanExpression => BooleanExpression =
    (or: BooleanExpression) => or match
      case NOR(a1: BooleanExpression, a2: BooleanExpression) =>
        val o1 = matchCase(a1)
        val o2 = matchCase(a2)
        if o1 == Value(true) then
          Value(false)
        else if o1 == Value(false) then
          if o2 == Value(false) then
            Value(true)
          else
            NOT(o2).map(optimizerNOT)
        else if o2 == Value(true) then
          Value(false)
        else if o2 == Value(false) then
          NOT(o1).map(optimizerNOT)
        else
          NOR(o1, o2)
      case _ => throw new Exception("abnormal behavior")


  def optimizerXOR : BooleanExpression => BooleanExpression =
    (or: BooleanExpression) => or match
      case XOR(a1: BooleanExpression, a2: BooleanExpression) =>
        val o1 = matchCase(a1)
        val o2 = matchCase(a2)
        if o1 == Value(false) then
          o2
        else if o1 == Value(true) then
          NOT(o2).map(optimizerNOT)
        else if o2 == Value(false) then
          o1
        else if o2 == Value(true) then
          NOT(o1).map(optimizerNOT)
        else
          XOR(o1, o2)

      case _ => throw new Exception("incorrect expression")

  def optimizerXNOR  : BooleanExpression => BooleanExpression =
    (or: BooleanExpression) => or match
      case XNOR(a1: BooleanExpression, a2: BooleanExpression) =>
        val o1 = matchCase(a1)
        val o2 = matchCase(a2)
        if o1 == Value(false) then
          NOT(o2).map(optimizerNOT)
        else if o1 == Value(true) then
          o2
        else if o2 == Value(false) then
          NOT(o1).map(optimizerNOT)
        else if o2 == Value(true) then
          o1
        else
          XNOR(o1, o2)
      case _ => throw new Exception("incorrect expression")










  //The function matchCase is used to find if the environment table env has the key, else
  // it throws an error.
  def matchCase(x : BooleanExpression) : BooleanExpression =
    x match
      case BooleanExpression.Variable(name) =>
        try {

          BooleanExpression.Value(env.getOrElse(name, throw new Exception("contains not works properly")))
        } catch {
          case foo: Exception =>
            BooleanExpression.Variable(name)
          case bar: RuntimeException =>
            BooleanExpression.Variable(name)
        }

      case _ =>
        x.operate

  enum BooleanExpression:
    //Cases for creating and managing classes, objects, interfaces, exceptions with names related to their actual function.
    case get_Field(name_field: String)
    case set_Field(name: String, value: BooleanExpression)
    case invokeMethod(name_field: String, parameters: collection.mutable.Map[String, BooleanExpression])
    case setParameter(name: String, value: BooleanExpression)
    case getParameter(name: String)
    case Field(name: String, access: accessSpecifier, value: BooleanExpression)
    case Method(name: String, access: accessSpecifier, parameters: collection.mutable.Map[String, BooleanExpression], value: List[BooleanExpression])
    case ClassDef(name: String, isAbstract: Boolean, constructor: List[BooleanExpression], fields: List[Field], method: List[Method], inherits: String, implements: List[String])
    case NewObject(name: String, classType: String)
    case Object(name: String, action: BooleanExpression)
    case abstractMethod()
    case Reason(name : String)
    case exceptionClassDef(name : String, reason : BooleanExpression.Reason)
    case Catch(name : String, thenClause : BooleanExpression, reason : BooleanExpression.Reason)
    case throwException(name : String)
    case interface(name: String, fields: List[Field], method: List[Method], inherits: List[String])

    //Declaring the Boolean Functions with their names related to their actual operation

    val resultStack : collection.mutable.Stack[Boolean] = collection.mutable.Stack[Boolean]()

    case Variable(name : String)
    case input(state : String)
    case Value(v: Boolean)
    case gate_Value(l: LogicGate)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression )
    case AND(o1: BooleanExpression, o2: BooleanExpression )
    case NAND(o1: BooleanExpression, o2: BooleanExpression )
    case NOR(o1: BooleanExpression , o2: BooleanExpression )
    case XOR(o1: BooleanExpression , o2: BooleanExpression )
    case XNOR(o1: BooleanExpression, o2: BooleanExpression )
    case assign(inputClass : Input | LogicGate, value : BooleanExpression)
    case scope(logicGate: LogicGate, action : BooleanExpression)

    case PRINT(statement : BooleanExpression)

    //Using function eval to evaluate the Boolean Functions declared under BooleanExpression.
    def eval: Boolean =
    //empty Stack implies there are no exceptions, so the operations can be executed.
      if exceptionStack.isEmpty then
        this match
          case Value(x: Boolean) => resultStack.push(x)

          case NOT(o1) => resultStack.push(!o1.eval)
          case OR(o1, o2) => resultStack.push(o1.eval | o2.eval)
          case AND(o1, o2) => resultStack.push(o1.eval & o2.eval)
          case NAND(o1, o2) => resultStack.push(!(o1.eval & o2.eval))
          case NOR(o1, o2) => resultStack.push(!(o1.eval | o2.eval))
          case XOR(o1, o2) => resultStack.push(o1.eval ^ o2.eval)
          case XNOR(o1, o2) => resultStack.push(!(o1.eval ^ o2.eval))
          case input(c: String) => resultStack.push(inputGateMap(logicGateStack.top)(c).eval)
          case gate_Value(gate: LogicGate) =>
            logicGateStack.push(gate.name)
            val result = logicGateMap.getOrElse(gate.name, throw new Exception(gate.name)).eval
            logicGateStack.pop
            resultStack.push(result)
          case _ => throw new Exception("Matching case not found under eval.")
        val evalResult = resultStack.top
        resultStack.clear()
        evalResult
      else
        this match
          case _ => throw new Exception("Exception Exists")
    //Using function operate to manage creation of classes, objects and interfaces.
    def operate : BooleanExpression =
      //The BooleanFunctions are again rewritten to support the operations of the boolean functions
      //with operate function.
      if exceptionStack.isEmpty then
        this match
          //Value(x) is an identity function used to match the datatype
          case Value(x: Boolean) => Value(x)
          //Variable is used to find the key in the environment table
          case Variable(name : String) => matchCase(Variable(name))
          //NOT is used to return a partial function of NOT expression
          case NOT(o1) =>
            val tempX = matchCase(o1)
            NOT(tempX).map(optimizerNOT)
          //OR is used to return a partial function of OR expression
          case OR(a1, a2) =>
            val tempX = matchCase(a1)
            val tempY = matchCase(a2)
            OR(tempX,tempY).map(optimizerOR)

          //AND is used to return a partial function of AND expression
          case AND(o1, o2) =>
            val tempX = matchCase(o1)
            val tempY = matchCase(o2)
            AND(tempX,tempY).map(optimizerAND)
          //NAND is used to return a partial function of NAND expression
          case NAND(o1, o2) =>
            val tempX = matchCase(o1)
            val tempY = matchCase(o2)
            NAND(tempX,tempY).map(optimizerNAND)
          //NOR is used to return a partial function of NOR expression
          case NOR(o1, o2) =>
            val tempX = matchCase(o1)
            val tempY = matchCase(o2)
            NOR(tempX,tempY).map(optimizerNOR)
          //XOR is used to return a partial function of XOR expression
          case XOR(o1, o2) =>
            val tempX = matchCase(o1)
            val tempY = matchCase(o2)
            XOR(tempX,tempY).map(optimizerXOR)
          //XNOR is used to return a partial function of XNOR expression
          case XNOR(o1, o2) =>
            val tempX = matchCase(o1)
            val tempY = matchCase(o2)
            XNOR(tempX,tempY).map(optimizerXNOR)

          case gate_Value(l: LogicGate) => Value(gate_Value(l).eval)
          case PRINT(statement : BooleanExpression) =>
            println(statement)
            Value(true)
          //Checks if the thrown exception is valid and stores it in the stack
          case throwException(name : String) =>
            if exceptionMap.contains(name) then
              println("Exception thrown : " + name)
              exceptionStack.push(name)
              Value(true)
            else throw new Exception("Exception not defined.")
          //The case is used to define a exception.
          case exceptionClassDef(name : String, reason : BooleanExpression.Reason) =>
            exceptionMap.put(name,reason.name)
            Value(true)


          //case interface is used to create an interface and assigning the maps with the corresponding values.
          case interface(name_interface, interface_fields, methods, inherits) =>
            println("creating interface")
            if interfaceList.contains(name_interface) then throw new Exception("Interface name already defined.")
            else interfaceList.append(name_interface)
            updating_tempFieldMaps(interface_fields)
            interfaceFieldMap.update(name_interface, temp_fieldMap.clone())
            interfaceFieldAccessMap.update(name_interface, temp_fieldAccessMap.clone())
            updating_tempMethodMaps(methods)
            interfaceMethodMap.update(name_interface, temp_methodMap.clone())
            interfaceMethodAccessMap.update(name_interface, temp_methodAccessMap.clone())
            interfaceMethodParameterMap.update(name_interface, temp_methodParameterMap.clone())
            interfaceInheritanceMap.update(name_interface,inherits)
            Value(true)
          //case ClassDef is used to create a class and assigning the maps with the corresponding values.
          case ClassDef(name_class, isAbstract, constructor, fields, methods, inherits, implements) =>
            println("Creating Class " + name_class)
            if classList.contains(name_class) then throw new Exception("Class name already defined.")
            else classList.append(name_class)
            isAbstractMap.put(name_class,isAbstract)
            constructorMap.put(name_class,constructor)
            updating_tempFieldMaps(fields)
            fieldMap.update(name_class, temp_fieldMap.clone())
            fieldAccessMap.update(name_class, temp_fieldAccessMap.clone())
            updating_tempMethodMaps(methods)
            methodMap(name_class)=temp_methodMap.clone()
            methodAccessMap.update(name_class, temp_methodAccessMap.clone())
            methodParametersMap.update(name_class, temp_methodParameterMap.clone())
            inheritanceMap.put(name_class,inherits)
            if inherits!= null && isAbstractMap(inherits) then
              for (method,value) <- methodMap(inherits) do
                if !methodMap(name_class).contains(method) then throw new Exception("Abstract Method" + method + " not implemented in Class." + "Value" + value)
            else if  inherits!=null then
                for (iMethod, iValue) <- methodMap(inherits) do

                  if iValue==List(BooleanExpression.abstractMethod()) then
                    println(iMethod + "    " + iValue )
                    if !methodMap(name_class).contains(iMethod) then throw new Exception("Abstract Method" + iMethod + " not implemented in Class.")

            interfaceClassBind.put(name_class,implements)
            println("Interface implemented :"+implements)
            if implements!= null then
              for implement <- implements do
                interfaceMethodCheck(name_class,implement)
                if interfaceInheritanceMap(implement) != null then
                  for inheritedInterface <- interfaceInheritanceMap(implement) do
                    interfaceMethodCheck(name_class,inheritedInterface)

            Value(true)
          //case NewObject is used to create an object with the given classType.It also looks for the inherited class and implemented interfaces.
          //The NewObject also calls the constructor of the class.
          case NewObject(name_object: String, classTypeName: String) =>
            println("Creating Object :"+name_object+" for class :"+classTypeName)
            if !classList.contains(classTypeName) then throw new Exception("Class name not found."+classTypeName)
            if isAbstractMap(classTypeName) then throw new Exception("Object cannot be created for abstract class.")
            if objectList.contains(name_object) then throw new Exception("Object name already defined."+objectList)
            else objectList.append(name_object)
            objectTypeMap.update(name_object, classTypeName)
            //own fields
            objectFieldMap.update(name_object,collection.mutable.Map(objectScope.own->fieldMap(classTypeName)).clone())
            objectFieldAccessMap.update(name_object,collection.mutable.Map(objectScope.own->fieldAccessMap(classTypeName)).clone())
            objectFieldMap(name_object).update(objectScope.inherited,collection.mutable.Map())
            objectFieldAccessMap(name_object).update(objectScope.inherited,collection.mutable.Map())
            objectMethodMap.update(name_object,collection.mutable.Map(objectScope.own->methodMap(classTypeName)).clone())
            objectMethodAccessMap.update(name_object,collection.mutable.Map(objectScope.own->methodAccessMap(classTypeName)).clone())
            objectParameterMap.update(name_object,collection.mutable.Map(objectScope.own->methodParametersMap(classTypeName)).clone())

            objectMethodMap(name_object).update(objectScope.inherited,collection.mutable.Map())
            objectMethodAccessMap(name_object).update(objectScope.inherited,collection.mutable.Map())
            objectParameterMap(name_object).update(objectScope.inherited,collection.mutable.Map())

            if inheritanceMap(classTypeName)!=null then
              if !classList.contains(inheritanceMap(classTypeName)) then throw new Exception("Inherited Class not found.")
              objectFieldMap(name_object).update(objectScope.inherited,fieldMap(inheritanceMap(classTypeName)).clone())
              objectFieldAccessMap(name_object).update(objectScope.inherited,fieldAccessMap(inheritanceMap(classTypeName)).clone())

              objectMethodMap(name_object)(objectScope.inherited)=methodMap(classTypeName).clone()
              objectMethodAccessMap(name_object)(objectScope.inherited)=methodAccessMap(classTypeName).clone()
              objectParameterMap(name_object)(objectScope.inherited)=methodParametersMap(classTypeName).clone()

            objectFieldMap(name_object).update(objectScope.implemented,collection.mutable.Map())
            objectFieldAccessMap(name_object).update(objectScope.implemented,collection.mutable.Map())


            if interfaceClassBind(classTypeName)!=null then
              for implementedInterfaces <- interfaceClassBind(classTypeName) do
                if !interfaceList.contains(implementedInterfaces) then throw new Exception("Implemented Interface not found")
                objectFieldMap(name_object).update(objectScope.implemented,interfaceFieldMap(implementedInterfaces).clone())
                objectFieldAccessMap(name_object).update(objectScope.implemented,interfaceFieldAccessMap(implementedInterfaces).clone())
            objectStack.push(name_object)
            println("Calling constructor of the class : " + classTypeName)
            executeMethod(constructorMap(classTypeName))
            objectStack.pop
            println("Object   :"+ name_object+" created.")
            Value(true)
          //getField is a statement used in class constructor or methods to get the field of the corresponding object.
          case get_Field(name_field: String) =>
            if objectFieldMap(objectStack.top)(objectScope.own).contains(name_field) then
              objectFieldMap(objectStack.top)(objectScope.own)(name_field)

            else if objectFieldMap(objectStack.top)(objectScope.inherited).contains(name_field) then
              if objectFieldAccessMap(objectStack.top)(objectScope.inherited)(name_field) !=accessSpecifier.private_access then
                objectFieldMap(objectStack.top)(objectScope.inherited)(name_field)
              else throw new Exception("The field :" + name_field + "is private. It cannot be accessed from the object :" + objectStack
              .top)

            else if objectFieldMap(objectStack.top)(objectScope.implemented).contains(name_field) then
              if objectFieldAccessMap(objectStack.top)(objectScope.implemented)(name_field) !=accessSpecifier.private_access then
                objectFieldMap(objectStack.top)(objectScope.implemented)(name_field)
              else throw new Exception("The field :" + name_field + "is private. It cannot be accessed from the object :" + objectStack
                .top)

            else
              throw new Exception(name_field + " not found in fields of the object :" + objectStack.top )
          //setField is a statement used in class constructor or methods to set the field of the corresponding object.
          case set_Field(name_field: String, value: BooleanExpression) =>
            println("Set Field Called" + value)
            val temp = value.operate
            if objectFieldMap(objectStack.top)(objectScope.own).contains(name_field) then
              objectFieldMap(objectStack.top)(objectScope.own).update(name_field,temp)
            else if objectFieldMap(objectStack.top)(objectScope.inherited).contains(name_field) then
              if objectFieldAccessMap(objectStack.top)(objectScope.inherited)(name_field) ==accessSpecifier.private_access then
                throw new Exception("Field :" + name_field + "is private and cannot be accessed from object"+ objectStack.top)
              objectFieldMap(objectStack.top)(objectScope.inherited)(name_field) = temp
            else if objectFieldMap(objectStack.top)(objectScope.implemented).contains(name_field) then
              if objectFieldAccessMap(objectStack.top)(objectScope.implemented)(name_field) ==accessSpecifier.private_access then
                throw new Exception("Field :" + name_field + "is private and cannot be accessed from object"+ objectStack.top )
              objectFieldMap(objectStack.top)(objectScope.implemented)(name_field) = temp
            else
              throw new Exception(name_field + " not found in fields of the object :" + objectStack.top)
            temp
          //setParameter is a statement used in methods to set Parameter of the current method.
          case setParameter(parameter_name: String, value: BooleanExpression) =>
            val temp = value.operate
            if objectParameterMap(objectStack.top)(objectScope.own).contains(methodStack.top) then
              if !objectParameterMap(objectStack.top)(objectScope.own)(methodStack.top).contains(parameter_name) then
                throw new Exception("Parameter :" + parameter_name + "is not present in method : " + methodStack.top + " of object :" + objectStack.top)
              objectParameterMap(objectStack.top)(objectScope.own)(methodStack.top)(parameter_name) = temp
            else if objectParameterMap(objectStack.top)(objectScope.inherited).contains(methodStack.top) then
              if !objectParameterMap(objectStack.top)(objectScope.inherited)(methodStack.top).contains(parameter_name) then
                throw new Exception("Parameter :" + parameter_name + "is not present in method : " + methodStack.top + " of object :" + objectStack.top)
              objectParameterMap(objectStack.top)(objectScope.inherited)(methodStack.top)(parameter_name) = temp
            else throw new Exception("Method not found in parameter map.")
            temp
          //getParameter is a statement used in methods to get Parameter of the current method.
          case getParameter(parameter_name: String) =>
            if objectParameterMap(objectStack.top)(objectScope.own).contains(methodStack.top) then
              if !objectParameterMap(objectStack.top)(objectScope.own)(methodStack.top).contains(parameter_name) then throw new Exception("Parameter :" + parameter_name + "is not present in method : "+ methodStack.top +" of object :"+ objectStack.top )
              objectParameterMap(objectStack.top)(objectScope.own)(methodStack.top)(parameter_name)
            else if objectParameterMap(objectStack.top)(objectScope.inherited).contains(methodStack.top) then
              if !objectParameterMap(objectStack.top)(objectScope.inherited)(methodStack.top).contains(parameter_name) then throw new Exception("Parameter :" + parameter_name + "is not present in method : " + methodStack.top + " of object :" + objectStack.top)
              objectParameterMap(objectStack.top)(objectScope.inherited)(methodStack.top)(parameter_name)
            else throw new Exception("Method not found in parameter map.")
          //invokeMethod is used in constructor, method, objects to invoke a method.
          case invokeMethod(name_method: String, parameters: collection.mutable.Map[String, BooleanExpression]) =>
            println()
            println("Invoking Method" + name_method)
            println()
            if objectMethodMap(objectStack.top)(objectScope.own).contains(name_method) then
              methodStack.push(name_method)
              objectParameterMap(objectStack.top)(objectScope.own)(name_method)=parameters
              val result = executeMethod(objectMethodMap(objectStack.top)(objectScope.own)(name_method))
              methodStack.pop
              result
            else if objectMethodMap(objectStack.top)(objectScope.inherited).contains(name_method) then
              methodStack.push(name_method)
              objectParameterMap(objectStack.top)(objectScope.inherited)(name_method)=parameters
              val result = executeMethod(objectMethodMap(objectStack.top)(objectScope.inherited)(name_method))
              methodStack.pop
              result
            else throw new Exception(name_method + "not found in the object : "+ objectStack.top)
          //Object is used to instantiate the object for a class and operate on it.
          case Object(name_object : String, action: BooleanExpression) =>
              if !objectList.contains(name_object) then throw new Exception("Object not created."+name_object)
              println("Object List Checked.")
              objectStack.push(name_object)
              println("Object : " + name_object + "  pushed.")
              println("Action "+ action )
              val result = action.operate
              println("result     :" + result  )
              objectStack.pop
              println("Object : " + name_object + "  deleted from stack.")
              result
          case assign(inputClass, function: BooleanExpression) =>
              function match
                case assign(nested_input,nested_function) =>
                  assign(nested_input,nested_function).operate
                case scope(nested_gate, nested_function) =>
                  scope(nested_gate, nested_function).operate
                case _ =>
                  inputClass match
                    case LogicGate(name) =>
                      logicGateMap.put(name,function)
                      logicGateMap(name)
                    case Input(name) =>
                      if inputGateMap.contains(logicGateStack.top) then inputGateMap(logicGateStack.top)(name)=function
                      else inputGateMap.put(logicGateStack.top,collection.mutable.Map(name->function))
                      println("Assigning Input  :"+ name + "  , value = " + function.eval)
                      inputGateMap(logicGateStack.top)(name)
          case scope(logicGate: LogicGate, action : BooleanExpression) =>
              logicGateStack.push(logicGate.name)
               println("Entering the scope   :"+logicGateStack.top)
              val result = action.operate
              println("Exiting the scope    :"+logicGateStack.top)
              logicGateStack.pop
              result
          case _ =>
            throw new Exception("Matching case not found under operate.")

      else
        this match
          //When the exception Stack has an exception, catch case is used to catch the exception.
          case Catch(name : String, thenClause : BooleanExpression, reason : BooleanExpression) =>
            println("name   :" + name + "   exception " + exceptionStack.top)
            if name==exceptionStack.top then
              println("Exception Caught"+ name + "Reason" + reason.name)
              thenClause.operate
              caughtList.append(reason.name)
              exceptionStack.pop
              Value(true)
            else
              Value(false)
          case _ => null
    //map is used to simply the partial functions evaluated by operate.
    //The cases are optimized such that the expressions can be evaluated without any boolean operators.
    def map(f : BooleanExpression => BooleanExpression ) : BooleanExpression =
      f(this)
      //case _ => throw new Exception("Match not found in map def")
  @main def runIT(): Unit =
    println(Sets_Classes)
    println(env.getOrElse("A",throw new Exception("contains not works properly")))
