## **LogicGates and Boolean Expressions**

 
The objective is to implement a domain-specific language (DSL) using Scala for writing and evaluating set operation expressions for designers of the digital logic to implement logic gates.

The following concepts of object oriented Language are impentented in the language developed.

 1. Defining a Class with fields, methods, constructor, inherited class, implemented interfaces.
 2. Creating an Object for the Class.
 3. Concept of Inheritance and Access Specifiers
 4. Invoking Methods and Fields of both Base and Derived Class
 5. Concept of Abstract Class
 6. Concept of Interfaces
 7. Control Statements : IF, ELSEIF, THEN, ELSE and WHILE
 8. Exceptions : throwException, CatchException 
 9. Partially Evaluated Functions and Optimizers


**

## Features in detail.

**

 1. Defining a Class with fields, methods, constructor, inherited class, implemented interfaces.

	

	The ClassDef command is used to create a class Datatype. 
		    	**Command** : ClassDef(name : String, isAbstract : Boolean,
	    constructor : List[BooleanExpression],fields : List[Field],method :
	    List[Method],inherits : String,implements : List[String])

	  

			 1. 'name' represents the name of the datatype.
			 2. 'isAbstract' if true, it means it is a abstract class. else it is a non abstract class.
			 3. 'constructor' takes list of BooleanExpressions which are used to set the fields.It is called when an object is created.
			 4. 'fields' is a List of basic datatype Field. The Field has 'name', 'access specifier' (private,protected,public) and   'value' BooleanExpression.
			 5. 'method' is a List of basic datatype Method. The Field has 'name', 'access specifier' (private,protected,public) and 'value' List(BooleanExpression).The methods take List of Boolean Expressions and evaluate them.
			 6. 'inherits' is a String used to inherit another class datatype with the given name.
			 7. 'implements' is List of String which holds the string of interfaces that can be implemented to the class.	
 2.  Creation of an Object for the Class.
 The NewObject command is used to create a newObject of the given datatype with the given name.
	**Command** : NewObject(name : String, classType : String)        
        During the Object Creation, constructor is called. The Fields and Methods of base class, derived class and implemented interfaces are stored in corresponding maps with their access Specifiers.
 3. Invoking Methods and Fields of both Base and Derived Class
		 The Object Command is used to use the created object for evaluating the fields and Methods.
		**Command** : Object(name : String, action : BooleanExpression)
       
        3.1) Invoking a Method
        Command : invokeMethod (name_method)
        invokeMethod command invokes the method using the tables created during the object creation. The methods can be override.
        The methods are first checked in the child then it is checked in the base class.
        The private methods of the base class cannot be accessed from the object.
        The objectMethodMap stores the objectName -> ClassName (Base or Derived) -> List[Methods].This Map acts like a Virtual Dispatch Table.
        
        3.2) Getting a Field Value
        Command : get_Field(name_field: String)
        The command gets the value of the field in the object.

        3.3) Setting a Field Value
        Command : set_Field(name_field: String,value : BooleanExpression)
        The command sets the name_field in the object to the field to the value 'value'.
        
        3.4) Getting a parameter Value
        Command : getParameter(name_field: String)
        The command gets the value of the parameter of the currently executing method  in the object.
        
        3.5) Setting a parameter Value
        Command : setParameter(name_field: String,value : BooleanExpression)
        The command sets the value of the parameter of the currently executing method  in the object.
        
 4. Concepts of Inheritance and Access Specifiers 
			4.1) The Fields and Methods of a class can be inherited by another however multiple inheritance is not allowed.
			4.2) Only one class can inherited by another class. The check for cyclic inheritance will be added in future.2
			4.3) The access Specifiers help to restict the usage of the parent class methods and fields in the child class.
			4.4) The private cannot be accessed by the child class. Protected can be accessed through the methods and Public can be accessed by the objects and methods of the child class.	
 5. Concepts of Abstract Class 
 5.1) The abstract Class implements is like any non abstract class with isAbstarct equal to true.
5.2) The methods in the abstract class has to overridden in the derived class orelse an exception is thrown by the compiler.
5.3) An object cannot be created for the abstract class.The abstract Class is created with dataype ClassDef using an extra argument "isAbstract" (Boolean) in it.
 6. Concept of Interfaces
 The command interface is used to declare a interface.
 **Command** : interface(name: String, fields: List[Field], method: List[Method], inherits: List[String])
			 1. 'name' represents the name of the datatype.
			 2. 'fields' is a List of basic datatype Field. The Field has 'name', 'access specifier' (private,protected,public) and   'value' BooleanExpression.
			 3. 'method' is a List of basic datatype Method. The Field has 'name', 'access specifier' (private,protected,public) and 'value' List(BooleanExpression).The methods take List of Boolean Expressions and evaluate them.
			 4.  'inherits' is List of String which holds the string of interfaces that can be implemented to the class.
			 5. Interfaces are new datatype, it holds fields, methods and list of interfaces which can be extended by the interface.
			 6. The interfaces cannot have same method signature, if given the compiler throws and ambiguity error.
7.  Control Statements : IF, ELSEIF, THEN, ELSE and WHILE
			IF statement takes a condition (Boolean) as an input and checks if it is true or false. If the condition is true, the statements in the THEN function is executed or else ELSEIF or ELSE can be executed. ELSEIF takes condition (Boolean) as input and ELSE takes statements (Boolean Expression) as input.
			WHILE takes condition and statements as input. If the condition is true, then statements are executed and the while loop is recursed until it fails or stack overflows.	 
8. Exceptions:  throwException, CatchException
		exceptionClassDef(name : String, reason : BooleanExpression.Reason)
				exceptionCalssDef should be used with the operator .operate, it is used to define a new exception and the Reason for the exception using the datatype Reason which takes string as input.
		throwException(name : String)
				throwException throws exception when found.
		Catch(name : String, thenClause : BooleanExpression, reason : BooleanExpression.Reason)
				Catch is used tp catch the thrown exception. If the exception is caught, it executed the thenClause and prints the reason.
		CatchException(name: String, block: => BooleanExpression, catchBlock: BooleanExpression)
				CatchException is function which is used to define a scope (block), where a particular exception is expected. catchBlock :  takes the input as Catch case mentioned above and if the exception is caught, it proceeds like the catch case.
9. Partially Evaluated Functions and Optimizers
			The language developed supports partially evaluated functions which means the expressions can be partially evaluated without complete data. 
			eg : Variable("A") -> true, Variable("B") is not declared.
			OR(Variable("A"),Variable("B"))  ->  true as OR(true,x) -> true.
			Similar evaluation and optimization is done for all the Boolean Operators.

## Addressing Few Questions.

 1. Can a class/interface inherit from itself? 	
	 No. If a class inherits itself, then all the methods and fields are overwritten and it does     not provide any additional usage without super and this pointers which are not yet implemented in the language.
 2. Can an interface inherit from an abstract class with all pure methods? 
	 No. In the language implemented class and interfaces are treated as different
    entities, so an interface cannot inherit a class(also abstract
    class).
3. Can an interface implement another interface? 
	No but it can extend multiple interfaces. 
4. Can a class implement two or more different interfaces that declare methods with exactly the same signatures? 
	No. It leads to ambiguity. 
5. Can an abstract class inherit from another abstract class and implement interfaces where all interfaces and the abstract class have methods with the same
    signatures? 
    Yes. As the classes and interfaces are treated differently, this is possible however the methods of classes will be given first preference.
 6. Can an abstract class implement interfaces?
    Yes.Abstract class implent the interfaces.
  7. Can a class implement two or more interfaces that have methods whose signatures differ only in return types? 
	  No. It leads to ambiguity.
8.  Can an abstract class inherit from a concrete class? 
	No. It is not allowed to protect the abstract nature of the class.
9. Can an abstract class/interface be instantiated as anonymous concrete classes? No.Currently it is not possible but can be implemented in future.

## How to Install and Run the package

Step 1 : Download or clone the git repository.

Step 2 : Download and install  sbt using the following link.   https://www.scala-sbt.org/download.html

Step 3 : Run sbt from the downloaded repository.

Step 4 : Run the following command through terminal opened from the repository.
        sbt clean compile test
        sbt clean compile run
