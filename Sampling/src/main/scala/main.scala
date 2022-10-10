import class_trial.ArithmeticExpression

import scala.collection.mutable.ListBuffer
object class_trial:

  enum accessSpecifier:
    case private_access
    case public_access
    case protected_access

  case class Field(name : String, access_specifier : accessSpecifier , value : ArithmeticExpression)
  case class Method(name : String, access_specifier : String , value : ArithmeticExpression)
  case class ClassBody(name : String, fields : collection.mutable.ListBuffer[Field], methods : List[Method])

  def extend_class(Derived_Class :ClassBody, Base_Class : ClassBody, access_Specifier : accessSpecifier ) =
    for(field_list<-Base_Class.fields)
      print("Field   :")
      println(field_list)
      if (field_list.access_specifier==accessSpecifier.public_access) then
          println("In public if : ")
          val field_to_add = field_list.copy(access_specifier = access_Specifier)
          Derived_Class.fields += field_to_add


      else if(field_list.access_specifier==accessSpecifier.protected_access)
        println("In protected if : ")
        if(access_Specifier==accessSpecifier.private_access) then
          val field_to_add = field_list.copy(access_specifier =accessSpecifier.private_access)
          Derived_Class.fields += field_to_add
          println(Derived_Class)
          print("Derived Class Fields")
          println(Derived_Class.fields)

        else
          val field_to_add = field_list.copy(access_specifier = accessSpecifier.protected_access)
          Derived_Class.fields += field_to_add
          println(Derived_Class)
          print("Derived Class Fields")
          println(Derived_Class.fields)
      println(Derived_Class)



  object Field {
    def apply(name : String, value : ArithmeticExpression): Field =  this(name,accessSpecifier.public_access,value)
    def apply(name : String): Field =  this(name,accessSpecifier.public_access,ArithmeticExpression.Value(0))
    def apply(name : String, access_specifier : accessSpecifier): Field =  this(name,access_specifier,ArithmeticExpression.Value(0))

    //def apply(name : String,access_specifier : String , value : ArithmeticExpression): Field =  this(name,access_specifier,value)
  }

  object Method {
    def apply(name: String, value: ArithmeticExpression): Method = this (name,"public", value)
  }


  enum ArithmeticExpression:
    case Value(v: Int)
    case Variables(name: String)
    case Add(o1: ArithmeticExpression | Int, o2: ArithmeticExpression)
    case Mult(o1: ArithmeticExpression, o2: ArithmeticExpression)
    case Scope(name: String, expressions: List[ArithmeticExpression])

    val EnvironmentTable: Map[String, ArithmeticExpression | Int] = Map("Karan" -> 1, "Xiao" -> 2)



    def eval: Int = this match
      case Variables(name) => EnvironmentTable.getOrElse(name, throw new Exception(name)) match
        case v: ArithmeticExpression => v.eval
        case v: Int => v
      case Value(x) => x
      case Add(o1, o2) => o1 match
        case v: ArithmeticExpression => v.eval
        case v: Int => v
          + o2.eval
      case Mult(o1, o2) => o1.eval * o2.eval




  @main def runIt() : Unit =
    import ArithmeticExpression.*
    val class1 = ClassBody("One",ListBuffer( Field("A",accessSpecifier.protected_access,Value(3)) ,Field("B",Value(20))),List(Method("adding",Add(Value(10),Value(20)))) )
    println("Declared")
    println(class1.fields)
    println("End")
    println(class1.fields.append(Field("C",accessSpecifier.protected_access,Value(2))))
    val list1 =class1.fields

    for(list2<-list1)
      println(list2.name)

    for(list2<-class1.methods)
      println(list2.value.eval)
    var class2 = ClassBody("Thambi",ListBuffer( Field("A1",accessSpecifier.protected_access,Value(3)) ,Field("B1",Value(20))),List(Method("adding1",Add(Value(10),Value(20)))) )
    extend_class(class2,class1,accessSpecifier.public_access)
    println(class2)
    println(class1)