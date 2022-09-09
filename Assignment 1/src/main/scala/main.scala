package scala



object BoolExp:
  enum BooleanExpression:
    case Value(v:Boolean)
    case NOT(o1: BooleanExpression)
    case OR(o1: BooleanExpression, o2: BooleanExpression)
    case AND(o1: BooleanExpression, o2: BooleanExpression)
    case NAND(o1: BooleanExpression, o2: BooleanExpression)
    case NOR(o1: BooleanExpression, o2: BooleanExpression)
    case XOR(o1: BooleanExpression, o2: BooleanExpression)
    case XNOR(o1: BooleanExpression, o2: BooleanExpression)

    def eval: Boolean = this match
      case Value(x:Boolean) => x
      case NOT(o1) => !o1.eval
      case OR(o1, o2) => o1.eval | o2.eval
      case AND(o1, o2) => o1.eval & o2.eval
      case NAND(o1, o2) => !(o1.eval & o2.eval)
      case NOR(o1, o2) => !(o1.eval | o2.eval)
      case XOR(o1, o2) => o1.eval ^ o2.eval
      case XNOR(o1,o2) => !(o1.eval ^ o2.eval)

  @main def runBoolExp =
    import BooleanExpression.*
    //    Add(Add(Add(...)))
    println("The value is : ")
    println(XOR(Value(true), NOT(Value(false))).eval)