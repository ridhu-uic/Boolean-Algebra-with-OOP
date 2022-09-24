LogicGates and Boolean Expressions


The object is to implement a domain-specific language (DSL) using Scala for writing and evaluating set operation expressions for designers of the digital logic to implement logic gates.


The user can implement and evaluate logic gates using the the following functions.
1.Assign
2.Scope
3.TestGate


1.Assign is used to assign BooleanExpression to the logic gates.
Ex : assign(LogicGate("logicGate1"),XOR(Value(true),input_Value(LogicGate("logicGate1"),"A")))


Inputs: parameter 1 : LogicGate, parameter 2 : Boolean Expression


input_Value is used to pass a input value, it checks the input in the particular logicGate mentioned to it.


2.Scope is used to assign inputs to the particular logic gate
scope(LogicGate("logicGate1"),Input("A"),Value(true))


Inputs : parameter 1 : LogicGate, parameter 2 : Input, parameter 3 : Boolean Value to be assigned


3.TestGate is used to evaluate and compare the value with an expected value
TestGate(LogicGate("logicGate1"),true)


Input : parameter 1 : LogicGate 2.Expected Value


How to Install and Run the package

Step 1 : Download or clone the git repository.
Step 2 : Download and install  sbt using the following link.   https://www.scala-sbt.org/download.html
Step 3 : Run sbt from the downloaded repository.
Step 4 : Run the following command through terminal opened from the repository.
        sbt clean compile test
        sbt clean compile run