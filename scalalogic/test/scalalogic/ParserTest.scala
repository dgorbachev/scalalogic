package scalalogic

object ParserTest extends PrologParser with Application{

	object DefaultScalaLogic extends ScalaLogic with NamedVars with DontCares with Atoms with Lists with Integers

	val scalaLogic = DefaultScalaLogic

	val theoryString = """
male(frank).
male(dean).
female(ella).
female(judy).
acts(frank).
acts(judy).
acts(dean).
sings(frank).
sings(judy).
sings(dean).
sings(ella).
person(X) :- male(X).
person(X) :- female(X).
actor(X) :- male(X),acts(X).
actress(X) :- female(X), acts(X).
sang_with(frank, judy).
sang_with(frank, dean).
	""";
 
	val theoryResult = parseAll(theory,theoryString);
    println("THEORY:")
	println(theoryResult)
	
	val goalResult = parseAll(formula,"sang_with(_,Y),female(Y)")
    println("GOAL:")
	println(goalResult)
 
	val solutions = theoryResult.get ?= goalResult.get
    if(solutions.isEmpty) println("NO SOLUTIONS")
    else{ 
      println("SOLUTIONS:")
      println(solutions mkString "\n")
    }

}