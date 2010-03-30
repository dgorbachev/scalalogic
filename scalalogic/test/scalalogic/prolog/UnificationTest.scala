package scalalogic.prolog

import scalalogic._

object UnificationTest extends PrologParser with Application{

	object DefaultScalaLogic extends ScalaLogic with DSL
 
	import DefaultScalaLogic._

	override val scalaLogic = DefaultScalaLogic

	val a = parseAll(predicate,"male(X)").get;
	val b = parseAll(predicate,"male(xyz)").get;
	println(a.unify(b).get)

	val c = parseAll(predicate,"male(X,[a,Y])").get;
	val d = parseAll(predicate,"male([a,Y],[a,b])").get;
	println(c.unify(d).get)
 
	val e = parseAll(predicate,"male([a,b,c|X])").get;
	val f = parseAll(predicate,"male([a|Y])").get;
	println(e.unify(f).get)
 
	println(('Head  :: 'Rest).unify(List(1)).get)
 
}
