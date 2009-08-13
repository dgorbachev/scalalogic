package scalalogic

import scala.collection._

/**
 * A trait that adds support for don't-care variables ("_")
 */
trait DontCares extends ScalaLogic {
	 
	object DontCare extends Var{
	    override val toString = "_"
	    override def addTo(vars:mutable.Set[Var]) = {}
	    override def substitute(f: Var=>Term) = this
		override def unify(other:Term, unifier:MutableUnifier) = true
		override def unifyWithNonVar(term:NonVarTerm, unifier:MutableUnifier) = true
	}

	//Alias
	def ^ = DontCare
 
}
