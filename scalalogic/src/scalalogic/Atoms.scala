package scalalogic

/**
 * A trait for simple atom terms
 */
trait Atoms extends ScalaLogic{
	 
	case class Atom(symbol:Symbol) extends NonVarTerm(0) {
		override def toString = symbol.name
		override def unify(other:Term, unifier:MutableUnifier) = other match{
		  case Atom(otherSymbol) => (symbol == otherSymbol)
		  case v:Var => v.unifyWithNonVar(this,unifier)
		  case _ => false
		}
	}
 
}
