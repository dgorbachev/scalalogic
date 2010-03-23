package scalalogic

import scala.collection._

/**
 * This could be "Numbers" if java.lang.Number had a rich wrapper that defines <,>,... Too bad?
 */
trait Integers extends ScalaLogic{
	 
	val i = 1
  
	case class Integer(number:Int) extends NonVarTerm(0) {
		override def toString = number.toString
		override def unify(other:Term, unifier:MutableUnifier) = other match{
		  case Integer(otherNumber) => (number == otherNumber)
		  case v:Var => v.unifyWithNonVar(this,unifier)
		  case _ => false
		}
	}
 
	class NumberPredicate(symbol:Symbol, op: (Int,Int)=>Boolean, left:Term, right:Term) 
			extends Predicate(symbol, List(left,right)){
		override def toString = left+symbol.name+right
		override def query(theory:Theory) = (left,right) match{
		  case (Integer(l),Integer(r)) => if(op(l,r)) Stream(immutable.Map.empty) else Stream.empty
		  case _ => throw new IllegalStateException(symbol.name+"2: Arguments are not sufficiently instantiated: "+toString)
		}
		
		override def substitute(f: Var=>Term) = new NumberPredicate(symbol, op, left.substitute(f),right.substitute(f))
		
		override def unify(goal:Predicate):Option[Unifier] = None
	}
 
	class < (left:Term,right:Term) extends NumberPredicate('<, _<_, left,right)
	class > (left:Term,right:Term) extends NumberPredicate('>, _>_, left,right)
	class <= (left:Term,right:Term) extends NumberPredicate('<=, _<=_, left,right)
	class >= (left:Term,right:Term) extends NumberPredicate('>=, _>=_, left,right)
 
}
