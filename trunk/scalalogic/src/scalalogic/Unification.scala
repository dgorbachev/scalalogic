package scalalogic

import scala.collection._
import jdd.bdd.BDD;

trait Unification extends ScalaLogic{
  
	class <>(left:Term, right:Term) 
			extends Predicate('neq, List(left,right)){
		
		override def toString = left+" neq "+right
  
		override def query(theory:Theory) =  left.unify(right) match {
			  case None => {
				    //println(left+" != "+right)
				    Stream(new Unifier(mutable.Map.empty))
			  	}
			  case Some(_) => {
				    //println(left+" == "+right)
				    Stream.empty
			    }
			}
  
		override def substitute(f: Var=>Term) = new <>(left.substitute(f),right.substitute(f))
		
		override def unify(goal:Predicate):Option[Unifier] = None
	
		override def bdd(theory:Theory,env:BDD) = left.unify(right) match {
			  case None => 1
			  case Some(_) => 0
		}
  
		override def relbdd(theory:Theory,env:BDD) = left.unify(right) match {
			  case None => Stream((new Unifier(mutable.Map.empty),1))
			  case Some(_) => Stream.empty 
		}
  
   }
}
