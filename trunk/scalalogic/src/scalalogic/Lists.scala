package scalalogic

import scala.collection._

/**
 * A trait that adds support for lists.
 */
trait Lists extends ScalaLogic{
    
	abstract class ScalaLogicList extends NonVarTerm{
		def :: (term:Term) = new ListNode(term, this)
		def toPartialString:String;
	}
 
	case class ListNode(elem:Term, next:Term) extends ScalaLogicList{
		override def toString = "["+toPartialString +"]"
		def toPartialString = elem + nextPartialString
		def nextPartialString = next match{
		  case l:ListNode => ","+l.toPartialString
		  case EmptyList => ""
		  case _ => "|"+next
		}
		
		override def substitute(f: Var=>Term):ListNode = new ListNode(elem.substitute(f),next.substitute(f))
		
		override def addTo(vars: mutable.Set[Var]) = {
			elem.addTo(vars)
			next.addTo(vars)
		}
  
		override def unify(other:Term, unifier:MutableUnifier) = other match{
			case ListNode(otherElem, otherNext) => elem.unify(otherElem, unifier) && next.unify(otherNext, unifier)
			case v:Var => v.unifyWithNonVar(this,unifier)
			case _ => false
		}
	}
	
	case object EmptyList extends ScalaLogicList{
		override def toString = "[]"
		override def toPartialString = ""
		override def substitute(f: Var=>Term) = this
		override def unify(other:Term, unifier:MutableUnifier) = other match {
		  case EmptyList => true
		  case v:Var => v.unifyWithNonVar(this,unifier)
		  case _ => false
		} 
	}
 
}
