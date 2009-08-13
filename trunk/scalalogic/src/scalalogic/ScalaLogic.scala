package scalalogic

import scala.collection._

/**
 * A basic set of features to run Prolog programs in Scala
 */
trait ScalaLogic{
	
	//Theories and Clauses
  
  	case class Theory(val clauses: List[Clause]){
		
  		//TODO add Map of clause head symbols
		override def toString = clauses.mkString("\n")
  
		def ?=(formula:Formula) : Stream[Unifier] = formula.query(this)
  
		def ?=(goal:Predicate) : Stream[Unifier] = 
			clauses.toStream.flatMap{_.standardizeApart.query(goal, this)} //Stream uses lazy evaluation
		
	}
		
	
	class Unifier(val map:Map[Var,Term]) extends{
		override def toString = if(map.isEmpty) "true.\n" 
                          		else map.map{case (v,t) => v+" = "+t}.mkString("",",\n",".\n")
		
		def + (other:Unifier) = new Unifier(mutable.Map.empty ++ other.map ++ map) // temp hack, fix in Scala 2.8?
		
		def reduce(vars:Set[Var]) : Unifier = {
			val reducedMap = mutable.Map[Var,Term]()
			reducedMap ++= vars.filter(map.contains(_)).map{v => (v->v.substitute(replaceOrKeep _))}
			new Unifier(reducedMap)
		}
  
        def replaceOrKeep(v:Var):Term = map.get(v) match{
		    case None => v
		    case Some(term) => term.substitute(replaceOrKeep _)
        }
        
	}
 
	implicit def map2Unifier(map:immutable.Map[Var,Term]):Unifier = new Unifier(map)
	    
	type Renaming = mutable.Map[Var,Var]
 
	case class Clause(head:Predicate, body:Formula) {
		override def toString = head + (if (body == True) "." else " :- " + body.toString)
		
		def standardizeApart: Clause = {
		  val renaming:Renaming = mutable.Map.empty
		  new Clause(head.standardizeApartWith(renaming), body.standardizeApartWith(renaming))
		}
		
		def query(goal:Predicate, theory:Theory) : Stream[Unifier] = {
			head.unify(goal) match {
			  case None => Stream.empty
			  case Some(unifier) => {
			    val bodyStream = body.substitute(unifier).query(theory)
			    bodyStream.map{b => (b + unifier).reduce(goal.vars)}
			  }
			}
		}
  
	}
 
	//Formulae
 
	sealed abstract class Formula{
	  def & (other:Formula) = new And(this,other)
	  def standardizeApartWith(renaming:Renaming) = this
	  def query(theory:Theory):Stream[Unifier]
      def substitute(f: Var=>Term):Formula = this
	  def substitute(unifier:Unifier):Formula = if(unifier.map.isEmpty) this else substitute{unifier.replaceOrKeep _}
	}
 
	object True extends Formula{ 
	  override def query(theory:Theory) = Stream(immutable.Map.empty)
	}
	
	object False extends Formula{ 
	  override def query(theory:Theory) = Stream.empty 
	}
	
	class And(first:Formula, second:Formula) extends Formula{
	  override def toString = first+","+second
	  
	  override def standardizeApartWith(renaming:Renaming) = 
		  new And(first.standardizeApartWith(renaming),second.standardizeApartWith(renaming))
	  
	  //TODO Lazy vals for second substitution?
	  override def substitute(f: Var=>Term) = new And(first.substitute(f),second.substitute(f))
   
	  override def query(theory:Theory) = {
	    first.query(theory).flatMap{ firstUnifier =>
	      second.substitute(firstUnifier).query(theory).map{firstUnifier + _}
	    }
	  }
	}
 
	type MutableUnifier = mutable.Map[Var,Term]
 
	class Predicate(val symbol:Symbol, val args:List[Term]) extends Formula{
		def this(symbol:Symbol) = this(symbol, Nil)
		def apply(newArgs:Term*) = new Predicate(symbol, newArgs.toList)
		def := (formula:Formula) = new Clause(this,formula)
		def vars:Set[Var] = {val set:mutable.Set[Var] = mutable.Set.empty; addTo(set); set}
		def addTo(vars:mutable.Set[Var]) = args.foreach{_.addTo(vars)}
		
		override def toString = symbol.name + (if (args.isEmpty) "" else args.mkString("(",",",")"))
		override def query(theory:Theory) = theory ?= this
		override def substitute(f: Var=>Term):Predicate = new Predicate(symbol, args.map(_.substitute(f)))
	  
		override def standardizeApartWith(renaming: Renaming):Predicate = 
			substitute{renaming.getOrElseUpdate(_, new TempVar)}
	
		def unify(goal:Predicate):Option[Unifier] = {
			if(symbol == goal.symbol) {
			  val unifier : MutableUnifier = mutable.Map.empty
			  def unifyTerms(a:List[Term], b:List[Term]):Boolean =  {
				  if(a.isEmpty && b.isEmpty) true
				  else if(a.isEmpty || b.isEmpty) false
				  else if(a.head.unify(b.head,unifier)) unifyTerms(a.tail, b.tail) else false
			  }
			  if(unifyTerms(args, goal.args)) Some(new Unifier(unifier)) else None
			} else None
		}
	}
 
	//Terms

	abstract class Term {
		def substitute(f: Var=>Term):Term
	    def addTo(vars:mutable.Set[Var])
		def unify(other:Term, unifier:MutableUnifier):Boolean

		final def unify(other:Term):Option[Unifier] = {
			val map: MutableUnifier = mutable.Map.empty
			if(unify(other, map)) Some(new Unifier(map)) else None
		}
	}
 
	abstract class NonVarTerm extends Term{
		override def substitute(f: Var=>Term):Term = this
	    override def addTo(vars:mutable.Set[Var]) = {}
	}
 
	abstract class Var extends Term{
	  def unifyWithNonVar(term:NonVarTerm, unifier:MutableUnifier):Boolean
	}
 
	class UnifiableVar extends Var{
		override def substitute(f: Var=>Term):Term = f(this)
		override def addTo(vars:mutable.Set[Var]) = vars += this
		
		override def unify(other:Term, unifier:MutableUnifier) = other match{
			  case v:UnifiableVar => unifyWithVar(v, unifier)
			  case _ => other.unify(this,unifier)
		}
  
		def unifyWithVar(other:UnifiableVar, unifier:MutableUnifier) = 
			  if(this == other) true
			  else unifier.get(this) match{
				  case Some(boundTerm) => boundTerm.unify(other, unifier)
				  case None => unifier.get(other) match{
					  case Some(boundTerm) => boundTerm.unify(this, unifier)
					  case None => unifier(this) = other; true
			  }
		}
  
		override def unifyWithNonVar(other:NonVarTerm, unifier:MutableUnifier) = unifier.get(this) match{
			  case Some(boundTerm) => boundTerm.unify(other, unifier)
			  case None => unifier(this) = other; true
		}
  
	}
 
	class TempVar extends UnifiableVar
}
