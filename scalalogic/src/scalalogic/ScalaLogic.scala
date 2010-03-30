package scalalogic

import scala.collection._
import jdd.bdd.BDD;

/**
 * A basic set of features to run Prolog programs in Scala
 */
trait ScalaLogic{
	
	//Theories and Clauses
	
  	case class Theory(val clauses: List[Clause]){
  		
  	   type Key = (Symbol,Int)
  	   
  	   class Index(val clauses: List[Clause]){
		
			val termMapping: Map[Term,List[Clause]] = {
				val firstGroundArgs: Set[Term] = 
					mutable.Set() ++ clauses.map(_.head.args.head).filter(_.isGround)
				val nonGroundClauses = clauses.filter(!_.head.args.head.isGround)
				val mapping = immutable.Map() ++ firstGroundArgs.map{
					 arg => 
					 val matchingClauses = clauses.filter{c => !c.head.args.head.isGround || c.head.args.head == arg}
					 (arg->matchingClauses)
				 }
				 mapping.withDefaultValue(nonGroundClauses)
			 }
			
			def clausesFor(terms: List[Term]): List[Clause] = {
	       		if(terms.isEmpty || !terms.head.isGround) clauses
	       		else termMapping(terms.head)
	       	}
		 
  	   }
		
       val indexes: Map[Key,Index] = {
    	   val symbols : mutable.Set[Key] = 
    	  	   mutable.Set() ++ clauses.map(clause => (clause.head.symbol,clause.head.arity))
    	   val mapping = immutable.Map() ++  symbols.map{ key:Key =>
	    	   		val index: Index = new Index(clauses.filter{
	  	       			clause => clause.head.symbol == key._1 && clause.head.arity == key._2
	  	       		})
	  	       		(key->index)
	       }
    	   val emptyIndex = new Index(Nil)
    	   mapping.withDefaultValue(emptyIndex)
       }
       
       //TODO hardwire clause body literals to theory clauses.
       
       	def clausesFor(goal:Predicate):List[Clause] = 
       		indexes((goal.symbol,goal.arity)).clausesFor(goal.args)
       		
		override def toString = clauses.mkString("\n")
  
		def ?=(formula:Formula) : Stream[Unifier] = formula.query(this)
  
		def ?=(goal:Predicate) : Stream[Unifier] = 
			clausesFor(goal).toStream.flatMap{_.standardizeApart.query(goal, this)}
		
//  		def how(goal:Predicate) : Stream[(Unifier,List[Int])] = 
//			clauses.toStream.flatMap{_.standardizeApart.how(goal, this)} //Stream uses lazy evaluation
		
	    def bdd(goal:Predicate, env:BDD) : Int = {
	    	def or(remainingClauses: List[Clause],otherBDD:Int) : Int = 
			  remainingClauses match{
			    case Nil => otherBDD
			    case clause :: tail => {
			      val thisBDD = clause.standardizeApart.bdd(goal, this, env)
			      val temp = env.ref(env.or(thisBDD,otherBDD))
			      env.deref(thisBDD,otherBDD)
			      or(tail,temp)
			      
			    }
			}
			or(clausesFor(goal),0)
		}
            
        def relbddtotal(goal:Predicate, env:BDD) : Int = {
	    	def or(left:Int,tuple:(Unifier,Int)) : Int = {
	    		val right = tuple._2
	    		val tmp = env.ref(env.or(left,right))
			    env.deref(left,right)
			    tmp
			}
	    	relbdd(goal, env).foldLeft(0)(or(_,_))
		}                                       
                                                     
	    def relbdd(goal:Predicate, env:BDD) : Stream[(Unifier,Int)] = {
	    	clausesFor(goal).toStream.flatMap{_.standardizeApart.relbdd(goal, this, env)}
		}
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
 
	class Clause(val head:Predicate, val body:Formula) {
		
		//TODO remove, useless
		val isGround = head.isGround && body == True
		
		override def toString = head + (if (body == True) "." else " :- " + body.toString)
		
		def standardizeApart: Clause = {
		  if(isGround) this
		  else {
		 	  val renaming:Renaming = mutable.Map.empty
		 	  new Clause(head.standardizeApartWith(renaming), body.standardizeApartWith(renaming))
		  }
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
		
//		def how(goal:Predicate, theory:Theory) : (Unifier,List[Int]) = {
//			head.unify(goal) match {
//			  case None => Stream.empty
//			  case Some(unifier) => {
//			    val bodyStream = body.substitute(unifier).query(theory)
//			    bodyStream.map{b => (b + unifier).reduce(goal.vars)}
//			  }
//			}
//		}
  
		def bdd(goal:Predicate, theory:Theory, env:BDD) : Int = {
			head.unify(goal) match {
			  case None => 0
			  case Some(unifier) => {
			    body.substitute(unifier).bdd(theory,env)
			  }
			}
		}
		
		def relbdd(goal:Predicate, theory:Theory, env:BDD) : Stream[(Unifier,Int)] = {
			head.unify(goal) match {
			  case None => Stream.empty
			  case Some(unifier) => {
				val bodyStream = body.substitute(unifier).relbdd(theory, env)
			    bodyStream.map{case (bodyUnif,bdd) => ((bodyUnif + unifier).reduce(goal.vars),bdd)}
			  }
			}
		}
	}
 
    class ProbFact(fact:Predicate, prob:Double) extends Clause(fact, True){
        
    	var bdd:Option[Int] = None;
      
    	override def toString = prob+" :: "+fact.toString
		
    	// Prob clauses must be ground!
		override def standardizeApart: ProbFact = this;
  
		override def bdd(goal:Predicate, theory:Theory, env:BDD) : Int = {
			head.unify(goal) match {
			  case None => 0
			  case Some(unifier) => {
			     if(bdd.isEmpty){
			       val ref = env.createVar(fact.toString,prob)
			       bdd = Some(ref)
			     }
			     bdd.get
			  }
			}
		}
  
		override def relbdd(goal:Predicate, theory:Theory, env:BDD) : Stream[(Unifier,Int)] = {
			head.unify(goal) match {
			  case None => Stream.empty
			  case Some(unifier) => {
			     if(bdd.isEmpty){
			       val ref = env.createVar(fact.toString,prob)
			       bdd = Some(ref)
			     }
			     Stream((unifier,bdd.get))
			  }
			}
		}
    }
 
	//Formulae
 
	sealed abstract class Formula{
	  def & (other:Formula) = new And(this,other)
	  def standardizeApartWith(renaming:Renaming) = this
	  def query(theory:Theory):Stream[Unifier]
	  def bdd(theory:Theory,env:BDD):Int
	  def relbdd(theory:Theory,env:BDD):Stream[(Unifier,Int)]
      def substitute(f: Var=>Term):Formula = this
	  def substitute(unifier:Unifier):Formula = 
	 	  if(unifier.map.isEmpty) this else substitute{unifier.replaceOrKeep _}
	}
 
	object True extends Formula{ 
	  override def query(theory:Theory) = Stream(immutable.Map.empty)
	  override def bdd(theory:Theory,env:BDD) = 1
	  override def relbdd(theory:Theory,env:BDD) = Stream((immutable.Map.empty,1))
	}
	
	object False extends Formula{ 
	  override def query(theory:Theory) = Stream.empty 
	  override def bdd(theory:Theory,env:BDD) = 0
	  override def relbdd(theory:Theory,env:BDD) = Stream.empty 
	}
	
	class And(first:Formula, makeSecond:Formula) extends Formula{
		
	  lazy val second = makeSecond
		
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
   
	  override def bdd(theory:Theory,env:BDD) = {
	    val bdd1 = first.bdd(theory:Theory,env:BDD)
        val bdd2 = second.bdd(theory:Theory,env:BDD)
	    val result = env.ref(env.and(bdd1,bdd2))
	    env.deref(bdd1,bdd2)
        result
	  }
   
	  override def relbdd(theory:Theory,env:BDD) = {
	    first.relbdd(theory,env:BDD).flatMap{ 
	      case (firstUnifier, firstBDD) => {
		      val outerMapping = second.substitute(firstUnifier).relbdd(theory,env:BDD).map{
		        case (u,bdd) => {
		          val innerMapping = (firstUnifier + u,env.ref(env.and(firstBDD,bdd)))
		          env.deref(bdd)
		          innerMapping
		        }
		      }
		      env.deref(firstBDD)
		      outerMapping
	      }
	    }
	  }
   
	}
 
	class !(pred:Predicate) extends Formula{
	  override def toString = "!"+pred
	  
	  override def standardizeApartWith(renaming:Renaming) = 
		  new !(pred.standardizeApartWith(renaming))
	  
	  override def substitute(f: Var=>Term) = new !(pred.substitute(f))
   
	  override def query(theory:Theory) = {
	    pred.query(theory) match{
	      case Stream.Empty => Stream(new Unifier(mutable.Map.empty))
	      case Stream.cons(_,_) => Stream.empty
	    }
	  }
   
	  override def bdd(theory:Theory,env:BDD) = {
	    val bdd1 = pred.bdd(theory:Theory,env:BDD)
        val result = env.ref(env.not(bdd1))
	    env.deref(bdd1)
        result
	  }
   
	  override def relbdd(theory:Theory,env:BDD) = {
	    val bdd1 = theory.relbddtotal(pred,env:BDD)
        val result = env.ref(env.not(bdd1))
	    env.deref(bdd1)
        Stream((new Unifier(mutable.Map.empty),result))
	  }
   
	}
 
	type MutableUnifier = mutable.Map[Var,Term]
 
	class Predicate(val symbol:Symbol, val arity:Int, val args:List[Term]) extends Formula{
		
		val nbVars = args.foldLeft(0)(_+_.nbVars)
	  
		def this(symbol:Symbol) = this(symbol, 0, Nil)
		
		def apply(newArgs:Term*) = new Predicate(symbol, newArgs.length, newArgs.toList)
		
		def := (formula:Formula) = new Clause(this,formula)
		
		def vars:Set[Var] = {val set:mutable.Set[Var] = mutable.Set.empty; addTo(set); set}
		
		def addTo(vars:mutable.Set[Var]) = args.foreach{_.addTo(vars)}
		
		def :: (prob:Double) = new ProbFact(this,prob)
		
		def unary_! = new !(this)
		
		override def toString = symbol.name + (if (args.isEmpty) "" else args.mkString("(",",",")"))
		
		override def query(theory:Theory) = theory ?= this
		
		def isGround:Boolean = nbVars==0
		
		override def substitute(f: Var=>Term):Predicate = {
		  if(isGround) this
		  else new Predicate(symbol, arity, args.map(_.substitute(f)))
		}
		
		override def standardizeApartWith(renaming: Renaming):Predicate = 
			if(isGround) this
			else substitute{renaming.getOrElseUpdate(_, new TempVar)}
	
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
  
  
		override def bdd(theory:Theory,env:BDD) = {
		  theory.bdd(this,env)
		}
  
		override def relbdd(theory:Theory,env:BDD) = {
		  theory.relbdd(this,env)
		}
  
	}
 
	//Terms

	abstract class Term(val nbVars:Int) {
		def substitute(f: Var=>Term):Term
	    def addTo(vars:mutable.Set[Var])
		def unify(other:Term, unifier:MutableUnifier):Boolean
		def isGround:Boolean = nbVars==0

		final def unify(other:Term):Option[Unifier] = {
			val map: MutableUnifier = mutable.Map.empty
			if(unify(other, map)) Some(new Unifier(map)) else None
		}
	}
 
	abstract class NonVarTerm(nbVars:Int) extends Term(nbVars) {
		override def substitute(f: Var=>Term):Term = this
	    override def addTo(vars:mutable.Set[Var]) = {}
	}
 
	abstract class Var extends Term(1){
	  def unifyWithNonVar(term:NonVarTerm, unifier:MutableUnifier):Boolean
	}
 
	class UnifiableVar extends Var{
	  
		override final def substitute(f: Var=>Term):Term = f(this)
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
 
	class TempVar extends UnifiableVar {
	   override def toString = "Z"+hashCode
	}
}
