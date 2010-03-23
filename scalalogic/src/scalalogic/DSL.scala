package scalalogic

/**
 * A trait for a Domain-Specific Language in Scala for Prolog programs.
 * The Prolog syntax is slightly altered to comply with Scala conventions:
 * <ul>
 * 	<li> :- is :=
 *  <li> , is &
 *  <li> . is ,
 *  <li> [] is Nil
 *  <li> | is ::
 * </ul>
 */
trait DSL extends ScalaLogic 
	with NamedVars with DontCares with Atoms 
	with Lists with Integers with Unification{
  
  def scalog(clauses:Clause*):Theory = new Theory(clauses.toList)
  
  def print(solutions:Stream[Unifier]) = if(solutions.isEmpty) println("false.")
                                         else println(solutions mkString "\n")
  
  //Implicits
  
  //implicit def symbol2Predicate(sym:Symbol):Predicate = new Predicate(sym)
  
  implicit def predicate2Clause(pred:Predicate):Clause = new Clause(pred, True)
  
  implicit def symbol2Term(symbol:Symbol):Term =
    if(symbol.name.charAt(0).isUpperCase) new NamedVar(symbol)
    else new Atom(symbol)
  
  implicit def int2Term(n:Int):Integer = new Integer(n)
    
  implicit def string2Term(name:String):Term =
    if(name.charAt(0) == '_') DontCare
    else if(name.charAt(0).isUpperCase) new NamedVar(Symbol(name))
    else new Atom(Symbol(name))
  
  // Not sure why this is needed: compiler complains about divergent implicits
  def Nil = EmptyList
  
  implicit def list2ScalogList[A](list: List[A])(implicit elem2Term: A => Term): ScalaLogicList =
	  (list :\ (EmptyList:ScalaLogicList))(ListNode(_,_))
	  
//  implicit def list2ScalogList[T <% Term](list:List[T]):ScalaLogicList = 
//	  (list :\ (EmptyList:ScalaLogicList))(ListNode(_,_))
  
  class RichTerm(term:Term){
    def :: (leftTerm:Term) = new ListNode(leftTerm,term)
    def < (rightTerm:Term) = new <(term,rightTerm)
    def > (rightTerm:Term) = new >(term,rightTerm)
    def >= (rightTerm:Term) = new >=(term,rightTerm)
    def <= (rightTerm:Term) = new <=(term,rightTerm)
    def <>(rightTerm:Term) = new <>(term,rightTerm)
  }
  
  implicit def term2RichTerm(term:Term):RichTerm = new RichTerm(term)
  
  implicit def symbol2RichTerm(symbol:Symbol):RichTerm = new RichTerm(symbol2Term(symbol))
  
}
