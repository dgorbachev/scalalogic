package scalalogic

import scala.util.parsing.combinator.JavaTokenParsers;

/**
 * A parser for Prolog code to Scalog objects
 */
abstract class PrologParser extends JavaTokenParsers{

	val scalaLogic : ScalaLogic with NamedVars with DontCares with Atoms with Lists with Integers
	
	import scalaLogic._

	def theory:Parser[Theory] = rep(clause) ^^ {list => new Theory(list)}
 
	def clause: Parser[Clause] = (
			predicate~opt(":-"~>formula)<~"." 
				^^ { 
					case head~None => new Clause(head,True)
					case head~Some(body) => new Clause(head,body)
			}
			| failure("Illegal clause")
	)
 
	def formula: Parser[Formula] = (
			"true" ^^ {_ => True}
			| "false" ^^ {_ => False}
			//TODO predicate should be formula? Will work in Scala 2.8?
			| predicate~","~formula 
				^^ {case first~_~second => new And(first, second)}
			| predicate
			| failure("Illegal formula")
	)
 
 
	def predicate: Parser[Predicate] = (
			"""[a-z]\w*""".r ~ opt("("~>repsep(term,",")<~")") 
				^^ {
					case name ~ None => new Predicate(Symbol(name),List())
					case name ~ Some(arglist) => new Predicate(Symbol(name),arglist)
			}
			| failure("Illegal predicate")
	)
 
	def term: Parser[Term] = (
			atom
			| variable
			| wholeNumber ^^ {n => new Integer(n.toInt)}
			//| decimalNumber ^^ {n => new Number(n.toDouble)}
			| list
			| failure("Illegal term")
	)
 
	def atom: Parser[Atom] = """[a-z]\w*""".r ^^ {name => new Atom(Symbol(name))}
 
	def variable: Parser[Var] = (
		"""[A-Z]\w*""".r ^^ {name => new NamedVar(Symbol(name))}
		| """_\w*""".r ^^ {_ => DontCare}
	)
   
	def list: Parser[ScalaLogicList] = (
		"["~>repsep(term,",")<~"]"
			^^ { _.foldRight(EmptyList:ScalaLogicList){ (e,l) => new ListNode(e,l) } }
		| "["~rep1sep(term,",")~"|"~variable~"]"
			^^ {  case "["~(e::r)~"|"~end~"]" => new ListNode(e, r.foldRight(end:Term){ ListNode(_,_) } )}
		| failure("Illegal list")
	)
 
    override def toString = "PrologParser"
   
}