package scalalogic

/**
 * A trait that adds support for named variables
 */
trait NamedVars extends ScalaLogic {
   
	case class NamedVar(val symbol:Symbol) extends UnifiableVar {
		override def toString = symbol.name
	}
}
