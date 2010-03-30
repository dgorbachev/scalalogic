package scalalogic.prolog.dsl

import scalalogic._

object AppendTest extends ScalaLogic with DSL with Application{
  
  val append = new Predicate('append)
  
  print(
    scalog(
		append(Nil, 'L, 'L),
		append('H ::'L1,'L2,'H ::'L3) := append('L1,'L2,'L3)
    ) ?= append(List(1,2,3) , List(4,5,6), 'Sum)
  )
  
}

