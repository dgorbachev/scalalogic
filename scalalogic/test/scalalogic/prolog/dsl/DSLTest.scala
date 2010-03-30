package scalalogic.prolog.dsl

import scalalogic._

object DSLTest extends ScalaLogic 	with DSL 
								with Application
{
  val male = new Predicate('male)
  val singer = new Predicate('singer)
  //or
  val sings = new Predicate('sings)
  val sings_songs = new Predicate('sings_songs)
  
  val theory = scalog(
	  male('jef),
	  male('jos),
	  sings('jef),
	  sings_songs('S, List('abc, 'lala, 'ping)),
	  singer('M) := male('M) & sings('M)
  )
		  
  val singers =  theory ?= singer('Man) & sings_songs('Man, ^ :: 'Song2  :: ^)
  
  print(singers) 
		  
  val men =  theory ?= male('Man)
  
  print(men) 
}