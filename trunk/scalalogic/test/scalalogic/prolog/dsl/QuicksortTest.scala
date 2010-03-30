package scalalogic.prolog.dsl

import scalalogic._

object QuicksortTest extends ScalaLogic with DSL 
									with Application
{
  
  val append = new Predicate('append)
  val quicksort = new Predicate('quicksort)
  val partition = new Predicate('partition)
  
  print(
    scalog(
        //Quicksort
    	quicksort(Nil, Nil),
    	quicksort('HEAD ::'TAIL,'SORTED) := partition('HEAD,'TAIL,'LEFT,'RIGHT) &
                                    		quicksort('LEFT,'SORTEDL) &
                                    		quicksort('RIGHT,'SORTEDR) &
                                    		append('SORTEDL,'HEAD ::'SORTEDR,'SORTED),
        
        //Partition
        partition('PIVOT, Nil, Nil, Nil),
        partition('PIVOT,'HEAD ::'TAIL,'HEAD::'LEFT,'RIGHT) :='HEAD < 'PIVOT &
                                                         	   partition('PIVOT,'TAIL,'LEFT,'RIGHT),
        partition('PIVOT,'HEAD ::'TAIL,'LEFT,'HEAD ::'RIGHT) :='HEAD >= 'PIVOT &
                                                         	    partition('PIVOT, 'TAIL, 'LEFT, 'RIGHT),
        
        //Append
		append(Nil, 'L, 'L),
		append('H ::'L1,'L2,'H ::'L3) := append('L1,'L2,'L3)
  
    ) ?= quicksort(List(3,2,1,10,8,9,7,5,6,4,3,2,1,10,8,9,7,5,6,4,3), 'Sorted)
  )
  
}
