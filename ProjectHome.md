## Introduction ##

ScalaLogic is a light-weight Prolog interpreter written in Scala.

It can parse and execute Prolog code or it can write logic programs in a Prolog-like DSL language inside Scala. Emphasis is on simplicity, not performance. Most standard library predicates are not supported but can easily be added.

## Example ##
Define the theory in the DSL language:
```
  val append = 'append
  val quicksort = 'quicksort
  val partition = 'partition
  
  val theory = scalog(
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
  )
```

Execute a goal:
```
  val solutions = 
    theory ?= quicksort(List(3,2,1,10,8,9,7,5,6,4,3,2,1,10,8,9,7,5,6,4,3), 'Sorted)
```
Print each solution in the Stream:
```
  print(solutions)
```
```
  Sorted = [1,1,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,10].
```