package scalalogic.problog

import scalalogic._
import jdd.bdd.BDD;

object ProblogTutorialTest extends ScalaLogic with DSL with Application{
		
	    val t1 = System.currentTimeMillis();
		
        val env = new BDD(1000,100)
		
		val dir_edge = new Predicate('dir_edge)
		val path = new Predicate('path)
		val edge = new Predicate('edge)
		val absent = new Predicate('absent)
		
		val theory = scalog(
		    
			//probabilistic facts
			0.9::dir_edge(1,2),
			0.8::dir_edge(2,3),
			0.6::dir_edge(3,4),
			0.7::dir_edge(1,6),
			0.5::dir_edge(2,6),
			0.4::dir_edge(6,5),
			0.7::dir_edge(5,3),
			0.2::dir_edge(5,4),
			
			//Now comes the background knowledge
			//definition of acyclic path using list of visited nodes
			path('X,'Y) := path('X,'Y,List('X),DontCare),
			
			path('X,'X, 'A, 'A),
			path('X,'Y,'A,'R) := 
			        'X <> 'Y &
			        edge('X,'Z) & 
			        absent('Z,'A) & 
			        path('Z,'Y,'Z :: 'A,'R),
			
			//using directed edges in both directions
			edge('X,'Y) := dir_edge('Y,'X),
			edge('X,'Y) := dir_edge('X,'Y),
			
			//checking whether node hasn't been visited before
			absent(DontCare,Nil),
			absent('X,'Y :: 'Z) := 'X <> 'Y & absent('X,'Z)
		)
  
		//print(theory ?= path(1,4))
  
		val bdd = theory.relbddtotal(path(1,4),env)
		
		println("prob = "+env.dfs(bdd))
		
		val t2 = System.currentTimeMillis();
		println("time = "+(t2-t1)+" ms")
  
		env.printDot("bdd", bdd)
        
		env.cleanup()
		
}
