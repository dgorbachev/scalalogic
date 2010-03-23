package scalalogic

import jdd.bdd.BDD;

object ProblogTest extends ScalaLogic with DSL with Application{

		val env = new BDD(300,100)

		val q = new Predicate('q)
		
		val d1 = new Predicate('d1)
		val d2 = new Predicate('d2)
		
		val p1 = new Predicate('p1)
		val p2 = new Predicate('p2)
		val p3 = new Predicate('p3)
		
		val f = new Predicate('f)
  
		val theory = scalog(
		  0.5 :: d1,
		  0.5 :: d2,
    
		  0.5 :: p1,
		  0.8 :: p2,
		  0.3 :: p3,
	
		  f('a),
    
		  q := d1 & p1 & p3, f('X),
		  q := d2 & p1 & p2
		)
  
		val bdd = theory.bdd(q,env)
  
		env.printDot("bdd", bdd)
		
		println("Prob="+env.dfs(bdd))
  
		env.cleanup()
  
}
