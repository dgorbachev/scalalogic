package scalalogic

import jdd.bdd.BDD;

object MarketingTest extends ScalaLogic with DSL with Application{
		
	   	val env = new BDD(1000,100)

		val umbrella = new Predicate('umbrella)
		val raincoat = new Predicate('raincoat)
		
		val rainy = new Predicate('rain)
		val windy = new Predicate('wind)
		
		val dry = new Predicate('dry)
		val broken_umbrella = new Predicate('broken_umbrella)
  
		val theory = scalog(
		  0.5 :: umbrella,
		  0.5 :: raincoat,
    
		  0.3 :: rainy,
		  0.5 :: windy,
    
		  broken_umbrella := umbrella & rainy & windy,
    
		  dry := rainy & umbrella & ! broken_umbrella,
		  dry := rainy & raincoat,
		  dry := ! rainy
    
		)
  
		val bddg1 = theory.relbddtotal(dry,env)
		val bddg2 = theory.relbddtotal(broken_umbrella,env)
  
		env.printDot("dry.dot", bddg1)
		env.printDot("broken_umbrella.dot", bddg2)
		
		//println("Prob="+env.dfs(bdd))
  
		env.cleanup()
		
}
