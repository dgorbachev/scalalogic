package scalalogic

import jdd.bdd.BDD;

object BDDTest extends Application{

		val bdd = new BDD(1000,100)

		val d1 = bdd.createVar("d1")
		val d2 = bdd.createVar("d2")
		val p1 = bdd.createVar("p1")
		val p2 = bdd.createVar("p2")
		val p3 = bdd.createVar("p3")

		val q1 = bdd.ref(bdd.and(d1,p1,p3))
		val q2 = bdd.ref(bdd.and(d2,p1,p2))
	
		val q = bdd.ref(bdd.or(q1, q2))
		bdd.deref(q1,q2)
		
		bdd.printDot("q", q)

		bdd.cleanup()
  
}
