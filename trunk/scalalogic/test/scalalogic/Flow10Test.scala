package scalalogic

import jdd.bdd.BDD;

object Flow10Test extends ScalaLogic with DSL with Application{
		
	    val t1 = System.currentTimeMillis();
		
        val env = new BDD(300,300)
        
		val path = new Predicate('path)
		val edge = new Predicate('edge)
		val dir_edge = new Predicate('dir_edge)
		val absent = new Predicate('absent)
		
		val theory = scalog(
			
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
			absent('X,'Y :: 'Z) := 'X <> 'Y & absent('X,'Z),
   
			0.5::dir_edge(11,21),
			0.5::dir_edge(11,22),
			0.5::dir_edge(11,12),
			0.5::dir_edge(12,22),
			0.5::dir_edge(12,23),
			0.5::dir_edge(12,13),
			0.5::dir_edge(13,23),
			0.5::dir_edge(13,24),
			0.5::dir_edge(13,14),
			0.5::dir_edge(14,24),
			0.5::dir_edge(14,25),
			0.5::dir_edge(14,15),
			0.5::dir_edge(15,25),
			0.5::dir_edge(15,26),
			0.5::dir_edge(15,16),
			0.5::dir_edge(16,26),
			0.5::dir_edge(16,27),
			0.5::dir_edge(16,17),
			0.5::dir_edge(17,27),
			0.5::dir_edge(17,28),
			0.5::dir_edge(17,18),
			0.5::dir_edge(18,28),
			0.5::dir_edge(18,29),
			0.5::dir_edge(18,19),
			0.5::dir_edge(19,29),
			0.5::dir_edge(19,210),
			0.5::dir_edge(19,110),
			0.5::dir_edge(110,210),
			0.5::dir_edge(21,31),
			0.5::dir_edge(21,32),
			0.5::dir_edge(21,22),
			0.5::dir_edge(22,32),
			0.5::dir_edge(22,33),
			0.5::dir_edge(22,23),
			0.5::dir_edge(23,33),
			0.5::dir_edge(23,34),
			0.5::dir_edge(23,24),
			0.5::dir_edge(24,34),
			0.5::dir_edge(24,35),
			0.5::dir_edge(24,25),
			0.5::dir_edge(25,35),
			0.5::dir_edge(25,36),
			0.5::dir_edge(25,26),
			0.5::dir_edge(26,36),
			0.5::dir_edge(26,37),
			0.5::dir_edge(26,27),
			0.5::dir_edge(27,37),
			0.5::dir_edge(27,38),
			0.5::dir_edge(27,28),
			0.5::dir_edge(28,38),
			0.5::dir_edge(28,39),
			0.5::dir_edge(28,29),
			0.5::dir_edge(29,39),
			0.5::dir_edge(29,310),
			0.5::dir_edge(29,210),
			0.5::dir_edge(210,310)
//			0.5::dir_edge(31,41),
//			0.5::dir_edge(31,42),
//			0.5::dir_edge(31,32),
//			0.5::dir_edge(32,42),
//			0.5::dir_edge(32,43),
//			0.5::dir_edge(32,33),
//			0.5::dir_edge(33,43),
//			0.5::dir_edge(33,44),
//			0.5::dir_edge(33,34),
//			0.5::dir_edge(34,44),
//			0.5::dir_edge(34,45),
//			0.5::dir_edge(34,35),
//			0.5::dir_edge(35,45),
//			0.5::dir_edge(35,46),
//			0.5::dir_edge(35,36),
//			0.5::dir_edge(36,46),
//			0.5::dir_edge(36,47),
//			0.5::dir_edge(36,37),
//			0.5::dir_edge(37,47),
//			0.5::dir_edge(37,48),
//			0.5::dir_edge(37,38),
//			0.5::dir_edge(38,48),
//			0.5::dir_edge(38,49),
//			0.5::dir_edge(38,39),
//			0.5::dir_edge(39,49),
//			0.5::dir_edge(39,410),
//			0.5::dir_edge(39,310),
//			0.5::dir_edge(310,410),
//			0.5::dir_edge(41,51),
//			0.5::dir_edge(41,52),
//			0.5::dir_edge(41,42),
//			0.5::dir_edge(42,52),
//			0.5::dir_edge(42,53),
//			0.5::dir_edge(42,43),
//			0.5::dir_edge(43,53),
//			0.5::dir_edge(43,54),
//			0.5::dir_edge(43,44),
//			0.5::dir_edge(44,54),
//			0.5::dir_edge(44,55),
//			0.5::dir_edge(44,45),
//			0.5::dir_edge(45,55),
//			0.5::dir_edge(45,56),
//			0.5::dir_edge(45,46),
//			0.5::dir_edge(46,56),
//			0.5::dir_edge(46,57),
//			0.5::dir_edge(46,47),
//			0.5::dir_edge(47,57),
//			0.5::dir_edge(47,58),
//			0.5::dir_edge(47,48),
//			0.5::dir_edge(48,58),
//			0.5::dir_edge(48,59),
//			0.5::dir_edge(48,49),
//			0.5::dir_edge(49,59),
//			0.5::dir_edge(49,510),
//			0.5::dir_edge(49,410),
//			0.5::dir_edge(410,510),
//			0.5::dir_edge(51,61),
//			0.5::dir_edge(51,62),
//			0.5::dir_edge(51,52),
//			0.5::dir_edge(52,62),
//			0.5::dir_edge(52,63),
//			0.5::dir_edge(52,53),
//			0.5::dir_edge(53,63),
//			0.5::dir_edge(53,64),
//			0.5::dir_edge(53,54),
//			0.5::dir_edge(54,64),
//			0.5::dir_edge(54,65),
//			0.5::dir_edge(54,55),
//			0.5::dir_edge(55,65),
//			0.5::dir_edge(55,66),
//			0.5::dir_edge(55,56),
//			0.5::dir_edge(56,66),
//			0.5::dir_edge(56,67),
//			0.5::dir_edge(56,57),
//			0.5::dir_edge(57,67),
//			0.5::dir_edge(57,68),
//			0.5::dir_edge(57,58),
//			0.5::dir_edge(58,68),
//			0.5::dir_edge(58,69),
//			0.5::dir_edge(58,59),
//			0.5::dir_edge(59,69),
//			0.5::dir_edge(59,610),
//			0.5::dir_edge(59,510),
//			0.5::dir_edge(510,610),
//			0.5::dir_edge(61,71),
//			0.5::dir_edge(61,72),
//			0.5::dir_edge(61,62),
//			0.5::dir_edge(62,72),
//			0.5::dir_edge(62,73),
//			0.5::dir_edge(62,63),
//			0.5::dir_edge(63,73),
//			0.5::dir_edge(63,74),
//			0.5::dir_edge(63,64),
//			0.5::dir_edge(64,74),
//			0.5::dir_edge(64,75),
//			0.5::dir_edge(64,65),
//			0.5::dir_edge(65,75),
//			0.5::dir_edge(65,76),
//			0.5::dir_edge(65,66),
//			0.5::dir_edge(66,76),
//			0.5::dir_edge(66,77),
//			0.5::dir_edge(66,67),
//			0.5::dir_edge(67,77),
//			0.5::dir_edge(67,78),
//			0.5::dir_edge(67,68),
//			0.5::dir_edge(68,78),
//			0.5::dir_edge(68,79),
//			0.5::dir_edge(68,69),
//			0.5::dir_edge(69,79),
//			0.5::dir_edge(69,710),
//			0.5::dir_edge(69,610),
//			0.5::dir_edge(610,710),
//			0.5::dir_edge(71,81),
//			0.5::dir_edge(71,82),
//			0.5::dir_edge(71,72),
//			0.5::dir_edge(72,82),
//			0.5::dir_edge(72,83),
//			0.5::dir_edge(72,73),
//			0.5::dir_edge(73,83),
//			0.5::dir_edge(73,84),
//			0.5::dir_edge(73,74),
//			0.5::dir_edge(74,84),
//			0.5::dir_edge(74,85),
//			0.5::dir_edge(74,75),
//			0.5::dir_edge(75,85),
//			0.5::dir_edge(75,86),
//			0.5::dir_edge(75,76),
//			0.5::dir_edge(76,86),
//			0.5::dir_edge(76,87),
//			0.5::dir_edge(76,77),
//			0.5::dir_edge(77,87),
//			0.5::dir_edge(77,88),
//			0.5::dir_edge(77,78),
//			0.5::dir_edge(78,88),
//			0.5::dir_edge(78,89),
//			0.5::dir_edge(78,79),
//			0.5::dir_edge(79,89),
//			0.5::dir_edge(79,810),
//			0.5::dir_edge(79,710),
//			0.5::dir_edge(710,810),
//			0.5::dir_edge(81,91),
//			0.5::dir_edge(81,92),
//			0.5::dir_edge(81,82),
//			0.5::dir_edge(82,92),
//			0.5::dir_edge(82,93),
//			0.5::dir_edge(82,83),
//			0.5::dir_edge(83,93),
//			0.5::dir_edge(83,94),
//			0.5::dir_edge(83,84),
//			0.5::dir_edge(84,94),
//			0.5::dir_edge(84,95),
//			0.5::dir_edge(84,85),
//			0.5::dir_edge(85,95),
//			0.5::dir_edge(85,96),
//			0.5::dir_edge(85,86),
//			0.5::dir_edge(86,96),
//			0.5::dir_edge(86,97),
//			0.5::dir_edge(86,87),
//			0.5::dir_edge(87,97),
//			0.5::dir_edge(87,98),
//			0.5::dir_edge(87,88),
//			0.5::dir_edge(88,98),
//			0.5::dir_edge(88,99),
//			0.5::dir_edge(88,89),
//			0.5::dir_edge(89,99),
//			0.5::dir_edge(89,910),
//			0.5::dir_edge(89,810),
//			0.5::dir_edge(810,910),
//			0.5::dir_edge(91,101),
//			0.5::dir_edge(91,102),
//			0.5::dir_edge(91,92),
//			0.5::dir_edge(92,102),
//			0.5::dir_edge(92,103),
//			0.5::dir_edge(92,93),
//			0.5::dir_edge(93,103),
//			0.5::dir_edge(93,104),
//			0.5::dir_edge(93,94),
//			0.5::dir_edge(94,104),
//			0.5::dir_edge(94,105),
//			0.5::dir_edge(94,95),
//			0.5::dir_edge(95,105),
//			0.5::dir_edge(95,106),
//			0.5::dir_edge(95,96),
//			0.5::dir_edge(96,106),
//			0.5::dir_edge(96,107),
//			0.5::dir_edge(96,97),
//			0.5::dir_edge(97,107),
//			0.5::dir_edge(97,108),
//			0.5::dir_edge(97,98),
//			0.5::dir_edge(98,108),
//			0.5::dir_edge(98,109),
//			0.5::dir_edge(98,99),
//			0.5::dir_edge(99,109),
//			0.5::dir_edge(99,1010),
//			0.5::dir_edge(99,910),
//			0.5::dir_edge(910,1010),
//			0.5::dir_edge(101,102),
//			0.5::dir_edge(102,103),
//			0.5::dir_edge(103,104),
//			0.5::dir_edge(104,105),
//			0.5::dir_edge(105,106),
//			0.5::dir_edge(106,107),
//			0.5::dir_edge(107,108),
//			0.5::dir_edge(108,109),
//			0.5::dir_edge(109,1010)
		)
  
		print(theory ?= path(11,25))
  
//		val bdd = theory.relbddtotal(path(11,25),env)
//		println("Built BDD")
//  
//		//env.printDot("bdd", bdd)
//		
//		val t2 = System.currentTimeMillis();
//		println("time = "+(t2-t1)+" ms")
//  
//		println("prob = "+env.dfs(bdd))
		
		val t3 = System.currentTimeMillis();
		println("time = "+(t3-t1)+" ms")
        
		env.cleanup()
		
}
