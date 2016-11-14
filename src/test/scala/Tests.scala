package xyz.hyperreal.gerbil

import org.scalatest._
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with Matchers {
	
	"tests" in {
		capture( """=:f->2+%1%2$.+1@f3 4""" ) shouldBe "8"
 		captureReturn( """.?=1 1."yes"123?.."done"890""" ) shouldBe (890, "yes\n123\ndone")
		captureReturn( """.?=1 2."yes"123?.."done"890""" ) shouldBe (890, "()\ndone")
		captureReturn( """.?=1 1."yes"123:."no"567?.."done"890""" ) shouldBe (890, "yes\n123\ndone")
		captureReturn( """.?=1 2."yes"123:."no"567?.."done"890""" ) shouldBe (890, "no\n567\ndone")
		Gerbil.run( "|+|+|" ) shouldBe true
 		Gerbil.run( "|+|-|" ) shouldBe true
		Gerbil.run( "|-|-|" ) shouldBe false
		Gerbil.run( "&+|+|" ) shouldBe true
		Gerbil.run( "&+|-|" ) shouldBe false
		Gerbil.run( "&-|-|" ) shouldBe false
		Gerbil.run( "|:+|+|" ) shouldBe false
		Gerbil.run( "|:+|-|" ) shouldBe true
		Gerbil.run( "|:-|-|" ) shouldBe false
		Gerbil.run( "~.|:-|-|" ) shouldBe true
		Gerbil.run( """-* 2 /2 3 /1 3""" ) shouldBe 1
		Gerbil.run( """=:fact->1/.`*..1%1$@fact5""" ) shouldBe 120
		Gerbil.run( """
			=: fact             ## assign the function to variable 'fact'
				-> 1              ## define lambda function that takes 1 argument
					/.              ## reduce a sequence to a single value by applying an operator to each element
						`*            ## the multiplication operator as an object (section) to be used by reduce (/.)
						..1 %1        ## sequence of integers from 1 to whatever the argument is
					$               ## end function
					
			@fact 5             ## call function assigned to 'fact' with 5 as the argument
			""" ) shouldBe 120
		Gerbil.run( """=:fact->1?=0%1 1:*%1@fact-%1 1?.$@fact5""" ) shouldBe 120
		capture( "=:a1(.a^:=a5=:a+a1)" ) shouldBe "1\n2\n3\n4\n5"
		Gerbil.run( "-.123" ) shouldBe -123
		Gerbil.run( """=>->1+1%1$,1,2,3;""" ) shouldBe List( 2, 3, 4 )
		Gerbil.run( "^4 /1 2" ) shouldBe 2.0
		capture( """=:B->1/.`+=>->1/.`+=>->1*^-.1%1*/!\%%1%1!%1/^%1%%%1+1%%1$..0%1$..0%1$ .@B40""" ) shouldBe "-261082718496449122051/13530"
		Gerbil.run( """@`(-1 5""" ) shouldBe -4
		capture( "(:..1 3;._1)" ) shouldBe "1\n2\n3"
		Gerbil.run( "/:`+3,1,2,3;" ) shouldBe 9
		Gerbil.run( """@->3%:$1 2 3""" ) shouldBe List( 1, 2, 3 )
	}
	
}