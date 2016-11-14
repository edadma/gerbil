package xyz.hyperreal

import java.io.{PrintStream, ByteArrayOutputStream}

package object gerbil {
	def capture( code: String ) = {
		val buf = new ByteArrayOutputStream
		
		Console.withOut( new PrintStream(buf) )( Gerbil.run(code) )
		buf.toString.trim
	}
	
	def captureReturn( code: String ) = {
		val buf = new ByteArrayOutputStream
		val ret = Console.withOut( new PrintStream(buf) )( Gerbil.run(code) )
		
		(ret, buf.toString.trim)
	}
}