package xyz.hyperreal.gerbil

import java.io._


object Main extends App {

	Gerbil.run( Gerbil.compile(new FileReader(args(0))) )
	
}