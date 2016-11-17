package xyz.hyperreal.gerbil

import java.io._


object Main extends App {

	Gerbil.run( new FileReader(args(0)), new Env )
	
}