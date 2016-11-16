package xyz.hyperreal.gerbil


object Main extends App {
	
  println( Gerbil.run( """=:flip ->1; ->2,%1;@%.1%2%1$ $ /:@flip`,;,1,2,3;""" ) )

}