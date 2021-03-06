package xyz.hyperreal.gerbil

import java.io.{StringReader, Reader}

import collection.mutable.{ArrayBuffer, HashSet, HashMap, ArrayStack}

import xyz.hyperreal.rtcep.{Operator => _, _}
import xyz.hyperreal.lia.{FunctionMap, Math}
import xyz.hyperreal.numbers.ComplexBigInt


object Gerbil {
	
	val symbols =
		new SymbolLexeme( 'symbol, Nil ) {
			add( "+", "-", "~", "*", "/", "^", ".", ".:", "=:", "+:", "-:", "+.", "-.", "@", ",", ";", "#",
				"->", "%", "%:", "%.", "%%", "%%%", "$", "?", ":", "?.",
				"<", "<=", "=", ">", ">=", "+|", "-|", "..",
				"&", "|", "|:", "~.", "(", "(:", ")", "^:", "`", "`(", "`)",
				"/:", "\\:", "/.", "\\.", "!", "!\\", "=>", "()", "_", "__", "><",
				"[", "]", "{", "}", "<-", "==", "=::"
			)
		}
	val reserved = new ReservedLexeme( "i", "sqrt" )
	val l =
		new Lexer {
			add( new StringLexeme('string, '"') )
			ignore( new LineCommentLexeme("##") )
			add( symbols )
			add( reserved )
			add( new FloatingLexeme('float) )
			add( new IntegerLexeme('integer, Set()) )
			add( new NameLexeme('ident) )
			ignore( WhitespaceLexeme )
			add( EOFLexeme )
		}
	val operators = new HashMap[Any, (Token, ArrayBuffer[Instruction], ArrayStack[Control]) => Operator]
	
	def operator( kind: Any, op: (Token, ArrayBuffer[Instruction], ArrayStack[Control]) => Operator ) = operators(kind) = op
	
	operator( 'i, (_, _, _) => env => Some( Math('*, ComplexBigInt.i, env.evalo) ) )
	operator( 'sqrt, (_, _, _) => env => Some( Math.sqrtFunction(env.evalo) ) )
	operator( '+',
		(_, _, _) => env =>
			Some( env.evalo match {
				case s: Seq[Any] =>
 					for ((l, r) <- s zip env.evals) yield Math( '+, l, r )
				case l =>
					val r = env.evalo
					
					if (l.isInstanceOf[String] || r.isInstanceOf[String])
						String.valueOf( l ) + String.valueOf( r )
					else
						Math( '+, l, r )
				} )
		)
	operator( '-',
		(_, _, _) => env =>
			Some( env.evalo match {
				case s: Seq[Any] =>
 					for ((l, r) <- s zip env.evals) yield Math( '+, l, r )
				case l =>
					val r = env.evalo
					
					if (l.isInstanceOf[String] || r.isInstanceOf[String])
						String.valueOf( l ) + String.valueOf( r )
					else
						Math( '-, l, r )
				} )
		)
	operator( '~',
		(_, _, _) => env =>
			Some( env.evalo match {
				case s: Seq[Any] => for (e <- s) yield Math( '-, e )
				case l => Math( '-, l )
			} )
		)
	operator( '*',
		(_, _, _) => env =>
			Some( env.evalo match {
				case s: String => s*env.evali
				case s: Seq[Any] =>
					var sum: AnyRef = new Integer( 0 )
					
 					for (n <- for ((l, r) <- s zip env.evals) yield Math( '*, l, r ))
						sum = Math( '+, sum, n )
						
					sum
				case l =>
					val r = env.evalo
					
					if (l.isInstanceOf[String] || r.isInstanceOf[String])
						String.valueOf( l ) + String.valueOf( r )
					else
						Math( '*, l, r )
				} )
		)
	operator( '/', (_, _, _) => env => Some( Math('/, env.evalo, env.evalo) ) )
	operator( '^', (_, _, _) => env => Some( Math('^, env.evalo, env.evalo) ) )
	operator( '=:, (_, _, _) => env => Some( env.evalv.value = env.evalo ) )
	operator( '+:, (_, _, _) =>
		env => {
			val v = env.evalv
			val res = Some( v.value )
			
			v.value = Math( '+, v.value, 1 )
			res
		} )
	operator( '-:, (_, _, _) =>
		env => {
			val v = env.evalv
			val res = Some( v.value )
			
			v.value = Math( '-, v.value, 1 )
			res
		} )
	operator( Symbol("+."), (_, _, _) =>
		env => {
			val v = env.evalv
			
			v.value = Math( '+, v.value, 1 )
			Some( v.value )
		} )
	operator( Symbol("-."), (_, _, _) =>
		env => {
			val v = env.evalv
			
			v.value = Math( '-, v.value, 1 )
			Some( v.value )
		} )
	operator( '.', (_, _, _) => env => Some( println(env.evalo) ) )
	operator( Symbol(".:"), (_, _, _) => env => Some( print(env.evalo) ) )
	operator( ',', (_, _, _) => env => Some( env.evalo :: env.evall ) )
	operator( ';', (_, _, _) => env => Some( Nil ) )
	operator( '#', (_, _, _) => env => Some( env.evalit.size ) )
	operator( '->,
		(_, _, _) => env => {
			val argc = env.evali
			val scope = env.evals map Some.apply toVector
			val start = env.ip
			var depth = 0
			
			def scan: Unit =
				if (env.inst(env.ip).tok.kind == '$') {
					env.ip +=1
					
					if (depth > 0) {
						depth -= 1
						scan
					}
				} else {
					if (env.inst(env.ip).tok.kind == '->)
						depth += 1
						
					env.ip +=1
					scan
				}

			scan
			Some( new Function( start, argc, scope ) )
		} )
	operator( '%', (_, _, _) =>
		env => {
			val arg = env.evali - 1
			val top = env.stack.top
			
// 			if (arg >= top.args.length) {
// 				val cur = env.ip
// 				
// 				env.stack.pop
// 				env.ip = top.ret
// 				
// 				while (arg >= top.args.length)
// 					top.args += env.evald
// 					
// 				top.ret = env.ip
// 				env.ip = cur
// 				env.stack.push( top )
// 			}
			
			top.args(arg)
		} )
	operator( '%:, (_, _, _) => env => Some( env.stack.top.args map (_.get) toList ) )
	operator( Symbol("%."), (_, _, _) => env => env.stack.top.scope( env.evali - 1 ) )
	operator( '%%, (_, _, _) => env => env.stack( 1 ).args( env.evali - 1 ) )
	operator( '%%%, (_, _, _) => env => env.stack( 2 ).args( env.evali - 1 ) )
	operator( '$',
		(_, _, _) => env => {
			env.ip = env.stack.pop.ret
			env.last
		} )
	operator( '@',
		(_, _, _) => env => {
			val cur = env.ip
			
			env.evalo match {
				case s: String => Some( s( env.evali ) )
				case l: Seq[Any] => Some( l( env.evali ) )
				case c: collection.Set[Any] => Some( c( env.evalo ) )
				case m: collection.Map[Any, Any] => Some( m( env.evalo ) )
				case o: Operator => o( env )
				case _ => env.inst(cur).tok.pos.error( "expected a string, iterable, map or function" )
			}
		} )
	operator( 'integer, (t, _, _) => {val n = t.s.toInt; env => Some( n )} )
	operator( 'float, (t, _, _) => {val n = t.s.toDouble; env => Some( n )} )
	operator( 'string, (t, _, _) => env => Some( t.s ) )
	operator( 'ident,
		(t, _, _) => env =>
			env.defined get t.s match {
				case Some( f ) => f( env )
				case None =>
					Some( env.vars get t.s match {
						case Some( v: Variable ) => v
						case None =>
							val v = new Variable
							
							env.vars( t.s ) = v
							v
					} )
			}
		)
	operator( '<', (_, _, _) => env => Some( Math('<, env.evalo, env.evalo) ) )
	operator( '<=, (_, _, _) => env => Some( Math('<=, env.evalo, env.evalo) ) )
	operator( '=', (_, _, _) => env => Some( env.evalo == env.evalo ) )
	operator( '>', (_, _, _) => env => Some( Math('>, env.evalo, env.evalo) ) )
	operator( '>=, (_, _, _) => env => Some( Math('>=, env.evalo, env.evalo) ) )
	operator( '?',
		(t, code, control) => {
			control.push( Conditional(code.length) )
			_ => t.pos.error( "unclosed conditional" )
		} )
	operator( ':',
		(t, code, control) => {
			control.top match {
				case c@Conditional( trueIndex, _ ) =>
					c.falseIndex = code.length
				case _ => t.pos.error( "not inside a conditional" )
			}
			
			_ => t.pos.error( "unclosed conditional" )
		} )
	operator( Symbol("?."),
		(t, code, control) => {
			val cur = code.length
			
			control.pop match {
				case Conditional( trueIndex, -1 ) =>
					code(trueIndex) = 
						new Instruction( code(trueIndex).tok,
							env => {
								if (env.evalb) {
									val res = env.execute( cur )
									
									env.ip += 1
									res
								}
								else {
									env.ip = cur + 1
									Some( () )
								}
							} )
				case Conditional( trueIndex, falseIndex ) =>
					code(trueIndex) = 
						new Instruction( code(trueIndex).tok,
							env => {
								if (env.evalb) {
									val res = env.execute( falseIndex )
									
									env.ip = cur + 1
									res
								}
								else {
									env.ip = falseIndex + 1
									
									val res = env.execute( cur )
									
									env.ip += 1
									res
								}
							} )
				case _ => t.pos.error( "not inside a conditional" )
			}
			
			null
		} )
	operator( '+|, (_, _, _) => _ => Some( true ) )
	operator( '-|, (_, _, _) => _ => Some( false ) )
	operator( Symbol(".."), (_, _, _) => env => Some( env.evali to env.evali ) )
	operator( '&', (_, _, _) =>
		env => {
			Some( env.evalo match {
				case b: Boolean => {
					val r = env.evalb
					
					b && r
				}
				case l => Math( 'and, l, env.evalo )
			} )
		} )
	operator( '|', (_, _, _) =>
		env => {
			Some( env.evalo match {
				case b: Boolean => {
					val r = env.evalb
					
					b || r
				}
				case l => Math( 'or, l, env.evalo )
			} )
		} )
	operator( '|:, (_, _, _) =>
		env => {
			Some( env.evalo match {
				case b: Boolean => b ^ env.evalb
				case l => Math( 'xor, l, env.evalo )
			} )
		} )
	operator( Symbol("~."), (_, _, _) =>
		env =>
			Some( env.evalo match {
				case b: Boolean => !b
				case l => Math( 'not, l )
			} )
		)
	operator( '(',
		(t, code, control) => {
// 			evalo( env ) match {
// 				case t: TraversableOnce[Any] =>
// 				case s: String => 
// 			}
			control.push( SimpleLoop(code.length) )
			_ => t.pos.error( "unclosed loop" )
		} )
	operator( Symbol("(:"),
		(t, code, control) => {
			control.push( ForLoop(code.length) )
			_ => t.pos.error( "unclosed loop" )
		} )
	operator( ')',
		(t, code, control) => {
			val cur = code.length

			def processExits( exits: Seq[Int] ) {
				for (e <- exits)
					code(e) =
						new Instruction( code(e).tok,
							env => {
								if (env.evalb) {
									env.ip = cur + 1
									None
								} else
									Some( () )
							} )
			}
			
			control.pop match {
				case SimpleLoop( start, exits ) =>
					code(start) = new Instruction( code(start).tok, env => env.execute(cur) )
					processExits( exits )
					env => Some( env.ip = start )
				case ForLoop( start, exits ) =>
					code(start) = new Instruction( code(start).tok,
						env => {
							if (env.stack.top.loops.isEmpty || env.stack.top.loops.top.start != start)
								env.stack.top.loops.push( ForControl(start, env.evalit.iterator) )
							
							val top = env.stack.top.loops.top
							
							if (top.iter.hasNext) {
								top.cur = Some( top.iter.next )
								env.execute(cur)
							} else {
								env.stack.top.loops.pop
								env.ip = cur + 1
								Some( () )
							}
						} )
					processExits( exits )
					env => Some( env.ip = start )
				case _ => t.pos.error( "not inside a loop" )
			}
		} )
	operator( '^:,
		(t, code, control) => {
			control.top match {
				case SimpleLoop( _, exits ) => exits += code.length
				case _ => t.pos.error( "not inside a loop" )
			}
			
			null
		} )
	operator( '`', (_, _, _) =>
		env => {
			if (env.ip == env.inst.length)
				env.inst(env.ip - 1).tok.rest.head.pos.error( "operator was expected" )
			
			val inst = env.inst(env.ip)
		
			env.ip += 1
			Some( inst )
		} )
	operator( Symbol("`("), (_, _, _) =>
		env => {
			if (env.ip == env.inst.length)
				env.inst(env.ip - 1).tok.rest.head.pos.error( "operator was expected" )
			
			val inst = env.inst(env.ip)
			
			env.ip += 1
			
			val l = env.evalo
			
			Some( (env: Env) => inst( new OperatorEnv(List(l), env) ) )
		} )
	operator( '/:, (_, _, _) =>
		env => {
			val f = env.evalf
			val z = env.evalo
			val s = env.evalit
			
			Some( s.foldLeft(z)((x, y) => f(new OperatorEnv(List(x, y), env)).get) )
		} )
	operator( Symbol("/."), (_, _, _) =>
		env => {
			val f = env.evalf
			val s = env.evalit
			
			Some( s.reduceLeft((x, y) => f(new OperatorEnv(List(x, y), env)).get) )
		} )
	operator( Symbol("\\:"), (_, _, _) =>
		env => {
			val f = env.evalf
			val z = env.evalo
			val s = env.evalit
			
			Some( s.foldRight(z)((x, y) => f(new OperatorEnv(List(x, y), env)).get) )
		} )
	operator( Symbol("\\."), (_, _, _) =>
		env => {
			val f = env.evalf
			val s = env.evalit
			
			Some( s.reduceRight((x, y) => f(new OperatorEnv(List(x, y), env)).get) )
		} )
	
	def product( lower: Int, upper: Int ) = {
		var res: AnyRef = new Integer( lower )
		
		for (i <- lower + 1 to upper)
			res = Math( '*, res, i )
			
		res
	}
	
	operator( '!', (_, _, _) =>
		env => {
			val cur = env.ip
			val n = env.evali
			
			if (n < 0)
				env.inst(cur).tok.pos.error( "non-negative integer expected" )
			else if (n == 0 || n == 1)
				Some( 1 )
			else
				Some( product(2, n) )
		} )
	operator( Symbol("!\\"), (_, _, _) =>
		env => {
			val n = env.evali
			val k = env.evali
			
				Some( product(n - k + 1, n) )
		} )
	operator( '=>, (_, _, _) =>
		env => {
			val f = env.evalf
			val s = env.evalit

			Some( s map (x => f(new OperatorEnv(List(x), env)).get) )
		} )
	operator( Symbol("()"), (_, _, _) => env => Some( () ) )
	operator( '_', (t, _, _) => env =>
		if (env.stack.top.loops.isEmpty)
			t.pos.error( "not inside a loop" )
		else
			env.stack.top.loops.top.cur )
	operator( '__, (t, _, _) => env =>
		if (env.stack.top.loops.length < 2)
			t.pos.error( "not inside a nested loop" )
		else
			env.stack.top.loops(1).cur )
	operator( '><,
		(_, _, _) => env => {
			val f = env.evalf
			val l = env.evalo
			val r = env.evalo
			
			f( new OperatorEnv(List(r, l), env) )
		} )
	operator( '[',
		(_, _, _) => env => {
			val array = new ArrayBuffer[Any]
			
			while (env.ip < env.inst.length && env.inst(env.ip).tok.kind != ']')
				array += env.evalo
				
			if (env.ip == env.inst.length)
				env.inst(env.ip - 1).tok.rest.head.pos.error( "expected ']'" )
				
			env.ip += 1
			Some( array )
		} )
	operator( ']',
		(t, _, _) => env => t.pos.error( "not inside an array" ) )
	operator( '{',
		(_, _, _) => env => {
			val array = new HashSet[Any]
			
			while (env.ip < env.inst.length && env.inst(env.ip).tok.kind != '}')
				array += env.evalo
				
			if (env.ip == env.inst.length)
				env.inst(env.ip - 1).tok.rest.head.pos.error( "expected '}'" )
				
			env.ip += 1
			Some( array )
		} )
	operator( '}',
		(t, _, _) => env => t.pos.error( "not inside a set" ) )
	operator( '<-,
		(_, _, _) => env => {
			val e = env.evalo
			val cur = env.ip
			
			Some( env.evalo match {
				case s: String => s contains e
				case l: Seq[Any] => l contains e
				case c: collection.Set[Any] => c contains e
				case m: collection.Map[Any, Any] => m contains e
				case _ => env.inst(cur).tok.pos.error( "expected a string or iterable" )
			} )
		} )
	operator( '==, (_, _, _) => env => env.evald )
	operator( '=::, (_, _, _) =>
		env => {
			val cur = env.ip
			
			if (cur == env.inst.length)
				env.inst(cur - 1).tok.rest.head.pos.error( "operator name was expected" )
			
			val sym = env.inst(cur).tok match {
				case Token( 'ident, s, _, _ ) => s
				case Token( _, _, ch, _ ) => ch.head.pos.error( "unreserved operator name was expected" )
			}
		
			env.ip += 1
			
// 			val cur = env.ip
//			val sym = env.evalstr
			val symbolic =
				if (sym.matches( "[a-zA-Z]+" ))
					false
				else if (sym.isEmpty || sym.matches( """.*(?:\s|[0-9a-zA-Z]).*""" ))
					env.inst(cur).tok.pos.error( "expected a name (alpha only) or a symbol" )
				else
					true
			val f = env.evalf
// 			if (symbolic)
// 				symbols.add( sym )
// 			else
// 				reserved.add( sym )
				
//			operator( if (sym.length == 1) sym.head else Symbol(sym), (_, _, _) => f )

			if (env.defined contains sym)
				env.inst(cur).tok.pos.error( "operator already defined" )
				
			env.defined(sym) = f
			Some( () )
		} )
// 	operator( 'symbol, (t, _, _) =>
// 		env => {
// 			Some( println( t.s ) )
// 		} )

	def compile( r: Reader ) = {
		val code = new ArrayBuffer[Instruction]
		val control = new ArrayStack[Control]
		
		control push null	// so that .top is always valid
		
		for (t <- l.scan( r, 4 ) if !t.end)
			operators get t.kind match {
				case None => t.pos.error( "unknown operator" )
				case Some( op ) =>
//				println( code.length, t )
					code += new Instruction( t, op(t, code, control) )
			}

//			println( "-------" )
		code
	}
	
	def run( env: Env ): Any = {
		env.stack.clear
		env.ip = 0
		env.stack push new Activation( -1, null, null )
		env.execute( env.inst.length ).get
	}
	
	def run( code: Reader, env: Env ): Any = {
		env.inst = compile( code )
		run( env )
	}

	def run( code: String, env: Env = new Env ): Any = run( new StringReader(code), env )
}

trait Control

case class Conditional( trueIndex: Int, var falseIndex: Int = -1 ) extends Control

case class SimpleLoop( start: Int, exits: ArrayBuffer[Int] = new ArrayBuffer ) extends Control

case class ForLoop( start: Int, exits: ArrayBuffer[Int] = new ArrayBuffer ) extends Control

class Instruction( val tok: Token, action: Operator ) extends Operator {
	def apply( env: Env ) = action( env )
	
	override def toString = "<" + tok + ">"
}

// class Activation( var ret: Int, val args: ArrayBuffer[Option[Any]] = new ArrayBuffer )
class Activation( val ret: Int, val args: IndexedSeq[Option[Any]], val scope: Vector[Option[Any]], val loops: ArrayStack[ForControl] = new ArrayStack )

case class ForControl( start: Int, iter: Iterator[Any], var cur: Option[Any] = null )

class Function( val start: Int, val argc: Int, scope: Vector[Option[Any]] ) extends Operator {
	def apply( env: Env ) = {
		val args = (for (_ <- 1 to argc) yield env.evala) toIndexedSeq
		val here = env.ip
		
		env.stack.push( new Activation(here, args, scope) )
		env.ip = start
		env.execute( here )
	}
	
	override def toString = "Function( " + start + ", " + argc + " )"
}

class Variable {
	var value: Any = null
	
	override def toString = "Var( " + value + " )"
}

class Env {
	var inst: IndexedSeq[Instruction] = null
	var _ip = 0
	val vars = new HashMap[String, Variable]
	val stack = new ArrayStack[Activation]
	var last: Option[Any] = null
	val defined = new HashMap[String, Operator]
	
	def ip = _ip
	
	def ip_=( loc: Int ) = _ip = loc
	
	def eval = {
		if (ip == inst.length)
			inst(inst.length - 1).tok.rest.head.pos.error( "expected more operators" )
			
		val cur = ip
		
		ip += 1
//println( cur, inst(cur) )
		inst(cur)( this )
	}
	
	def evalv: Variable = {
		val cur = ip
		
		eval match {
			case Some( v: Variable ) => v
			case _ => inst(cur).tok.pos.error( "expected a varialble" )
		}
	}
		
	def evald: Option[Any] =
		eval match {
			case Some( v: Variable ) => Some( v.value )
			case v => v
		}

	def evalo = {
		val cur = ip
		
		evald match {
			case Some( o ) => o
			case None => inst(cur).tok.pos.error( "operand was expected" )
		}
	}
	
	def evalf = {
		val cur = ip
		
		evalo match {
			case o: Operator => o
			case _ => inst(cur).tok.pos.error( "operator (or function) was expected" )
		}
	}

	def evali = {
		val cur = ip
		
		evalo match {
			case o: Int => o
			case _ => inst(cur).tok.pos.error( "integer was expected" )
		}
	}
		
	def evalb = {
		val cur = ip
		
		evalo match {
			case o: Boolean => o
			case _ => inst(cur).tok.pos.error( "boolean was expected" )
		}
	}
		
	def evall = evalo.asInstanceOf[List[Any]]	
		
	def evals = {
		val cur = ip
		
		evalo match {
			case l: Seq[Any] => l
			case s: String => s.toList
			case _ => inst(cur).tok.pos.error( "sequence was expected" )
		}
	}
		
	def evalstr = {
		val cur = ip
		
		evalo match {
			case s: String => s
			case _ => inst(cur).tok.pos.error( "string was expected" )
		}
	}
	
	def evalit = {
		val cur = ip
		
		evalo match {
			case l: Iterable[Any] => l
			case s: String => s.toList
			case v => inst(cur).tok.pos.error( "iterable was expected" )
		}
	}
	
	def evala = evald
	
	def execute( upto: Int ) = {
		def loop: Option[Any] =
			if (ip < upto) {
				evald match {
					case v@Some(_) =>
						last = v
						loop
					case None => Some( () )
				}
			} else
				last
				
		loop
	}
}

class OperatorEnv( operands: List[Any], exec: Env ) extends Env {
	
 	var cur = operands
 	override val vars = exec.vars
 	override val stack = exec.stack
 	override val defined = exec.defined
 	
 	inst = exec.inst
	_ip = exec._ip	// don't know why keeping this line is necessary
	
	override def ip = exec._ip
	
	override def ip_=( loc: Int ) = exec._ip = loc
	
	override def evala = {
		val res = Some( cur.head )
		
		cur = cur.tail
		res
	}
	
	override def evalo =
		if (cur isEmpty)
			super.evalo
		else {
			val res = cur.head
			
			cur = cur.tail
			res
		}
}