package idealized.scala

object ideal {
	val a4 = new Succ(new Succ(new Succ(new Succ(Zero))))
	val an8 = a4 +(a4)
	println( a4 )
}

abstract class Booleano {
	def ifThenElse[T](t: => T, e: => T): T
	
	def && (x: => Booleano): Booleano = ifThenElse(x, falso)
	def || (x: => Booleano): Booleano = ifThenElse(truo, x)
	def unary_! : Booleano = ifThenElse(falso, truo)
	
	def == (x: Booleano): Booleano = ifThenElse(x, x.unary_!)
	def != (x: Booleano): Booleano = ifThenElse(x.unary_!,x)
}
 
object truo extends Booleano {
	def ifThenElse[T](t: => T, e: => T) = t
}

object falso extends Booleano {
	def ifThenElse[T](t: => T, e: => T) = e
}

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat  = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
	def isZero = true
	def predecessor = throw new Error("less than zero")
	def + (that: Nat): Nat = that
	def - (that: Nat): Nat = {
		if ( that.isZero ) this else throw new Error("less than zero!")
	}
}

class Succ(n: Nat) extends Nat {
	def isZero = false
	def predecesor = n
	// this is a recursive call to Succ (not  to + !)
	// Succ is required because n is before the successor was obtained
	def + (that: Nat): Nat = new Succ (n + that)
	def - (that: Nat): Nat = if (that.isZero) this else n - that.predecessor
}