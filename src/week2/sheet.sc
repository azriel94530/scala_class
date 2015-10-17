package week2

object sheet {
  println("Welcome to the Scala worksheet week 2")//> Welcome to the Scala worksheet week 2
  // write a tail recursive version
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a, 0)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  println( sum( x=>x, 1, 4) )                     //> 10
  
  def product(f: Int => Int)(a: Int, b: Int): Int = {
  	if (a > b) 1 else f(a) * product(f) (a+1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  println( product( x => x ) (1, 4) )             //> 24
  
  def factorial(b: Int): Int = product( x => x )(1, b)
                                                  //> factorial: (b: Int)Int
  	
  println( factorial(5) )                         //> 120
  
  def accumulator(accumf: (Int, Int) => Int, identity: Int, f: Int => Int)(a: Int, b: Int): Int = {
  	if (a > b) identity else accumf(f(a),accumulator(accumf, identity, f) (a+1,b) )
  }                                               //> accumulator: (accumf: (Int, Int) => Int, identity: Int, f: Int => Int)(a: In
                                                  //| t, b: Int)Int
  println( accumulator((x,y) => x*y, 1, x=>x ) (1,5) )
                                                  //> 120
}