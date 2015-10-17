package week1

object sheet {
  println("This is the Newton method for Sqrt")   //> This is the Newton method for Sqrt

  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqr(x: Double) = {
    def sqrIter(guess: Double): Double =
      if (goodEnough(guess)) guess else
        sqrIter(refine(guess))

    def goodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.0001

    def refine(guess: Double) = (guess + x / guess) / 2

    sqrIter(1.0)
  }                                               //> sqr: (x: Double)Double

  sqr(2.0)                                        //> res0: Double = 1.4142156862745097
  sqr(1e-6)                                       //> res1: Double = 0.0010000001533016628
  sqr(1e60)                                       //> res2: Double = 1.0000000031080746E30

  def factorialUp(n: Int) = {

    def factorialIter(k: Int, accum: Int): Int =
      if (k == n) accum else factorialIter(k + 1, accum * (k + 1))

    factorialIter(1, 1)
  }                                               //> factorialUp: (n: Int)Int
  factorialUp(13)                                 //> res3: Int = 1932053504

// this version does not require an extra k store
  def factorial(n: Int) = {

    def factorialIter(n: Int, accum: Int): Int =
      if (n == 0) accum else factorialIter(n - 1, accum * n)

    factorialIter(n, 1)
  }                                               //> factorial: (n: Int)Int
  factorial(13)                                   //> res4: Int = 1932053504
  
  // this implementation is correct but it is not tail recursive
  def pascal(c: Int, r:Int):Int =
  	if (c == r || c == 0) 1 else pascal(c, r-1) + pascal(c-1,r-1)
                                                  //> pascal: (c: Int, r: Int)Int

	pascal(3,5)                               //> res5: Int = 10
	

}