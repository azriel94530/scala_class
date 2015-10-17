package week2

object sets {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val bound: Int = 1000                           //> bound  : Int = 1000
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: week2.sets.Set, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => (x == elem)
                                                  //> singletonSet: (elem: Int)week2.sets.Set

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => (contains(s, x) || contains(t, x))
                                                  //> union: (s: week2.sets.Set, t: week2.sets.Set)week2.sets.Set

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => (contains(s, x) && contains(t, x))
                                                  //> intersect: (s: week2.sets.Set, t: week2.sets.Set)week2.sets.Set

  def diff(s: Set, t: Set): Set = (x: Int) => (contains(s, x) && !contains(t, x))
                                                  //> diff: (s: week2.sets.Set, t: week2.sets.Set)week2.sets.Set

  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)
                                                  //> filter: (s: week2.sets.Set, p: Int => Boolean)week2.sets.Set
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > 1000) true
      else if (contains(s, a) && !p(a)) false
      else iter(a + 1)
    }
    iter(-1000)
  }                                               //> forall: (s: week2.sets.Set, p: Int => Boolean)Boolean

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, (b: Int) => !p(b))
                                                  //> exists: (s: week2.sets.Set, p: Int => Boolean)Boolean

  def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, (b: Int) => ( f(b)==x ))
                                                  //> map: (s: week2.sets.Set, f: Int => Int)week2.sets.Set

  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: week2.sets.Set)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: week2.sets.Set)Unit
  contains(singletonSet(8), 8)                    //> res0: Boolean = true

  contains(union(singletonSet(1), singletonSet(2)), 1)
                                                  //> res1: Boolean = true
  contains(intersect(union(singletonSet(1), singletonSet(2)), singletonSet(2)), 3)
                                                  //> res2: Boolean = false
  contains(diff(
    union(union(singletonSet(1), singletonSet(2)), singletonSet(5)),
    union(union(singletonSet(5), singletonSet(3)), singletonSet(4))), 55)
                                                  //> res3: Boolean = false
  contains(
    filter(union(union(singletonSet(1), singletonSet(2)), singletonSet(5)),
      x => (x > 4)), 3)                           //> res4: Boolean = false

  forall(union(singletonSet(8), singletonSet(9)), (x: Int) => (x < 10))
                                                  //> res5: Boolean = true
  forall((x: Int) => (x < 100), (x: Int) => (x < 98))
                                                  //> res6: Boolean = false

  exists((x: Int) => (x < -1000), (x: Int) => (x > -1000))
                                                  //> res7: Boolean = false
  printSet(union(singletonSet(8), singletonSet(9)))
                                                  //> {8,9}
  printSet(x => (x > -20) && (x < 20))            //> {-19,-18,-17,-16,-15,-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3
                                                  //| ,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19}
  
  printSet( map( x => (0<x) && (x<3), (x: Int) => x*x) )
                                                  //> {1,4}
}