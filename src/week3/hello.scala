package week3

/**
 * @author azrielgoldschmidt
 */
object hello {
  def main(args: Array[String]) = {
    println("hello dude")
    val t1 = Empty incl 4 incl 6
    val t2 = Empty incl 8 incl 5
    println(t1 union t2)
  }
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet): IntSet = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = {
    // this is BLACK MAGIC
    ((left union right) union other) incl elem
    // ((left union right) incl elem) union other
  }

  override def toString = "{" + left + elem + right + "}"
}

