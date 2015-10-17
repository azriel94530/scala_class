import week4._

object ConsLists {
  def singleton[T](elem: T): List[T] = new Cons[T](elem: T, new Nil[T]: List[T])
                                                  //> singleton: [T](elem: T)week4.List[T]
  def nth[T](thelist: List[T], n: Int): T = {
    if ( !thelist.isEmpty &&  n == 0 ) thelist.head
    else if (thelist.isEmpty) throw new IndexOutOfBoundsException("requested index larger than list size")
    else  nth(thelist.tail, n - 1)
  }                                               //> nth: [T](thelist: week4.List[T], n: Int)T

  val mylist = new Cons(23, new Cons(11, new Cons(91, new Nil)))
                                                  //> mylist  : week4.Cons[Int] = week4.Cons@b684286
  nth(mylist, -1)                                 //> java.lang.IndexOutOfBoundsException: requested index larger than list size
                                                  //| 	at ConsLists$$anonfun$main$1.nth$1(ConsLists.scala:7)
                                                  //| 	at ConsLists$$anonfun$main$1.apply$mcV$sp(ConsLists.scala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at ConsLists$.main(ConsLists.scala:3)
                                                  //| 	at ConsLists.main(ConsLists.scala)
}