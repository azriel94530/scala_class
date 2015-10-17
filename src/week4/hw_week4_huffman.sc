/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(lf, rg, ch, weight) => weight
    case Leaf(ch, weight)         => weight
  }                                               //> weight: (tree: Huffman.CodeTree)Int

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(lf, rg, charList, we) => charList
    case Leaf(ch, weight)           => List(ch)
  }                                               //> chars: (tree: Huffman.CodeTree)List[Char]

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))
                                                  //> makeCodeTree: (left: Huffman.CodeTree, right: Huffman.CodeTree)Huffman.Fork
                                                  //| 

  val sampleTree = makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2))                                 //> sampleTree  : Huffman.Fork = Fork(Fork(Leaf(x,1),Leaf(e,1),List(x, e),2),Le
                                                  //| af(t,2),List(x, e, t),4)

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList
                                                  //> string2Chars: (str: String)List[Char]

  // val mystring = string2Chars("Ut queant lactis Resonare fibris Mira gesuorum Famuli tuorum Solve poluti Lavi reatum Santus Ioannis")
  val mystring = string2Chars("abcdd")            //> mystring  : List[Char] = List(a, b, c, d, d)

  //   val mystring = string2Chars("aadcbbb")
  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */
  def times(chars: List[Char]): List[(Char, Int)] = times_accum(chars, List())
                                                  //> times: (chars: List[Char])List[(Char, Int)]

  def times_accum(chars: List[Char], accum_list: List[(Char, Int)]): List[(Char, Int)] = {
    if (chars.isEmpty) accum_list
    else {
      times_accum(chars.tail, freq_fill(chars.head, accum_list, List()))
    }
  }                                               //> times_accum: (chars: List[Char], accum_list: List[(Char, Int)])List[(Char, 
                                                  //| Int)]

  def freq_fill(char: Char, f_left: List[(Char, Int)], f_right: List[(Char, Int)]): List[(Char, Int)] = {
    if (f_left.isEmpty) f_left ::: List((char, 1)) ::: f_right
    else if (char == f_left.head._1) f_left.tail ::: List((char, f_left.head._2 + 1)) ::: f_right
    else freq_fill(char, f_left.tail, f_right ::: List(f_left.head))
  }                                               //> freq_fill: (char: Char, f_left: List[(Char, Int)], f_right: List[(Char, Int
                                                  //| )])List[(Char, Int)]
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    makeOrderedLeafListAccum(freqs, List())       //> makeOrderedLeafList: (freqs: List[(Char, Int)])List[Huffman.Leaf]

  def makeOrderedLeafListAccum(freqs: List[(Char, Int)], LeafListAccum: List[Leaf]): List[Leaf] = {
    if (freqs.isEmpty) LeafListAccum
    else if (LeafListAccum.isEmpty)
      makeOrderedLeafListAccum(freqs.tail, insertLeafInOrderedList(freqs.head, List(), List()))
    else {
      makeOrderedLeafListAccum(freqs.tail, insertLeafInOrderedList(freqs.head, LeafListAccum, List()))
    }
  }                                               //> makeOrderedLeafListAccum: (freqs: List[(Char, Int)], LeafListAccum: List[Hu
                                                  //| ffman.Leaf])List[Huffman.Leaf]

  def insertLeafInOrderedList(freq_pair: (Char, Int), LeafLeft: List[Leaf], LeafRight: List[Leaf]): List[Leaf] = {
    if (LeafLeft.isEmpty) LeafRight ::: List(Leaf(freq_pair._1, freq_pair._2))
    else if ((freq_pair._2 < weight(LeafLeft.head))) LeafRight ::: List(Leaf(freq_pair._1, freq_pair._2)) ::: LeafLeft
    else insertLeafInOrderedList(freq_pair, LeafLeft.tail, LeafRight ::: List(LeafLeft.head))
  }                                               //> insertLeafInOrderedList: (freq_pair: (Char, Int), LeafLeft: List[Huffman.Le
                                                  //| af], LeafRight: List[Huffman.Leaf])List[Huffman.Leaf]

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = (!trees.isEmpty) && trees.tail.isEmpty
                                                  //> singleton: (trees: List[Huffman.CodeTree])Boolean

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case Nil         => trees
    case tree :: Nil => trees
    case t1 :: t2 :: trem => {
      val tmerge = Fork(t1, t2, chars(t1) ::: chars(t2), weight(t1) + weight(t2))
      insertTreeInOrderedList(tmerge, trem, List())
    }
  }                                               //> combine: (trees: List[Huffman.CodeTree])List[Huffman.CodeTree]

  def insertTreeInOrderedList(tree: CodeTree, treeLeft: List[CodeTree], treeRight: List[CodeTree]): List[CodeTree] = {
    if (treeLeft.isEmpty) treeRight ::: List(tree)
    else if ((weight(tree) < weight(treeLeft.head))) treeRight ::: List(tree) ::: treeLeft
    else insertTreeInOrderedList(tree, treeLeft.tail, treeRight ::: List(treeLeft.head))
  }                                               //> insertTreeInOrderedList: (tree: Huffman.CodeTree, treeLeft: List[Huffman.Co
                                                  //| deTree], treeRight: List[Huffman.CodeTree])List[Huffman.CodeTree]

  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(single: List[CodeTree] => Boolean, comb: List[CodeTree] => List[CodeTree])(zzz: List[CodeTree]): List[CodeTree] =
    if (singleton(zzz)) zzz
    else until(single, comb)(comb(zzz))           //> until: (single: List[Huffman.CodeTree] => Boolean, comb: List[Huffman.CodeT
                                                  //| ree] => List[Huffman.CodeTree])(zzz: List[Huffman.CodeTree])List[Huffman.Co
                                                  //| deTree]

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
                                                  //> createCodeTree: (chars: List[Char])Huffman.CodeTree

  createCodeTree(string2Chars("abcdd"))           //> res0: Huffman.CodeTree = Fork(Fork(Leaf(c,1),Leaf(b,1),List(c, b),2),Fork(L
                                                  //| eaf(a,1),Leaf(d,2),List(a, d),3),List(c, b, a, d),5)

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] =
    decode_accum(tree, tree, bits, List())        //> decode: (tree: Huffman.CodeTree, bits: List[Huffman.Bit])List[Char]

  def decode_accum(tree: CodeTree, subtree: CodeTree, bits_remain: List[Bit], decoded: List[Char]): List[Char] = {
    subtree match {
      case Leaf(ch, w) => if (bits_remain.isEmpty) decoded ::: List(ch) else
        decode_accum(tree, tree, bits_remain, decoded ::: List(ch))
      case Fork(l, r, chs, w) => if (bits_remain.isEmpty) throw new Error("incomplete code") else if (bits_remain.head == 1) decode_accum(tree, r, bits_remain.tail, decoded)
      else decode_accum(tree, l, bits_remain.tail, decoded)
    }
  }                                               //> decode_accum: (tree: Huffman.CodeTree, subtree: Huffman.CodeTree, bits_rema
                                                  //| in: List[Huffman.Bit], decoded: List[Char])List[Char]

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)
                                                  //> frenchCode  : Huffman.CodeTree = Fork(Fork(Fork(Leaf(s,121895),Fork(Leaf(d,
                                                  //| 56269),Fork(Fork(Fork(Leaf(x,5928),Leaf(j,8351),List(x, j),14279),Leaf(f,16
                                                  //| 351),List(x, j, f),30630),Fork(Fork(Fork(Fork(Leaf(z,2093),Fork(Leaf(k,745)
                                                  //| ,Leaf(w,1747),List(k, w),2492),List(z, k, w),4585),Leaf(y,4725),List(z, k, 
                                                  //| w, y),9310),Leaf(h,11298),List(z, k, w, y, h),20608),Leaf(q,20889),List(z, 
                                                  //| k, w, y, h, q),41497),List(x, j, f, z, k, w, y, h, q),72127),List(d, x, j, 
                                                  //| f, z, k, w, y, h, q),128396),List(s, d, x, j, f, z, k, w, y, h, q),250291),
                                                  //| Fork(Fork(Leaf(o,82762),Leaf(l,83668),List(o, l),166430),Fork(Fork(Leaf(m,4
                                                  //| 5521),Leaf(p,46335),List(m, p),91856),Leaf(u,96785),List(m, p, u),188641),L
                                                  //| ist(o, l, m, p, u),355071),List(s, d, x, j, f, z, k, w, y, h, q, o, l, m, p
                                                  //| , u),605362),Fork(Fork(Fork(Leaf(r,100500),Fork(Leaf(c,50003),Fork(Leaf(v,2
                                                  //| 4975),Fork(Leaf(g,13288),Leaf(b,13822),List(g, b),27110),List(v, g, b),5208
                                                  //| 5),List(c, v, g, b),102
                                                  //| Output exceeds cutoff limit.

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
                                                  //> secret  : List[Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 
                                                  //| 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,
                                                  //|  0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1
                                                  //| )

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)
                                                  //> decodedSecret: => List[Char]

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] =
    encode_accum(tree, tree, List())(text)        //> encode: (tree: Huffman.CodeTree)(text: List[Char])List[Huffman.Bit]

  def encode_accum(tree: CodeTree, subtree: CodeTree, code: List[Bit])(text: List[Char]): List[Bit] =
    if (text.isEmpty) code
    else subtree match {
      case Leaf(ch, _) => if (ch == text.head) encode_accum(tree, tree, code)(text.tail) else
        throw new Error("Encoding: got to the leaf but the character is not present")
      case Fork(l, r, chs, _) => if (isinlist(text.head, chars(l))) encode_accum(tree, l, code ::: List(0))(text)
      else if (isinlist(text.head, chars(r))) encode_accum(tree, r, code ::: List(1))(text)
      else throw new Error("Encoding: in fork neither side has the character")
    }                                             //> encode_accum: (tree: Huffman.CodeTree, subtree: Huffman.CodeTree, code: Li
                                                  //| st[Huffman.Bit])(text: List[Char])List[Huffman.Bit]

  def isinlist(thechar: Char, thelist: List[Char]): Boolean = {
    if (thelist.isEmpty) false
    else if (thelist.head == thechar) true
    else isinlist(thechar, thelist.tail)
  }                                               //> isinlist: (thechar: Char, thelist: List[Char])Boolean
  
  encode(frenchCode)(string2Chars("huffmanestcool"))
                                                  //> res1: List[Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1
                                                  //| , 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 
                                                  //| 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = ???
                                                  //> codeBits: (table: Huffman.CodeTable)(char: Char)List[Huffman.Bit]

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = ???    //> convert: (tree: Huffman.CodeTree)Huffman.CodeTable

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???
                                                  //> mergeCodeTables: (a: Huffman.CodeTable, b: Huffman.CodeTable)Huffman.CodeT
                                                  //| able

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???
                                                  //> quickEncode: (tree: Huffman.CodeTree)(text: List[Char])List[Huffman.Bit]
}