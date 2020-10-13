
package patmat

import scala.annotation.tailrec
import scala.collection.immutable.Queue

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

case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int)
  extends CodeTree {
  override def toString: String = s"Fork{$weight}\n $left , $right"
}

case class Leaf(char: Char, weight: Int) extends CodeTree {
  override def toString: String = s"Leaf($char,$weight)"
}

/**
 * Assignment 4: Huffman coding
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = // tree match ...
    tree match {
      case Fork(_, _, _, weight) => weight
      case Leaf(_, weight) => weight
    }

  def chars(tree: CodeTree): List[Char] = // tree
    tree match {
      case Fork(_, _, chars, _) => chars
      case Leaf(char: Char, _) => char :: Nil
    }

  def makeCodeTree(left: CodeTree, right: CodeTree): Fork =
    Fork(
      left,
      right,
      chars(left) ::: chars(right),
      weight(left) + weight(right)
    )

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   * times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   * List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   * val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   * val theChar = pair._1
   * val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   * pair match {
   * case (theChar, theInt) =>
   * println("character is: "+ theChar)
   * println("integer is  : "+ theInt)
   * }
   */
  def times(chars: List[Char]): List[(Char, Int)] =
    chars
      .map(c => (c, 1))
      .groupMapReduce(elem => elem._1)(elem => elem._2)(_ + _)
      .toList

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =
    freqs.sortBy(elem => elem._2).map(e => Leaf(e._1, e._2))

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    val (sum, max) = trees.foldLeft((0, 0))((acc, e) => {
      val (lw, fw) = e match {
        case l: Leaf => (l.weight, 0)
        case f: Fork => (0, f.weight)
      }
      (acc._1 + lw, if (acc._2 > fw) acc._2 else fw)
    })
    trees match {
      case Nil => false
      case _ :: Nil => true
      case _ => sum == max
    }
  }

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

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    //二分查找
    val findIndex: (Fork, List[CodeTree]) => Int = (f, l) => {
      @tailrec
      def findIndexAcc(start: Int, stop: Int): Int = {
        val medium = (start + stop) / 2
        val (left, right) = (weight(l(medium)), weight(f))
        if (start >= stop || left == right) stop
        else if (left > right) findIndexAcc(start, medium - 1)
        else if (left < right) findIndexAcc(medium + 1, stop)
        else throw new Exception(s"findIndex 未知参数 $start,$stop")
      }

      findIndexAcc(0, l.length - 1)
    }
    //有序插入元素
    val add: (Fork, List[CodeTree]) => List[CodeTree] = (fork, l) => {
      //      l.sortBy(node => weight(node))
      l match {
        case Nil => fork :: Nil
        case _ => val (f, b) = l.splitAt(findIndex(fork, l))
          f ::: (fork :: b)
      }

    }
    trees.sortBy(e => weight(e)) match {
      case Nil => Nil
      case h1 :: h2 :: tail => add(makeCodeTree(h1, h2), tail)
      case l => l
    }

  }

  /**
   * This function will be called in the following way:
   *
   * until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
  def until(
             done: List[CodeTree] => Boolean,
             merge: List[CodeTree] => List[CodeTree]
           )(trees: List[CodeTree]): List[CodeTree] = {
    @tailrec
    def untilAcc(trees: List[CodeTree]): List[CodeTree] =
      if (done(trees)) trees else untilAcc(merge(trees))

    untilAcc(trees)
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    @tailrec
    def bit2char(bits: List[Bit], tree: CodeTree): (Char, List[Bit]) =
      (bits, tree) match {
        case (bits, Leaf(char, _)) => (char, bits)
        case (0 :: tail, Fork(left, _, _, _)) => bit2char(tail, left)
        case (1 :: tail, Fork(_, right, _, _)) => bit2char(tail, right)
        case _ => throw new Exception("decodeAcc 未知参数")
      }

    @tailrec
    def decodeAcc(bits: List[Int], chars: List[Char]): List[Char] =
      bits match {
        case Nil => chars
        case bits =>
          val cb = bit2char(bits, tree)
          decodeAcc(cb._2, chars :+ cb._1)
      }

    decodeAcc(bits, Nil)
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   * http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(
    Fork(
      Fork(
        Leaf('s', 121895),
        Fork(
          Leaf('d', 56269),
          Fork(
            Fork(
              Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279),
              Leaf('f', 16351),
              List('x', 'j', 'f'),
              30630
            ),
            Fork(
              Fork(
                Fork(
                  Fork(
                    Leaf('z', 2093),
                    Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492),
                    List('z', 'k', 'w'),
                    4585
                  ),
                  Leaf('y', 4725),
                  List('z', 'k', 'w', 'y'),
                  9310
                ),
                Leaf('h', 11298),
                List('z', 'k', 'w', 'y', 'h'),
                20608
              ),
              Leaf('q', 20889),
              List('z', 'k', 'w', 'y', 'h', 'q'),
              41497
            ),
            List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'),
            72127
          ),
          List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'),
          128396
        ),
        List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'),
        250291
      ),
      Fork(
        Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430),
        Fork(
          Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856),
          Leaf('u', 96785),
          List('m', 'p', 'u'),
          188641
        ),
        List('o', 'l', 'm', 'p', 'u'),
        355071
      ),
      List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm',
        'p', 'u'),
      605362
    ),
    Fork(
      Fork(
        Fork(
          Leaf('r', 100500),
          Fork(
            Leaf('c', 50003),
            Fork(
              Leaf('v', 24975),
              Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110),
              List('v', 'g', 'b'),
              52085
            ),
            List('c', 'v', 'g', 'b'),
            102088
          ),
          List('r', 'c', 'v', 'g', 'b'),
          202588
        ),
        Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915),
        List('r', 'c', 'v', 'g', 'b', 'n', 't'),
        422503
      ),
      Fork(
        Leaf('e', 225947),
        Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575),
        List('e', 'i', 'a'),
        458522
      ),
      List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'),
      881025
    ),
    List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm',
      'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'),
    1486387
  )

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the 'frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0,
    1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    //遍历树
    @tailrec
    def find(c: Char, l: List[Bit], node: CodeTree): List[Bit] =
      node match {
        case Fork(Leaf(char, _), _, _, _) if char==c => l :+ 0
        case Fork(_, Leaf(char, _), _, _) if char==c => l :+ 1
        case Fork(left@Fork(_, _, chars, _), _, _, _) if chars.contains(c) =>
          find(c, l :+ 0, left)
        case Fork(_, right@Fork(_, _, chars, _), _, _) if chars.contains(c) =>
          find(c, l :+ 1, right)
        case _ => throw new Exception("find参数类型不匹配")
      }

    @tailrec
    def encodeAcc(text: List[Char], bits: List[Bit]): List[Bit] =
      text match {
        case Nil => bits
        case head :: tail =>
          encodeAcc(tail, bits ::: find(head, Nil, tree))
      }

    encodeAcc(text, Nil)
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = {
    val map = table.toMap
    map(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = {
    //层序遍历
    @tailrec
    def convertAcc(
                    q: Queue[Fork],
                    result: Map[Char, List[Bit]]
                  ): Map[Char, List[Bit]] =
      q.dequeue match {
        case (Fork(l: Fork, r: Fork, _, _), q) =>
          val nq = q.enqueue(l).enqueue(r)
          val nr = update(update(result, l.chars, 0), r.chars, 1)
          convertAcc(nq, nr)

        case (Fork(l: Leaf, r: Fork, _, _), q) =>
          val nq = q.enqueue(r)
          val nr = update(
            result.updated(l.char, result(l.char) :+ 0),
            r.chars,
            1
          )
          convertAcc(nq, nr)

        case (Fork(l: Fork, r: Leaf, _, _), q) =>
          val nq = q.enqueue(l)
          val nr = update(
            result.updated(r.char, result(r.char) :+ 1),
            l.chars,
            0
          )
          convertAcc(nq, nr)

        case (Fork(l: Leaf, r: Leaf, _, _), q) if q.nonEmpty =>
          val nr = result
            .updated(l.char, result(l.char) :+ 0)
            .updated(r.char, result(r.char) :+ 1)
          convertAcc(q, nr)

        case (Fork(l: Leaf, r: Leaf, _, _), q) if q.isEmpty =>
          result
            .updated(l.char, result(l.char) :+ 0)
            .updated(r.char, result(r.char) :+ 1)
      }


    //更新结果Map的辅助函数
    def update(
                result: Map[Char, List[Bit]],
                char: List[Char],
                b: Bit
              ): Map[Char, List[Bit]] =
      result.map(e => if (char.contains(e._1)) (e._1, e._2 :+ b) else e)

    tree match {
      case f: Fork =>
        convertAcc(
          Queue.empty[Fork].enqueue(f),
          f.chars.map(c => (c, Nil)).toMap
        ).toList
      case Leaf(char, _) => Map(char -> (0 :: Nil)).toList
    }
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  //merge Fork的left子树的codeTables和right子树的codeTables
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
    require((a.map(e => e._1) intersect b.map(e => e._1)).isEmpty)
    a ::: b
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val map = convert(tree).toMap

    @tailrec
    def quickEncodeAcc(text: List[Char], result: List[Bit]): List[Bit] =
      text match {
        case Nil => result
        case head :: next =>
          quickEncodeAcc(next, result ::: map(head))
      }

    quickEncodeAcc(text, Nil)
  }
}

object Huffman extends Huffman

object Main extends App {

  import Huffman._

  val str =
    "In order to find bugs in your code, we advise to perform the following steps".toList
  val tree = createCodeTree(str)
  println(decode(tree, encode(tree)(str)).mkString)
  println(decode(tree, quickEncode(tree)(str)).mkString)
}
