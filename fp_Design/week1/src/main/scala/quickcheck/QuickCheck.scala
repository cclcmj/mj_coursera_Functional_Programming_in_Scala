package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  //first
  lazy val genHeap: Gen[H] = {
    def loop(n: Int): Gen[H] = {
      oneOf(
        const(empty),
        for {
          h <- if (n <= 0) const(empty) else oneOf(const(empty), loop(n - 1))
          x <- arbitrary[Int]
        } yield (insert(x, h))
      )
    }
    loop(10)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest of the two elements back.
    */
  property("gen2") = forAll { (x: Int, y: Int) =>
    val h = insert(y, insert(x, empty))
    findMin(h) == ord.min(x, y)
  }

  /**
    * If you insert an element into an empty heap
    * , then delete the minimum, the resulting heap should be empty.
    */
  property("gen3") = forAll { (x: Int) =>
    val h = insert(x, empty)
    isEmpty(deleteMin(h))
  }

  /**
    * Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.
    */
  property("gen4") = forAll(genHeap) { h =>
    @tailrec
    def loop(acc: List[Int], accH: H): List[Int] =
      if (isEmpty(accH)) acc
      else loop(acc :+ findMin(accH), deleteMin(accH))
    val result = loop(Nil, h)
    result == result.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("gen5") = forAll(genHeap, genHeap) { (h1, h2) =>
    val m = Try(findMin(meld(h1, h2)))
    val m1 = Try(findMin(h1))
    val m2 = Try(findMin(h2))
    m match {
      case Failure(_)      => true
      case m @ Success(mv) => m == m1 || m == m2
    }
  }
  property("isH") = forAll{
    (xl:List[Int])=>
      val h = xl.foldLeft(empty)((acc,e)=>insert(e,acc))
      def loop(acc: List[Int], accH: H): List[Int] =
      if (isEmpty(accH)) acc
      else loop(acc :+ findMin(accH), deleteMin(accH))
      val result = loop(Nil, h)
      result.toSet==xl.toSet
  }
}
