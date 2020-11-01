package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class binomialHeapCheck
    extends Properties("binomiaHeapCheck")
    with BinomialHeap {
  type A = Int
  def  genNode(max:Int,length:Int): Gen[Node] = {
      for{
          x <- arbitrary[Int] suchThat(_ < max)
          l <- frequency((9,const(0)),(1,arbitrary[Int] suchThat(_ <=3)))
          nl <- containerOfN[List,Node](l,genNode(x,l))
      }yield (Node(x,length,nl))
  }
  lazy val genHeap: Gen[H] = for{
       l <- arbitrary[Int] suchThat(_<=5)
       max <- arbitrary[Int]
        subl <- frequency((9,const(0)),(1,arbitrary[Int] suchThat(_<=3)))
       nl <- containerOfN[List,Node](l,genNode(max,subl))
  }yield(nl)
}
