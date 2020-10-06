package recfun

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */

  def pascal(c: Int, r: Int): Int = {
    @tailrec
    def loop(accl: List[(Int, Int)], acc: Int): Int =
      (accl, acc) match {
        case (Nil, acc) => acc
        case ((cl, rl) :: tail, acc) if (cl == 0) => loop(tail, acc + 1)
        case ((cl, rl) :: tail, acc) if (cl == rl) => loop(tail, acc + 1)
        case ((cl, rl) :: tail, acc) =>
          loop((cl - 1, rl - 1) :: (cl, rl - 1) :: tail, acc)
      }

    loop((c, r) :: Nil, 0)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @annotation.tailrec
    def loop(chars: List[Char], acc: Int): Boolean = (chars, acc) match {
      case (Nil, 0) => true
      case (Nil, _) => false
      case (chars, acc) if (acc < 0) => false
      case (head :: tail, acc) => if (head == ('(')) loop(tail, acc + 1) else if (head == (')')) loop(tail, acc - 1) else loop(tail, acc)
    }

    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    @tailrec
    //q中元素
    def loop(q:Queue[(Int,Int)],acc:Int): Int = (q,acc) match {
      case (q,acc) if(q.isEmpty) => acc
      case (q,acc) if(q.head._1==0) => loop(q.dequeue._2 ,acc+1)
      case (q,acc) if (q.head._1 < 0) => loop(q.dequeue._2,acc)
      case (q,acc) =>{
        val elems = coins.drop(q.head._2).map(c => (q.head._1-c,coins.indexOf(c)))
        loop(q.dequeue._2.enqueueAll(elems),acc)
      }
    }
    if (coins.length > 0 && money >0) loop(Queue[(Int, Int)]((money,0)),0)
    else 0
  }
}
