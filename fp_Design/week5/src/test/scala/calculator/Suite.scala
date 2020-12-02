package calculator

import org.junit._
import org.junit.Assert.assertEquals
import scala.collection.immutable.Queue

class Suite {
  import TweetLength._
  import Calculator._
  @Test def `mysuit`: Unit = {
    val aexpr = Var[Expr](Plus(Ref("b"), Literal(1)))
    val bexpr = Var[Expr](Times(Ref("c"), Ref("d")))
    val cexpr = Var[Expr](Plus(Literal(5), Ref("d")))
    val dexpr = Var[Expr](Minus(Literal(4), Literal(3)))
    val input = Map("a" -> aexpr, "b" -> bexpr, "c" -> cexpr, "d" -> dexpr)
    def verifyCyclic(
        namedExpressions: Map[String, Signal[Expr]]
    ): List[String] = {
      def findRef(expr: Expr): List[String] = {
        def findRef(l: List[String], q: Queue[Expr]): List[String] = {
          if (q.isEmpty) l
          else {
            val (expr, nq) = q.dequeue
            expr match {
              case Literal(v)   => findRef(l, nq)
              case Ref(name)    => findRef(name :: l, nq)
              case Plus(a, b)   => findRef(l, nq.enqueue(a).enqueue(b))
              case Minus(a, b)  => findRef(l, nq.enqueue(a).enqueue(b))
              case Times(a, b)  => findRef(l, nq.enqueue(a).enqueue(b))
              case Divide(a, b) => findRef(l, nq.enqueue(a).enqueue(b))
            }
          }
        }
        findRef(Nil, Queue.empty.enqueue(expr))
      }
      val sides: List[(String, String)] = namedExpressions.toList.flatMap {
        case (name, sig) => findRef(sig()).map((name, _))
      }
      def isSelfAndNotExistsRef(side: List[(String, String)]): List[String] =
        side
          .filter(elem =>
            elem._1 == elem._2 || namedExpressions.get(elem._2).isEmpty
          )
          .map(_._1)
      def isCyclic(side: List[(String, String)]): List[String] = {
        def toPoint(side: List[(String, String)]): List[(String, Int)] = {
          side
            .flatMap { case (a, b) => (a, 1) :: (b, 1) :: Nil }
            .groupMapReduce(_._1)(_._2)(_ + _)
            .toList
        }
        def minusLoop(
            mside: List[(String, String)],
            mpoint: List[(String, Int)]
        ): List[String] = {
          val onepoint = mpoint.filter(_._2 == 1).map(_._1)
          if (onepoint.isEmpty) mpoint.map(_._1)
          else {
            val newside = mside.filter {
              case (a, b) => !onepoint.contains(a) && !onepoint.contains(b)
            }
            val delepoint = mside
              .filter {
                case (a, b) => onepoint.contains(a) || onepoint.contains(b)
              }
              .flatMap(elem => (elem._1, -1) :: (elem._2, -1) :: Nil)
            val newpoint =
              (mpoint ::: delepoint)
                .groupMapReduce(_._1)(_._2)(_ + _)
                .toList
                .filter(_._2 != 0)
            println(s"side:${side.toBuffer}")
            println(
              s"newside:${newside.toBuffer},newpoint:${newpoint.toBuffer}"
            )
            minusLoop(newside, newpoint)
          }
        }
        minusLoop(side, toPoint(side))
      }
      def direction(cyc: List[String]) = {
        val length = sides
          .filter { case (a, b) => cyc.contains(a) && cyc.contains(b) }
          .flatMap { case (a, b) => (a, 1) :: (b, -1) :: Nil }
          .groupMapReduce(_._1)(_._2)(_ + _).toList
          .filter(_._2 != 0)
          .length
        if(length==0) cyc else Nil
      }
      println(isCyclic(sides).toBuffer)
      isSelfAndNotExistsRef(sides) ::: isCyclic(sides)
    }
    val output = verifyCyclic(input)
  }
}
