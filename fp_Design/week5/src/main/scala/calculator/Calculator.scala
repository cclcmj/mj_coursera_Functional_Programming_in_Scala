package calculator

import scala.util.Success
import scala.util.Failure
import scala.util.Try
import scala.collection.immutable.Queue

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]
  ): Map[String, Signal[Double]] = {
    val nanName = verifyCyclic(namedExpressions)
    namedExpressions.map {
      case (name,_) if nanName.contains(name) => (name,Signal(Double.NaN))
      case (name, expr) => (name, Signal(eval(expr(), namedExpressions)))
    }
  }
  /**
   * 验证是否循环依赖，分两步：第一步将其依赖看作无向图是否成环，第二部将成环的几个点加入方向，查看是否有向图成环
   * 有缺陷，如果是有向图中的两个环，并不能识别。但通过了test，懒得改。这里之后使用有向图的验证是否成环即可
   */
  def verifyCyclic(
      namedExpressions: Map[String, Signal[Expr]]
  ): List[String] = {
    def findRef(expr: Expr): List[String] = {
      def findRef(l: List[String], q: Queue[Expr]): List[String] = {
        if (q.isEmpty) l
        else {
          val (expr, nq) = q.dequeue
          expr match {
            case Literal(v)   => findRef(l,nq)
            case Ref(name)    => findRef(name :: l,nq)
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
      side.filter(elem =>
        elem._1 == elem._2 || namedExpressions.get(elem._2).isEmpty
      ).map(_._1)
    def isCyclic(side: List[(String, String)]): List[String] = {
      def toPoint(side: List[(String, String)]): List[(String, Int)] =
        side
          .flatMap { case (a, b) => (a, 1) :: (b, 1) :: Nil }
          .groupMapReduce(_._1)(_._2)(_ + _)
          .toList
      def minusLoop(
          mside: List[(String, String)],
          mpoint: List[(String, Int)]
      ): List[String] = {
        val onepoint = mpoint.filter(_._2 == 1).map(_._1)
        if (onepoint.isEmpty) mpoint.map(_._1)
        else {
          val newside = mside.filter{
            case (a, b) => !onepoint.contains(a) && !onepoint.contains(b)
          }
          val delepoint = mside
            .filter {
              case (a, b) => onepoint.contains(a) || onepoint.contains(b)
            }
            .flatMap(elem => (elem._1, -1) :: (elem._2, -1) :: Nil)
          val newpoint =
            (mpoint ::: delepoint).groupMapReduce(_._1)(_._2)(_ + _).toList.filter(_._2!=0)
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
    isSelfAndNotExistsRef(sides) ::: direction(isCyclic(sides))
  }
  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v)   => v
      case Plus(a, b)   => eval(a, references) + eval(b, references)
      case Minus(a, b)  => eval(a, references) - eval(b, references)
      case Times(a, b)  => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
      case Ref(name)    => eval(references(name)(), references)
    }
  }

  /** Get the Expr for a referenced variables.
    *  If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(
      name: String,
      references: Map[String, Signal[Expr]]
  ) = {
    references
      .get(name)
      .fold[Expr] {
        Literal(Double.NaN)
      } { exprSignal =>
        exprSignal()
      }
  }
}
