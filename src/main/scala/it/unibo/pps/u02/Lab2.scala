package it.unibo.pps.u02

import it.unibo.pps.u02.task4.Expression.Expr

object Lab2 extends App:
//  Task 1
  print("Hello, Scala")

  def divide(x: Double, y: Double): Double = x / y

  def divideCurried(x: Double)(y: Double): Double = x / y

  val res1: Double = divide(4, 4)

  val res2: Double = divideCurried(4)(4)

  val intermediateDiv: Double => Double = divideCurried(4)

  val finalDiv: Double = intermediateDiv(4)

  enum IntList:
    case Cons(head: Int, tail: IntList)
    case Nil

  def sum(l: IntList): Int = l match
    case IntList.Cons(h, t) => h + sum(t)
    case _ => 0

  println(sum(IntList.Cons(10, IntList.Cons(20, IntList.Nil))))

//  Task 2

  val positive: Int => String = {
    case n if n >= 0 => "positive"
    case _ => "negative"
  }

  def positive2(n: Int): String = n match
    case n if n >= 0 => "positive"
    case _ => "negative"

  val negative: (String => Boolean) => (String => Boolean) = predicate => (x => !predicate(x))

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  def p3(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  def p4(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

//  Task 3
def power(base: Double, exponent: Int): Double = exponent match {
  case exponent if exponent == 0 => 1
  case _ => base * power(base, exponent - 1)
}

def powerTail(base: Double, exponent: Int): Double =
  @annotation.tailrec
  def _power(base: Double, exponent: Int, acc: Double): Double = exponent match {
    case exponent if exponent == 0 => acc
    case _ => _power(base, exponent - 1, base * acc)
  }
  _power(base, exponent, 1)


def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _recursive(n: Int, acc: Int): Int = (n, acc) match {
      case (n, acc) if n == 0 => acc
      case (_, _) => _recursive(n/10, acc * 10 + n % 10)
    }
    _recursive(n, 0)

//  Task 4
object Expression:

  enum Expr:
    case Literal(value: Int)
    case Add(left: Expr, right: Expr)
    case Multiply(left: Expr, right: Expr)

  object Expr:

    def evaluate(expr: Expr): Int = expr match {
      case Expr.Literal(v) => v
      case Add(left, right) => evaluate(left) + evaluate(right)
      case Multiply(left, right) => evaluate(left) * evaluate(right)
    }

    def show(expr: Expr): String = expr match {
      case Expr.Literal(v) => s"$v"
      case Add(left, right) => s"(${show(left)} + ${show(right)})"
      case Multiply(left, right) => s"(${show(left)} * ${show(right)})"
    }

