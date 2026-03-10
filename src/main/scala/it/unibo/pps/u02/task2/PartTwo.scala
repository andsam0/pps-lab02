package it.unibo.pps.u02.task2

object PartTwo {

  val positive: Int => String = {
    case n if n >= 0 => "positive"
    case _ => "negative"
  }

  def positive2: Int => String = {
    case n if n >= 0 => "positive"
    case _ => "negative"
  }

  val negative: (String => Boolean) => (String => Boolean) = predicate => (x => !predicate(x))

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y==z

  val p2: (Int, Int, Int) => Boolean = (x,y,z) => x <= y && y==z

  def p3(x: Int, y: Int, z: Int): Boolean = x <= y && y==z

  def p4(x: Int)(y: Int)(z: Int): Boolean = x <= y && y==z

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))
}
