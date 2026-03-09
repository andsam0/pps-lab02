package task4

import org.junit.*
import org.junit.Assert.*
import it.unibo.pps.u02.task4.Expression.*
import it.unibo.pps.u02.task4.Expression.Expr.*

class ExpressionTest:

  @Test
  def testLiteral (): Unit =
    val lit: Literal = Expr.Literal(4)
    assertEquals(4, lit.value)

  @Test
  def testAdd(): Unit =
    val left: Literal = Expr.Literal(4)
    val right: Literal = Expr.Literal(6)
    val sum: Add = Expr.Add(left, right)
    assertEquals(10, Expr.evaluate(sum))

  @Test
  def testMultiply(): Unit =
    val left: Literal = Expr.Literal(4)
    val right: Literal = Expr.Literal(6)
    val mult: Multiply = Expr.Multiply(left, right)
    assertEquals(24, Expr.evaluate(mult))

  @Test
  def testComplex(): Unit =
    val l1: Literal = Expr.Literal(4)
    val l2: Literal = Expr.Literal(6)
    val add1: Add = Expr.Add(l1, l2)
    val l3: Literal = Expr.Literal(5)
    val mult1: Multiply = Expr.Multiply(add1, l3)
    assertEquals(50, Expr.evaluate(mult1))

  @Test
  def testShowMultiply(): Unit =
    val l1: Literal = Expr.Literal(4)
    val l2: Literal = Expr.Literal(6)
    val mult1: Multiply = Expr.Multiply(l1, l2)
    assertEquals("(4 * 6)", Expr.show(mult1))

  @Test
  def testShowAdd(): Unit =
    val l1: Literal = Expr.Literal(4)
    val l2: Literal = Expr.Literal(6)
    val add1: Add = Expr.Add(l1, l2)
    //    val l3: Literal = Expr.Literal(5)
    //    val add1: Add = Expr.Add(mult1, l3)
    assertEquals("(4 + 6)", Expr.show(add1))

  @Test
  def testShowComplex(): Unit =
    val l1: Literal = Expr.Literal(4)
    val l2: Literal = Expr.Literal(6)
    val add1: Add = Expr.Add(l1, l2)
    val l3: Literal = Expr.Literal(5)
    val mult1: Multiply = Expr.Multiply(add1, l3)
    assertEquals("((4 + 6) * 5)", Expr.show(mult1))