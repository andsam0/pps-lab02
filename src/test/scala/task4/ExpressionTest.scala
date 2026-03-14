package task4

import org.junit.*
import org.junit.Assert.*
import it.unibo.pps.u02.task4.Expression.*
import it.unibo.pps.u02.task4.Expression.Expr.*

class ExpressionTest:

  final val left: Literal = Expr.Literal(4)
  final val right: Literal = Expr.Literal(6)
  final val sum: Add = Expr.Add(left, right)
  final val mult: Multiply = Expr.Multiply(left, right)
  final val multComplex: Multiply = Expr.Multiply(sum, Expr.Literal(5))

  @Test
  def testLiteral (): Unit =
    val lit: Literal = Expr.Literal(4)
    assertEquals(4, lit.value)

  @Test
  def testAdd(): Unit =
    assertEquals(10, Expr.evaluate(sum))

  @Test
  def testMultiply(): Unit =
    assertEquals(24, Expr.evaluate(mult))

  @Test
  def testComplex(): Unit =
    assertEquals(50, Expr.evaluate(multComplex))

  @Test
  def testShowMultiply(): Unit =
    assertEquals("(4 * 6)", Expr.show(mult))

  @Test
  def testShowAdd(): Unit =
    assertEquals("(4 + 6)", Expr.show(sum))

  @Test
  def testShowComplex(): Unit =
    assertEquals("((4 + 6) * 5)", Expr.show(multComplex))