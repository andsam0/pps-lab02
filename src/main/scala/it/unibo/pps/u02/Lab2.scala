package it.unibo.pps.u02

import it.unibo.pps.u02.task4.Expression.Expr
import task5.Optionals.OptionalInt

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
  val positive: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"

  def positive2: Int => String =
    case n if n >= 0 => "positive"
    case _ => "negative"

  val negative: (String => Boolean) => String => Boolean = predicate => x => !predicate(x)

  val p1: Int => Int => Int => Boolean = x => y => z => x <= y && y == z

  val p2: (Int, Int, Int) => Boolean = (x, y, z) => x <= y && y == z

  def p3(x: Int, y: Int, z: Int): Boolean = x <= y && y == z

  def p4(x: Int)(y: Int)(z: Int): Boolean = x <= y && y == z

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

//  Task 3
  def power(base: Double, exponent: Int): Double = (base, exponent) match
    case (_, exponent) if exponent == 0 => 1
    case (_, _) => base * power(base, exponent - 1)


  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exponent: Int, acc: Double): Double = (base, exponent) match
      case (_, exponent) if exponent == 0 => acc
      case (_, _) => _power(base, exponent - 1, base * acc)
    _power(base, exponent, 1)

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _recursive(n: Int, acc: Int): Int = (n, acc) match
      case (n, acc) if n == 0 => acc
      case (_, _) => _recursive(n / 10, acc * 10 + n % 10)
    _recursive(n, 0)

//  Task 4
  enum Expr:
    case Literal(value: Int)
    case Add(left: Expr, right: Expr)
    case Multiply(left: Expr, right: Expr)

  object Expr:
    def evaluate(expr: Expr): Int = expr match
      case Expr.Literal(v) => v
      case Add(left, right) => evaluate(left) + evaluate(right)
      case Multiply(left, right) => evaluate(left) * evaluate(right)


    def show(expr: Expr): String = expr match
      case Expr.Literal(v) => s"$v"
      case Add(left, right) => s"(${show(left)} + ${show(right)})"
      case Multiply(left, right) => s"(${show(left)} * ${show(right)})"


//  Task4 Test
class ExpressionTest:
  final val left: Literal = Expr.Literal(4)
  final val right: Literal = Expr.Literal(6)
  final val sum: Add = Expr.Add(left, right)
  final val mult: Multiply = Expr.Multiply(left, right)
  final val multComplex: Multiply = Expr.Multiply(sum, Expr.Literal(5))

  @Test
  def testLiteral (): Unit =
    assertEquals(4, left.value)

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

//  Task 5
enum OptionalInt:
  case Just(value: Int)
  case Empty()

  object OptionalInt:
    def isEmpty(opt: OptionalInt): Boolean = opt match
      case Empty() => true
      case _       => false

    def orElse(opt: OptionalInt, orElse: Int): Int = opt match
      case Just(a) => a
      case _       => orElse

    def mapInt(opt: OptionalInt)(f: Int => Int): OptionalInt = opt match
      case Just(a) => Just(f(a))
      case _ => OptionalInt.Empty()


    def filter(opt: OptionalInt)(pred: Int => Boolean): OptionalInt = opt match
      case Just(a) if pred(a) => Just(a)
      case _ => OptionalInt.Empty()


//  Task 5 Test
class OptionalIntTest:
  final val empty = OptionalInt.Empty()
  final val nonEmpty = OptionalInt.Just(0)

  @Test def emptyOptionalShouldBeEmpty(): Unit =
    assertTrue(OptionalInt.isEmpty(empty))

  @Test def nonEmptyOptionalShouldNotBeEmpty(): Unit =
    assertFalse(OptionalInt.isEmpty(nonEmpty))

  @Test def orElseShouldReturnDefaultWhenEmpty(): Unit =
    assertEquals(0, OptionalInt.orElse(nonEmpty, 1))

  @Test def orElseShouldReturnValueWhenNonEmpty(): Unit =
    assertEquals(1, OptionalInt.orElse(empty, 1))

  /** Task 5: do test for map * */
  @Test def mapShouldReturnValue(): Unit =
    assertEquals(OptionalInt.Just(1), OptionalInt.mapInt(nonEmpty)(_ + 1))

  @Test def mapShouldReturnEmpty(): Unit =
    assertEquals(OptionalInt.Empty(), OptionalInt.mapInt(empty)(_ + 1))

  @Test def filterShouldPass(): Unit =
    val okOpt = OptionalInt.Just(5)
    assertEquals(OptionalInt.Just(5), OptionalInt.filter(okOpt)(_ > 2))

  @Test def filterShouldNotPassWhenNonEmpty(): Unit =
    val notOkOpt = OptionalInt.Just(5)
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(notOkOpt)(_ > 8))

  @Test def filterShouldNotPassWhenEmpty(): Unit =
    assertEquals(OptionalInt.Empty(), OptionalInt.filter(empty)(_ > 2))
