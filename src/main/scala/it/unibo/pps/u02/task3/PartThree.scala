package it.unibo.pps.u02.task3

import scala.annotation.tailrec


object PartThree {
//  2^4 = 2*2^3 ... 2^0

  def power(base: Double, exponent: Int): Double = (base, exponent) match {
    case (_, exponent) if exponent == 0 => 1
    case (_, _) => base * power(base, exponent - 1)
  }

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exponent: Int, acc: Double): Double= (base, exponent) match {
      case (_, exponent) if exponent == 0 => acc
      case (_, _) => _power(base, exponent - 1, base * acc)
    }
    _power(base, exponent, 1)

  def reverseNumber(n: Int): Int =
    @annotation.tailrec
    def _recursive(n: Int, acc: Int): Int = (n, acc) match {
      case (n, acc) if n == 0 => acc
      case (_, _) => _recursive(n/10, acc * 10 + n % 10)
    }
    _recursive(n, 0)
}
