package it.unibo.pps.u02


object PartThree {
//  2^4 = 2*2^3 ... 2^0

  def power(base: Double, exponent: Int): Double = (base, exponent) match {
    case (_, exponent) if exponent == 0 => 1
    case (_, _) => base * power(base, exponent-1)
  }

  def powerTail(base: Double, exponent: Int): Double =
    @annotation.tailrec
    def _power(base: Double, exponent: Int, acc: Double): Double= (base, exponent) match {
      case (_, exponent) if exponent == 0 => acc
      case (_, _) => _power(base, exponent - 1, base*acc)
    }
    _power(base, exponent, 1)

  def reverseNumber(n: Int): Int = n match {
    case 
  }

  def helper(remaining: Int, current: Int): Int = {
    
  }
}
