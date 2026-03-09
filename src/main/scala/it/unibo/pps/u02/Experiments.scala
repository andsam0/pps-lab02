package it.unibo.pps.u02

object Experiments extends App:

    def divide(x: Double, y: Double): Double = x / y

    def divideCurried(x: Double)(y: Double): Double = x / y

    val res1: Double = divide(4, 4)

    val res2: Double = divideCurried(4)(4)

    val intermediateDiv: Double => Double = divideCurried(4)

    val finalDiv: Double = intermediateDiv(4)

    // A LinkedList of Int
    enum IntList: // a recursive type
        case Cons(head: Int, tail: IntList)
        case Nil

    def sum(l: IntList): Int = l match
        case IntList.Cons(h, t) => h + sum(t)
        case _ => 0

    println(sum(IntList.Cons(10, IntList.Cons(20, IntList.Nil))))