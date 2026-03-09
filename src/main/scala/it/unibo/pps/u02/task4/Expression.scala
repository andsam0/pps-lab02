package it.unibo.pps.u02.task4

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

