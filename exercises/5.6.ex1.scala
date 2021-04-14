/* Excercise: Define a function that uses pattern matching on the Exprs we saw
  earlier to perform simple agebraic simplifacations.
  My method differs slightly and is more similar to a classic big-step operational
  semantics.
*/

sealed trait Expr { def reducible: Boolean }
case class BinOp(left: Expr, op: String, right: Expr) extends Expr {
  def reducible = (left, right) match {
    case (Literal(_), Literal(_)) => true
    case _ if right.reducible => true
    case _ if left.reducible => true
    case _ => false
  }
}
case class Literal(value: Int) extends Expr {
  def reducible = false
}
case class Variable(name: String) extends Expr {
  def reducible = false
}

object Expr {
  def stringify(expr: Expr): String = expr match {
    case BinOp(left, op, right) => s"(${stringify(left)} ${op} ${stringify(right)})"
    case Literal(value) => value.toString
    case Variable(name) => name
  }
  
  def reduce(expr: Expr): Expr = {
    val result = expr match {
      case BinOp(Literal(left), "+", Literal(right)) => Literal(left + right) 
      case BinOp(Literal(left), "*", Literal(right)) => Literal(left * right) 
      case BinOp(Literal(left), "-", Literal(right)) => Literal(left - right) 
      case BinOp(left, op, right) if left.reducible => BinOp(reduce(left), op, right)
      case BinOp(left, op, right) if right.reducible => BinOp(left, op, reduce(right))
      
      case Variable(name) => Variable(name)
      case Literal(value) => Literal(value)
    }

    if (result.reducible) reduce(result)
    else result 
  }
}