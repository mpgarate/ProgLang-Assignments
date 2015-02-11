package js.hw1

import scala.util.parsing.input.Positional

object ast {
  sealed abstract class Expr extends Positional
  
  /* Literals and Values*/
  case class Num(n: Double) extends Expr
  
  /* Unary and Binary Operators */
  case class UnOp(op: Uop, e1: Expr) extends Expr
  case class BinOp(op: Bop, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Uop
  
  case object UMinus extends Uop /* - */

  sealed abstract class Bop
  
  case object Plus extends Bop /* + */
  case object Minus extends Bop /* - */
  case object Times extends Bop /* * */
  case object Div extends Bop /* / */

  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case Num(_) => true
    case _ => false
  }
  
  /*
   * Pretty-print values.
   * 
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case Num(n) => n.toString
    }
  }
}