package js.hw2

import scala.util.parsing.input.Positional

object ast {
  sealed abstract class Expr extends Positional
  
  /* Variables */
  case class Var(x: String) extends Expr
  
  /* Declarations */
  case class ConstDecl(x: String, e1: Expr, e2: Expr) extends Expr
  
  /* Literals and Values*/
  case class Num(n: Double) extends Expr
  case class Bool(b: Boolean) extends Expr
  case class Str(s: String) extends Expr
  case object Undefined extends Expr
  
  /* Unary and Binary Operators */
  case class UnOp(uop: Uop, e1: Expr) extends Expr
  case class BinOp(bop: Bop, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Uop
  
  case object UMinus extends Uop /* - */
  case object Not extends Uop /* ! */

  sealed abstract class Bop
  
  case object Plus extends Bop /* + */
  case object Minus extends Bop /* - */
  case object Times extends Bop /* * */
  case object Div extends Bop /* / */
  
  case object Eq extends Bop /* === */
  case object Ne extends Bop /* !== */
  case object Lt extends Bop /* < */
  case object Le extends Bop /* <= */
  case object Gt extends Bop /* > */
  case object Ge extends Bop /* >= */
  
  case object And extends Bop /* && */
  case object Or extends Bop /* || */
  
  case object Seq extends Bop /* , */
  
  /* Control constructs */
  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr
  
  /* I/O */
  case class Print(e1: Expr) extends Expr 
  
  
  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case Num(_) | Bool(_) | Str(_) | Undefined => true
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
      case Bool(b) => b.toString
      case Str(s) => s
      case Undefined => "undefined"
    }
  }
}