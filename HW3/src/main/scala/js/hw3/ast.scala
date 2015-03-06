package js.hw3

import scala.util.parsing.input.Positional
import js.util._


object ast {
  sealed abstract class Expr extends Positional {
    // pretty print as AST
    override def toString(): String = print.prettyAST(this)
    // pretty print as JS expression
    def prettyJS(): String = print.prettyJS(this)
    // pretty print as value
    def prettyVal(): String = print.prettyVal(this)
  }
  
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
  
  /* Functions */
  case class Function(p: Option[String], x: String, e1: Expr) extends Expr
  case class Call(e1: Expr, e2: Expr) extends Expr
  
  /* Convenience function for making Seq constructors */
  def mkSeq(e1: Expr, e2: Expr): Expr =
    (e1, e2) match {
      case (Undefined, _) => e2
      case (_, Undefined) => e1
      case _ => BinOp(Seq, e1, e2)
    }
  
  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case Num(_) | Bool(_) | Str(_) | Function(_, _, _) | Undefined => true
    case _ => false
  }
  
  /* Define statements (used for pretty printing). */
  def isStmt(e: Expr): Boolean = e match {
    case ConstDecl(_, _, _) | Print(_) => true
    case BinOp(Seq, _, e2) => isStmt(e2)
    case _ => false
  }
  
  /* Get the free variables of e. */
  def fv(e: Expr): Set[String] = e match {
    case Var(x) => Set(x)
    case ConstDecl(x, e1, e2) => fv(e1) | (fv(e2) - x)
    case Function(None, x, e1) => fv(e1) - x
    case Function(Some(x1), x2, e1) => fv(e1) - x2 - x1
    case Num(_) | Bool(_) | Undefined | Str(_) => Set.empty
    case UnOp(_, e1) => fv(e1)
    case BinOp(_, e1, e2) => fv(e1) | fv(e2)
    case If (e1, e2, e3) => fv(e1) | fv(e2) | fv(e3)
    case Call(e1, e2) => fv(e1) | fv(e2)
    case Print(e1) => fv(e1)
  }
  
  /* Check whether the given expression is closed. */
  def closed(e: Expr): Boolean = fv(e).isEmpty
  
 /*
  * Dynamic Type Error exception.  Throw this exception to signal a dynamic type error.
  * 
  *   throw DynamicTypeError(e)
  * 
  */
  case class DynamicTypeError(e: Expr) extends JsException("Type Error", e.pos)
  
}