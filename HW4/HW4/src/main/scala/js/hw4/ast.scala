package js.hw4

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
  case class Function(p: Option[String], xs: List[(String, Typ)], tann: Option[Typ], e1: Expr) extends Expr
  case class Call(e1: Expr, es: List[Expr]) extends Expr
  
  /* Objects */
  case class Obj(fs: Map[String, Expr]) extends Expr
  case class GetField(e1: Expr, f: String) extends Expr
  
  /* Types */
  sealed abstract class Typ {
    // pretty print as AST
    override def toString(): String = print.prettyAST(this)
    // pretty print as JS expression
    def pretty(): String = print.prettyTyp(this)
  }
  case object TNumber extends Typ
  case object TBool extends Typ
  case object TString extends Typ
  case object TUndefined extends Typ
  case class TFunction(txs: List[(String,Typ)], tret: Typ) extends Typ
  case class TObj(tfs: Map[String, Typ]) extends Typ
  
  
  /* Convenience function for making Seq constructors */
  def mkSeq(e1: Expr, e2: Expr): Expr =
    (e1, e2) match {
      case (Undefined, _) => e2
      case (_, Undefined) => e1
      case _ => BinOp(Seq, e1, e2)
    }
  
  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case Num(_) | Bool(_) | Str(_) | Function(_, _, _, _) | Undefined => true
    case Obj(fs) => fs forall { case (_, ei) => isValue(ei) }
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
    case Function(p, xs, _, e1) => fv(e1) -- (xs map (_._1)) -- p
    case Num(_) | Bool(_) | Undefined | Str(_) => Set.empty
    case UnOp(_, e1) => fv(e1)
    case BinOp(_, e1, e2) => fv(e1) | fv(e2)
    case If (e1, e2, e3) => fv(e1) | fv(e2) | fv(e3)
    case Call(e1, es) => fv(e1) | (es.toSet flatMap fv)
    case Print(e1) => fv(e1)
    case Obj(fs) => 
      
      fs.foldLeft(Set.empty: Set[String]){ (acc: Set[String], p: (String, Expr)) => acc | fv(p._2) }
    case GetField(e1, _) => fv(e1)
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
  
  /*
   * Static Type Error exception.  Throw this exception to signal a static
   * type error.
   * 
   *   throw StaticTypeError(tbad, esub, e)
   * 
   */
  case class StaticTypeError(tbad: Typ, e: Expr) extends 
    JsException("Type Error: " + "invalid type " + tbad.pretty(), e.pos)
  
 /*
  * Stuck Type Error exception.  Throw this exception to signal getting
  * stuck in evaluation.  This exception should not get raised if
  * evaluating a well-typed expression.
  * 
  *   throw StuckError(e)
  * 
  */
  case class StuckError(e: Expr) extends JsException("stuck while evaluating expression", e.pos)

  
}