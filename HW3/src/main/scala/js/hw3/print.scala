package js.hw3

import org.kiama.output.PrettyPrinter
import ast._

object print extends PrettyPrinter {
  override val defaultIndent = 2
  
  /*
   * Pretty-print values.
   * 
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def prettyVal(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case Num(n) => n.toString
      case Bool(b) => b.toString
      case Str(s) => s
      case Undefined => "undefined"
      case Function(p, _, _) =>
        "[Function%s]".format(p match { case None => "" case Some(s) => ": " + s })
    }
  }
  
  /*
   * Determine precedence level of top-level constructor in an expression
   */  
  def prec(e: Expr): Int =
    e match {
      case v if isValue(v) => 0
      case Var(_) => 0
      case Call(_, _) => 1
      case Print(_) => 1
      case UnOp(_, _) => 2
      case BinOp(bop, _, _) =>
        bop match {
          case Times | Div => 3
          case Plus | Minus => 4
          case Gt | Ge | Lt | Le => 6
          case Eq | Ne => 7
          case And => 11
          case Or => 12
          case Seq => 16
        }
      case If(_, _, _) | ConstDecl(_, _, _) => 15
    }
  
  /* Maximal precedence level of statements */
  val precStmt = 15
  
  /* Associativity of binary operators */
  sealed abstract class Assoc
  case object Left extends Assoc
  case object Right extends Assoc
  
  /* 
   * Get associativity of a binary operator 
   * (all current operators are left-associative) 
   */
  def assoc(bop: Bop): Assoc = Left
  
  /*
   * Pretty-print expressions in concrete JavaScript syntax.
   */
  def showJS(e: Expr): Doc =
    e match {
      case Undefined => "undefined"
      case Num(d) => value(d)
      case Bool(b) => b.toString()
      case Str(s) => "'" <> s <> "'"
      case Var(x) => x
      case UnOp(uop, e) =>
        val op = uop match {
          case UMinus => "-"
          case Not => "!"
        }
        op <+> parens(showJS(e))
      case BinOp(bop, e1, e2) => {
        val op: Doc = bop match {
          case Plus => " + "
          case Minus => " - "
          case Times => " * "
          case Div => " / "
          case And => " && "
          case Or => " || "
          case Eq => " === "
          case Ne => " !== "
          case Lt => " < "
          case Le => " <= "
          case Gt => " > "
          case Ge => " >= "
          case Seq => 
            if (isStmt(e2)) ";" <> line else ", "
        }
        def eToDoc(e1: Expr, as: Assoc): Doc =
          if(prec(e1) < prec(e) || prec(e1) == prec(e) && as == assoc(bop)) 
            showJS(e1) 
          else parens(showJS(e1))
        eToDoc(e1, Left) <> op <> eToDoc(e2, Right)
      }
      case If(e1, e2, e3) =>
        showJS(e1) <+> "?" <+> showJS(e2) <+> ":" <+> showJS(e3)
      case Print(e) =>
        "console.log" <> parens(showJS(e))
      case ConstDecl(x, e1, e2) =>
        "const" <+> x <+> "=" <+> 
        nest(showJS(e1)) <> semi <> line <> line <> showJS(e2)
      case Call(e1, e2) =>
        showJS(e1) <> parens(showJS(e2))
      case Function(p, x, e) =>
        val name = p getOrElse ""
        "function" <+> name <> 
          parens(x) <+> braces(nest(line <> "return" <+> showJS(e)) <> line)
    }  
  
  def prettyAST(e: Expr): String = pretty(any(e))
  def prettyJS(e: Expr): String = pretty(showJS(e))
}
  
  