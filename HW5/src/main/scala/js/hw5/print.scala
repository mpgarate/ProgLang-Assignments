package js.hw5

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
      case Function(p, _, _, _) =>
        "[Function%s]".format(p match { case None => "" case Some(s) => ": " + s })
      case Obj(fs) =>
        val fields =
          fs map {
            case (f, Str(s)) => f + ": '" + s + "'"
            case (f, v) => f + ": " + prettyVal(v)
          } reduceRight {
            (s, acc) => s + ",\n  " + acc
          }
        "{ %s }".format(fields)
    }
  }
  
  def prettyVal(m: Mem, v: Expr): String = {
    (v: @unchecked) match {
      case a @ Addr(_) if m contains a => prettyVal(m, m(a))
      case Obj(fields) =>
        val pretty_fields =
          fields map {
            case (f, Str(s)) => f + ": '" + s + "'"
            case (f, v) => f + ": " + prettyVal(m, v)
          } reduceRight {
            (s, acc) => s + ",\n  " + acc
          }
        "{ %s }".format(pretty_fields)
      case _ => prettyVal(v)
    }
  }
  
  /*
   * Determine precedence level of top-level constructor in an expression
   */  
  def prec(e: Expr): Int =
    e match {
      case v if isValue(v) => 0
      case Var(_) => 0
      case Obj(_) => 0
      case GetField(_, _) => 0
      case Addr(_) => 0
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
          case Seq => 17
          case Assign => 16
        }
      case If(_, _, _) => 15
      case Decl(_, _, _, _) => 15
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
  def assoc(bop: Bop): Assoc = bop match {
    case Seq => Right
    case _ => Left
  }
  
  def showTyp(typ: Typ): Doc = 
    typ match {
      case TBool => "bool"
      case TNumber => "number"
      case TString => "string"
      case TUndefined => "Undefined"
      case TFunction(txs, tret) =>
        parens(showTIdList(txs)) <+> "=>" <+> showTyp(tret)
      case TObj(tfs) => 
        braces(showTIdList(tfs.toList map {case (x, t) => (PConst, x, t)}))
    }
  
  def showTIdList(txs: Params): Doc = 
    hsep(txs map showTId, comma)
    
  def showTId(tid: (PMode, String, Typ)): Doc = {
      val pmode: Doc = tid._1 match {
        case PConst => empty
        case PVar => "var "
        case PName => "name "
        case PRef => "ref "
      }
      pmode <> tid._2 <> colon <+> showTyp(tid._3)
    }
    
  
  /*
   * Pretty-print expressions in concrete JavaScript syntax.
   */
  def showJS(e: Expr): Doc = {
    e match {
      case Undefined => "undefined"
      case Num(d) => value(d)
      case Bool(b) => b.toString()
      case Addr(a) => "@a"
      case Str(s) => "'" <> s <> "'"
      case Var(x) => x
      case eu @ UnOp(uop, e) =>
        val op: Doc = uop match {
          case UMinus => "-"
          case Not => "!"
          case Deref => "*"
        }
        op <+> (if (prec(e) < prec(eu)) showJS(e) else parens(showJS(e)))
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
          case Assign => " = "
        }
        def eToDoc(e1: Expr, as: Assoc): Doc =
          if (prec(e1) < prec(e) || prec(e1) == prec(e) && as == assoc(bop)) 
            showJS(e1) 
          else parens(showJS(e1))
        eToDoc(e1, Left) <> op <> eToDoc(e2, Right)
      }
      case ei @ If(e1, e2, e3) => {
        def eToDoc(e: Expr): Doc = {
          if (prec(e) < prec(ei)) showJS(e)
          else parens(showJS(e))
        }
        eToDoc(e1) <+> "?" <+> eToDoc(e2) <+> ":" <+> eToDoc(e3)
      }
      case Print(e) =>
        "console.log" <> parens(showJS(e))
      case Decl(m, x, e1, e2) =>
        val mut = m match {
          case MConst => "const"
          case MVar => "var"
        }
        mut <+> x <+> "=" <+> 
        nest(showJS(e1)) <> semi <> line <> line <> showJS(e2)
      case Call(e1, List(e @ BinOp(Seq, _, _))) if isStmt(e) =>
        showJS(e1) <> parens(braces(line <> indent(showJS(e)) <> line))
      case Call(e1, es) =>
        showJS(e1) <> parens(hsep(es map showJS, comma))
      case Function(p, xs, tann, e) =>
        val name = p getOrElse ""
        val params = parens(showTIdList(xs))
        val rtyp = tann map (":" <+> showTyp(_)) getOrElse empty
        "function" <+> name <> 
          params <> rtyp <+> braces(nest(line <> "return" <+> showJS(e)) <> line)
      case Obj(fs) =>
        val fields = fs map {
          case (f, e) => f <+> colon <+> nest(showJS(e))
        } reduce {
          (f1, f2) => f1 <> comma <+> softline <> f2
        }
        braces(empty <+> nest(fields) <+> empty)
      case GetField(e1, f) =>
        val e1_doc = if (prec(e1) <= prec(e)) showJS(e1) else parens(showJS(e1))
        e1_doc <> dot <> f
    }  
  }
  
  def prettyAST(x: Any): String = pretty(any(x))
  def prettyJS(e: Expr): String = pretty(showJS(e))
  def prettyTyp(typ: Typ): String = pretty(showTyp(typ))
}
  
  