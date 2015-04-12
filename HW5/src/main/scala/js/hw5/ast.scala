package js.hw5

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
    def prettyVal(m: Mem): String = print.prettyVal(m, this)
  }
  
  /* Variables */
  case class Var(x: String) extends Expr
  
  /* Declarations */
  case class Decl(mut: Mut, x: String, e1: Expr, e2: Expr) extends Expr

  sealed abstract class Mut
  case object MConst extends Mut
  case object MVar extends Mut

  
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
  type Params = List[(PMode,String,Typ)]
  case class Function(p: Option[String], xs: Params, t: Option[Typ], e1: Expr) extends Expr
  case class Call(e1: Expr, es: List[Expr]) extends Expr
  
  /* Objects */
  case class Obj(fs: Map[String, Expr]) extends Expr
  case class GetField(e1: Expr, f: String) extends Expr
  
  /* Addresses and Mutation */
  case object Assign extends Bop
  case object Deref extends Uop /* *e */
  case class Addr private[ast] (addr: Int) extends Expr

 
  /* Parameter Passing Modes */
  sealed abstract class PMode
  case object PConst extends PMode
  case object PName extends PMode
  case object PVar extends PMode
  case object PRef extends PMode
   
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
  case class TFunction(txs: Params, tret: Typ) extends Typ {
    override def equals(other: Any) = other.isInstanceOf[TFunction] && {
      other match {
        case TFunction(otxs, otret) if otret == tret =>
          def proj(ps: Params) = 
            ps map { case (m, _, typ) => (m, typ) }
          proj(otxs) == proj(txs)
        case _ => false
      }
    }
  }
  case class TObj(tfs: Map[String, Typ]) extends Typ
  
  
  /* Memory */
  class Mem private (map: Map[Addr, Expr], nextAddr: Int) {
    def apply(key: Addr): Expr = map(key)
    def get(key: Addr): Option[Expr] = map.get(key)
    def +(kv: (Addr, Expr)): Mem = new Mem(map + kv, nextAddr)
    def contains(key: Addr): Boolean = map.contains(key)
    
    private def alloc(v: Expr): (Mem, Addr) = {
      val fresha = Addr(nextAddr)
      (new Mem(map + (fresha -> v), nextAddr + 1), fresha)
    }
    
    override def toString: String = map.toString
  }
  object Mem {
    def empty = new Mem(Map.empty, 1)
    def alloc(v: Expr): State[Mem, Addr] = {
      for {
        m <- State[Mem]
        (mp, a) = m.alloc(v)
        _ <- State.modify { (_: Mem) => mp }
      } yield
      a
    }
  }
  
  /* Convenience function for making Seq constructors */
  def mkSeq(e1: Expr, e2: Expr): Expr =
    (e1, e2) match {
      case (Undefined, _) => e2
      case (_, Undefined) => e1
      case _ => BinOp(Seq, e1, e2)
    }
  
  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case Num(_) | Bool(_) | Str(_) | Function(_, _, _, _) | 
         Undefined | Addr(_) => true
    case _ => false
  }
  
  /* Define statements (used for pretty printing). */
  def isStmt(e: Expr): Boolean = e match {
    case Undefined | Decl(_, _, _, _) | Print(_) | BinOp(Assign, _, _) => true
    case BinOp(Seq, _, e2) => isStmt(e2)
    case _ => false
  }
  
  def isLExpr(e: Expr): Boolean = e match {
    case Var(_) | GetField(_, _) => true
    case _ => false
  }
  
  def isLValue(e: Expr): Boolean = e match {
    case UnOp(Deref, Addr(_)) | GetField(Addr(_), _) => true
    case _ => false
  }
  
  def isBaseType(t: Typ): Boolean = t match {
    case TNumber | TBool | TString | TUndefined  => true
    case _ => false
  }
  
  /* Get the free variables of e. */
  def fv(e: Expr): Set[String] = e match {
    case Var(x) => Set(x)
    case Decl(_, x, e1, e2) => fv(e1) | (fv(e2) - x)
    case Function(p, xs, _, e1) => fv(e1) -- (xs map (_._2)) -- p
    case Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) => Set.empty
    case UnOp(_, e1) => fv(e1)
    case BinOp(_, e1, e2) => fv(e1) | fv(e2)
    case If (e1, e2, e3) => fv(e1) | fv(e2) | fv(e3)
    case Call(e1, es) => fv(e1) | (es.toSet flatMap fv)
    case Print(e1) => fv(e1)
    case Obj(fs) =>       
      fs.foldLeft(Set.empty: Set[String]){ (acc: Set[String], p: (String, Expr)) => acc | fv(p._2) }
    case GetField(e1, _) => fv(e1)
  }
  
  /* Transformation visitor. */
  def transformVisitor[Env](visitant: (Env => Expr => Expr) => Env => PartialFunction[Expr, Expr])(env: Env)(e: Expr): Expr = {
    def loop(env: Env)(e: Expr): Expr = {
      val tr: Expr => Expr = loop(env)
      val f = visitant(loop)(env).orElse[Expr,Expr] {
        case Var(_) | Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) => e
        case Print(e1) => Print(tr(e1))
        case UnOp(uop, e1) => UnOp(uop, tr(e1))
        case BinOp(bop, e1, e2) => BinOp(bop, tr(e1), tr(e2))
        case If(e1, e2, e3) => If(tr(e1), tr(e2), tr(e3))
        case Decl(mut, y, e1, e2) => Decl(mut, y, tr(e1), tr(e2))
        case Function(p, xs, retty, e1) => Function(p, xs, retty, tr(e1))
        case Call(e1, args) => Call(tr(e1), args map tr)
        case Obj(fields) => Obj(fields mapValues tr)
        case GetField(e1, f) => GetField(tr(e1), f)
      }
      f(e)
    }
    loop(env)(e)
  }
  
  def transformVisitorSimple(visitant: (Expr => Expr) => PartialFunction[Expr, Expr])(e: Expr): Expr = {
    def myvisitant(tr: Unit => Expr => Expr): Unit => PartialFunction[Expr,Expr] = { _ => visitant(tr()) }
    transformVisitor[Unit](myvisitant)()(e)
  }
  
    
  
  
  /* Rename bound variables in e to avoid capturing free variables in esub. */
  def avoidCapture(avoidVars: Set[String], e: Expr): Expr = {
    def renameVar(x: String): String = if (avoidVars contains x) renameVar(x + "$") else x
    
    def rename(env: Map[String,String], e: Expr): Expr = {
      def ren(e: Expr): Expr = rename(env, e)
      e match {
        case Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) => e
        case Print(e1) => Print(ren(e1))
        case UnOp(uop, e1) => UnOp(uop, ren(e1))
        case BinOp(bop, e1, e2) => BinOp(bop, ren(e1), ren(e2))
        case If(e1, e2, e3) => If(ren(e1), ren(e2), ren(e3))
        case Var(y) => Var(env.getOrElse(y, y))
        case Decl(mut, y, e1, e2) =>
          val yrenamed = renameVar(y)
          Decl(mut, yrenamed, ren(e1), rename(env + (y -> yrenamed), e2))
        case Function(p, xs, retty, e1) =>
          val (env1, pRenamed) = p match {
            case None => (env, None)
            case Some(y) =>
              val yrenamed = renameVar(y)
              (env + (y -> yrenamed), Some(yrenamed))
          }
          val (env2, xsRenamed) = {
            val (envnew, revxsRenamed) = xs.foldLeft((env1, Nil: Params)) {
              case ((envacc, renamedacc), (m, y, t)) =>
                val yrenamed = renameVar(y)
                (envacc + (y -> yrenamed), (m, yrenamed, t) :: renamedacc)
            }
            (envnew, revxsRenamed.reverse)
          }
          Function(pRenamed, xsRenamed, retty, rename(env2, e1))
        case Call(e1, args) => Call(ren(e1), args map ren)
        case Obj(fs) => Obj(fs map { case (f,e) => (f, ren(e)) })
        case GetField(e1, f) => GetField(ren(e1), f)      
      }
    }
    rename(Map.empty, e)
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