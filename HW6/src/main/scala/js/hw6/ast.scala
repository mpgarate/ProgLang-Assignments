package js.hw6

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
  case class InterfaceDecl(tvar: String, tobj: Typ, e: Expr) extends Expr
  
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
  type Params = List[(String, Typ)]
  case class Function(p: Option[String], xs: Params, t: Option[Typ], e1: Expr) extends Expr
  case class Call(e1: Expr, es: List[Expr]) extends Expr
  
  /* Objects */
  case class Obj(fs: Map[String, Expr]) extends Expr
  case class GetField(e1: Expr, f: String) extends Expr
  
  /* Addresses and Mutation */
  case object Assign extends Bop
  case object Deref extends Uop /* *e */
  case class Addr private[ast] (addr: Int) extends Expr
  case object Null extends Expr
 
     
  /* Casting */
  case class Cast(t: Typ) extends Uop

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
  case object TNull extends Typ
  case class TFunction(txs: Params, tret: Typ) extends Typ {
    override def equals(other: Any) = other.isInstanceOf[TFunction] && {
      other match {
        case TFunction(otxs, otret) if otret == tret =>
          def proj(ps: Params) = ps map (_._1)
          proj(otxs) == proj(txs)
        case _ => false
      }
    }
  }
  case class TObj(tfs: Map[String, Typ]) extends Typ
  case class TVar(tvar: String) extends Typ
  case class TInterface(tvar: String, typ: Typ) extends Typ {
    def unfold: Typ = typSubstitute(typ, tvar, this)
  }
  
  object TInterface {
    def apply(tvar: String)(typ: Typ): Typ = {
      if (ftv(typ) contains tvar) TInterface(tvar, typ) else typ
    }
  }
  
  object TUnfold {
    def unapply(t: Typ): Some[Typ] = {
      def unfoldFully(t: Typ, seen: Set[String]): Some[Typ] = t match {
        case t @ TInterface(tvar, _) if !(seen contains tvar) => 
          unfoldFully(t.unfold, seen + tvar)
        case _ => Some(t)
      }
      unfoldFully(t, Set.empty)
    }
  }
  
  
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
        m <- State.init[Mem]
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
         Undefined | Addr(_) | Null => true
    case _ => false
  }
  
  /* Define statements (used for pretty printing). */
  def isStmt(e: Expr): Boolean = e match {
    case Undefined | Decl(_, _, _, _) | InterfaceDecl(_, _, _) | 
         Print(_) | BinOp(Assign, _, _) => true
    case BinOp(Seq, _, e2) => isStmt(e2)
    case _ => false
  }
  
  def hasFunction(e: Expr): Boolean = e match {
    case Function(_, _, _, _) => true
    case BinOp(_, e1, e2) => hasFunction(e1) || hasFunction(e2)
    case UnOp(_, e1) => hasFunction(e1)
    case Print(e1) => hasFunction(e1)
    case Decl(_, _, e1, e2) => hasFunction(e1) || hasFunction(e2)
    case InterfaceDecl(_, _, e) => hasFunction(e)
    case Call(e, args) => (e :: args) exists hasFunction
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
    case TNumber | TBool | TString | TUndefined | TNull => true
    case _ => false
  }
  
  /* Get the free variables of e. */
  def fv(e: Expr): Set[String] = e match {
    case Var(x) => Set(x)
    case Decl(_, x, e1, e2) => fv(e1) | (fv(e2) - x)
    case Function(p, xs, _, e1) => fv(e1) -- (xs map (_._1)) -- p
    case Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) | Null => Set.empty
    case UnOp(_, e1) => fv(e1)
    case BinOp(_, e1, e2) => fv(e1) | fv(e2)
    case If (e1, e2, e3) => fv(e1) | fv(e2) | fv(e3)
    case Call(e1, es) => fv(e1) | (es.toSet flatMap fv)
    case Print(e1) => fv(e1)
    case Obj(fs) =>       
      fs.foldLeft(Set.empty: Set[String]){ (acc: Set[String], p: (String, Expr)) => acc | fv(p._2) }
    case GetField(e1, _) => fv(e1)
    case InterfaceDecl(_, _, e1) => fv(e1)
  }
  
  /* Get the free type variables of t. */
  def ftv(t: Typ): Set[String] = t match {
    case TNumber | TBool | TString | TUndefined | TNull => Set()
    case TVar(tvar) => Set(tvar)
    case TInterface(tvar, t1) => ftv(t1) - tvar
    case TFunction(xs, tret) => ftv(tret) | (xs map (_._2)).toSet.flatMap(ftv)
    case TObj(tfs) => (tfs foldLeft (Set.empty: Set[String])) { 
      case (tvars, (f, t)) => tvars | ftv(t)
    }
  }
  
  /* Transformation visitor. */
  def transformVisitor[Env](visitant: (Env => Expr => Expr) => Env => PartialFunction[Expr, Expr])(env: Env)(e: Expr): Expr = {
    def loop(env: Env)(e: Expr): Expr = {
      val tr: Expr => Expr = loop(env)
      val f = visitant(loop)(env).orElse[Expr,Expr] {
        case Var(_) | Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) | Null => e
        case Print(e1) => Print(tr(e1))
        case UnOp(uop, e1) => UnOp(uop, tr(e1))
        case BinOp(bop, e1, e2) => BinOp(bop, tr(e1), tr(e2))
        case If(e1, e2, e3) => If(tr(e1), tr(e2), tr(e3))
        case Decl(mut, y, e1, e2) => Decl(mut, y, tr(e1), tr(e2))
        case Function(p, xs, retty, e1) => Function(p, xs, retty, tr(e1))
        case Call(e1, args) => Call(tr(e1), args map tr)
        case Obj(fields) => Obj(fields mapValues tr)
        case GetField(e1, f) => GetField(tr(e1), f)
        case InterfaceDecl(tvar, t, e1) => InterfaceDecl(tvar, t, tr(e1))
      }
      f(e)
    }
    loop(env)(e)
  }
  
  def transformVisitorSimple(visitant: (Expr => Expr) => PartialFunction[Expr, Expr])(e: Expr): Expr = {
    def myvisitant(tr: Unit => Expr => Expr): Unit => PartialFunction[Expr,Expr] = { _ => visitant(tr(())) }
    transformVisitor[Unit](myvisitant)()(e)
  }
  
    
  def transformTypVisitor[Env](visitant: (Env => Typ => Typ) => Env => PartialFunction[Typ, Typ])(env: Env)(t: Typ): Typ = {
    def loop(env: Env)(t: Typ): Typ = {
      val tr: Typ => Typ = loop(env)
      val f = visitant(loop)(env).orElse[Typ,Typ] {
        case TNumber | TBool | TString | TUndefined | TNull | TVar(_) => t
        case TFunction(xs, rt) =>
          val xsp = xs map { case (x,t) => (x, tr(t)) }
          TFunction(xsp, tr(rt))
        case TObj(fs) => TObj(fs mapValues tr)
        case TInterface(tvar, t1) => TInterface(tvar)(tr(t1))
      }
      f(t)
    }
    loop(env)(t)
  }
  
    def transformTypVisitorSimple(visitant: (Typ => Typ) => PartialFunction[Typ, Typ])(t: Typ): Typ = {
    def myvisitant(tr: Unit => Typ => Typ): Unit => PartialFunction[Typ,Typ] = { _ => visitant(tr(())) }
    transformTypVisitor[Unit](myvisitant)()(t)
  }
  
  /* Substitute in type t replacing uses of type variable tvar with type tp */
  def typSubstitute(t: Typ, tvar: String, tp: Typ): Typ = {
    def subst(tr: Typ => Typ): PartialFunction[Typ,Typ] = {
      case TVar(tvarp) => if (tvar == tvarp) tp else t
      case TInterface(tvarp, t1) =>
        if (tvar == tvarp) t // tvar shadowed by tvarp
        else TInterface(tvarp, tr(t1))
    }
    transformTypVisitorSimple(subst)(t)
  }
  
  /* Substitute in an expression e all uses of type variable tvar with type tp */
  def typSubstituteExpr(tp: Typ, tvar: String, e: Expr): Expr = {
    def tysubst(t: Typ): Typ = typSubstitute(t, tvar, tp)
    def subst(tr: Expr => Expr): PartialFunction[Expr, Expr] = {
      case UnOp(Cast(t), e1) => UnOp(Cast(tysubst(t)), tr(e1))
      case Function(p, xs, tann, e1) =>
        val xsp = xs map { case (x, t) => (x, tysubst(t)) }
        Function(p, xsp, tann map tysubst, tr(e1))
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException
    }
    transformVisitorSimple(subst)(e)
  }
  
  /* Remove interface declarations. */
  def removeInterfaceDecl(e: Expr): Expr = {
    type Env = Map[String, Typ]
    def removeFromTyp(env: Env, t: Typ): Typ = {
      def tyrm(tr: Env => Typ => Typ)(env: Env): PartialFunction[Typ,Typ] = {
        case TVar(tvar) => env.getOrElse(tvar, throw new IllegalArgumentException("Unknown type name %s.".format(tvar)))
        /* Should never match because introduced in this pass. */
        case TInterface(_, _) => throw new IllegalArgumentException("Gremlins: Encountered TInterface in removeInterfaceDecl.")
      }
      transformTypVisitor(tyrm)(env)(t)
    }
    def loop(env: Map[String, Typ], e: Expr): Expr = {
      def tyrm(t: Typ): Typ = removeFromTyp(env, t)
      def rm(e: Expr): Expr = loop(env, e)
      val r =
        e match {
          case UnOp(Cast(t), e1) => UnOp(Cast(tyrm(t)), rm(e1))
          case Function(p, xs, tann, e1) =>
            val xsp = xs map { case (x, t) => (x, tyrm(t)) }
            Function(p, xsp, tann map tyrm, rm(e1))
          case InterfaceDecl(tvar, t, e1) =>
            val tp = TInterface(tvar)(removeFromTyp(env + (tvar -> TVar(tvar)), t))
            loop(env + (tvar ->tp), e1)
          /* Pass through cases. */
          case Var(_) | Num(_) | Bool(_) | Undefined | Str(_) | Null | Addr(_) => e
          case Print(e1) => Print(rm(e1))
          case UnOp(uop, e1) => UnOp(uop, rm(e1))
          case BinOp(bop, e1, e2) => BinOp(bop, rm(e1), rm(e2))
          case If(e1, e2, e3) => If(rm(e1), rm(e2), rm(e3))
          case Decl(mut, y, e1, e2) => Decl(mut, y, rm(e1), rm(e2))
          case Call(e1, args) => Call(rm(e1), args map rm)
          case Obj(fields) => Obj(fields map { case (f, e) => (f, rm(e)) })
          case GetField(e1, f) => GetField(rm(e1), f)
        }
      /* Patch up positions for error messages. */
      e match {
        case InterfaceDecl(_, _, _) => r
        case _ => r setPos e.pos
      }
    }
    loop(Map.empty, e)
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
   *   throw StaticTypeError(tbad, e)
   * 
   */
  /*
  case class StaticTypeError(texp: Typ, tbad: Typ, e: Expr) extends 
    JsException("Type Error: expected type: " + texp.pretty + " but got: " + tbad.pretty(), e.pos)
  */
  case class StaticTypeError(tbad: Typ, e: Expr) extends 
    JsException("Type Error: invalid type: " + tbad.pretty(), e.pos)

 /*
   * Static Subtype Error exception.  Throw this exception to signal a static
   * violation of the subtyping relation.
   * 
   *   throw StaticSubTypeError(texp, tbad, e)
   * 
   */
  
 case class StaticSubTypeError(texp: Typ, tbad: Typ, e: Expr) extends 
    JsException("Type Error: type " + tbad.pretty + " is not a subtype of " + texp.pretty, e.pos)

 /*
  * Stuck Type Error exception.  Throw this exception to signal getting
  * stuck in evaluation.  This exception should not get raised if
  * evaluating a well-typed expression.
  * 
  *   throw StuckError(e)
  * 
  */
  case class StuckError(e: Expr) extends JsException("stuck while evaluating expression", e.pos)

 /*
  * Null Dereference Error exception.  Throw this exception to signal a null
  * pointer dereference error.
  * 
  *   throw NullDereferenceError(e)
  * 
  */
  case class NullDereferenceError(e: Expr) extends JsException("Null Dereference Error", e.pos)
}