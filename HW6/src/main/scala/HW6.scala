import js.util.State

object HW6 extends js.util.JsApp {
  import js.hw6.ast._
  import js.hw6._
  import js.util._
  import scala.language.implicitConversions
  import scala.util.parsing.input.NoPosition
  
  /*
   * CSCI-UA.0480-006: Homework 6
   * <Your Name>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */


  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your solution will _not_ be graded if it does not compile!!
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   *
   */
  
  
  /*** Subtyping ***/
  
  
  /** subtype checks whether type s is a subtype of type t.
   *  
   *  The function ties the coinductive knots by returning a
   *  state monad. This monad caches the trail of subtype 
   *  relationships that are currently being checked, respectively,
   *  have already been checked. 
   */
  def subtype(s: Typ, t: Typ): StateBoolean[Set[(Typ, Typ)]] = 
    State.bool({ (c: Set[(Typ, Typ)]) =>
      if (c contains (s,t)) (c, true) 
      else (c + ((s,t)), false) 
    }) || subtypeBasic(s, t)
    
  /** subtypeBasic implements the actual subtype relation. 
   *  All recursive calls inside subtypeBasic must go to the function subtype.
   */
  def subtypeBasic(s: Typ, t: Typ): StateBoolean[Set[(Typ, Typ)]] = 
    (s, t) match {
      // SubFun
      case (TFunction(sxs, sret), TFunction(txs, tret)) =>
        if (sxs.length <= txs.length || subtype(sret, tret) == State.falseS){
          State.falseS
        } else {
          ((sxs, txs).zipped foldLeft State.trueS[Set[(Typ, Typ)]]){
            case (b, (xs, tx)) => subtype(xs._2, tx._2)
          }
        }
  
      // SubObj
      case (TObj(sfs), TObj(tfs)) =>
        (tfs foldLeft State.trueS[Set[(Typ, Typ)]]) {
          case (b, (f, t1)) => sfs.get(f) match {
            case Some(t2) => subtype(t2, t1)
            case None => State.falseS
          }
        }
      case (TNull, TObj(_)) => State.trueS
      case (_, t @ TInterface(tvar, t1)) =>
        subtype(s, t.unfold)
      case (s @ TInterface(svar, s1), _) =>
        subtype(s.unfold, t)
      case (s, t) => State.bool(s == t)
    }

  /** Joins and meets need to be computed simultaneously.
   *  We use a cache that stores the trail of joins and meets for
   *  the recursive types that are encountered during the traversal.
   */
  abstract class Bound
  case object Meet extends Bound
  case object Join extends Bound
  
  type Cache = Map[(Typ, Bound, Typ), Typ]
  
  /* Helper function used by join and meet. */
  def checkCache(b: Bound, s: Typ, t: Typ): StateOption[Cache, Typ] = 
    State.option( c => {
      val c1 = (s, t) match {
        case (_, TInterface(tvar, t1)) =>
          c + ((s, b, t) -> TVar(tvar))
        case (TInterface(svar, s1), _) =>
          c + ((s, b, t) -> TVar(svar))
        case _ => c
      }
      (c1, c.get(s, b, t))
    })
  
  /** Compute the join of s and t if it exists.
   *  
   * The call to checkCache ties the coinductive knots. The actual
   * implementation of the join operator is in joinBasic. 
   */
  def join(s: Typ, t: Typ): StateOption[Cache, Typ] = 
    checkCache(Join, s, t) orElse joinBasic(s, t)
     
  def joinBasic(s: Typ, t: Typ): StateOption[Cache, Typ] = {
    (s, t) match {
      case (TNull, TObj(_)) => State.some(t)
      case (TObj(_), TNull) => State.some(s)
      case (_, t @ TInterface(tvar, t1)) =>
        for { u1 <- join(s, t.unfold) } yield TInterface(tvar)(u1)
      case (s @ TInterface(svar, s1), _) =>
        for { u1 <- join(s.unfold, t) } yield TInterface(svar)(u1)
      // JoinFun
      case (TFunction(sxs, sret), TFunction(txs, tret)) if sxs.length == txs.length =>
        ???
      // JoinObj
      case (TObj(sfs), TObj(tfs)) =>
        val sufs = (sfs foldLeft State.some[Cache, Map[String, Typ]](Map.empty)) {
          case (sufs, (f, s1)) => 
            ???
        }
        for { ufs <- sufs } yield TObj(ufs)
      case (s, t) =>
        if (s == t) State.some(s) else State.none
    }
  }
  
  /** Compute the meet of s and t if it exists.
   *  
   * The call to checkCache ties the coinductive knots. The actual
   * implementation of the meet operator is in meetBasic. 
   */
  def meet(s: Typ, t: Typ): StateOption[Cache, Typ] = 
    checkCache(Meet, s, t) orElse meetBasic(s, t)
  
  // All recursive calls must go to function meet.
  def meetBasic(s: Typ, t: Typ): StateOption[Cache, Typ] = {
    (s, t) match {
      case (TNull, TObj(_)) | (TObj(_), TNull) => 
        State.some(TNull)
      case (_, t @ TInterface(tvar, t1)) =>
        for { u1 <- meet(s, t.unfold) } yield TInterface(tvar)(u1)
      case (s @ TInterface(svar, s1), _) =>
        for { u1 <- meet(s.unfold, t) } yield TInterface(svar)(u1)
      // MeetFun
      case (TFunction(sxs, sret), TFunction(txs, tret)) if sxs.length == txs.length =>
        ???
      // MeetObj
      case (TObj(sfs), TObj(tfs)) =>
        ???
      case (s, t) =>
        if (s == t) State.some(s) else State.none
    }
  }
  
  /** We use the "pimp my library" pattern to extend Typ with
   *  new operators related to subtyping. 
   */
  class Subtyp(t: Typ) {
    // check whether t is a subtype of s
    def <:<(s: Typ): Boolean = subtype(t, s) get (Set.empty)
    
    // check whether t is a supertype of s
    def >:>(s: Typ): Boolean = subtype(s, t) get (Set.empty)
    
    // check whether t and s are equal modulo subtyping
    def =:=(s: Typ): Boolean = 
      (subtype(s, t) && subtype(t, s)) get (Set.empty)
    
    // compute join of t and s if it exists
    def |:|(s: Typ): Option[Typ] = join(t, s) get (Map.empty)
    
    // compute meet of t and s if it exists
    def &:&(s: Typ): Option[Typ] = meet(t, s) get (Map.empty)
  }
  
  // implicit conversion of Typ to Subtyp
  implicit def toSubtyp(t: Typ): Subtyp = new Subtyp(t)
  
  
  /*** Type Inference ***/

  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fs) if (fs exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  } 

  def typeInfer(env: Map[String,(Mut,Typ)], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1)
    def suberr[T](texp: Typ, tgot: Typ, e1: Expr): T = 
      throw new StaticSubTypeError(texp, tgot, e1)
    
    def typLE(e: Expr) = e match {
      case Var(x) => 
        env(x) match {
          case (MVar, t1) => t1
          case (MConst, t1) => err(t1, e)
        }
      case GetField(_, _) => typ(e)
      case _ => err(typ(e), e)
    }
    
    e match {
      case Print(e1) => typ(e1); TUndefined
      case Num(_) => TNumber
      case Bool(_) => TBool
      case Undefined => TUndefined
      case Str(_) => TString
      case Null => TNull
      case Var(x) =>
        val (_, t) = env(x)
        t
      case UnOp(UMinus, e1) => typ(e1) match {
        case TUnfold(TNumber) => TNumber
        case tgot => err(tgot, e1)
      }
      case UnOp(Not, e1) => typ(e1) match {
        case TUnfold(TBool) => TBool
        case tgot => err(tgot, e1)
      }
      case BinOp(Plus, e1, e2) => typ(e1) match {
        case TUnfold(TNumber) => typ(e2) match {
          case TUnfold(TNumber) => TNumber
          case tgot => err(tgot, e2)
        }
        case TUnfold(TString) => typ(e2) match {
          case TUnfold(TString) => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(Minus|Times|Div, e1, e2) => typ(e1) match {
        case TUnfold(TNumber) => typ(e2) match {
          case TUnfold(TNumber) => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      // TypeEqual
      case BinOp(Eq|Ne, e1, e2) => typ(e1) match {
        case t1 if !hasFunctionTyp(t1) => 
          ???
        case tgot => err(tgot, e1)
      }
      case BinOp(Lt|Le|Gt|Ge, e1, e2) => typ(e1) match {
        case TUnfold(TNumber) => typ(e2) match {
          case TUnfold(TNumber) => TBool
          case tgot => err(tgot, e2)
        }
        case TUnfold(TString) => typ(e2) match {
          case TUnfold(TString) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(And|Or, e1, e2) => typ(e1) match {
        case TUnfold(TBool) => typ(e2) match {
          case TUnfold(TBool) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(Seq, e1, e2) => typ(e1); typ(e2)
      case If(e1, e2, e3) => typ(e1) match {
        case TUnfold(TBool) =>
          ???
        case tgot => err(tgot, e1)
      }
      case Obj(fs) => TObj(fs map { case (f,t) => (f, typ(t)) })
      case GetField(e1, f) => typ(e1) match {
        case TUnfold(TObj(tfs)) if (tfs contains f) => tfs(f)
        case tgot => err(tgot, e1)
      } 
      
      case Function(p, xs, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(rt)) =>
            val tprime = TFunction(xs, rt)
            env + (f -> (MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with the parameters.
        val env2 = (xs foldLeft env1) {
          case (env2, (x, typ)) => env2 + (x -> (MConst, typ))
        }
        // Infer the type of the function body
        val t1 = typeInfer(env2, e1)
        // Need both TypeFunAnn and TypeFunRec
        ???
      }
      
      case Call(e1, args) => typ(e1) match {
        case TUnfold(TFunction(xs, tret)) if (xs.length == args.length) => {
          (xs, args).zipped.foreach {
            ???
          }
          tret
        }
        case tgot => err(tgot, e1)
      }
      
      case Decl(mut, x, e1, e2) =>
        val t1 = typ(e1)
        val env1 = env + (x -> (mut, t1))
        typeInfer(env1, e2)
       
      // TypeAssign --- need both TypeAssignVar and TypeAssignFld
      case BinOp(Assign, e1, e2) => 
        val t1 = typLE(e1)
        val t2 = typ(e2)
        ???
      
      /*** Fill-in more cases here. ***/
        
//
//      // TypeCast
//      case UnOp(Cast(t), e1) =>
//        ???
        
      /* Should not match: non-source expressions or should have been eliminated */
      case Addr(_) | UnOp(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }
  
  /*** Small-Step Interpreter ***/
  
  /* Do the operation for an inequality. */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (Str(s1), Str(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (Num(n1), Num(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
  
  /* Substitution in e replacing variable x with esub. */
  def substitute(e: Expr, x: String, esub: Expr): Expr = {
    def subst(e: Expr): Expr = substitute(e, x, esub)
    val ep = e match {
      case Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) | Null => e
      case Print(e1) => Print(subst(e1))
      case UnOp(uop, e1) => UnOp(uop, subst(e1))
      case BinOp(bop, e1, e2) => BinOp(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else e
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, xs, tann, e1) =>
        if (p == Some(x) || (xs exists (_._1 == x))) e
        else Function(p, xs, tann, subst(e1))      
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fs) => Obj(fs mapValues (subst(_)))
      case GetField(e, f) => GetField(subst(e), f)
      case InterfaceDecl(tvar, t, e) => InterfaceDecl(tvar, t, subst(e))
    }
    if (ep.pos != NoPosition) ep else ep.setPos(e.pos)
  }

  /* A small-step transition. */
  def step(e: Expr): State[Mem, Expr] = {
    require(!isValue(e), "stepping on a value:\n%s".format(e))
    
    println("e: " + e)
    
    /*** Helper for Call ***/

    def stepFirst(l: List[Expr]): State[Mem, List[Expr]] = l match {
      case Nil => State.insert(Nil)
      case e :: es if !isValue(e) => 
       for (ep <- step(e)) yield (List(ep))
      case e :: es =>
        stepFirst(es)
    }
    
    /*** Body ***/
    val s: State[Mem, Expr] = e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => 
        for (m <- State.init[Mem]) yield { println(v1.prettyVal(m)); Undefined }
      case UnOp(UMinus, Num(n1)) => 
        State.insert( Num(- n1) )
      case UnOp(Not, Bool(b1)) => 
        State.insert( Bool(! b1) )
      case BinOp(Seq, v1, e2) if isValue(v1) => 
        State.insert( e2 )
      case BinOp(Plus, Str(s1), Str(s2)) => 
        State.insert( Str(s1 + s2) )
      case BinOp(Plus, Num(n1), Num(n2)) => 
        State.insert( Num(n1 + n2) )
      case BinOp(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => 
        State.insert( Bool(inequalityVal(bop, v1, v2)) )
      case BinOp(Eq, v1, v2) if isValue(v1) && isValue(v2) => 
        State.insert( Bool(v1 == v2) )
      case BinOp(Ne, v1, v2) if isValue(v1) && isValue(v2) => 
        State.insert( Bool(v1 != v2) )
      case BinOp(And, Bool(b1), e2) => 
        State.insert( if (b1) e2 else Bool(false) )
      case BinOp(Or, Bool(b1), e2) => 
        State.insert( if (b1) Bool(true) else e2 )
      case BinOp(Minus, Num(n1), Num(n2)) => 
        State.insert( Num(n1 - n2) )
      case BinOp(Times, Num(n1), Num(n2)) => 
        State.insert( Num(n1 * n2) )
      case BinOp(Div, Num(n1), Num(n2)) => 
        State.insert( Num(n1 / n2) )
      case If(Bool(b1), e2, e3) => 
        State.insert( if (b1) e2 else e3 )
      // DoObj?
      case Obj(fs) if (fs forall { case (_, vi) => isValue(vi)}) =>
        for (a <- Mem.alloc(e)) yield a
      case GetField(a @ Addr(_), f) =>
        for (m <- State.init[Mem]) yield { 
          val Obj(fs) = m(a); 
          fs.get(f) getOrElse { throw DynamicTypeError(e) } 
        }
      case Call(v1, es) if isValue(v1) && (es forall isValue) =>
        v1 match {
          case Function(p, txs, _, e1) => {
            val e1p = (txs, es).zipped.foldRight(e1){
              //substitute each parameter into the expression
              (param, en) => println("param: " + param + "\nen: " + en); param match {
                case ((str, _), ei) => println("in substitution"); substitute(en, str, ei)
              }
            }
            println("e1p: " + e1p)
            p match {
              case None => State.insert(e1p)
              case Some(x1) => State.insert(substitute(e1p, x1, v1))
            }
          }
          case _ => throw new StuckError(e)
        }
      // DoConst
      case Decl(MConst, x, v1, e2) if isValue(v1) =>
        State.insert(substitute(e2, x, v1))
      // DoVarDecl
      case Decl(MVar, x, v1, e2) if isValue(v1) =>
        for (a <- Mem.alloc(v1)) yield substitute(e2, x, UnOp(Deref, a))
      // DoAssignVar
      case BinOp(Assign, UnOp(Deref, a @ Addr(_)), v) if isValue(v) =>
        for (_ <- State.modify { (m: Mem) => m + (a -> v) }) yield v
      // DoAssignField
      case BinOp(Assign, GetField(a @ Addr(_), f), v) if isValue(v) =>
        for (m <- State.init[Mem];
             _ <- State.modify { (m: Mem) => 
               val e1 @ Obj(fs) = m(a)
               if (fs contains f) m + (a -> Obj(fs + (f -> v)).setPos(e1.pos))
               else throw DynamicTypeError(e)
             }) yield v
      // DoDeref
      case UnOp(Deref, a @ Addr(_)) =>
        for (m <- State.init[Mem]) yield m(a)
 
      /*** Fill-in more Do cases here. ***/
      // DoCast
      case UnOp(Cast(ta), e1) => 
        State.insert(e1) 
      // DoNullDeref
      case UnOp(Deref, e1) => throw NullDereferenceError(e1)
      // DoNullAssign
      case BinOp(Assign, Null, e2) => throw NullDereferenceError(e2)
        
      /* Inductive Cases: Search Rules */
      case Print(e1) =>
        for (e1p <- step(e1)) yield Print(e1p)
      case UnOp(uop, e1) =>
        for (e1p <- step(e1)) yield UnOp(uop, e1p)
      // SearchAssign2
      case BinOp(Assign, lv1, e2) if isLValue(lv1) =>
        for (e2p <- step(e2)) yield BinOp(Assign, lv1, e2p)
      case BinOp(bop, v1, e2) if isValue(v1) =>
        for (e2p <- step(e2)) yield BinOp(bop, v1, e2p)
      //SearchAssign1 + SearchBop1?
      case BinOp(bop, e1, e2) =>
        for (e1p <- step(e1)) yield BinOp(bop, e1p, e2)
      case If(e1, e2, e3) =>
        for (e1p <- step(e1)) yield If(e1p, e2, e3)
      case Obj(fs) => fs find { case (_, ei) => !isValue(ei) } match {
        case Some((fi,ei)) =>
          for (eip <- step(ei)) yield Obj(fs + (fi -> eip))
        case None => throw StuckError(e)
      }
      case GetField(e1, f) =>
        for (e1p <- step(e1)) yield GetField(e1p, f)
      case Decl(mut, x, v1, e2) if isValue(v1) =>
        for (e2p <- step(e2)) yield Decl(mut, x, v1, e2p)
      case Decl(mut, x, e1, e2) =>
        for (e1p <- step(e1)) yield Decl(mut, x, e1p, e2)

      /*** Fill-in more Search cases here. ***/
      
//      case Call(v1, e2) if (isValue(v1)) =>
//        State.insert(stepFirst(e2))
        
      // SearchCall2
//      case Call(v1, arg :: e2) if (isValue(v1)) =>
//        for (argp <- step(arg)) yield Call(v1, List(argp))
//      case Call(v1, arg :: e2) if (isValue(v1)) =>
//        State.insert(Call(v1, List(arg)))

      //SearchCall2 
      case Call(v1, args) if (isValue(v1)) =>
        println("in searchCall2")
        for (argp <- stepFirst(args)) yield Call(v1, argp)
        
      // SearchCall1
      case Call(e1, e2) =>
        for (e1p <- step(e1)) yield Call(e1p, e2)
        
      /* Everything else is a dynamic type error. */
      case _ => println(e); (throw DynamicTypeError(e)): State[Mem, Expr]
    }
    s map ((ep: Expr) => if (ep.pos != NoPosition) ep else ep.setPos(e.pos))
  } 

  /*** External Interfaces ***/
  
  override def init() = {
    //this.debug = true // comment this out or set to false if you don't want print debugging information
    this.maxSteps = Some(500) // comment this out or set to None to not bound the number of steps.
  }
  
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    } 
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("\nInferred type: " + print.prettyTyp(t))
    }
    t
  }
  
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging. 
  
  case class TerminationError(e: Expr) extends JsException("TerminationError: ran out of steps during evaluation", e.pos)
  
  def iterateStep(e: Expr): Expr = {
    require(closed(e), "not a closed expression: free variables: %s".format(fv(e)) )
    def loop(e: Expr, n: Int): State[Mem,Expr] =
      if (Some(n) == maxSteps) throw TerminationError(e)
      else if (isValue(e)) State.insert( e )
      else {
        for {
          m <- State.init[Mem]
          _ = if (debug) { println("Step %s:%n  %s%n  %s".format(n, m, e)) }
          ep <- step(e)
          epp <- loop(ep, n + 1)
        } yield
        epp
      }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val (m,v) = loop(e, 0)(Mem.empty)
    if (debug) {
      println("Result:%n  %s%n  %s".format(m,v))
    }
    v
  }

  // Convenience to pass in a js expression as a string.
  def iterateStep(s: String): Expr = iterateStep(parse.fromString(s))
  
  // Interface for main
  def processFile(file: java.io.File) {
    if (debug) {
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")
    }
    
    val expr = handle(fail()) {
      parse.fromFile(file)
    }
      
    if (debug) {
      println("Parsed expression:")
      println(expr.prettyJS())
    }
      
    val exprRem = removeInterfaceDecl(expr)
    
    handle(fail()) {
      val t = inferType(exprRem)
    }
    
    handle() {
      if (eval) {
        val v1 = iterateStep(exprRem)
        println(v1.prettyVal())
      }
    }
  }
    
}