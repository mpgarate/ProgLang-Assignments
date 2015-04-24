import js.util.State

object HW5 extends js.util.JsApp {
  import js.hw5.ast._
  import js.hw5._
  import js.util._
  
  /*
   * CSCI-UA.0480-006: Homework 5
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
  
  
  /*** Type Inference ***/
  
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fs) if (fs exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  } 
    
  def mut(m: PMode): Mut = m match {
    case PName | PConst => MConst
    case PVar | PRef => MVar
  }
  
  def typeInfer(env: Map[String,(Mut,Typ)], e: Expr): Typ = {
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw new StaticTypeError(tgot, e1)

    e match {
      case Print(e1) => typ(e1); TUndefined
      case Num(_) => TNumber
      case Bool(_) => TBool
      case Undefined => TUndefined
      case Str(_) => TString
      case Var(x) =>
        val (_, t) = env(x)
        t
      case UnOp(UMinus, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case UnOp(Not, e1) => typ(e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case BinOp(Plus, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TString
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(Minus|Times|Div, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TNumber
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(Eq|Ne, e1, e2) => typ(e1) match {
        case t1 if !hasFunctionTyp(t1) => typ(e2) match {
          case t2 if (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(Lt|Le|Gt|Ge, e1, e2) => typ(e1) match {
        case TNumber => typ(e2) match {
          case TNumber => TBool
          case tgot => err(tgot, e2)
        }
        case TString => typ(e2) match {
          case TString => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(And|Or, e1, e2) => typ(e1) match {
        case TBool => typ(e2) match {
          case TBool => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
      case BinOp(Seq, e1, e2) => typ(e1); typ(e2)
      case If(e1, e2, e3) => typ(e1) match {
        case TBool =>
          val (t2, t3) = (typ(e2), typ(e3))
          if (t2 == t3) t2 else err(t3, e3)
        case tgot => err(tgot, e1)
      }
      case Obj(fs) => TObj(fs map { case (f,t) => (f, typ(t)) })
      case GetField(e1, f) => typ(e1) match {
        case TObj(tfs) if (tfs.contains(f)) => tfs(f)
        case tgot => err(tgot, e1)
      } 
      /* not sure where TypeFun, TypeFunAnn, and TypeFunRec go- is it all in this case below? */
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
        val env2 = env1 ++
          xs.map {
            case ((PConst | PName), s, t) => (s -> (MConst, t))
            case (_, s, t) => (s -> (MVar, t))
          }
        // Infer the type of the function body
        val t1 = typeInfer(env2, e1)
        tann foreach { rt => if (rt != t1) err(t1, e1) };
        TFunction(xs, t1)
      }
      
      case Call(e1, args) => typ(e1) match {
        case TFunction(xs, tret) if (xs.length == args.length) => {
          (xs, args).zipped.foreach { 
            (param, ei) => param match {
              case (p, xi , ta) => {
                if ((p != PRef) || (isLExpr(ei))) {
                  val ti = typ(ei);
                  if(ti == ta) ta else err(ti, ei)
                }
                else err( ta ,ei);
              }
            }  
          }
          tret
        }
        case tgot => err(tgot, e1)
      }
      
      /*** Fill-in more cases here. ***/
      
      //TypeDecl
      case Decl(mut, x, ed, eb) =>{
        val env2 = env + (x -> (mut, typ(ed)))
        typeInfer(env2, eb)
      } 
        
      //TypeAssignVar
      case BinOp(Assign, Var(x), e2) => {
        env.get(x) match {
          case Some(re) => re match {
            case (MVar, ta) => val tp = typ(e2); if (tp == ta)tp else err(tp, e2)
            case (MConst, ta) => err(ta, BinOp(Assign, Var(x), e2))
          }
          case None => err(TUndefined, BinOp(Assign, Var(x), e2))
        } 
      }
      
      //TypeAssignField
      case BinOp(Assign, GetField(obj, f), e2) => println("in TypeAssignField"); typ(obj) match {
        case TObj(tfs) if (tfs.contains(f)) => {
          val te2 = typ(e2);
          if (te2 == tfs(f)) te2 else err(te2, e2)
        }
        case tgot => err(tgot, GetField(obj, f))
      }
      
      
      
      /* Should not match: non-source expressions */
      case Addr(_) | UnOp(Deref, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
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
  
  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, x: String, esub: Expr): Expr = {
    println("substitute " + x + " for " + esub)
    def subst(e: Expr): Expr = substitute(e, x, esub)
    val ep: Expr = e
    ep match {
      case Num(_) | Bool(_) | Undefined | Str(_) | Addr(_) => e
      case Print(e1) => Print(subst(e1))
      case UnOp(uop, e1) => UnOp(uop, subst(e1))
      case BinOp(bop, e1, e2) => BinOp(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else e
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, xs, tann, e) =>
        // we might want to subst within each expr in xs
        println("----substing function")
        Function(p, xs, tann, subst(e))
      case Call(e1, args) => Call(subst(e1), args map subst)
      case Obj(fs) => Obj(fs mapValues (subst(_)))
      case GetField(e, f) => GetField(subst(e), f)
    }
  }

  /* A small-step transition. */
  def step(e: Expr): State[Mem, Expr] = {
    println("in step")
    println(e)
    require(!isValue(e), "stepping on a value: %s".format(e))
    
    /*** Helpers for Call ***/
    
    def stepIfNotValue(e: Expr): Option[State[Mem,Expr]] = 
      if (isValue(e)) None else Some(step(e))
    
    /* Check whether or not the argument expression is ready to be applied. */
    def argApplyable(mode: PMode, arg: Expr): Boolean = mode match {
      case PConst | PVar => isValue(arg)
      case PName => true
      case PRef => isLValue(arg)
    }

    /*** Body ***/
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => 
        for (m <- State[Mem]) yield { println(v1.prettyVal(m)); Undefined }
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
      case Obj(fs) if (fs forall { case (_, vi) => isValue(vi)}) =>
        //for (a <- Mem.alloc(e)) yield UnOp(Deref, a)
        Mem.alloc(e)
        
      case GetField(a @ Addr(_), f) => 
        println("in GetField(Obj)")
        println("getting: " + f)
        
        for (m <- State[Mem]) yield {
          m.get(a) match {
            case (Some(Obj(xs))) => {
              xs.get(f) match {
                case Some(v) => v
                case _ => println(xs); throw StuckError(e)
              }
            }
            case _ => throw StuckError(e)
          }
        }
        
      case Call(v @ Function(p, _, _, e), Nil) => 
        /*** Fill-in the DoCall and DoCallRec cases */
        val ep = p match {
          case None => e
          case Some(x) => substitute(e, x, v)
        }
        State.insert(ep)

        
      case Call(Function(p, (m, x, _) :: xs, tann, e), arg :: args) if argApplyable(m, arg) =>
        (m, arg) match {
          /*** Fill-in the remaining DoCall cases  ***/
          case (PConst, arg) => {

            // Thought on a more functional style:
            //
             State.insert(Call(Function(p, xs, tann, substitute(e, x, arg)), args))
            //
            // But, when there are only 2 args left, the Call signature does not match this case
            // and the final step is not performed. 

//            var expr = substitute(e, x, arg)
//            (xs, args).zipped.foreach(((xn), argn) => expr = substitute(expr, xn._2, argn))
//            State.insert(expr)
          }
          case (PName, arg) => ???
          case (PRef, arg) => ???
          case (PVar, arg) => Mem.alloc(arg).map (p => substitute(e, x, UnOp(Deref, p)))
          case _ => throw StuckError(e)
        } 
      
      case Decl(MConst, x, v1, e2) if isValue(v1) =>
        State.insert(substitute(e2, x, v1)) //not tested yet
        //State.modify( ... substitute(e2, x, v1)
        
      // DoVarDecl
      case Decl(MVar, x, v1, e2) if (isValue(v1)) =>
        println("DoVarDecl")
        Mem.alloc(v1).map {
          a => {
            substitute(e2, x, UnOp(Deref, a))
          }
        }
        
      // DoAssignVar
      case BinOp(Assign, UnOp(Deref, a @ Addr(_)), v) if isValue(v) => {
        for (_ <- State.modify { (m: Mem) => (m + (a,v)): Mem }) yield v
      }
        
      //DoAssignField
      case BinOp(Assign, GetField(a @ Addr(_), f), v) if isValue(v) => 
        println("DoAssignField")
        for (_ <- State.modify {
          (m: Mem) => {
            m.get(a) match {
              case Some(Obj(xs)) => m + ((a, Obj(xs + ((f, v))))): Mem
              case _ => throw StuckError(e)
            }
          }
          }) yield v
        
        
      /*** Fill-in more Do cases here. ***/

        
      //DoDeref
      case UnOp(Deref, a @ Addr(_)) => {
        for (m <- State[Mem]) yield {
          m.get(a) match {
            case Some(e) => e
            case None => throw StuckError(e)
          }
        }
      }
        
      /* Inductive Cases: Search Rules */
      case Print(e1) =>
        for (e1p <- step(e1)) yield Print(e1p)
      case UnOp(uop, e1) =>
        for (e1p <- step(e1)) yield UnOp(uop, e1p)
       
      //SearchAssign 1 
      case BinOp(Assign, e1, e2) if (!isLValue(e1)) =>
        println("searchassign 1")
        for (e1p <- step(e1)) yield BinOp(Assign, e1p, e2)

      //SearchAssign 2 
      case BinOp(Assign, v1 @ UnOp(Deref, Addr(_)), e2) => 
        println("searchassign 2")
        for (e2p <- step(e2)) yield BinOp(Assign, v1, e2p) 
        
        
      case BinOp(bop, v1, e2) if isValue(v1) =>
        for (e2p <- step(e2)) yield BinOp(bop, v1, e2p)
      case BinOp(bop, e1, e2) =>
        for (e1p <- step(e1)) yield BinOp(bop, e1p, e2)
      case If(e1, e2, e3) =>
        for (e1p <- step(e1)) yield If(e1p, e2, e3)
      case Obj(fs) => fs find { case (_, ei) => !isValue(ei) } match {
        case Some((fi,ei)) =>
          for (e1p <- step(ei)) yield Obj(fs.updated(fi, e1p))
        case None => throw StuckError(e)
      }
      case GetField(e1, f) => for (e1p <- step(e1)) yield GetField(e1p, f)
      
      /*** Fill-in more Search cases here. ***/
      
      //SearchDecl
      case Decl(m, x, e1, e2) => {
        println("search decl...")
        for (e1p <- step(e1)) yield Decl(m, x, e1p, e2)
      }
      
      
      // SearchCallRef + SearchCallVarConst
      case Call(func @ Function(_, (m, _, _) :: xs, tann, e), arg :: e2) =>
        println("SearchCallRef + SearchCallVarConst")
        (m, arg) match {
          //SearchCallVarConst
          case ((PConst | PVar), arg) => for (argp <- step(arg)) yield {
            Call(func, List(argp))
          }
          case (PName, arg) => ???
          //SearchCallRef
          case (PRef, arg) if (isLValue(arg) && !isValue(arg)) => ???
//             Call(func, e2.map { x => if (x == arg) step(arg) })
        } 
      
      //SearchCallFun
      case Call(e1, e2) =>
        println("SearchCallFun")
        println(e1)
        println(e2)
        for (e1p <- step(e1)) yield Call(e1p, e2)
      
        
        
      // ^^I think thats all the search rules
      
      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** External Interfaces ***/
  
  override def init() = {
    this.debug = true // comment this out or set to false if you don't want print debugging information
    this.maxSteps = Some(500) // comment this out or set to None to not bound the number of steps.
  }
  
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    } 
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("Type: " + print.prettyTyp(t))
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
          m <- State[Mem]
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
      
    handle(fail()) {
     val t = inferType(expr)
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(v1.prettyVal())
    }
  }
    
}