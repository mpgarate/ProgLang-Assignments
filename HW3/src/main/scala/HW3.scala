object HW3 extends js.util.JsApp {
  import js.hw3._
  import js.hw3.ast._
  
  /*
   * CSCI-UA.0480-006: Homework 3
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
  
  type Env = Map[String, Expr]
  def emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  def toNum(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case Num(n) => n
      case Bool(false) => 0
      case Bool(true) => 1
      case Undefined => Double.NaN
      case Str(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBool(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case Num(n) if (n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN => false
      case Num(_) => true
      case Bool(b) => b
      case Undefined => false
      case Str("") => false
      case Str(_) => true
      case Function(_, _, _) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case Num(n) => if (n.isWhole) "%.0f" format n else n.toString
      case Bool(b) => b.toString
      case Undefined => "undefined"
      case Str(s) => s
      case Function(_, _, _) => "function"
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
	require(isValue(v1))
	require(isValue(v2))
	require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (Str(s1), Str(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case _ =>
        val (n1, n2) = (toNum(v1), toNum(v2))
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }

  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * This code is a reference implementation of JakartaScript without
   * functions (i.e., HW 2). You are welcome to replace it with your
   * code from HW 2.
   */
  def eval(env: Env, e: Expr): Expr = {
    def eToNum(e: Expr): Double = toNum(eval(env, e))
    def eToBool(e: Expr): Boolean = toBool(eval(env, e))
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      case Var(x) => get(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(eval(env, e1).prettyVal); Undefined
      
      case UnOp(UMinus, e1) => Num(-eToNum(e1))
      case UnOp(Not, e1) => Bool(!eToBool(e1))
      
      case BinOp(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match {
        case (Str(s1), v2) => Str(s1 + toStr(v2))
        case (v1, Str(s2)) => Str(toStr(v1) + s2)
        case (v1, v2) => Num(toNum(v1) + toNum(v2))
      }      
      case BinOp(Minus, e1, e2) => Num(eToNum(e1) - eToNum(e2))
      case BinOp(Times, e1, e2) => Num(eToNum(e1) * eToNum(e2))
      case BinOp(Div, e1, e2) => Num(eToNum(e1) / eToNum(e2))
      
      case BinOp(bop @ (Eq | Ne), e1, e2) => {
        
        def ensureNotFunction(value: Expr): Unit = value match {
          case Function(_, _, _) => {
            throw DynamicTypeError(e)
          }
          case _ => return
        }
        
        val v1 = eToVal(e1)
        ensureNotFunction(v1)
        
        val v2 = eToVal(e2)
        ensureNotFunction(v2)
        
        bop match {
          case Eq => return Bool(v1 == v2)
          case Ne => return Bool(v1 != v2)
        }
      }
      case BinOp(bop @ (Lt | Le | Gt | Ge), e1, e2) => 
        Bool(inequalityVal(bop, eToVal(e1), eToVal(e2)))
      
      case BinOp(And, e1, e2) => 
        val v1 = eToVal(e1)
        if (toBool(v1)) eToVal(e2) else v1
      case BinOp(Or, e1, e2) =>
        val v1 = eToVal(e1)
        if (toBool(v1)) v1 else eToVal(e2)
      
      case BinOp(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      
      case If(e1, e2, e3) => if (eToBool(e1)) eToVal(e2) else eToVal(e3)
      
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)
      
      case Call(e1, e2) => {
        val v1 = eToVal(e1)
        val v2 = eToVal(e2)
        
        v1 match {
          case Function(None, x, e_1) => {
            eval(extend(env, x, v2), e_1)
          }
          case Function(Some(x1), x2, e_1) => {
            eval(extend(extend(env, x1, v1), x2, v2), e_1)
          }
          case _ => throw DynamicTypeError(e)
        }
      }
    }
  }

  
  def isFunction(e: Expr): Boolean = {
    e match {
      case Function(_, _, _) => true
      case _ => false
    }
  }
  
  /* Small-Step Interpreter with Static Scoping */
  
  def substitute(e: Expr, x: String, v: Expr): Expr = {
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, x, v)
    /* Body */
    println("\ne: " + e + "\nx: " + x + "\nv: "  + v);
    e match {
      //case Var(v1) => if (v1 == x) v else Var(v1)
      case Var(v1) if (v1 == x) => v
      case Var(v1)  => Var(v1)
      case Num(_) | Bool(_) | Undefined | Str(_) => e
      // SearchPrint
      case Print(e1) => Print(subst(e1))
      // SearchBop2, although what does the bar over the arrow mean?
      case BinOp(bop @ (
          Plus | Minus | Times | Div | Lt | Le | Gt | Ge
          ), v1 , e2) if isValue(v1) => {
          BinOp(bop, v1, subst(e2))

      }
      case BinOp(bop @ (
          Plus | Minus | Times | Div | Lt | Le | Gt | Ge),
          Var(v1), e2) => if (v1 == x) BinOp(bop, v, subst(e2))
            else BinOp(bop, Var(v1), subst(e2))
      // SearchBop1
      case BinOp(bop @ (Seq | And | Or), e1, e2) => {
        BinOp(bop, subst(e1), subst(e2))
      }
      // SearchEqual
      case BinOp(bop @ (Eq | Ne), v1, e2) if isFunction(v1) => {
        BinOp(bop, v1, subst(e2))
      }
      case BinOp(bop @ (Eq | Ne), v1, e2) => BinOp(bop, subst(v1), subst(e2))

      }
      // SearchUop
      case UnOp(uop, e1) => UnOp(uop, subst(e1))
      // SearchIf
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      // SearchConst
      case ConstDecl(x, e1, e2) => ConstDecl(x, subst(e1), e2)
      // SearchCall2
      case Call(e1, e2) if isValue(e1) => Call(e1, subst(e2))
      // SearchCall1
      case Call(e1, e2) => Call(subst(e1), e2)
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(v1.prettyVal); Undefined
      
        // ****** Your cases here
      
      //Do Seq
      case BinOp (Seq, v1, v2) if (isValue(v1)) => step(v2)
      
      //Do Math and Inequalities
      case BinOp (bop, v1, v2) if (isValue(v1) && isValue(v2) && (bop == Plus) || 
          (bop == Times) || (bop == Minus) || (bop == Div) || (bop == Eq) || 
          (bop == Ne) || (bop == Ge) || (bop == Gt) || (bop == Le) || 
          (bop == Lt))  => bop match{
        case Plus => (v1, v2) match {
          case (Str(v1), v2) => Str( v1 + toStr(v2))
          case (v1, Str(v2)) => Str( toStr(v1) + v2)
          case (v1, v2) => Num(toNum(v1) + toNum(v2))
        }
        case Minus => Num (toNum(v1) - toNum(v2))
        case Times => Num(toNum(v1) * toNum(v2))
        case Div => Num( toNum(v1) / toNum(v2))
        case Eq => Bool(v1 == v2)
        case Ne => Bool(v1 != v2)
        case bop =>
          Bool(inequalityVal(bop, v1, v2))
      }
      
      //Do And
      case BinOp(And, v1, e2) if (isValue(v1)) => 
        if (toBool(v1)) {
          if (isValue(e2)) {
            e2 match {
              case s2 @ Str (e2) => s2
              case n2 @ Num(e2) => n2
              case b2 @ Bool(e2) => b2
              case Undefined => Undefined
              //function type error?
            }
          } else step(BinOp(And, v1, step(e2)))
        }//return false value after
        else{
          v1 match {
              case s1 @ Str (v1) => s1
              case n1 @ Num(v1) => n1
              case b1 @ Bool(v1) => b1
              case Undefined => Undefined
              //function type error?
            }
        }
      
      //Do Or
      case BinOp(Or, v1, e2) if (isValue(v1)) => 
      if (toBool(v1)) {
        v1 match {
          case s1 @ Str (v1) => s1
          case n1 @ Num(v1) => n1
          case b1 @ Bool(v1) => b1
          case Undefined => Undefined
            //function type error?
        }
      }//return false value after
      else
        if (isValue(e2)) {
          e2 match {
            case s2 @ Str (e2) => s2
            case n2 @ Num(e2) => n2
            case b2 @ Bool(e2) => b2
            case Undefined => Undefined
            //function type error?
          }
        } else step(BinOp(Or, v1, step(e2)))
      
      case BinOp(Or, v1, e2) if (isValue(v1)) => if(toBool(v1)) { Bool(true)}
        else { if(isValue(e2)) Bool(toBool(e2)) else Bool(toBool(step(e2))) } 
      
      //Do UMinus
      case UnOp(UMinus, v1) if(isValue(v1)) => Num(-toNum(v1))
      //Do Not
      case UnOp(Not, v1) if(isValue(v1)) => Bool(!toBool(v1))
      //Do If
      case If(v1, e2, e3) if (isValue(v1)) => if (toBool(v1)) { if (isValue(e2)) e2 else step(e2) }
        else if (isValue(e3)) e3 else step(e3)
      //Do ConstDect
      case ConstDecl(x, v1, e2) if (isValue(v1)) => substitute(e2, x, v1)
      //Do Call
      case Call(v1, v2) if (isValue(v1) && isValue(v2)) => {
        v1 match {
          case Function(None, x, e1) => {
            substitute(e1, x, v2)
          }
          case Function(Some(x1), x2, e1) => {
            println("in some Function")
            val sub = substitute(e1, x1, v1)
            println("\nafter first sub")
            substitute(sub, x2, v2)
          }
          case _ => throw DynamicTypeError(e)
        }
      }
      
      /* Inductive Cases: Search Rules */
      //Search print
      case Print(e1) => Print(step(e1))
      
      //Search Equal
      case BinOp(bop, v1, e2) if (isValue(v1) && (v1 != Function) && ((bop == Eq)|| (bop == Ne)) ) => BinOp(bop, v1, step(e2))
      //Search Bop2
      case BinOp(bop, v1, e2) if (isValue(v1) && ((bop != And) && (bop != Or) && (bop != Eq) && (bop != Ne) && (bop != Seq))) => 
          BinOp(bop, v1, step(e2))
      //Search Bop1
      case BinOp(bop, e1, e2) => BinOp(bop, step(e1), e2)
      //Search UOp
      case UnOp(uop, e) if(!isValue(e)) => UnOp(uop, step(e))
      //Search If
      case If(e1, e2, e3) if (!isValue(e1)) => If(step(e1), e2, e3)
      //Search Const
      case ConstDecl(x, e1, e2) if (!isValue(e1)) => ConstDecl(x, step(e1), e2)
      //Search Call 1
      case Call(e1, e2) if (!isValue(e1)) => Call(step(e1), e2)
      //Search Call 2
      case Call(v1, e2) if (isValue(v1) && !isValue(e2)) => Call(v1, step(e2))
      
        // ****** Your cases here
      
      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not a closed expression.")
      case Num(_) | Bool(_) | Undefined | Str(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }
  

  /* External Interfaces */
  
  override def init(): Unit = {
    this.debug = true // comment this out or set to false if you don't want to print debugging information
  }
  
  // Interface to run your big-step interpreter starting from an empty environment and print out
  // the test input if debugging.
  def evaluate(e: Expr): Expr = {
    require(closed(e))
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with eval ...")
      println(s"\nExpression:\n $e")
    }
    val v = eval(emp, e)
    if (debug) {
      println(s"Value: $v")
    }
    v
  } 
  
  // Convenience to pass in a js expression as a string.
  def evaluate(s: String): Expr = evaluate(parse.fromString(s))
   
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging. 
  def iterateStep(e: Expr): Expr = {
    require(closed(e))
    def loop(e: Expr, n: Int): Expr = {
      if (debug) { println(s"Step $n: $e") }
      if (isValue(e)) e else loop(step(e), n + 1)
    }
    if (debug) {
      println("------------------------------------------------------------")
      println("Evaluating with step ...")
    }
    val v = loop(e, 0)
    if (debug) {
      println("Value: " + v)
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
    
    val expr =
      handle(fail()) {
        parse.fromFile(file)
      }
    
    if (debug) {
      println("Parsed expression:")
      println(expr.prettyJS())
    }
    
    handle() {
      val v = evaluate(expr)
      println(v.prettyVal)
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(v1.prettyVal)
    }
  }
    
}
