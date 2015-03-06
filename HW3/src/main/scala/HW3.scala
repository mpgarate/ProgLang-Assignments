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
          case Function(p, x, e_1) => {
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
      
      case Function(p, x, e1) => ???
      case Call(e1, e2) => ???
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */
  
  def substitute(e: Expr, x: String, v: Expr): Expr = {
    require(isValue(v))
    /* Simple helper that calls substitute on an expression
     * with the input value v and variable name x. */
    def subst(e: Expr): Expr = substitute(e, x, v)
    /* Body */
    e match {
      case Num(_) | Bool(_) | Undefined | Str(_) => e
      case Print(e1) => Print(subst(e1))
      case _ => ???
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(v1.prettyVal); Undefined
      
        // ****** Your cases here
      
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      
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
