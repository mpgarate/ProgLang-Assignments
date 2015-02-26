object HW2 extends js.util.JsApp {
  import js.hw2._
  import js.hw2.ast._
  
  /*
   * CSCI-UA.0480-006: Homework 2
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
   * - adding "extends App" or "extends Application" to your Homework object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your solution will _not_ be graded if it does not compile!!
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */
  
  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNum(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case Num(n) => n
      case Bool(true) => 1
      case Bool(false) => 0
      case Str(s) => {
        try {
          return s.toDouble
        } catch {
          case e: NumberFormatException => // silently ignore
        }

        return Double.NaN
      }
      case Undefined => Double.NaN
    } 
  }
  
  def toBool(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case Bool(b) => b
      case Num(0) => false
      case Num(n) => true
      case Str("") => false
      case Str(s) => true
      case Undefined => false
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case Str(s) => s
      case Undefined => "undefined"
      case Bool(false) => "false"
      case Bool(true) => "true"
      case Num(n) => n.toString
    }
  }
  
  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)
    e match {
      /* Base Cases */
      case Bool(_) => e
      case Num(_) => e
      
      /* Inductive Cases */
      case Print(e) => println(pretty(eToVal(e))); Undefined
      
      case BinOp(And, e1, e2) => { 
        val b1 = toBool(eToVal(e1))
        if (false == b1) return e1
        else return e2
      }

      case BinOp(Or, e1, e2) => {
        val b1 = toBool(eToVal(e1))
        if (true == b1) return e1
        else return e2
      }
      
      case BinOp(Plus, e1, e2) => {
        val lVal = eToVal(e1)
        val rVal = eToVal(e2)
        (lVal, rVal) match {
          case (Str(s1), val2) => {
            // TODO: add test cast for string concatenation 
            Str("%s%s" format(s1, toStr(val2)))
          }
          case _ => Num(toNum(lVal) + toNum(rVal))
        }
      }
      
      case BinOp(Minus, e1, e2) => {
        val lNum = toNum(eToVal(e1))
        val rNum = toNum(eToVal(e2))
        Num(lNum - rNum)
      }
      
      case UnOp(Not, e) => {
        return Bool(true != toBool(eToVal(e)))
      }
    }
  }
    
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = parse(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }

}