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
  def emp: Env = Map()
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
      case Num(n)      => n
      case Bool(true)  => 1
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
      case Bool(b)   => b
      case Num(0)    => false
      case Num(n)    => true
      case Str("")   => false
      case Str(s)    => true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case Str(s)      => s
      case Undefined   => "undefined"
      case Bool(false) => "false"
      case Bool(true)  => "true"
      case Num(n)      => n.toString
    }
  }

  /**
   * http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.6
   */
  def strictEquality(v1: Expr, v2: Expr): Boolean = (v1, v2) match {
    case (Undefined, _) => true
    case (Num(n1), Num(n2)) => (v1, v2) match {
      case (Num(Double.NaN), _) => false
      case (_, Num(Double.NaN)) => false
      case (_, _)               => n1 == n2
    }
    case (Str(s1), Str(s2)) => {
      s1 == s2
    }
    case (Bool(b1), Bool(b2)) => {
      b1 == b2
    }
    case (_, _) => false
  }

  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case Bool(_)   => e
      case Num(_)    => e
      case Str(_)    => e
      case Var(x)    => get(env, x)
      case Undefined => Undefined

      /* Inductive Cases */
      case Print(e) =>
        println(pretty(eToVal(e))); Undefined

      case ConstDecl(x, e1, e2) => {
        val xVal = eToVal(e1)
        val newEnv = extend(env, x, xVal)
        eval(newEnv, e2)
      }
      
      case If(e1, e2, e3) => {
        if (toBool(eToVal(e1))){
          eToVal(e2)
        } else {
          eToVal(e3)
        }
      }
      
      case BinOp(Seq, e1, e2) => {
          eToVal(e1)
          eToVal(e2)
      }

      case BinOp(And, e1, e2) => {
        val b1 = toBool(eToVal(e1))
        if (false == b1) return eToVal(e1)
        else return eToVal(e2)
      }

      case BinOp(Or, e1, e2) => {
        val b1 = toBool(eToVal(e1))
        if (true == b1) return eToVal(e1)
        else return eToVal(e2)
      }

      case BinOp(Plus, e1, e2) => {
        val lVal = eToVal(e1)
        val rVal = eToVal(e2)
        (lVal, rVal) match {
          case (Str(s1), val2) => {
            // TODO: add test cast for string concatenation 
            Str("%s%s" format (s1, toStr(val2)))
          }
          case _ => Num(toNum(lVal) + toNum(rVal))
        }
      }

      case BinOp(Minus, e1, e2) => {
        val lNum = toNum(eToVal(e1))
        val rNum = toNum(eToVal(e2))
        Num(lNum - rNum)
      }

      case BinOp(Times, e1, e2) => {
        val leftNum = toNum(eToVal(e1))
        val rightNum = toNum(eToVal(e2))

        return Num(leftNum * rightNum)
      }

      case BinOp(Div, e1, e2) => {
        val leftNum = toNum(eToVal(e1))
        val rightNum = toNum(eToVal(e2))

        return Num(leftNum / rightNum)
      }

      case BinOp(Eq, e1, e2) => {
        Bool(strictEquality(eToVal(e1), eToVal(e2)))
      }

      case BinOp(Ne, e1, e2) => {
        Bool(eToVal(e1) != eToVal(e2))
      }

      case BinOp(Lt, e1, e2) => {
        Bool(toNum(eToVal(e1)) < toNum(eToVal(e2)))
      }

      case BinOp(Gt, e1, e2) => {
        Bool(toNum(eToVal(e1)) > toNum(eToVal(e2)))
      }

      case BinOp(Ge, e1, e2) => {
        Bool(toNum(eToVal(e1)) >= toNum(eToVal(e2)))
      }

      case BinOp(Le, e1, e2) => {
        Bool(toNum(eToVal(e1)) <= toNum(eToVal(e2)))
      }

      case UnOp(Not, e) => {
        Bool(!toBool(eToVal(e)))
      }

      case UnOp(UMinus, e) => {
        Num(-toNum(eToVal(e)))
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