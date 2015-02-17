//package main.scala

object HW1 extends js.util.JsApp {
  import js.hw1.ast._
  import js.hw1._
  
  /*
   * CSCI-UA.0480-006: Homework 1
   * <Jesse Lifshitz>
   * 
   * Partner: <Michael Garate>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expressions with your code in each function.
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
  
  /*
   * Example with a Unit Test
   * 
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter.
   * 
   * To run a selection in the interpreter in Eclipse, highlight the code of interest
   * and type Ctrl+Shift+X (on Windows/Linux) or Cmd+Shift+X (on Mac).
   * 
   * Highlight the next few lines below to try it out.  The assertion passes, so
   * it appears that nothing happens.  You can uncomment the "bad test specification"
   * and see that a failed assert throws an exception.
   * 
   * You can try calling 'plus' with some arguments, for example, plus(1,2).  You
   * should get a result something like 'res0: Int = 3'.
   * 
   * As an alternative, the testPlus2 function takes an argument that has the form
   * of a plus function, so we can try it with different implementations.  For example,
   * uncomment the "testPlus2(badplus)" line, and you will see an assertion failure.
   * 
   * Our convention is that these "test" functions are testing code that are not part
   * of the "production" code.
   * 
   * While writing such testing snippets is convenient, it is not ideal.  For example,
   * the 'testPlus1()' call is run whenever this object is loaded, so in practice,
   * it should probably be deleted for "release".  A more robust way to maintain
   * unit tests is in a separate file.  For us, we use the convention of writing
   * tests in a file called HWXSpec.scala (i.e., HW1Spec.scala for Homework 1).
   */
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus1() {
    assert(plus(1,1) == 2)
    //assert(plus(1,1) == 3) // bad test specification
  }
  //testPlus1()

  def badplus(x: Int, y: Int): Int = x - y
  def testPlus2(plus: (Int, Int) => Int) {
    assert(plus(1,1) == 2)
  }
  //testPlus2(badplus)

  /* Exercises */

  def abs(n: Double): Double = if (n<0) -n else n

  
  def swap(p: (Int, Int)): (Int, Int) = (p._2,p._1)


  def repeat(s: String, n: Int): String = {
    require(n>=0)

    if (n > 0)
      s + repeat(s, n-1)
    else {
      ""
    }
  }
  
  def sqrtStep(c: Double, xn: Double): Double = {
    xn - (((xn * xn) - c) / (2 * xn))
  }

  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require(n >= 0)

    if (n == 0){
      return x0
    }

    def getSqrt(c: Double, x: Double, n: Int): Double = {
      if (n == 1){
        return sqrtStep(c, x)
      }
      return getSqrt(c, sqrtStep(c, x), n - 1)
    }

    getSqrt(c, x0, n)
  }
  
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    require(epsilon > 0)

    def errorIsWithinEpsilon(c: Double, x: Double, epsilon: Double): Boolean = {
      return abs((x * x) - c) < epsilon
    }

    def getSqrt(c: Double, x: Double, epsilon: Double): Double = {
      if (errorIsWithinEpsilon(c, x, epsilon)){
        return x
      }
      return getSqrt(c, sqrtStep(c, x), epsilon)
    }

    getSqrt(c, x0, epsilon)
  }

  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }
  
  /* Binary Search Tree */
  
  sealed abstract class BSTree
  case object Empty extends BSTree
  case class Node(l: BSTree, d: Int, r: BSTree) extends BSTree
  
  def repOk(t: BSTree): Boolean = {
    def check(t: BSTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => {
        if ((d > max) || (d < min)) false
        else check(l, min, d) && check(r, d, max)
      }
    }
    check(t, Int.MinValue, Int.MaxValue)
  }
  
  def insert(t: BSTree, n: Int): BSTree = t match {
    case Empty => Node(Empty, n, Empty)
    case Node(l, d, r) => {
      if (n == d) return Node(l, d, insert(r, n))
      else if (n < d) return Node(insert(l, n), d, r)
      else return Node(l, d, insert(r, n))
    }
  }
  
  def deleteMin(t: BSTree): (BSTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l)
        (Node (l1, d, r), m)
    }
  }
 
  def delete(t: BSTree, n: Int): BSTree = t match {
    case Empty => Empty //base case
    case Node (l, d, r) if (n < d) => Node (delete(l, n), d, r)
    case Node (l, d, r) if (n > d) => Node (l, d, delete(r, n))
    case Node (Empty, d, Empty) if (d == n) => Empty
    case Node(Empty, d, r) if (d == n) => r
    case Node(l, d, Empty) if (d == n) => l
    case Node (l, d ,r) if (d==n) => Node(l, getValue(r), getRightSubTree(r))    
  }
  
  //will never be called on Empty
  def getValue(t:BSTree): Int = t match{
    case Node(Empty, d, Empty) => d
  }
  def getRightSubTree(t:BSTree):BSTree = t match {
    case Node (l, d , r) => r
  }
  //always choose the right one
  
//  def percolate (t: BSTree): BSTree = t match{
//    
//    case Node(l, _, r) => Node(l, r.d, percolate(r)) 
//  }
  
  /* JakartaScript */
  
  //implementing the expression tree
  
  def eval(e: Expr): Double = e match {
    case Num(n) => n

    case BinOp(Plus, exp1, exp2) => eval(exp1) + eval(exp2)
    case BinOp(Minus, exp1, exp2) => eval(exp1) - eval(exp2)
    case BinOp(Times, exp1, exp2) => eval(exp1) * eval(exp2)
    case UnOp(op, a @ BinOp(_ , _ ,_)) => -eval(a)
    case BinOp(Div, exp1, exp2) => div(exp1, eval(exp2))
    case UnOp(op, a) => -eval(a)
  }
  
  def div(exp1: Expr, n: Double ): Double ={
    if (n != 0) {
      eval(exp1) / n
    }
    else if (eval(exp1) < 0) Double.NegativeInfinity else Double.PositiveInfinity
  }
  
 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */ 
  
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = parse(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(v)
  }

}
