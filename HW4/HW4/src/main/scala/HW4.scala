object HW4 extends js.util.JsApp {
  import js.hw4._
  import js.hw4.ast._
  
  /*
   * CSCI-UA.0480-006: Homework 4
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
  
  /* Collections and Higher-Order Functions */
  
  /* Lists */
  
  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => ???
    case h1 :: (t1 @ (h2 :: _)) => ???
  }
  
  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => ???
  }
  
  def mapFirst[A](f: A => Option[A])(l: List[A]): List[A] = l match {
    case Nil => ???
    case h :: t => ???
  }
  
  /* Search Trees */
  
  sealed abstract class Tree {
    def insert(n: Int): Tree = this match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => if (n < d) Node(l insert n, d, r) else Node(l, d, r insert n)
    } 
    
    def foldLeft[A](z: A)(f: (A, Int) => A): A = {
      def loop(acc: A, t: Tree): A = t match {
        case Empty => ???
        case Node(l, d, r) => ???
      }
      loop(z, this)
    }
    
    def pretty: String = {
      def p(acc: String, t: Tree, indent: Int): String = t match {
        case Empty => acc
        case Node(l, d, r) =>
          val spacer = " " * indent
          p("%s%d%n".format(spacer, d) + p(acc, l, indent + 2), r, indent + 2)
      } 
      p("", this, 0)
    }
  }
  case object Empty extends Tree
  case class Node(l: Tree, d: Int, r: Tree) extends Tree
  
  def treeFromList(l: List[Int]): Tree =
    l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }
  
  def sum(t: Tree): Int = t.foldLeft(0){ (acc, d) => acc + d }
  
  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = t.foldLeft((true, None: Option[Int])){
      ???
    }
    b
  }
  

  /* Type Inference */
  
  // A helper function to check whether a JS type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fs) => fs exists { case (_, t) => hasFunctionTyp(t) }
    case _ => false
  }
  
  def typeInfer(env: Map[String, Typ], e: Expr): Typ = {
    // Some shortcuts for convenience
    def typ(e1: Expr) = typeInfer(env, e1)
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1)

    e match {
      case Print(e1) => typ(e1); TUndefined
      case Num(_) => TNumber
      case Bool(_) => TBool
      case Undefined => TUndefined
      case Str(_) => TString
      case Var(x) => env(x)
      case ConstDecl(x, e1, e2) => typeInfer(env + (x -> typ(e1)), e2)
      case UnOp(UMinus, e1) => typ(e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case UnOp(Not, e1) =>
        ???
      case BinOp(Plus, e1, e2) =>
        ???
      case BinOp(Minus|Times|Div, e1, e2) => 
        ???
      case BinOp(Eq|Ne, e1, e2) =>
        ???
      case BinOp(Lt|Le|Gt|Ge, e1, e2) =>
        ???
      case BinOp(And|Or, e1, e2) =>
        ???
      case BinOp(Seq, e1, e2) =>
        ???
      case If(e1, e2, e3) =>
        ???
      case Function(p, xs, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(xs, tret)
            env + (f -> tprime)
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for xs.
        val env2 = ???
        // Match on whether the return type is specified.
        tann match {
          case None => ???
          case Some(tret) => ???
        }
      }
      case Call(e1, es) => typ(e1) match {
        case TFunction(txs, tret) if (txs.length == es.length) => {
          (txs, es).zipped.foreach {
            ???
          }
          tret
        }
        case tgot => err(tgot, e1)
      }
      case Obj(fs) =>
        ???
      case GetField(e1, f) =>
        ???
    }
  }
  
  
  /* Small-Step Interpreter */
  
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
  
  def substitute(e: Expr, x: String, v: Expr): Expr = {
    require(isValue(v) && closed(v))
    
    def subst(e: Expr): Expr = substitute(e, x, v)
    
    e match {
      case Num(_) | Bool(_) | Undefined | Str(_) => e
      case Print(e1) => Print(subst(e1))
      case UnOp(uop, e1) => UnOp(uop, subst(e1))
      case BinOp(bop, e1, e2) => BinOp(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) v else e
      case ConstDecl(y, e1, e2) => ConstDecl(y, subst(e1), if (x == y) e2 else subst(e2))
      case Function(p, xs, tann, e1) =>
        ???
      case Call(e1, es) =>
        ???
      case Obj(fs) =>
        ???
      case GetField(e1, f) =>
        ???
    }
  }
  
  def step(e: Expr): Expr = {
    require(!isValue(e))
    assume(closed(e))
    
    def stepIfNotValue(e: Expr): Option[Expr] = if (isValue(e)) None else Some(step(e))
    
    val e1 = e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(v1.prettyVal); Undefined
      case UnOp(UMinus, Num(n1)) => Num(- n1)
      case UnOp(Not, Bool(b1)) => Bool(! b1)
      case BinOp(Seq, v1, e2) if isValue(v1) => e2
      case BinOp(Plus, Str(s1), Str(s2)) => Str(s1 + s2)
      case BinOp(Plus, Num(n1), Num(n2)) => Num(n1 + n2)
      case BinOp(bop @ (Lt|Le|Gt|Ge), v1, v2) if isValue(v1) && isValue(v2) => 
        Bool(inequalityVal(bop, v1, v2))
      case BinOp(Eq, v1, v2) if isValue(v1) && isValue(v2) => Bool(v1 == v2)
      case BinOp(Ne, v1, v2) if isValue(v1) && isValue(v2) => Bool(v1 != v2)
      case BinOp(And, Bool(b1), e2) => if (b1) e2 else Bool(false)
      case BinOp(Or, Bool(b1), e2) => if (b1) Bool(true) else e2
      case ConstDecl(x, v1, e2) if isValue(v1) => substitute(e2, x, v1)
      case Call(v1, es) if isValue(v1) && (es forall isValue) =>
        v1 match {
          case Function(p, txs, _, e1) => {
            val e1p = (txs, es).zipped.foldRight(e1){
              ???
            }
            p match {
              case None => ???
              case Some(x1) => ???
            }
          }
          case _ => throw new StuckError(e)
        }
      /*** Fill-in more cases here. ***/
        
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      case UnOp(uop, e1) => UnOp(uop, step(e1))
      case BinOp(bop, v1, e2) if isValue(v1) => BinOp(bop, v1, step(e2))
      case BinOp(bop, e1, e2) => BinOp(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      case ConstDecl(x, e1, e2) => ConstDecl(x, step(e1), e2)
      /*** Fill-in more cases here. ***/
      
      /* Everything else is a stuck error. Should not happen if e is well-typed. */
      case _ => throw StuckError(e)
    }
    if (e1.pos == null) e1.pos = e.pos
    e1
  } ensuring (e1 => closed(e1))
  
  
  /* External Interfaces */
  
  override def init(): Unit = {
    this.debug = true // comment this out or set to false if you don't want to print debugging information
  }
  
  def inferType(e: Expr): Typ = {
    if (debug) {
      println("------------------------------------------------------------")
      println("Type checking: %s ...".format(e))
    } 
    val t = typeInfer(Map.empty, e)
    if (debug) {
      println("Type: " + t.pretty)
    }
    t
  }
  
  // Interface to run your small-step interpreter and print out the steps of evaluation if debugging. 
  def iterateStep(e: Expr): Expr = {
    require(closed(e))
    def loop(e: Expr, n: Int): Expr = {
      if (debug) { println("Step %s: %s".format(n, e)) }
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
    
    handle(fail()) {
      val t = inferType(expr)
    }
    
    handle() {
      val v1 = iterateStep(expr)
      println(v1.prettyVal)
    }
  }

}