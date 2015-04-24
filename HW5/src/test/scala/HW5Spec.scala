import org.scalatest._
import js.hw5.ast._
import js.hw5._
import HW5._

class HW5Spec extends FlatSpec {
  
  // Probably you want to write some tests for typeInfer, substitute, and step.
  "typeInfer" should "throw an exception for unresolvable type" in {
    try{
      typeInfer(Map.empty, BinOp(Plus, Num(3), Print(Str("hi"))))
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
    
    //test reassigning variable to the wrong field
    try{
      typeInfer(Map.empty, Decl( MVar, "obj", Obj (Map ("n" -> Num (1.0))),BinOp (Assign, GetField (Var ("obj"), "n"), Str ("hello"))))
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
    
    //should throw error for redeclaring MConst object
    try{
      typeInfer(Map.empty, Decl (MConst, "o", Obj (Map ("f" -> Num (1.0)
          )), BinOp (Assign, Var ("o"), Obj (Map ("n" -> Num (2.0))))))
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
    
    //should throw error if an argument type doesn't match the parameter type
    
    try{
      val call = Call (Var ("f"), List (Num (1.0)))
      val func = Function (None, List (Tuple3 (PConst, "x", TString)), None, Print (Var ("x")))
      typeInfer(Map.empty, Decl ( MConst, "f", func , call) )
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
    
    
    
  }
  
  
  "substitute" should "replace references in object properties" in {
    val obj = Obj(Map("a" -> BinOp(Times,Var("x"),Num(20))))
    val exp = Decl(MConst,"x", Num(3), GetField(obj, "a"))
    
    assert(Num(60) == iterateStep(exp))
  }
  
  "MVar" should "allow for mutations of a value" in {
    val getVar = Var("n")
    val reAssign = BinOp(Assign,Var("n"), Num(10))
    val exp = Decl(MVar, "n", Num(3), BinOp(Seq, reAssign, getVar))
    
    assert(Num(10) == iterateStep(exp))
  }
  
  "MVar" should "allow for mutations of an object" in {
    val getField = GetField(Var("o"), "n")
    val seq = BinOp(Seq, BinOp (Assign, Var ("o"), Obj (Map ("n" -> Num (2.0)))), getField)
    val exp = Decl (
      MVar,
      "o",
      Obj (Map ("f" -> Num (1.0))),
      seq
    )

    assert(Num(2) == iterateStep(exp))
  }
  
  "MVar" should "allow for nested assignments" in {
    val exp4 = BinOp(Plus, Var("y"), Var("x"))
    val exp3 = BinOp(Assign, Var("x"), Num(3))
    val exp2 = BinOp(Seq, BinOp(Assign,Var("y"), exp3), exp4) 
    val exp1 = Decl(MVar, "y", Num(2), exp2)
    val exp = Decl(MVar, "x", Num(2), exp1)
    
    /**
     * var x = 2
     * var y = 2
     * y = x = 3
     * y + x
     */
    
    assert(Num(6) == iterateStep(exp))
  }
  
  /**
   * For this, could it be a type checking thing? Otherwise, since we substitute, we
   * get something like this:
   * 
   * 3 = 10
   * 
   * Which does not match any searchAssign rules, so we search with step(3) and fail
   */
  "MConst" should "not allow for mutations" in {
    try{
      val exp = Decl(MConst, "n", Num(3), BinOp(Assign,Var("n"), Num(10)))
      typeInfer(Map.empty, exp);
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
  }
  
  "functions" should "have multiple parameters" in {
    val fnExpr = BinOp(Times, Var("n1"), Var("n2"))
    val fn = Function(Some("times"), List((PConst, "n1", TNumber), (PConst,"n2", TNumber)), None, fnExpr)
    
    
    assert(Num(12) == iterateStep(Call(fn, List(Num(3), Num(4)))))
  }
  
  "functions" should "evaluate expressions in passed parameters" in {
    val fnExpr = BinOp(Plus, Var("n1"), Var("n2"))
    val paramList = List((PConst, "n1", TNumber), (PConst, "n2", TNumber))
    val fn = Function(Some("plus"), paramList, None, fnExpr)
    val callFn = Call(fn, List(Num(3), If(BinOp(Gt, Num(10), Num(5)), Num(4), Num(1))))
    
    assert(Num(7) == iterateStep(callFn))
  }
  
  "functions" should "have more than 3 parameters" in {
    val fnExpr = BinOp(Plus, Var("n1"), BinOp(Plus, Var("n2"), BinOp(Plus, Var("n3"), Var("n4"))))
    val paramList = List((PConst, "n1", TNumber), (PConst,"n2", TNumber), (PConst, "n3", TNumber), (PConst,"n4", TNumber))
    val fn = Function(Some("plusMany"), paramList, None, fnExpr)
    
    assert(Num(16) == iterateStep(Call(fn, List(Num(1), Num(3), Num(5), Num(7)))))
  }
  
  "PRef" should "pass by reference" in {

    val exp = 
      Decl (
        MConst,
        "f",
        Function (
          None,
          List (Tuple3 (PRef, "x", TNumber)),
          None,
          BinOp (Assign, Var ("x"), Num (3.0))),
        Decl (
          MVar,
          "y",
          Num (0.0),
          BinOp (Seq, Call (Var ("f"), List (Var ("y"))), Var ("y"))))
    assert(Num(3) == iterateStep(exp))
  }
  
  "Simple Assign Obj" should "change the value of the object field" in {
    val exp = 
      Decl (
        MConst,
          "o",
          Obj (Map ("f" -> Num (1.0))),
          BinOp (
            Seq,
            BinOp (Assign, GetField (Var ("o"), "f"), Num (4.0)),
            BinOp (Plus, GetField (Var ("o"), "f"), Num (2.0))))
     assert(Num(6) == iterateStep(exp))
        
  }
  
  "DoCall" should "evaluate a function without parameters" in {
    val exp = Decl (MConst, "func", Function (None, Nil, None, BinOp (Plus, Num (1.0), Num (1.0))),
    Call (Var ("func"), Nil))
    assert(Num(2) == iterateStep(exp))
  }
  
  "DoRecCall" should "allow user to call recursive functions" in {
     val exp = Call (
      Function (
        Some ("sum"),
        List (Tuple3 (PConst, "n", TNumber)),
        Some (TNumber),
        If (
          BinOp (Gt, Var ("n"), Num (0.0)),
          Call (
            Var ("sum"),
            List (BinOp (Minus, Var ("n"), Num (1.0)))),
          Num (0.0))),
      List (Num (5.0)))
      
      assert(Num(0) == iterateStep(exp))
    
    /**
     * const func = function sum(const n: number): => number {
     *   n > 0 ? (sum(n - 1)) : 0;
     * }
     * func(5);
     */
  }
  
  "DoCallConst" should "not allow parameter redeclerations" in {
    val exp = 
      Decl (
        MConst,
        "f",
        Function (
          None,
          List (Tuple3 (PConst, "x", TNumber)),
          None,
          BinOp (Assign, Var ("x"), Num (10.0))),
        Call (Var ("f"), List (Num (3.0))))
    try {
      typeInfer(Map.empty, exp)
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
  }
  
  "DoCallConst" should "evaluate parameters before passing them in" in {
    val exp = Decl (
      MConst,
      "f",
      Function (
        None,
        List (Tuple3 (PConst, "x", TNumber)),
        None,
        BinOp (Plus, Var ("x"), Var ("x"))),
      Decl (
        MVar,
        "y",
        Num (3.0),
        Decl (
          MConst,
          "r",
          Call (
            Var ("f"),
            List (
              BinOp (
                Assign,
                Var ("y"),
                BinOp (Plus, Var ("y"), Num (1.0))))),
          Var ("y"))))
    assert(Num(4) == iterateStep(exp))
  }
  "DoCalName" should "evaluate parameters each time they are used in the function" in {
    val exp = Decl (
      MConst,
      "f",
      Function (
        None, List (Tuple3 (PName, "x", TNumber)), None, BinOp (Plus, Var ("x"), Var ("x"))),
      Decl (
        MVar,
        "y",
        Num (3.0),
        Decl (
          MConst,
          "r",
          Call (
            Var ("f"),
            List (
              BinOp (
                Assign,
                Var ("y"),
                BinOp (Plus, Var ("y"), Num (1.0))))),
          Var("y"))))
    assert(Num(5) == iterateStep(exp))
  }
  "DoCallVar" should "allow for mutable parameters" in {
    val exp = Decl (
      MConst,
      "f",
      Function (
        None,
        List (Tuple3 (PVar, "x", TNumber)),
        None,
        BinOp (Assign, Var ("x"), BinOp (Plus, Var ("x"), Num (1.0)))),
      Decl (MVar, "y", Num (3.0), Call (Var ("f"), List (Var ("y")))))
    assert (Num(4) == iterateStep(exp))
  }
  
  "DoCallRef" should "pass by reference " in {
    val exp = Decl (
      MConst,
      "f",
      Function (
        None,
        List (Tuple3 (PRef, "x", TNumber)),
        None,
        BinOp (Assign, Var ("x"), BinOp (Plus, Var ("x"), Num (1.0)))),
      Decl (
        MVar,
        "y",
        Num (3.0),
        BinOp (Seq, Call (Var ("f"), List (Var ("y"))), Var ("y"))))
    assert(Num(4) == iterateStep(exp))
  }
  
  "doCallRef" should "not allow non location expressions to be assigned" in {
    val exp =  Decl (
      MConst,
      "f",
      Function (
        None,
        List (Tuple3 (PRef, "x", TNumber)),
        None,
        BinOp (Assign, Var ("x"), Num (10.0))),
      Call (Var ("f"), List (Num (1.0))))
    try {
      iterateStep(exp)
      fail()
    }
    catch {
      case _ => 
    }
    try {
      typeInfer(Map.empty, exp)
      fail()
    }
    catch {
      case _: StaticTypeError => 
    }
  }
  
  "object fields" should " allow nested objects" in {
      val obj2 = Obj(Map ("n" -> Num(1.0)))
      val exp = Decl (MConst, "o", Obj (Map ("f" -> obj2)), BinOp(Plus, GetField( GetField(Var("o"), "f"), "n" ), Num(2) ))
      assert(Num(3) == iterateStep(exp))

  }
  
  "Function call" should "pass by name" in {
    val exp = 
      Decl (
      MVar,
      "x",
      Num (0.0),
      Decl (
        MConst,
        "byname",
        Function (
          None,
          List (Tuple3 (PName, "b", TBool)),
          None,
          BinOp (
            Seq,
            BinOp (
              Seq,
              Print (Var ("b")),
              BinOp (Assign, Var ("x"), Num (100.0))),
            Var ("b"))),
        Call (
          Var ("byname"),
          List (BinOp (Lt, Var ("x"), Num (3.0))))))
     
    assert(Bool(false) == iterateStep(exp))
  }
  
  "Function" should "do simple recursion with parameter" in {
    var exp = Decl (
      MConst,
      "f",
      Function (
        Some ("func"),
        List (Tuple3 (PConst, "b", TBool)),
        Some (TBool),
        If (
          Var ("b"),
          Bool (true),
          Call (Var ("func"), List (Bool (true))))),
      Call (Var ("f"), List (Bool (false))))
      
      assert(Bool(true) == iterateStep(exp))
  }
}
