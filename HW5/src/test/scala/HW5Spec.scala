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
    
  }
  
  "substitute" should "replace references in object properties" in {
    val obj = Obj(Map("a" -> BinOp(Times,Var("x"),Num(20))))
    val exp = Decl(MConst,"x", Num(3), GetField(obj, "a"))
    
    assert(Num(60) == iterateStep(exp))
  }
  
  "MVar" should "allow for mutations of a value" in {
    val exp = Decl(MVar, "n", Num(3), BinOp(Assign,Var("n"), Num(10)))
    
    assert(Num(10) == iterateStep(exp))
  }
  
  "MVar" should "allow for nested assignments" in {
    //I think that's written right...
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
  
  "MConst" should "not allow for mutations" in {
    try{
      val exp = Decl(MConst, "n", Num(3), BinOp(Assign,Var("n"), Num(10)))
      iterateStep(exp);
      fail()
    }
//    catch {
//      ???
//    }
//    fill in what the error would be...
  }
  
  "functions" should "have multiple parameters" in {
    val fnExpr = BinOp(Times, Var("n1"), Var("n2"))
    val fn = Function(Some("times"), List((PConst, "n1", TNumber), (PConst,"n2", TNumber)), None, fnExpr)
    
    val obj = Obj(Map("a" -> Bool(true)))
    val exp = (BinOp(Or, Bool(false), GetField(obj, "a")))
    
    assert(Num(12) == iterateStep(Call(fn, List(Num(3), Num(4)))))
    assert(Num(18) == iterateStep(Call(fn, List(Num(3), If(GetField(obj, "a"), BinOp(Plus, Num(4), Num(2)), BinOp(Minus, Num(3), Num(3)) )))))
  }
  
  "functions" should "have more than 3 parameters" in {
    val fnExpr = BinOp(Plus, Var("n1"), BinOp(Plus, Var("n2"), BinOp(Plus, Var("n3"), Var("n4"))))
    val paramList = List((PConst, "n1", TNumber), (PConst,"n2", TNumber), (PConst, "n3", TNumber), (PConst,"n4", TNumber))
    val fn = Function(Some("plusMany"), paramList, None, fnExpr)
    
    assert(Num(16) == iterateStep(Call(fn, List(Num(1), Num(3), Num(5), Num(7)))))
  }
  
  "PRef" should "pass by reference" in {
    val fnExpr = BinOp(Assign, Var("b1"), Bool(false)) //is this right?
    val fn = Function(Some("times"), List((PRef, "b1", TBool)), None, fnExpr)
    
    val obj = Obj(Map("a" -> Bool(true)))
    val exp = (BinOp(Or, Bool(false), GetField(obj, "a")))
    
    assert(Num(12) == iterateStep(Call(fn, List(Num(3), Num(4)))))
    assert(Num(18) == iterateStep(Call(fn, List(Num(3), If(GetField(obj, "a"), BinOp(Plus, Num(4), Num(2)), BinOp(Minus, Num(3), Num(3)) )))))
  }
  
  "Assign Obj" should "change the value of the object field" in {
    val exp = Decl(MVar, "x", Obj(Map("f" -> Num (7.0))), BinOp (Assign, GetField (Var ("x"), "f"), Num (10.0)))
    assert(Num(10) == iterateStep(exp))
  }
  
  "DoCall" should "evaluate a function without parameters" in {
    val exp = Decl (MConst, "func", Function (None, Nil, None, BinOp (Plus, Num (1.0), Num (1.0))),
    Call (Var ("func"), Nil))
    assert(Num(2) == iterateStep(exp))
  }
  
  "DoRecCall" should "allow user to call recursive functions" in {
     val exp = Decl (
      MConst ,
      "func",
      Function (
        Some ("sum"),
        List (Tuple3 (PConst , "n", TNumber )),
        Some (TNumber ),
        If (
          BinOp (Gt, Var ("n"), Num (0.0)),
          BinOp (
            Plus,
            Call (
              Var ("sum"),
              List (BinOp (Minus, Var ("n"), Num (1.0)))),
            Var ("n")),
          Num (0.0))),
      Call (Var ("func"), List (Num (5.0))))
    assert(Num(15) == iterateStep(exp))
  }
  
  "DoCallConst" should "not allow parameter redeclerations" in {
    
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
    val exp = Decl (
      MConst,
      "f",
      Function (
        None,
        List (Tuple3 (PRef, "x", TNumber)),
        None,
        BinOp (Assign, Var ("x"), BinOp (Plus, Var ("x"), Num (1.0)))),
      Decl (MConst, "y", Num (3.0), Call (Var ("f"), List (Var ("y")))))
    //not sure what error should be called
  }
}
