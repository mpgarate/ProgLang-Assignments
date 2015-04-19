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
    val exp3 = BinOp(Assign, Var("x"),Num(3))
    val exp2 = Decl(MVar, "y", Num(2), BinOp(Seq, BinOp(Assign,Var("y"),exp3), exp4))
    val exp1 = Decl(MVar, "x", Num(2), exp2)
    
    assert(Num(6) == iterateStep(exp1))
  }
  
  "MConst" should "not allow for mutations" in {
    val exp = Decl(MConst, "n", Num(3), BinOp(Assign,Var("n"), Num(10)))
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
  "PRef" should "pass by reference" in {
    val fnExpr = BinOp(Assign, Var("b1"), Bool(false)) //is this right?
    val fn = Function(Some("times"), List((PRef, "b1", TBool)), None, fnExpr)
    
    val obj = Obj(Map("a" -> Bool(true)))
    val exp = (BinOp(Or, Bool(false), GetField(obj, "a")))
    
    assert(Num(12) == iterateStep(Call(fn, List(Num(3), Num(4)))))
    assert(Num(18) == iterateStep(Call(fn, List(Num(3), If(GetField(obj, "a"), BinOp(Plus, Num(4), Num(2)), BinOp(Minus, Num(3), Num(3)) )))))
  }
  
}
