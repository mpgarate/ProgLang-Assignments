import org.scalatest._
import js.hw4.ast._
import HW4._

class HW4Spec extends FlatSpec {

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  } 
  
  "mapFirst" should "map the first element where f returns Some" in {
     val l1 = List(1, 2, -3, 4, -5)
     val gold1 = List(1, 2, 3, 4, -5)
     assertResult(gold1) {
       mapFirst { (i: Int) => if (i < 0) Some(-i) else None } (l1)
     }
  }
  
  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
  }
  
  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
  } 
  
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
    val exp = ConstDecl("x", Num(3), GetField(obj, "a"))
    
    assert(Num(60) == iterateStep(exp))
  }
  
  "substitute" should "replace references in function param lists" in {
    val fnExpr = BinOp(Minus, Var("a"), Var("b"))
    val fn = Function(Some("minus"), List("a" -> TNumber, "b" -> TNumber), None, fnExpr)
    val e2 = ConstDecl("b", Num(5), Call(fn, List(Var("a"), Var("b"))))
    val e1 = ConstDecl("a", Num(20), e2)
    
    assert(Num(15) == iterateStep(e1))
  }
  
  "objects" should "have accessible properties" in {
    val obj = Obj(Map("a" -> Bool(true)))
    val exp = (BinOp(Or, Bool(false), GetField(obj, "a")))
    assert(Bool(true) == iterateStep(exp))
  }
  
  "functions" should "have multiple parameters" in {
    val fnExpr = BinOp(Times, Var("n1"), Var("n2"))
    val fn = Function(Some("times"), List("n1" -> TNumber, "n2" -> TNumber), None, fnExpr)
    
    val obj = Obj(Map("a" -> Bool(true)))
    val exp = (BinOp(Or, Bool(false), GetField(obj, "a")))
    
    assert(Num(12) == iterateStep(Call(fn, List(Num(3), Num(4)))))
    assert(Num(18) == iterateStep(Call(fn, List(Num(3), If(GetField(obj, "a"), BinOp(Plus, Num(4), Num(2)), BinOp(Minus, Num(3), Num(3)) )))))
  }
  
  "functions" should "have evaluate expressions in params list" in {
    val fnExpr = BinOp(Times, Var("n1"), Var("n2"))
    val fn = Function(Some("times"), List("n1" -> TNumber, "n2" -> TNumber), None, fnExpr)
    
    val obj = Obj(Map("a" -> Bool(true)))
    val exp = (BinOp(Or, Bool(false), GetField(obj, "a")))

    assert(Num(18) == iterateStep(Call(fn, List(Num(3), If(GetField(obj, "a"), BinOp(Plus, Num(4), Num(2)), BinOp(Minus, Num(3), Num(3)) )))))
  }

  "functions" should "be able to pass objects" in {
    val fnExpr = BinOp(Times, Var("n1"), GetField(Var("n2"), "n"))
    val fn = Function(Some("times"), List("n1" -> TNumber, "n2" -> TObj(Map("n"-> TNumber))), None, fnExpr)
    
    val obj = Obj(Map("a" -> Num(6)))

    assert(Num(18) == iterateStep(Call(fn, List(Num(3), Obj(Map("n" -> Num(6)))))))
  }
  
}
