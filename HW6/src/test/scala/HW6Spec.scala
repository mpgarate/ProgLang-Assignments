import org.scalatest._
import js.hw6.ast._
import js.hw6._
import HW6._

class HW6Spec extends FlatSpec {
  def isSubtype(s: Typ, t: Typ) = subtype(s, t) get (Set())
  
  "<:<" should "implement the subtype relation" in {
     val t = TObj(Map("x" -> TNumber, "y" -> TBool))
     val tp = TObj(Map("y" -> TBool, "x" -> TNumber))
     val s = TObj(Map("x" -> TNumber))
     val u = TObj(Map("x" -> TBool))
     val obj = TObj(Map())
     assert(TNumber <:< TNumber)
     assert(t <:< obj)
     assert(t <:< tp)
     assert(t <:< s)
     assert(!(TNumber <:< TBool))
     assert(!(s <:< t)) 
     assert(!(u <:< s))
  }
  
  "<:<" should "determine subtype for functions" in {
    val fn1xs = List(("a", TNumber))
    val fn1t = TBool
    val tfn1 = TFunction(fn1xs, fn1t)
    
    val fn2xs = List(("a", TNumber), ("b", TString))
    val fn2t = TBool
    val tfn2 = TFunction(fn2xs, fn2t)
    
    assert(tfn2 <:< tfn1)
    assert(!(tfn1 <:< tfn2))
  }

  "|:|" should "compute the join of two types if it exists" in {
    val t1 = TObj(Map("x" -> TNumber, "y" -> TBool))
    val s1 = TObj(Map("x" -> TNumber, "y" -> TNumber, "z" -> TObj(Map())))
    val u1 = TObj(Map("x" -> TNumber))
    assert((TNumber |:| t1) === None)
    assert(u1 =:= (t1 |:| s1).get)
    
    val t2 = TObj(Map("w" -> TBool, "x" -> TObj(Map("y" -> TNumber, "z" -> TUndefined))))
    val s2 = TObj(Map("w" -> TNumber, "x" -> TObj(Map("y" -> TBool, "z" -> TUndefined, "x" -> TBool))))
    val u2 = TObj(Map("x" -> TObj(Map("z" -> TUndefined))))    
    assert(u2 =:= (t2 |:| s2).get)
    
    val t3 = TInterface("Foo", TObj(Map("x" -> TNumber, "y" -> TObj(Map()), "z" -> TVar("Foo"))))
    val s3 = TInterface("Bar", TObj(Map("x" -> TNumber, "y" -> TVar("Bar"), "z" -> TObj(Map("q" -> TNumber)))))
    val u3 = TInterface("FooBar", TObj(Map("x" -> TNumber, "y" -> TObj(Map()), "z" -> TObj(Map()))))
    assert(u3 =:= (t3 |:| s3).get)
  }

  "&:&" should "compute the meet of two types if it exists" in {
    val t1 = TObj(Map("x" -> TNumber, "y" -> TBool))
    val s1 = TObj(Map("x" -> TNumber, "z" -> TObj(Map())))
    val u1 = TObj(Map("x" -> TNumber, "y" -> TBool, "z" -> TObj(Map())))
    assert((TNumber &:& t1) === None)
    assert(u1 =:= (t1 &:& s1).get)
    
    val t2 = TObj(Map("x" -> TNumber, "y" -> TBool))
    val s2 = TObj(Map("x" -> TNumber, "y" -> TNumber))
    println("meet: " + (t2 &:& s2).get)
    assert(TNull =:= (t2 &:& s2).get)
  }
  
  "typeEqual" should "compute join of two expressions" in {
    val e2 = BinOp(Plus, Num(3), Num(2))
    val e1 = BinOp(Plus, Num(7), Num(99))
    val e = BinOp(Eq, e1, e2)
    
    assert(inferType(e) == TBool)
  }
  
  "Do function test" should "successfully run a function" in {
    val xs = List(("x", TNumber), ("b", TBool));
    val e1 = If(Var("b"), Bool(true), Bool(false));
    val func = Function(None, xs, None,  e1);
    val params = List(Num(5), Bool(true))
    val call = Call(func, params)
    assert(Bool(true) == iterateStep(call))
  }
  
  "Passing in Object" should "successfully pass an object" in {
    val xs = List(("b", TBool),("o", TObj(Map("x" -> TNumber))));
    val e1 = If(Var("b"), GetField(Var("o"), "x"), Num(0));
    val func = Function(None, xs, None,  e1);
    val obj = Obj(Map("x" -> BinOp(Plus, Num(3), Num(3))))
    val params = List(Bool(true), obj)
    val call = Call(func, params)
    assert(Num(6) == iterateStep(call))
  } 
  
  "SearchCall1" should "search through e1" in {
    val xs = List(("b", TBool));
    val e1 = If(Var("b"), Num(6), Num(0));
    val func = Function(None, xs, None,  e1);
    val obj = Obj(Map("o" -> func))
    val params = List(Bool(true))
    val call = Call(GetField(obj, "o"), params)
    
    assert(Num(6) == iterateStep(call))
  }
  
  "Hungry" should "call itself" in {
    val exp = Decl ( 
      MConst, 
      "yum", 
      Function (
        Some ("yum"),
        List ("x" -> TNumber),
        Some (
          TInterface (
            "Hungry",
            TFunction (List ("x" -> TNumber), TVar ("Hungry")))),
      Var ("yum")),
      Call (
        Call (Call (Var ("yum"), List (Num (0.0))), List (Num (1.0))),
        List (Num (2.0))));
    val ans =  Function (
      Some ("yum"),
      List ("x" -> TNumber),
      Some (
        TInterface (
          "Hungry",
          TFunction (List ("x" -> TNumber), TVar ("Hungry")))),
      Var ("yum")) 
    assert(ans == iterateStep(exp))
  }

  "Infer Subtype Function" should "" in {
    val exp = Decl ( 
      MConst, 
      "yum", 
      Function (
        Some ("yum"),
        List ("x" -> TNumber),
        Some (
          TInterface (
            "Hungry",
            TFunction (List ("x" -> TNumber), TVar ("Hungry")))),
      Var ("yum")),
      Call (
        Call (Call (Var ("yum"), List (Num (0.0))), List (Num (1.0))),
        List (Num (2.0))));
  }
  "Join Function" should "join the functions" in {
    val fn1xs = List(("a", TNumber), ("c", TString))
    val fn1t = TObj(Map("x" -> TNumber))
    val tfn1 = TFunction(fn1xs, fn1t)
    
    val fn2xs = List(("a", TNumber), ("b", TString))
    val fn2t = TObj(Map("x" -> TNumber, "y" -> TBool))
    val tfn2 = TFunction(fn2xs, fn2t)
    
    val ans = TFunction(List(("a", TNumber), ("c", TString)), TObj(Map("x" -> TNumber)))
    
    val result = tfn1 |:| tfn2
    assert(ans =:= result.get)
  }
  
  "Meet Function" should "meet the functions" in {
    val fn1xs = List(("a", TNumber), ("c", TString))
    val fn1t = TObj(Map("x" -> TNumber))
    val tfn1 = TFunction(fn1xs, fn1t)
    
    val fn2xs = List(("a", TNumber), ("b", TString))
    val fn2t = TObj(Map("x" -> TNumber, "y" -> TBool))
    val tfn2 = TFunction(fn2xs, fn2t)
    
    // val ans = TFunction(List(("a", TNumber)), TObj(Map("x" -> TNumber, "y" -> TBool)))
    // ^^ We had this before, but I think it should be as written below.
    val ans = TFunction(List(("a", TNumber), ("b", TString)), TObj(Map("x" -> TNumber)))
    
    val result = tfn2 &:& tfn1
    assert(ans =:= result.get)
  }
  
  // You probably want to write some tests for typeInfer and step.
  
  
}
