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
    assert(TNull =:= (t2 &:& s2).get)
  }
  
  // You probably want to write some tests for typeInfer and step.
}
