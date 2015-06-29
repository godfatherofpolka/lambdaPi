package simply

import org.scalatest._

class SimplySyntaxSpec extends FlatSpec with Matchers {
  val id = Lam(Inf(Bound(0)))
  val const = Lam(Lam(Inf(Bound(1))))
  
  val vconst = VLam(x => VLam(y => Quote(0).free))

  val typeId = Fun(tfree("a"), tfree("a"))
  val annId = Ann(id, typeId)
  val term1 = App(annId, free("y"))
  
  val typeConst = Fun(Fun(tfree("b"),tfree("b")), Fun(tfree("a"), Fun(tfree("b"), tfree("b"))))
  val annConst = Ann(const, typeConst)
  val term2 = App(App(annConst, id), free("y"))
  
  val env1 : Context = Map[Name,Info]( (Global("y"), HasType(tfree("a"))), (Global("a"),HasKind(Star)))
  val env2 : Context = env1 + ( (Global("b"), HasKind(Star)) )
  
  "vconst.quote" should "be equal to const" in {
    vconst.quote should === (const)
  }
  
  "term1" should "evaluate to Inf(Free(Global(y)))" in {
    term1.eval(List()).quote should === (Inf(Free(Global("y"))))
  }
  
  "term2" should "evaluate to Lam(Inf(Bound(0)))" in {
    term2.eval(List()).quote should === (Lam(Inf(Bound(0))))
  }
  
  "term1" should "have type TFree(Global(a))" in {
    term1.infer(env1).get should === (TFree(Global("a")))
  }
  
  "term2" should "have type Fun(TFree(Global(b)),TFree(Global(b)))" in {
    term2.infer(env2).get should === (Fun(TFree(Global("b")),TFree(Global("b"))))
  }
}
