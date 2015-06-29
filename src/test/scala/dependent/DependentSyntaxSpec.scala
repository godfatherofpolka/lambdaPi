package dependent

import org.scalatest._
import scala.util.Success

class DependentSyntaxSpec extends FlatSpec with Matchers {
  val id = Lam(Lam(Inf(Bound(0))))
  val typeIdVal = VPi(VStar, x => VPi(Local(0).free , y => x))
  val typeId = Inf(Pi(
                 Inf(Star), 
                 Inf(Pi(
                   Inf(Bound(0)), 
                   Inf(Bound(1))
                 ))
               ))
  val annId = Ann(id, typeId)
  val term1 = App(annId, free("Bool"))
  val term2 = App(term1, free("False"))
  
  val env1 = Map[Name, Value]( (Global("Bool"), VStar) )
  val env2 = env1 + ( (Global("False"), Global("Bool").free) )
  
  "term1" should "evaluate to Lam(Inf(Bound(0)))" in {
    term1.eval(List()).quote should === (Lam(Inf(Bound(0))))
  }
  
  "term2" should "evaluate to Inf(Free(Global(False)))" in {
    term2.eval(List()).quote should === (Inf(Free(Global("False"))))
  }
  
  "typeIdVal" should "quote to Inf(Pi(Inf(Star),Inf(Pi(Inf(Free(Local(0))),Inf(Bound(1))))))" in {
    typeIdVal.quote should === (Inf(Pi(Inf(Star),Inf(Pi(Inf(Free(Local(0))),Inf(Bound(1)))))))
  }
  
  "typeId" should "evaluate to Inf(Pi(Inf(Star),Inf(Pi(Inf(Bound(0)),Inf(Bound(1))))))" in {
    typeId.eval(List()).quote should === (Inf(Pi(Inf(Star),Inf(Pi(Inf(Bound(0)),Inf(Bound(1)))))))
  }
  
  "id" should "successfully type check for type typeIdVal" in {
    id.check(0, env1, typeIdVal) should === (Success(()))
  }
  
  "typeId" should "successfully type check for type VStar" in {
    typeId.check(0, env1, VStar) should === (Success(()))
  }
  
  "id" should "successfully type check for type typeId" in {
    id.check(0, env1, typeId.eval(List())) should === (Success(()))
  }
  
  "annId" should "have type Inf(Pi(Inf(Star),Inf(Pi(Inf(Bound(0)),Inf(Bound(1))))))" in {
    annId.infer(env1).get.quote should === (Inf(Pi(Inf(Star),Inf(Pi(Inf(Bound(0)),Inf(Bound(1)))))))
  }
  
  "term1" should "have type Inf(Pi(Inf(Free(Global(Bool))),Inf(Free(Global(Bool)))))" in {
    term1.infer(env1).get.quote should === (Inf(Pi(Inf(Free(Global("Bool"))),Inf(Free(Global("Bool"))))))
  }
  
  "term2" should "have type Inf(Free(Global(Bool)))" in {
    term2.infer(env2).get.quote should === (Inf(Free(Global("Bool"))))
  }
}
