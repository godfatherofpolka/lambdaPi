package dependent

import scala.util.Try

sealed abstract class InferrableTerm {
	def eval(d : Env) : Value = this match {
		case Ann(e, _) => e.eval(d)
    case Star => VStar
    case Pi(t,s) => VPi(t.eval(d), x => s.eval(x :: d))
		case Free(x) => x.free
		case Bound(i) => d(i)
		case App(e,f) => (e.eval(d)).app(f.eval(d))
	}
	
	def infer(c : Context) : Result[Type] = infer(0, c)
	
	def infer(i : Int, c : Context) : Result[Type] = this match {
		case Ann(e, rho) => for(
        _ <- rho.check(i, c, VStar);
        tau = rho.eval(List());
        _ <- e.check(i, c, tau)
    ) yield tau
    case Star => returnResult(VStar)
    case Pi(r,s) => for(
        _ <- r.check(i, c, VStar);
        tau = r.eval(List());
        x = s.subst(0, Free(Local(i)));
        update = (Local(i), tau);
        _ <- x.check(i+1, c+update, VStar)
        ) yield VStar
		case Bound(_) => ???
		case Free(x) => c.get(x) match {
			case Some(tau) => returnResult(tau)
			case None => throwError("unknown identifier")
		}
		case App(e,f) => e.infer(i, c).flatMap({
				case VPi(t,s) => for(
					_ <- f.check(i,c,t) ) yield s(f.eval(List()))
				case _ => throwError("illegal application")
			})
	}
	
	def subst(i : Int, r : InferrableTerm) : InferrableTerm = this match {
		case Ann(e, tau) => Ann(e.subst(i, r), tau)
    case Star => Star
    case Pi(t,s) => Pi(t.subst(i,r), s.subst(i+1,r))
		case Bound(j) => if (i==j) r else Bound(j)
		case Free(y) => Free(y)
		case App(e, f) => App(e.subst(i,r), f.subst(i,r))
	}
}
case class Ann(e : CheckableTerm, tau : CheckableTerm) extends InferrableTerm
case object Star extends InferrableTerm
case class Pi(t : CheckableTerm, s : CheckableTerm) extends InferrableTerm
case class Bound(i : Int) extends InferrableTerm
case class Free(n : Name) extends InferrableTerm
case class App(e : InferrableTerm, f : CheckableTerm) extends InferrableTerm

sealed abstract class CheckableTerm {
	def eval(d: Env) : Value = this match {
		case Inf(i) => i.eval(d)
		case Lam(e) => VLam(x => e.eval(x :: d))
	}
	
	def check(i : Int, c : Context, tau : Type) : Result[Unit] = (this, tau) match {
		case (Inf(e), _) =>
		// TODO: how to do unless in Scala?
		  val result = for (
			  sigma <- e.infer(i,c)
			  if (tau.quote == sigma.quote)) yield ()
		  result.orElse({throwError("type mismatch, expected: " + tau.quote + " for " + e)})
		case (Lam(e), VPi(t,s)) => 
			val x = e.subst(0, Free(Local(i)))
			val update = ((Local(i), t))
			x.check(i+1, c+update, s(Local(i).free))		
		case (_, _) => throwError("type mismatch")
	}
	
	def subst(i : Int, r : InferrableTerm) : CheckableTerm = this match {
		case Inf(e) => Inf(e.subst(i,r))
		case Lam(e) => Lam(e.subst(i+1, r))
	}
}
case class Inf(e : InferrableTerm) extends CheckableTerm
case class Lam(e : CheckableTerm) extends CheckableTerm

sealed abstract class Name {
	def free : Value = VNeutral(NFree(this))
	
	def boundfree(i : Int) : InferrableTerm = this match {
		case Quote(k) => Bound(i-k-1)
		case _ => Free(this)
	}
}
case class Global(s : String) extends Name
case class Local(i : Int) extends Name
case class Quote(i : Int) extends Name

sealed abstract class Value {
	def app(v : Value) : Value = this match {
		case VLam(f) => f(v)
    case VStar => ???
    case VPi(_,_) => ???
		case VNeutral(n) => VNeutral(NApp(n,v))   
	}
	
	def quote : CheckableTerm = quote(0)
	
	def quote(i : Int) : CheckableTerm = this match {
		case VLam(f) => Lam(f(Quote(i).free).quote(i+1))
    case VStar => Inf(Star)
    case VPi(v,f) => Inf(Pi(v.quote(i),f(Quote(i).free).quote(i+1) ))
		case VNeutral(n) => Inf(n.quote(i))
	}
}
case class VLam(f : Value => Value) extends Value
case object VStar extends Value
case class VPi(v : Value, f : Value => Value) extends Value
case class VNeutral(n : Neutral) extends Value

sealed abstract class Neutral {
	def quote(i : Int) : InferrableTerm = this match {
		case NFree(x) => x.boundfree(i)
		case NApp(n, v) => App(n.quote(i), v.quote(i))
	}
}
case class NFree(n : Name) extends Neutral
case class NApp(n : Neutral, v : Value) extends Neutral
