import scala.collection.Map

import scala.util.Failure
import scala.util.Try

package simply {
	case class TypeCheckerException(message: String) extends Exception(message)
}

package object simply {
	type Env = List[Value]
	type Context = Map[Name, Info]
	type Result[T] = Try[T]
	
	def throwError(msg : String) = Try(throw TypeCheckerException(msg))
	def returnResult[T](x : T) = Try(x)
  
  def tfree(alpha : String) = TFree(Global(alpha))
  def free(x : String) = Inf(Free(Global(x)))
}