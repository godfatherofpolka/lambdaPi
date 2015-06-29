

/**
 * @author Samuel Bucheli
 */

import scala.collection.Map

import scala.util.Failure
import scala.util.Try

package dependent {
  case class TypeCheckerException(message: String) extends Exception(message)
}

package object dependent {
  type Type = Value
  type Env = List[Value]
  type Context = Map[Name, Type]
  type Result[T] = Try[T]
  
  def throwError(msg : String) = Try(throw TypeCheckerException(msg))
  def returnResult[T](x : T) = Try(x)
  
  def free(x : String) = Inf(Free(Global(x)))
}
