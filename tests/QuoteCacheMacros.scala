import scala.quoted.*

object QuoteCacheMacros:
  inline def twice[T](inline value: T): (T, T) = ${ twiceImpl[T]('value) }

  def twiceImpl[T: Type](value: Expr[T])(using Quotes): Expr[(T, T)] =
    '{
      val local = $value
      def get: T = local
      (get, get)
    }
