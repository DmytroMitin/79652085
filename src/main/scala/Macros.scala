import scala.annotation.tailrec
import scala.quoted.*

object Macros {
  def evalTerm[T: Type](using quotes: Quotes)(term: quotes.reflect.Term): Option[quotes.reflect.Literal] = {
    import quotes.reflect.*

    println(s"term=${term.show}=${term.show(using Printer.TreeStructure)}")

    @tailrec
    def rec(tree: Term): Option[Literal] = tree match {
      case Block(_, e) => rec(e)
      case Inlined(_, _, e) => rec(e)
      case Typed(e, _) => rec(e)
      case literal: Literal => Some(literal)
      case _ => None
    }

    val res = rec(term.underlying)

    println(s"res=${res.map(_.show)}=${res.map(_.show(using Printer.TreeStructure))}")

    res
  }

  def evalExpr[T: Type](expr: Expr[T])(using quotes: Quotes): Option[Expr[T]] = {
    import quotes.reflect.*
    evalTerm[T](expr.asTerm).map(_.asExprOf[T])
  }

  def evalImpl[T: Type](expr: Expr[T])(using quotes: Quotes): Expr[T] = evalExpr[T](expr).get

  inline def eval[T](inline t: T): T = ${evalImpl[T]('t)}
}