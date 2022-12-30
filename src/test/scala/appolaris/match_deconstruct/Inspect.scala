package appolaris.match_deconstruct

import scala.quoted.*


inline def inspect(inline x: Any): Any = ${ inspectCode('x) }
def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
  import quotes.reflect.*
  println("Expr: " + x.show)
  println("Tree: " + x.asTerm.show(using Printer.TreeStructure))
  x

