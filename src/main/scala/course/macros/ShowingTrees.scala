package course.macros

import scala.quoted.*

inline def debugExpr[A](inline value: A): Unit =
  ${ debugExprImpl('value) }

def debugExprImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Unit] =
  import q.reflect.*

  val term = value.asTerm
  val typeRepr = TypeRepr.of[A]

  println("=====================================debug start==========================================")

  println(term.show)
  println(term.show(using Printer.TreeShortCode))
  println(term.show(using Printer.TreeStructure))

  println(typeRepr.show)
  println(typeRepr.show(using Printer.TypeReprShortCode))
  println(typeRepr.show(using Printer.TypeReprStructure))

  println("======================================debug end===========================================")
  '{ () }