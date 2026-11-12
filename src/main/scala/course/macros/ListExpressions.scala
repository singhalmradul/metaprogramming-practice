package course.macros

import quoted.*

inline def listExpressions(inline args: Int*): String =
  ${ listExpressionsImpl('args) }

def listExpressionsImpl(args: Expr[Seq[Int]])(using Quotes): Expr[String] =
  var result = args match
    case Varargs(seq) => seq.map(_.show).mkString(", ")

  Expr(result)

inline def expressionsList: List[String] =
  ${ expressionsListImpl }

def expressionsListImpl(using Quotes): Expr[List[String]] =
  val args = List(Expr("ss" * 2), Expr("Scala"))
  Expr.ofList(args)

inline def expressionsList1: Seq[String] =
  ${ expressionsListImpl1 }

def expressionsListImpl1(using Quotes): Expr[Seq[String]] =
  val args = List(Expr("ss" * 2), Expr("Scala"))
  Varargs(args)