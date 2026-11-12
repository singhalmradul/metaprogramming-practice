package course.macros

import scala.quoted.*


inline def firstMacro(int: Int, string: String): String =
  ${ firstMacroImpl('int, 'string) }

def firstMacroImpl(intExpr: Expr[Int], stringExpr: Expr[String])(using Quotes): Expr[String] =
  val int = intExpr.valueOrAbort
  val string = stringExpr.valueOrAbort

  val result =
    if int < string.length then
      string.take(int)
    else
      string + "~" * (string.length - int)

  Expr(result)

inline def firstMacroInlineArguments(inline int: Int, inline string: String): String =
  ${ firstMacroInlineArgumentsImpl('int, 'string) }

def firstMacroInlineArgumentsImpl(intExpr: Expr[Int], stringExpr: Expr[String])(using Quotes): Expr[String] =
  val int = intExpr.show
  val string = stringExpr.show

  val result = List(int, string).mkString("[", ", ", "]")

  Expr(result)