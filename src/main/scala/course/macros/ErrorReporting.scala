package course.macros

import quoted.*

inline def reportErrors(a: Int, b: Int): Int =
  ${reportErrorsImpl('a, 'b)}

def reportErrorsImpl(aExpr: Expr[Int], bExpr: Expr[Int])(using q: Quotes): Expr[Int] =
  import q.reflect.*

  val a = aExpr.valueOrAbort
  val b = bExpr.valueOrAbort
  if a < 0 then
    report.errorAndAbort(s"$a is negative", aExpr)
  if b < 0 then
    report.errorAndAbort(s"$a is negative", bExpr)

  Expr(a + b)


inline def reportErrors1(a: Int, b: Int): Int =
  ${reportErrorsImpl1('a, 'b)}

def reportErrorsImpl1(aExpr: Expr[Int], bExpr: Expr[Int])(using Quotes): Expr[Int] =
  import quotes.reflect.*

  val a = aExpr.valueOrAbort
  val b = bExpr.valueOrAbort
  if a < 0 then
    report.error(s"$a is negative", aExpr)
  if b < 0 then
    report.error(s"$a is negative", bExpr)

  Expr(a + b)