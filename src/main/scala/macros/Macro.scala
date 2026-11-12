package macros

import scala.quoted.*

inline def `macro`(a: Int): Int =
  ${ macroImpl('a) }

def macroImpl(a: Expr[Int])(using q: Quotes): Expr[Int] =
  import q.reflect.*

  val defSymbol = Symbol.newMethod(
    Symbol.spliceOwner,
    "f",
    MethodType(List("a"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
  )

  val defDef = DefDef(defSymbol, argss =>
    given Quotes = defSymbol.asQuotes
    val List(List(aTerm)) = argss
    val valSym = Symbol.newVal(Symbol.spliceOwner, "b", TypeRepr.of[Int], Flags.EmptyFlags, Symbol.noSymbol)
    Some(Block(List(ValDef(valSym, Some(aTerm.asInstanceOf[Term]))), Ref(valSym)))
  )

  Block(List(defDef), Ref(defSymbol).appliedTo(a.asTerm)).asExprOf[Int]
