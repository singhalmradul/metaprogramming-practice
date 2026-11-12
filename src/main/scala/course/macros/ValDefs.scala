package course.macros

import quoted.*

inline def buildValueDef: Int =
  ${ buildValueDefImpl }

def buildValueDefImpl(using q: Quotes): Expr[Int] =
  import q.reflect.*

  val valSymbol = Symbol.newVal(
    parent = Symbol.spliceOwner,
    name = "x",
    tpe = TypeRepr.of[Int],
    flags = Flags.Lazy,
    privateWithin = Symbol.noSymbol
  )

  val valBody =
    given Quotes = valSymbol.asQuotes
    '{ "Scala".length }.asTerm

  val valDef = ValDef(valSymbol, Some(valBody))

  val valRef = Ref(valSymbol).asExprOf[Int]

  val finalValue = '{ $valRef * 4}.asTerm

  Block(
    stats = List(valDef),
    expr = finalValue
  ).asExprOf[Int]