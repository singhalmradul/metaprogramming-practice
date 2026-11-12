package course.macros


import quoted.*

inline def buildDefDef(inline int: Int, inline string: String, inline boolean: Boolean): Int =
  ${ buildDefDefImpl('int, 'string, 'boolean) }

def buildDefDefImpl(int: Expr[Int], string: Expr[String], boolean: Expr[Boolean])(using q: Quotes): Expr[Int] =
  import q.reflect.*

  val defSymbol = Symbol.newMethod(
    parent = Symbol.spliceOwner,
    name = "func",
    tpe = PolyType(
      paramNames = List("A")
    )(
      paramBoundsExp = _ => List(TypeBounds.upper(TypeRepr.of[Int])),
      resultTypeExp = typePramas =>
        MethodType(
          paramNames = List("int", "string", "boolean")
        )(
          paramInfosExp = _ => List(
            typePramas.param(0), TypeRepr.of[String], TypeRepr.of[Boolean]
          ),
          resultTypeExp = _ => TypeRepr.of[Int]
        )
    )
  )

  def defBody(args: List[List[Tree]]): Option[Term] = Some:
    given Quotes = defSymbol.asQuotes
    val List(listOfTypes, List(intTerm, stringTerm, booleanTerm)) = args
    val intExpr = intTerm.asExprOf[Int]
    val stringExpr = stringTerm.asExprOf[String]
    val booleanExpr = booleanTerm.asExprOf[Boolean]

    '{
      if $booleanExpr then
        $intExpr
      else
        $stringExpr.length
    }.asTerm

  end defBody

  val defDef = DefDef(defSymbol, defBody)
  val defRef = Ref(defSymbol)

  val defInvokation = defRef
    .appliedToType(TypeRepr.of[Int])
    .appliedTo(int.asTerm, string.asTerm, boolean.asTerm)

  Block(
    stats = List(defDef),
    expr = defInvokation
  ).asExprOf[Int]
