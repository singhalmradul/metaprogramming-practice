package macros

import scala.quoted.*

inline def angleClockMacro(hour: Int, minutes: Int): Double =
  ${ angleClockMacroImpl('hour, 'minutes) }

def angleClockMacroImpl(hour: Expr[Int], minutes: Expr[Int])(using q: Quotes): Expr[Double] =
  import q.reflect.*

  val defSymbol = Symbol.newMethod(
    Symbol.spliceOwner,
    "angleClock",
    MethodType(
      paramNames = List("hour", "minutes")
    )(
      paramInfosExp =  _ => List(TypeRepr.of[Int], TypeRepr.of[Int]),
      resultTypeExp = _ => TypeRepr.of[Double]
    )
  )

  def defBody(args: List[List[Tree]]): Option[Term] = Some:
    given Quotes = defSymbol.asQuotes

    case class Val[A](`def`: ValDef, ref: Expr[A])

    def buildVal[A: Type](name: String, value: Expr[A]): Val[A] =
      val valSymbol = Symbol.newVal(
        parent = defSymbol,
        name = name,
        tpe = TypeRepr.of[A],
        flags = Flags.EmptyFlags,
        privateWithin = Symbol.noSymbol
      )

      val valDef = ValDef(valSymbol, Some(value.asTerm))
      val valRef = Ref(valSymbol).asExprOf[A]

      Val(valDef, valRef)

    val List(List(hourTerm, minutesTerm)) = args
    val hourRef = hourTerm.asExprOf[Int]
    val minutesRef = minutesTerm.asExprOf[Int]

    val Val(maxHoursDef, maxHoursRef) = buildVal("MaxHours", '{ 12 })
    val Val(oneHourDegreeDef, oneHourDegreeRef) = buildVal("OneHourDegree", '{ 360d / $maxHoursRef })
    val Val(maxMinutesDef, maxMinutesRef) = buildVal("MaxMinutes", '{ 60 })
    val Val(oneMinuteDegreeDef, oneMinuteDegreeRef) = buildVal("OneMinuteDegree", '{ 360d / $maxMinutesRef })
    val Val(oneMinuteHourDisplacementDegreeDef, oneMinuteHourDisplacementDegreeRef) =
      buildVal("OneMinuteHourDisplacementDegree", '{ $oneHourDegreeRef / $maxMinutesRef })

    val Val(hourDegreesDef, hourDegreesRef) =
      buildVal("hourDegrees", '{ ($hourRef % $maxHoursRef) * $oneHourDegreeRef })
    val Val(minutesHourDisplacementDegreesDef, minutesHourDisplacementDegreesRef) =
      buildVal("minutesHourDisplacementDegrees", '{ $minutesRef * $oneMinuteHourDisplacementDegreeRef })
    val Val(minuteDegreesDef, minuteDegreesRef) =
      buildVal("minuteDegrees", '{ ($minutesRef % $maxMinutesRef) * $oneMinuteDegreeRef })
    val Val(differenceDef, differenceRef) = buildVal(
      "difference", '{ math.abs($hourDegreesRef + $minutesHourDisplacementDegreesRef - $minuteDegreesRef) }
    )
    // val x =

    val result = '{
      val x = 5
      math.min($differenceRef, 360 - $differenceRef)
    }

    Block(
      stats = List(
        maxHoursDef,
        oneHourDegreeDef,
        maxMinutesDef,
        oneMinuteDegreeDef,
        oneMinuteHourDisplacementDegreeDef,
        hourDegreesDef,
        minutesHourDisplacementDegreesDef,
        minuteDegreesDef,
        differenceDef,
      ),
      expr = result.asTerm
    )


  val defDef = DefDef(defSymbol, defBody)
  val defRef = Ref(defSymbol)

  val defInvokation = defRef.appliedTo(hour.asTerm, minutes.asTerm)

  Block(
    stats = List(defDef),
    expr = defInvokation
  ).asExprOf[Double]
