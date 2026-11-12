package course.macros


import scala.quoted.*

class Record(fields: Map[String, Any]) extends Selectable:
  def selectDynamic(key: String): Any = fields(key)


object Record:
  transparent inline def make(inline fields: (String, Any)*): Record =
    ${ makeImpl('fields) }

  private def makeImpl(fields: Expr[Seq[(String, Any)]])(using q: Quotes): Expr[Record] =
    import q.reflect.*
    val parent = TypeRepr.of[Record]

    inline def refine[A: Type](keyExpr: Expr[String], refinement: TypeRepr): Refinement =
      val key = keyExpr.valueOrAbort
      Refinement(refinement, key, TypeRepr.of[A])

    val result = fields match
      case Varargs(list) =>
        list.foldLeft(parent): (refinement, field) =>
          field match
            case '{ (($keyExpr: String), ($value: a)) } =>
              refine[a](keyExpr, refinement)
            case '{ ($keyExpr: String) -> ($value: a) } =>
              refine[a](keyExpr, refinement)
            case x: Expr[?] => report.errorAndAbort(s"unsupported type: ${x.show}")
      case _ => report.errorAndAbort(s"unexpected type: $fields")

    result.asType match
      case '[type a <: Record; `a`] => '{ Record($fields.toMap).asInstanceOf[a] }
