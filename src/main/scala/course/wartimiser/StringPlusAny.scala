package course.wartimiser

import scala.quoted.Quotes

object StringPlusAny extends Wartimisation:
  override def treeMap(using q: Quotes): q.reflect.TreeMap =
    import q.reflect.*

    new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        if !tree.isExpr then super.transformTerm(tree)(owner)
        else tree.asExpr match
          case '{ ($lhs: String) + ($rhs: a) } if !(TypeRepr.of[a] <:< TypeRepr.of[String]) =>
            report.errorAndAbort("adding anything else to string is forbidden")
          case _ => super.transformTerm(tree)(owner)