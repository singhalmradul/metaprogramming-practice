package course.wartimiser

import scala.quoted.*

object CollectionOptimiser extends Wartimisation:

  override def treeMap(using q: Quotes): q.reflect.TreeMap =
    import q.reflect.*

    new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        val expr = Option.when(tree.isExpr)(tree).map(_.asExpr)
        val transformedExpr = expr.collect:
          case '{ type x <: Map; ($x: x[k, v]).get($key).getOrElse($value) } => '{ $x.getOrElse($key, $value) }
          case '{ type x <: Iterable; ($x: x[t]).filter($f).headOption } => '{ $x.find($f) }
          case '{ type x <: Iterable; ($x: x[t]).filter($f).size } => '{ $x.count($f) }
          case '{type x <: Iterable; ($x: x[t]).collect($f).headOption } => '{ $x.collectFirst($f) }
          case '{ type x <: Iterable; ($x: x[t1]).map[t2]($f).map[t3]($g) } => '{ $x.map(a => $g($f(a))) }
          case '{ type x <: Iterable; ($x: x[t]).map[Unit]($f) } => report.errorAndAbort("use `foreach` instead of map", tree.pos)

        transformedExpr
          .map(_.asTerm)
          .map(transformTerm(_)(owner))
          .getOrElse(super.transformTerm(tree)(owner))