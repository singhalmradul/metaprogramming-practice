package course.wartimiser

import quoted.*

inline def wartimise[A](inline w: Wartimisation, inline ws: Wartimisation*)(inline block: A): A =
  ${ wartimiseImpl('w, 'ws, 'block) }

private def wartimiseImpl[A: Type](
  w: Expr[Wartimisation],
  ws: Expr[Seq[Wartimisation]],
  block: Expr[A]
)(using q: Quotes): Expr[A] =
  import q.reflect.*

  val wartimisations = w.valueOrAbort +: ws.valueOrAbort
  val treeMaps = wartimisations.map(_.treeMap)
  val finalTree = treeMaps.foldLeft(block.asTerm): (currentTree : Tree, treeMap: TreeMap) =>
    treeMap.transformTree(currentTree)(Symbol.spliceOwner)

  finalTree.asExprOf[A]