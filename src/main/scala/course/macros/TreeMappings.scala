package course.macros

import scala.quoted.*

inline def transformCode[A](inline code: A): A =
  ${ transformCodeImpl('code) }

def transformCodeImpl[A: Type](code: Expr[A])(using q: Quotes): Expr[A] =
  import q.reflect.*

  val treeMap = new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      println("--------------------------------------term start--------------------------------------------")
      println(tree.show(using Printer.TreeStructure))
      println("--------------------------------------------------------------------------------------------")
      println(tree.show(using Printer.TreeShortCode))
      println("---------------------------------------term end---------------------------------------------")
      super.transformTerm(tree)(owner)

    override def transformStatement(tree: Statement)(owner: Symbol): Statement =
      println("------------------------------------statement start-----------------------------------------")
      println(tree.show(using Printer.TreeStructure))
      println("--------------------------------------------------------------------------------------------")
      println(tree.show(using Printer.TreeShortCode))
      println("------------------------------------statement end-------------------------------------------")
      super.transformStatement(tree)(owner)

  treeMap.transformTerm(code.asTerm)(Symbol.spliceOwner).asExprOf[A]


inline def flipBooleans[A](inline code: A): A =
  ${ flipBooleansImpl('code) }

def flipBooleansImpl[A: Type](code: Expr[A])(using q: Quotes): Expr[A] =
  import q.reflect.*

  val treeMap = new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      tree match
        case Literal(BooleanConstant(bool)) => Literal(BooleanConstant(!bool))
        case _ => super.transformTerm(tree)(owner)

    override def transformStatement(tree: Statement)(owner: Symbol): Statement =
      tree match
        case vd @ ValDef(_, typeTree, Some(rhs)) if typeTree.tpe =:= TypeRepr.of[Boolean] =>
          given Quotes = vd.symbol.asQuotes
          val newRhs = '{ !${ rhs.asExprOf[Boolean] } }.asTerm
          ValDef(vd.symbol, Some(newRhs))
        case dd @ DefDef(_, _, typeTree, Some(rhs)) if typeTree.tpe =:= TypeRepr.of[Boolean] =>
          given Quotes = dd.symbol.asQuotes
          val newRhs = '{ !${ rhs.asExprOf[Boolean] } }.asTerm
          DefDef(dd.symbol, _ => Some(newRhs))
        case _ => super.transformStatement(tree)(owner)

  treeMap.transformTerm(code.asTerm)(Symbol.spliceOwner).asExprOf[A]


inline def treeAccumulator[A](inline code: A): List[String] =
  ${ treeAccumulatorImpl('code) }

def treeAccumulatorImpl[A](code: Expr[A])(using q: Quotes): Expr[List[String]] =
  import q.reflect.*

  val treeAccumulator = new TreeAccumulator[List[String]]:
    override def foldTree(accumulator: List[String], tree: Tree)(owner: Symbol): List[String] =
      if tree.isExpr then
        tree.asExpr match
          case '{ println($x) } =>
            x.show :: accumulator
          case _ => foldOverTree(accumulator, tree)(owner)
      else foldOverTree(accumulator, tree)(owner)

  val result = treeAccumulator.foldTree(List.empty, code.asTerm)(Symbol.spliceOwner)

  Expr(result)
