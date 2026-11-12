package course.wartimiser

import scala.quoted.*

object CopyChain extends Wartimisation:

  override def treeMap(using q: Quotes): q.reflect.TreeMap =
    import q.reflect.*

    val helper = Helper[q.type](using q)
    import helper.*

    new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        // println(
        //   "======================================================================="
        // )
        // println(tree.show(using Printer.TreeShortCode))
        // println(
        //   "-----------------------------------------------------------------------"
        // )
        // println(tree.show(using Printer.TreeStructure))
        // println(
        //   "======================================================================="
        // )

        tree match
          case CopyChainArgs(target, args, chainedArgs) =>
            val finalArgs = chooseLatestArgs(args, chainedArgs)
            val copyMethod = getCopyMethod(target)
            target
              .select(copyMethod)
              .appliedToArgs(finalArgs)
              .changeOwner(owner)

          case _ => super.transformTerm(tree)(owner)

  private class Helper[Q <: Quotes](using val q: Q):
    val Copy = "copy"
    val CopyDefault = "copy$default"
    import q.reflect.*

    def getCopyMethod(target: Term): Symbol =
      target.symbol
        .methodMember(Copy)
        .headOption
        .getOrElse(report.errorAndAbort("not a case class"))

    def chooseLatestArgs(
        originalArgs: List[Term],
        chainedArgs: List[List[Term]]
    ): List[Term] =
      originalArgs
        .zip(chainedArgs.transpose)
        .map:
          case (originalArg, argChain) =>
            argChain
              .find(arg =>
                !(arg.symbol.flags.is(Flags.Synthetic) && arg.symbol.name
                  .contains(CopyDefault))
              )
              .getOrElse(originalArg)

    private def resolve(args: List[Term], valDefs: Seq[Statement]): List[Term] =
      val valExprs = valDefs
        .collect:
          case ValDef(ident, _, Some(expr)) => ident -> expr
        .toMap

      args.map:
        case term @ Ident(ident) => valExprs.getOrElse(ident, term)
        case NamedArg(name, term @ Ident(ident)) =>
          valExprs.getOrElse(ident, term)
        case term => term

    object CopyChainArgs:

      def unapply(term: Term): Option[(Term, List[Term], List[List[Term]])] =
        term match
          case Apply(Select(target, Copy), args) =>
            Some(target, args, List.empty)

          case Block(
                List(
                  ValDef(
                    valDefIdent,
                    _,
                    Some(CopyChainArgs(target, args, chainedArgs))
                  ),
                  valDefs*
                ),
                Apply(Select(Ident(copyInvokationIdent), Copy), copyArgs)
              ) if valDefIdent == copyInvokationIdent =>
            Some(target, args , resolve(copyArgs, valDefs) :: chainedArgs)

          case Block(valDefs, Apply(Select(target, Copy), args)) =>
            Some(target, resolve(args, valDefs), List.empty)

          case _ => None
