package course.temp

import quoted.*
import course.wartimiser.*


object CopyChain extends Wartimisation:
  override def treeMap(using q: Quotes): q.reflect.TreeMap =
    import q.reflect.*
    val helper = new Helper[q.type](using q)
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

    def chooseLatestArgs(targetArgs: List[Term], chainArgs: List[List[Term]]): List[Term] =
      targetArgs.zip(chainArgs.transpose).map: // List[(Term, List[Term])]
        case (targetArg, argChain) =>
          argChain
            .findLast(arg => !arg.symbol.flags.is(Flags.Synthetic) || !arg.symbol.name.contains("copy$default$"))
            .getOrElse(targetArg)

    private def resolve(funcArgs: List[Term], valueDefs: Seq[Statement]): List[Term] =
        // map[value name, value expression]
        val valueExpressions = valueDefs
          .collect:
            case ValDef(name, _, Some(expression)) => name -> expression
          .toMap

        funcArgs.map:
          case term @ Ident(name) => valueExpressions.getOrElse(name, term)
          // `age = intermediateVal` becomes `age = 2 + 3`
          case NamedArg(argName, term @ Ident(name)) =>
            NamedArg(argName, valueExpressions.getOrElse(name, term))
          // anything else is left intact
          case term => term
    object CopyChainArgs:
      def unapply(term: Term): Option[(Term, List[Term], List[List[Term]])] =
        term match
          // base case: target.copy(arguments)
          case Apply(Select(target, "copy"), targetArgs) =>
            Some((target, targetArgs, List())) // no copy chain
          case Block(
            List(
              ValDef(localVal, _, Some(CopyChainArgs(target, targetArgs, innerChainArgs))), // final target of the copy call (is computed recursively)
              otherVals* // local value definitions, may be used in the last copy call
            ), // value defs
            Apply(Select(Ident(finalTarget), "copy"), args) // function application
          ) if localVal == finalTarget =>
            val localArgs = resolve(args, otherVals)
            Some((target, targetArgs, innerChainArgs :+ localArgs))
          // 2-chain copy: target.copy(args).copy(args2)
          case Block(
            List(intermediateVals*),
            Apply(Select(target, "copy"), targetArgs)
          ) =>
            Some((target, resolve(targetArgs, intermediateVals), List()))
          case _ => None
