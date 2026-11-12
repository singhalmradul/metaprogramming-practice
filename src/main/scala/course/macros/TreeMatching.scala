package course.macros

import quoted.*


inline def treeMatching[A](inline value: A): Unit =
  ${ treeMatchingImpl('value) }

def treeMatchingImpl[A: Type](value: Expr[A])(using q: Quotes): Expr[Unit] =
  import q.reflect.*

  val term = value.asTerm

  println("=====================================debug start==========================================")

  println(term.show(using Printer.TreeStructure))

  term match
    case Inlined(None, Nil, Apply(Ident(funcName), args)) =>
      println(s"$funcName with args ${args.map(_.show).mkString(", ")}")


  println("======================================debug end===========================================")

  '{ () }