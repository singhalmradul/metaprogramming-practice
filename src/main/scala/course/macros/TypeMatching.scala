package course.macros

import scala.quoted.*

inline def typeMatching[A]: String =
  ${ typeMatchingImpl[A] }

def typeMatchingImpl[A: Type](using Quotes): Expr[String] =
  val `type` = Type.of[A]
  val result = `type` match
    case '[None.type] => "none"
    case '[Some[a]] => s"some of ${Type.show[a]}"
    case '[a => b] => s"function from ${Type.show[a]} to ${Type.show[b]}"
    case '[type a <: AnyVal; (`a`, b, `a`)] => s"tuple of ${Type.show[a]}, ${Type.show[b]} and ${Type.show[a]}"
    case _ => "untracked"

  Expr(result)