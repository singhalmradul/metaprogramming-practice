package course.macros

import scala.quoted.*

inline def quoteMatching[A](inline obj: Any): String = ${ quoteMatchingImpl('obj) }

def quoteMatchingImpl(obj: Expr[Any])(using Quotes): Expr[String] =
  val result = obj match
    case '{None} => "none"
    case '{Some($x)} => s"some of $x"
    case '{
      type t1 <: AnyVal
      ($list: List[`t1`]).map[t2]($f).map[`t1`]($g)
    } =>
      s"list of ${Type.show[t1]} mapping to ${Type.show[t2]} and then back to ${Type.show[t1]}"
    case _ => "untracked"

  Expr(result)